# app.R
library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(slider)
library(gdata)       # kept because it was in your original script
library(scales)
library(DT)
library(readr)
library(jsonlite)
`%||%` <- function(x, y) if (!is.null(x)) x else y

# -----------------------------
# Parameters / Defaults
# -----------------------------
demandCols_options <- c(
  'Assignment.Tier1DirectIndirect','Job.Discipline','Level','FLSA.Status','Leader.or.IC',
  'Title','Business.Unit','Department','MSA','Country','SWP.Strategic.Priority',
  'Assignment.Org2','Assignment.Org5','Assignment.Org6','Assignment.Org7','Assignment.Org8'
)

tier1_choices <- c(
  "Overall",
  "Space Engineering and Technology - Direct",
  "Space Operations - Direct",
  "Space Strategic & Missile Defense Systems - Direct",
  "Space Finance and Business Operations - Direct",
  "Space Safety & Quality Assurance - Direct",
  "Space Enterprise Performance - Direct",
  "Space Security Services - Direct",
  "Space Operations - Indirect",
  "Space Ignite - Direct",
  "Space National Security Space - Direct"
)

models_dir <- "models"
dir.create(models_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# Data load (once)
# -----------------------------
raw_assignments <- read.csv('data//Space & RMS Assignments_20250916.csv') %>%
  filter(Assignment.Status %in% c('Firm','High Potential') & Resource.Type!='Placeholder') %>%
  filter(Assignment.Org2=='Space') %>%
  mutate(Resource.Type = ifelse(Resource.Type=='Pooled Services','SWA',Resource.Type)) %>%
  rename('Assignment.Tier1DirectIndirect'='Assignment.Org4')

# infer month columns (e.g., X9.1.25)
month_cols_all <- grep("^X[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{2}$", names(raw_assignments), value = TRUE)

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  titlePanel("Demand-Based Model"),
  sidebarLayout(
    sidebarPanel(
      h4("Position Fields"),
      checkboxGroupInput(
        inputId = "demandCols",
        label   = NULL,
        choices = demandCols_options,
        selected = demandCols_options
      ),
      tags$hr(),
      sliderInput("rollingMonths", "Rolling Months", min = 1, max = 8, value = 3, step = 1),
      sliderInput("roundingThreshold", "Rounding Threshold", min = 0.5, max = 0.95, value = 0.7, step = 0.05),
      selectInput("scope", "Scope", choices = tier1_choices, selected = "Overall"),
      helpText("Note: 'Scope' applies only if 'Assignment.Tier1DirectIndirect' is included in Position Fields."),
      actionButton("run", "Run", class = "btn btn-primary")
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "Monthly Summary",
          br(),
          fluidRow(
            column(4, actionButton("save_model", "Save Model", class = "btn-success")),
            column(8, verbatimTextOutput("groupInfo"))
          ),
          br(),
          tableOutput("demandTable")
        ),
        tabPanel(
          "Saved Models",
          br(),
          fluidRow(
            column(6, actionButton("rescan_models","Rescan")),
            column(6, actionButton("delete_model","Delete Selected", class="btn-danger"))
          ),
          br(),
          DTOutput("saved_models_dt"),
          br(),
          h4("Saved Model Details"),
          verbatimTextOutput("saved_params"),
          br(),
          tableOutput("saved_table_preview")
        )
      )
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session) {
  
  # ---------- Core model: runs only on click, shows progress ----------
  run_model <- eventReactive(input$run, {
    withProgress(message = "Running Model...", value = 0, {
      incProgress(0.1)
      
      assignments <- raw_assignments %>% filter(AG.Program.Type != 'Pooled Services')
      month_cols <- intersect(month_cols_all, names(assignments))
      
      # Respect user-selected grouping fields exactly
      selected_fields <- input$demandCols
      
      if (length(selected_fields) > 0) {
        grouped <- assignments %>%
          group_by(across(all_of(selected_fields))) %>%
          summarise(across(all_of(month_cols), ~ sum(.x, na.rm = TRUE), .names = "{.col}")) %>%
          ungroup()
      } else {
        # Overall only (single group)
        grouped <- assignments %>%
          summarise(across(all_of(month_cols), ~ sum(.x, na.rm = TRUE), .names = "{.col}")) %>%
          mutate(.overall = "Overall")
      }
      incProgress(0.35)
      
      # Give every group its own PositionKey; keep Tier1 column only if it exists
      cols_to_keep <- c("PositionKey", intersect("Assignment.Tier1DirectIndirect", names(grouped)), month_cols)
      
      grouped <- grouped %>%
        mutate(PositionKey = dplyr::row_number()) %>%
        dplyr::select(all_of(cols_to_keep))
      
      # Long format + date handling
      assignments_long <- grouped %>%
        pivot_longer(
          cols = -c(PositionKey, tidyselect::any_of("Assignment.Tier1DirectIndirect")),
          names_to = "Date",
          values_to = "Metric"
        ) %>%
        mutate(
          Date = str_replace(Date, "^X(\\d{1,2})\\.(\\d{1,2})\\.(\\d{2})$", "20\\3-\\1-\\2"),
          Date = ymd(Date),
          Date = ceiling_date(Date, "month") - days(1)
        ) %>%
        arrange(PositionKey, Date) %>%
        filter(Date <= as.Date("2027-03-31")) %>%
        rename(IntervalDate = Date)
      
      incProgress(0.65)
      
      # Rolling average & demand rounding per group
      assignments_roll <- assignments_long %>%
        group_by(PositionKey) %>%
        arrange(IntervalDate, .by_group = TRUE) %>%
        mutate(
          RollingAvg = slide_dbl(
            Metric,
            ~ mean(.x, na.rm = TRUE),
            .before = 0,
            .after  = input$rollingMonths - 1,
            .complete = FALSE
          ),
          Demand = if_else(
            RollingAvg - floor(RollingAvg) >= input$roundingThreshold,
            ceiling(RollingAvg),
            floor(RollingAvg)
          )
        ) %>%
        ungroup()
      
      incProgress(0.85)
      
      # Apply Scope ONLY if Tier1 field exists and user didn't pick "Overall"
      can_scope <- "Assignment.Tier1DirectIndirect" %in% names(assignments_roll)
      scoped <- assignments_roll %>%
        {
          if (can_scope && !is.null(input$scope) && input$scope != "Overall")
            dplyr::filter(., Assignment.Tier1DirectIndirect == input$scope)
          else .
        }
      
      # Final monthly rollup
      demand <- scoped %>%
        group_by(IntervalDate) %>%
        summarise(
          RawDemand     = round(sum(Metric, na.rm = TRUE)),
          RoundedDemand = sum(Demand, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          LostDemand   = RawDemand - RoundedDemand,
          IntervalDate = as.Date(IntervalDate)
        ) %>%
        arrange(IntervalDate)
      
      incProgress(1)
      
      list(
        demand = demand,
        n_groups = dplyr::n_distinct(assignments_roll$PositionKey),
        params = list(
          demandCols = selected_fields,
          rollingMonths = input$rollingMonths,
          roundingThreshold = input$roundingThreshold,
          scope = input$scope,
          scope_applicable = can_scope
        )
      )
    })
  })
  
  # ---------- Formatting helpers ----------
  format_summary_table <- function(df){
    # Ensure IntervalDate prints nicely
    if (!inherits(df$IntervalDate,"Date")) {
      suppressWarnings({
        as_dt <- ymd(df$IntervalDate)
        df$IntervalDate <- ifelse(!is.na(as_dt), format(as_dt,"%Y-%m-%d"), as.character(df$IntervalDate))
      })
    } else {
      df$IntervalDate <- format(df$IntervalDate, "%Y-%m-%d")
    }
    # Format numerics with commas, no decimals
    num_cols <- sapply(df, is.numeric)
    if (any(num_cols)) {
      df[, num_cols] <- lapply(df[, num_cols, drop = FALSE], function(x) comma(x, accuracy = 1))
    }
    df
  }
  
  # ---------- Live outputs ----------
  output$groupInfo <- renderText({
    req(run_model())
    meta <- run_model()
    paste0("Positions (groups) built: ", meta$n_groups)
  })
  
  output$demandTable <- renderTable({
    req(run_model())
    df <- run_model()$demand %>%
      mutate(
        IntervalDate   = format(IntervalDate, "%Y-%m-%d"),
        RawDemand      = formatC(RawDemand, format = "f", digits = 0, big.mark = ","),
        RoundedDemand  = formatC(RoundedDemand, format = "f", digits = 0, big.mark = ","),
        LostDemand     = formatC(LostDemand, format = "f", digits = 0, big.mark = ",")
      )
    df
  }, striped = TRUE, bordered = TRUE, spacing = "m")
  
  # ------------------ Saved Models (folder-per-model design) ------------------
  
  scan_models <- function() {
    if (!dir.exists(models_dir)) return(tibble::tibble())
    ids <- list.dirs(models_dir, full.names = FALSE, recursive = FALSE)
    if (length(ids) == 0) return(tibble::tibble())
    out <- lapply(ids, function(id) {
      meta_path <- file.path(models_dir, id, "meta.json")
      if (!file.exists(meta_path)) return(NULL)
      meta <- tryCatch(jsonlite::fromJSON(meta_path, simplifyVector = TRUE), error = function(e) NULL)
      if (is.null(meta)) return(NULL)
      tibble::tibble(
        id = id,
        timestamp = meta$timestamp %||% "",
        rolling_months = meta$rollingMonths %||% NA,
        rounding_threshold = meta$roundingThreshold %||% NA,
        scope = meta$scope %||% "Overall",
        scope_applicable = if (isTRUE(meta$scope_applicable)) "Tier1 present" else "No Tier1",
        demand_cols = paste(meta$demandCols %||% character(), collapse = "; "),
        table_path = file.path(models_dir, id, "summary.csv")
      )
    })
    dplyr::bind_rows(out)
  }
  
  saved_models_data <- reactiveVal(scan_models())
  observeEvent(input$rescan_models, { saved_models_data(scan_models()) })
  
  # Save Model: write models/<id>/meta.json + summary.csv
  observeEvent(input$save_model, {
    req(run_model())
    res <- run_model()
    tryCatch({
      id <- paste0("model_", format(Sys.time(), "%Y%m%dT%H%M%S", tz="UTC"))
      model_dir <- file.path(models_dir, id)
      dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
      
      # Write summary table (raw, but ensure ISO date for IntervalDate)
      to_write <- res$demand %>%
        mutate(IntervalDate = format(as.Date(IntervalDate), "%Y-%m-%d"))
      readr::write_csv(to_write, file.path(model_dir, "summary.csv"))
      
      # Write meta
      meta <- list(
        id = id,
        timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
        rollingMonths = res$params$rollingMonths,
        roundingThreshold = res$params$roundingThreshold,
        scope = res$params$scope,
        scope_applicable = res$params$scope_applicable,
        demandCols = res$params$demandCols
      )
      write(jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE),
            file.path(model_dir, "meta.json"))
      
      showNotification("Model saved.", type="message", duration = 2)
      saved_models_data(scan_models())
      updateTabsetPanel(session, "tabs", selected = "Saved Models")
    }, error = function(e) {
      showNotification(paste("Save failed:", conditionMessage(e)), type="error", duration = 6)
    })
  })
  
  # Delete selected model
  observeEvent(input$delete_model, {
    sel <- input$saved_models_dt_rows_selected
    df <- saved_models_data()
    if (length(sel) != 1 || nrow(df) < sel) return()
    id <- df$id[sel]
    model_dir <- file.path(models_dir, id)
    tryCatch({
      if (dir.exists(model_dir)) unlink(model_dir, recursive = TRUE, force = TRUE)
      showNotification("Model deleted.", type="message", duration=2)
      saved_models_data(scan_models())
      output$saved_params <- renderText({ "" })
      output$saved_table_preview <- renderTable({ NULL })
    }, error = function(e) {
      showNotification(paste("Delete failed:", conditionMessage(e)), type="error", duration = 6)
    })
  })
  
  # Models list
  output$saved_models_dt <- renderDT({
    df <- saved_models_data()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No saved models yet."),
                       options = list(dom = 't'), rownames = FALSE))
    }
    display <- df %>%
      mutate(
        Model = paste0(
          "RM=", rolling_months,
          " | RT=", rounding_threshold,
          " | Scope=", scope,
          " | ", scope_applicable,
          " | ", timestamp
        )
      ) %>%
      select(Model, timestamp, id, demand_cols)
    datatable(
      display,
      selection = "single",
      options = list(pageLength = 10, order = list(list(2, 'desc'))),
      rownames = FALSE
    )
  })
  
  # On selection, show params & table preview
  observeEvent(input$saved_models_dt_rows_selected, {
    idx <- input$saved_models_dt_rows_selected
    df <- saved_models_data()
    if (length(idx) != 1 || nrow(df) < idx) return()
    sel <- df[idx, , drop = FALSE]
    
    output$saved_params <- renderText({
      paste0(
        "Model ID: ", sel$id, "\n",
        "Timestamp: ", sel$timestamp, "\n",
        "Rolling Months: ", sel$rolling_months, "\n",
        "Rounding Threshold: ", sel$rounding_threshold, "\n",
        "Scope: ", sel$scope, " (", sel$scope_applicable, ")\n",
        "Position Fields: ", sel$demand_cols, "\n",
        "Table Path: ", sel$table_path
      )
    })
    
    output$saved_table_preview <- renderTable({
      if (file.exists(sel$table_path)) {
        tbl <- suppressMessages(readr::read_csv(sel$table_path, show_col_types = FALSE))
        head(format_summary_table(tbl), 20)
      } else {
        data.frame(Error = "Saved table not found on disk.")
      }
    }, striped = TRUE, bordered = TRUE, spacing = "m")
  })
}

shinyApp(ui, server)
