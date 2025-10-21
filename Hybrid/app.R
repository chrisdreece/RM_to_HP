# app.R
library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(slider)
library(DT)
library(readr)
library(jsonlite)
library(scales)
library(tibble)

`%||%` <- function(x, y) if (!is.null(x)) x else y

# -------------------------------------------------------------------
# Constants / Defaults
# -------------------------------------------------------------------
# Position fields the user can toggle (these will be ADDED to base_cols)
position_fields_choices <- c(
  'Tier1.Direct.Indirect','Job.Discipline','Level','FLSA.Status','Leader.or.IC',
  'Title','Business.Unit','Department','MSA','Country','Skill.Domain'
)

# Always-on fields (not shown to the user, not toggleable)
base_cols <- c('People.NodeID','Business.Area.Descr','EmployeeContractor')

# Saved models directory
models_dir <- "models"
dir.create(models_dir, showWarnings = FALSE, recursive = TRUE)

# -------------------------------------------------------------------
# Data that can be read once (assignment file)
# -------------------------------------------------------------------
raw_assignments <- read.csv('data/Space & RMS Assignments_20250916.csv') %>%
  filter(Assignment.Status %in% c('Firm','High Potential') & Resource.Type != 'Placeholder') %>%
  filter(Assignment.Org2 == 'Space') %>%
  mutate(Resource.Type = ifelse(Resource.Type == 'Pooled Services', 'SWA', Resource.Type)) %>%
  rename('Assignment.Tier1DirectIndirect' = 'Assignment.Org4')

month_cols_all <- grep("^X[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{2}$", names(raw_assignments), value = TRUE)

# -------------------------------------------------------------------
# UI
# -------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Supply + SWA Demand Summary"),
  sidebarLayout(
    sidebarPanel(
      h4("Position Fields"),
      checkboxGroupInput(
        inputId = "position_cols",
        label   = NULL,
        choices = position_fields_choices,
        selected = position_fields_choices  # all selected by default
      ),
      tags$hr(),
      sliderInput("rollingMonths", "Rolling Months", min = 1, max = 8, value = 3, step = 1),
      sliderInput("roundingThreshold", "Rounding Threshold", min = 0.50, max = 0.95, value = 0.70, step = 0.05),
      actionButton("run_btn", "Run", class = "btn btn-primary")
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
          DTOutput("summary_tbl")
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

# -------------------------------------------------------------------
# SERVER
# -------------------------------------------------------------------
server <- function(input, output, session) {
  
  # ============== RUN MODEL PIPELINE ==============
  result_tbl <- reactiveVal(NULL)
  n_groups_built <- reactiveVal(NA_integer_)
  last_params <- reactiveVal(NULL)
  
  observeEvent(input$run_btn, {
    withProgress(message = "Running Model", value = 0, {
      incProgress(0.05, detail = "Reading inputs & data")
      
      # Effective 'cols' = base + selected
      cols <- c(base_cols, input$position_cols)
      
      rollingMonths     <- input$rollingMonths
      roundingThreshold <- input$roundingThreshold
      
      # --- skill domain mapping + employees + reqs ---
      skillDomain <- read.csv('data/skillDomain.csv') %>%
        rename('People.NodeID' = 'People.Node.ID')
      
      employees <- read.csv('data/2025-09-16 Employees Space_RMS.csv') %>%
        filter(!(PeopleNodeID %in% c('Tier1 Placeholder Folder'))) %>%
        mutate(People.NodeID = suppressWarnings(as.integer(PeopleNodeID))) %>%
        left_join(skillDomain, by = 'People.NodeID') %>%
        mutate(Skill.Domain = ifelse(is.na(Skill.Domain), 'Not Specified', Skill.Domain)) %>%
        filter(Business.Area.Descr == 'Space') %>%
        mutate(Source = 'HRMS') %>%
        mutate(EmployeeContractor = ifelse(
          Workforce.Type %in% c('Contractor','Independent Contractor','Leased Labor'),
          'Contractor','Employee'
        )) %>%
        select(any_of(cols), Source)
      
      reqs <- read.csv('data/2025-09-16 Requisitions Space_RMS.csv') %>%
        filter(Business.Area.Descr == 'Space') %>%
        mutate(EmployeeContractor = ifelse(
          Workforce.Type %in% c('Contractor','Independent Contractor','Leased Labor'),
          'Contractor','Employee'
        ),
        Skill.Domain = 'Not Applicable',
        Source = 'Reqs') %>%
        select(any_of(cols), Source)
      
      assignments <- raw_assignments %>% filter(AG.Program.Type != 'Pooled Services')
      month_cols <- intersect(month_cols_all, names(assignments))
      
      # Collapse assignment rows to People.NodeID x Resource.Type
      assignments_people <- assignments %>%
        filter(Resource.Type %in% c('Contractor','Employee','Requisition')) %>%
        group_by(People.NodeID, Resource.Type) %>%
        summarise(
          Level.Group = dplyr::first(Level.Group[!is.na(Level.Group)]),
          across(all_of(month_cols), ~ sum(.x, na.rm = TRUE), .names = "{.col}")
        ) %>%
        ungroup() %>%
        pivot_longer(
          cols = -c(People.NodeID, Resource.Type, Level.Group),
          names_to = "Date",
          values_to = "Metric"
        ) %>%
        mutate(
          Date = str_replace(Date, "^X(\\d{1,2})\\.(\\d{1,2})\\.(\\d{2})$", "20\\3-\\1-\\2"),
          Date = ymd(Date),
          Date = ceiling_date(Date, "month") - days(1)
        ) %>%
        arrange(People.NodeID, Date) %>%
        filter(Date <= as.Date("2027-03-31"))
      
      incProgress(0.35, detail = "Merging employees/reqs with assignments")
      
      emp_reqs <- bind_rows(employees, reqs)
      
      # Cross join to ensure every IntervalDate exists for each person/row
      dates <- tibble(IntervalDate = unique(assignments_people$Date)) %>%
        mutate(IntervalDate = as.Date(IntervalDate))
      
      demand <- tidyr::crossing(emp_reqs, dates) %>%
        left_join(assignments_people, by = c("People.NodeID", "IntervalDate" = "Date")) %>%
        mutate(Metric = coalesce(Metric, 0)) %>%
        group_by(IntervalDate, Source, across(all_of(setdiff(cols, "People.NodeID")))) %>%
        summarise(
          Headcount = dplyr::n(),
          Metric    = sum(Metric, na.rm = TRUE),
          .groups   = "drop"
        ) %>%
        arrange(IntervalDate)
      
      # Position key (deterministic)
      position_fields <- setdiff(names(demand), c("IntervalDate", "Metric"))
      positionKeyMapping <- demand %>%
        distinct(across(all_of(position_fields))) %>%
        arrange(across(all_of(position_fields))) %>%
        mutate(PositionKey = row_number())
      
      demand <- demand %>%
        left_join(positionKeyMapping, by = position_fields) %>%
        arrange(PositionKey, IntervalDate) %>%
        select(PositionKey,
               IntervalDate,
               Tier1 = any_of("Tier1.Direct.Indirect"),
               Source, EmployeeContractor, Headcount, Metric)
      
      n_groups_built(length(unique(positionKeyMapping$PositionKey)))
      
      incProgress(0.60, detail = "Rolling averages & rounding (Supply/Reqs)")
      
      demand <- demand %>%
        group_by(PositionKey) %>%
        arrange(IntervalDate, .by_group = TRUE) %>%
        mutate(
          RollingAvg = slide_dbl(
            Metric,
            ~ mean(.x, na.rm = TRUE),
            .before = 0,
            .after  = rollingMonths - 1,
            .complete = FALSE
          ),
          Demand = if_else(
            RollingAvg - floor(RollingAvg) >= roundingThreshold,
            ceiling(RollingAvg),
            floor(RollingAvg)
          )
        ) %>%
        ungroup()
      
      demand <- demand %>%
        group_by(PositionKey) %>%
        arrange(IntervalDate, .by_group = TRUE) %>%
        mutate(
          Transaction  = if_else(row_number() == 1, Demand - Headcount, Demand - lag(Demand)),
          AdjHeadcount = Headcount + cumsum(Transaction)
        ) %>%
        ungroup()
      
      demand <- demand %>%
        group_by(PositionKey) %>%
        arrange(IntervalDate, .by_group = TRUE) %>%
        mutate(StartHeadcount = if_else(row_number() == 1, Headcount, lag(AdjHeadcount))) %>%
        ungroup()
      
      summary_by_date_simple <- demand %>%
        group_by(IntervalDate) %>%
        summarise(
          StartingHeadcount       = sum(StartHeadcount, na.rm = TRUE),
          EndingHeadcount         = sum(AdjHeadcount,   na.rm = TRUE),
          Transactions            = sum(Transaction,    na.rm = TRUE),
          ReqTransactions         = sum(if_else(Source == "Reqs", Transaction, 0), na.rm = TRUE),
          EmployeeTransactions    = sum(if_else(Source == "HRMS" & EmployeeContractor == "Employee",  Transaction, 0), na.rm = TRUE),
          ContractorTransactions  = sum(if_else(Source == "HRMS" & EmployeeContractor == "Contractor", Transaction, 0), na.rm = TRUE),
          .groups = "drop"
        )
      
      incProgress(0.80, detail = "SWA layer & join")
      
      # ----- SWA layer (from assignments where Resource.Type coerced to SWA) -----
      assignments_swa <- raw_assignments %>%
        filter(Resource.Type == 'SWA')
      
      month_cols_swa <- intersect(month_cols_all, names(assignments_swa))
      
      assignments_swa <- assignments_swa %>%
        group_by(Assignment.Org2) %>%  # minimal grouping (can expand later)
        summarise(across(all_of(month_cols_swa), ~ sum(.x, na.rm = TRUE), .names = "{.col}")) %>%
        ungroup() %>%
        mutate(PositionKey = row_number()) %>%
        select(PositionKey, all_of(month_cols_swa)) %>%
        pivot_longer(
          cols = -c(PositionKey),
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
        rename('IntervalDate' = 'Date')
      
      assignments_swa <- assignments_swa %>%
        group_by(PositionKey) %>%
        arrange(IntervalDate, .by_group = TRUE) %>%
        mutate(
          RollingAvg = slide_dbl(
            Metric,
            ~ mean(.x, na.rm = TRUE),
            .before = 0,
            .after  = rollingMonths - 1,
            .complete = FALSE
          ),
          Demand = if_else(
            RollingAvg - floor(RollingAvg) >= roundingThreshold,
            ceiling(RollingAvg),
            floor(RollingAvg)
          )
        ) %>%
        ungroup()
      
      demand_swa <- assignments_swa %>%
        group_by(IntervalDate) %>%
        summarise(
          RawSWADemand     = round(sum(Metric)),
          RoundedSWADemand = sum(Demand)
        ) %>%
        ungroup()
      
      # Final result
      summary_by_date_simple <- summary_by_date_simple %>%
        mutate(IntervalDate = as.Date(IntervalDate)) %>%
        left_join(demand_swa, by = 'IntervalDate') %>%
        mutate(
          EndingHeadcount_Supply = EndingHeadcount,
          EndingHeadcount_SWA    = RoundedSWADemand,
          TotalDemand            = EndingHeadcount_Supply + EndingHeadcount_SWA
        ) %>%
        select(
          IntervalDate,
          StartingHeadcount,
          EndingHeadcount_Supply,
          EndingHeadcount_SWA,
          TotalDemand
        ) %>%
        arrange(IntervalDate)
      
      incProgress(0.95, detail = "Finalizing output")
      
      result_tbl(summary_by_date_simple)
      
      # Save params for Saved Models
      last_params(list(
        position_cols     = input$position_cols,
        rollingMonths     = rollingMonths,
        roundingThreshold = roundingThreshold
      ))
      
      incProgress(1)
    })
  })
  
  # ============== DISPLAY ==============
  output$groupInfo <- renderText({
    n <- n_groups_built()
    if (!is.na(n)) paste0("Positions (groups) built: ", n) else ""
  })
  
  output$summary_tbl <- renderDT({
    df <- result_tbl()
    req(df)
    
    # Make sure IntervalDate is Date, and format numerics with commas, 0 decimals
    df <- df %>% mutate(IntervalDate = as.Date(IntervalDate))
    num_cols <- names(df)[sapply(df, is.numeric)]
    
    dt <- datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 25, scrollX = TRUE)
    )
    
    if (length(num_cols) > 0) {
      dt <- formatCurrency(
        dt,
        columns   = num_cols,
        currency  = "",
        interval  = 3,
        mark      = ",",
        digits    = 0
      )
    }
    dt
  })
  
  # ============== SAVED MODELS BOARD ==============
  format_summary_table <- function(df){
    if (!inherits(df$IntervalDate, "Date")) {
      suppressWarnings({
        as_dt <- ymd(df$IntervalDate)
        df$IntervalDate <- ifelse(!is.na(as_dt), format(as_dt, "%Y-%m-%d"), as.character(df$IntervalDate))
      })
    } else {
      df$IntervalDate <- format(df$IntervalDate, "%Y-%m-%d")
    }
    num_cols <- sapply(df, is.numeric)
    if (any(num_cols)) {
      df[, num_cols] <- lapply(df[, num_cols, drop = FALSE], function(x) comma(x, accuracy = 1))
    }
    df
  }
  
  scan_models <- function() {
    if (!dir.exists(models_dir)) return(tibble())
    ids <- list.dirs(models_dir, full.names = FALSE, recursive = FALSE)
    if (length(ids) == 0) return(tibble())
    out <- lapply(ids, function(id) {
      meta_path <- file.path(models_dir, id, "meta.json")
      if (!file.exists(meta_path)) return(NULL)
      meta <- tryCatch(jsonlite::fromJSON(meta_path, simplifyVector = TRUE), error = function(e) NULL)
      if (is.null(meta)) return(NULL)
      tibble(
        id = id,
        timestamp = meta$timestamp %||% "",
        rolling_months = meta$rollingMonths %||% NA,
        rounding_threshold = meta$roundingThreshold %||% NA,
        position_fields = paste(meta$position_cols %||% character(), collapse = "; "),
        table_path = file.path(models_dir, id, "summary.csv")
      )
    })
    dplyr::bind_rows(out)
  }
  
  saved_models_data <- reactiveVal(scan_models())
  observeEvent(input$rescan_models, { saved_models_data(scan_models()) })
  
  observeEvent(input$save_model, {
    req(result_tbl(), last_params())
    res <- result_tbl()
    params <- last_params()
    tryCatch({
      id <- paste0("model_", format(Sys.time(), "%Y%m%dT%H%M%S", tz="UTC"))
      model_dir <- file.path(models_dir, id)
      dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
      
      # Write raw table (use ISO date)
      to_write <- res %>% mutate(IntervalDate = format(as.Date(IntervalDate), "%Y-%m-%d"))
      readr::write_csv(to_write, file.path(model_dir, "summary.csv"))
      
      # Write meta
      meta <- list(
        id = id,
        timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
        rollingMonths = params$rollingMonths,
        roundingThreshold = params$roundingThreshold,
        position_cols = params$position_cols
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
          " | ", timestamp
        )
      ) %>%
      select(Model, timestamp, id, position_fields)
    datatable(
      display,
      selection = "single",
      options = list(pageLength = 10, order = list(list(2, 'desc'))),
      rownames = FALSE
    )
  })
  
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
        "Position Fields: ", sel$position_fields, "\n",
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
