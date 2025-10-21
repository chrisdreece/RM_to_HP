# app.R
library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(slider)
library(gdata)
library(scales)
library(DT)
library(readr)
library(jsonlite)
`%||%` <- function(x, y) if (!is.null(x)) x else y
# -----------------------------
# Parameters / Defaults
# -----------------------------
default_cols <- c(
  'People.NodeID','Business.Area.Descr','Tier1.Direct.Indirect','Job.Discipline','Level',
  'FLSA.Status','Leader.or.IC','Title','Business.Unit','Department','MSA','Country',
  'Skill.Domain','EmployeeContractor'
)
always_included <- c('People.NodeID','Business.Area.Descr','EmployeeContractor')
checkbox_candidates <- setdiff(default_cols, always_included)

models_dir <- "models"
dir.create(models_dir, showWarnings = FALSE, recursive = TRUE)

# Hardcoded Scope values
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

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  titlePanel("Supply-Based Models"),
  sidebarLayout(
    sidebarPanel(
      h4("Position Fields"),
      checkboxGroupInput(
        inputId = "cols_dynamic",
        label   = NULL,
        choices = checkbox_candidates,
        selected = setdiff(default_cols, always_included)
      ),
      tags$hr(),
      selectInput(
        "tier1_scope",
        label = "Scope",
        choices = tier1_choices,
        selected = "Overall"
      ),
      tags$hr(),
      sliderInput("rollingMonths","Rolling Months",min=1,max=8,value=3,step=1),
      sliderInput("roundingThreshold","Rounding Threshold",min=0.50,max=0.95,value=0.70,step=0.05),
      checkboxInput("adjustSupply",strong("Adjust Supply"),value=FALSE),
      actionButton("run","Run",class="btn-primary")
    ),
    mainPanel(
      tabsetPanel(
        id="tabs",
        tabPanel("Monthly Summary",
                 br(),
                 actionButton("save_model","Save Model"),
                 br(), br(),
                 tableOutput("summary_by_date_simple_head")),
        tabPanel("Saved Models",
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
                 tableOutput("saved_table_preview"))
      )
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session) {
  
  # Build the effective cols vector
  cols_reactive <- reactive({ c(always_included, input$cols_dynamic) })
  
  # Formatting helper for the summary table
  format_summary_table <- function(df){
    # Ensure date column prints nicely
    if (!inherits(df$IntervalDate,"Date")) {
      suppressWarnings({
        as_dt <- ymd(df$IntervalDate)
        df$IntervalDate <- ifelse(!is.na(as_dt), format(as_dt,"%Y-%m-%d"), as.character(df$IntervalDate))
      })
    } else {
      df$IntervalDate <- format(df$IntervalDate,"%Y-%m-%d")
    }
    # Format numerics with commas, no decimals
    num_cols <- sapply(df,is.numeric)
    if (any(num_cols)) {
      df[,num_cols] <- lapply(df[,num_cols,drop=FALSE], function(x) comma(x, accuracy=1))
    }
    df
  }
  
  # ------------------ Run the model (only on click) with progress UI ------------------
  results <- eventReactive(input$run, {
    withProgress(message = "Running model…", value = 0, {
      incProgress(0.05)
      
      cols <- cols_reactive()
      rollingMonths <- input$rollingMonths
      roundingThreshold <- input$roundingThreshold
      adjustedSupply <- ifelse(isTRUE(input$adjustSupply), 1, 0)
      scope_choice <- input$tier1_scope
      
      incProgress(0.10, detail = "Loading data")
      
      # ---------- LOAD ----------
      skillDomain <- read.csv('data//skillDomain.csv') %>%
        rename('People.NodeID'='People.Node.ID')
      
      employees <- read.csv('data//2025-09-16 Employees Space_RMS.csv') %>%
        filter(!(PeopleNodeID) %in% c('Tier1 Placeholder Folder')) %>%
        mutate(People.NodeID=as.integer(PeopleNodeID)) %>%
        left_join(.,skillDomain,by='People.NodeID') %>%
        mutate(Skill.Domain=ifelse(is.na(Skill.Domain),'Not Specified',Skill.Domain)) %>%
        filter(Business.Area.Descr=='Space')
      rm(skillDomain)
      
      reqs <- read.csv('data//2025-09-16 Requisitions Space_RMS.csv') %>%
        filter(Business.Area.Descr=='Space')
      
      assignments <- read.csv('data//Space & RMS Assignments_20250916.csv') %>%
        filter(Assignment.Status %in% c('Firm','High Potential') & Resource.Type!='Placeholder') %>%
        filter(Assignment.Org2=='Space')  %>%
        mutate(Resource.Type=ifelse(Resource.Type=='Pooled Services','SWA',Resource.Type))
      
      month_cols <- grep("^X[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{2}$", names(assignments), value = TRUE)
      
      incProgress(0.25, detail = "Transforming assignments")
      if (adjustedSupply==1){
        SWA <- assignments %>%
          filter(Resource.Type %in% c("SWA")) %>%
          group_by(Pooled.Services) %>%
          summarise(across(all_of(month_cols), ~ sum(.x, na.rm = TRUE), .names = "{.col}")) %>%
          ungroup() %>%
          rename("Assignment.Group" = "Pooled.Services") %>%
          pivot_longer(cols = all_of(month_cols), names_to = "Month", values_to = "Budget") %>%
          mutate(
            Date = str_replace(Month, "^X(\\d{1,2})\\.(\\d{1,2})\\.(\\d{2})$", "20\\3-\\1-\\2"),
            Date = ymd(Date),
            Date = ceiling_date(Date, "month") - days(1)
          ) %>%
          select(Assignment.Group, Date, Budget)
        
        PooledServiceUsage <- assignments %>%
          filter(AG.Program.Type == "Pooled Services") %>%
          group_by(Assignment.Group) %>%
          summarise(across(all_of(month_cols), ~ sum(.x, na.rm = TRUE), .names = "{.col}")) %>%
          ungroup() %>%
          pivot_longer(cols = all_of(month_cols), names_to = "Month", values_to = "Allocated") %>%
          mutate(
            Date = str_replace(Month, "^X(\\d{1,2})\\.(\\d{1,2})\\.(\\d{2})$", "20\\3-\\1-\\2"),
            Date = ymd(Date),
            Date = ceiling_date(Date, "month") - days(1)
          ) %>%
          select(Assignment.Group, Date, Allocated)
        
        ratios <- left_join(PooledServiceUsage,SWA,by=c('Assignment.Group','Date')) %>%
          mutate(ratio=ifelse(Allocated!=0 & !(is.na(Budget)),Budget/Allocated,0)) %>%
          select(Assignment.Group,Date,ratio)
        
        PooledEmps <- assignments %>%
          filter(AG.Program.Type == "Pooled Services") %>%
          select(People.NodeID, Assignment.Group, Resource.Type, all_of(month_cols)) %>%
          pivot_longer(cols = all_of(month_cols), names_to = "Month", values_to = "Value") %>%
          mutate(
            Date = str_replace(Month, "^X(\\d{1,2})\\.(\\d{1,2})\\.(\\d{2})$", "20\\3-\\1-\\2"),
            Date = ymd(Date),
            Date = ceiling_date(Date, "month") - days(1)
          ) %>%
          select(People.NodeID, Assignment.Group, Resource.Type, Date, Value) %>%
          arrange(People.NodeID, Date) %>%
          left_join(ratios, by = c("Date", "Assignment.Group")) %>%
          mutate(Metric = Value * ratio) %>%
          select(People.NodeID, Resource.Type, Date, Metric)
        
        assignments2 <- assignments %>%
          filter(Resource.Type %in% c('Contractor','Employee','Requisition') & AG.Program.Type!='Pooled Services') %>%
          group_by(People.NodeID,Resource.Type) %>%
          summarise(across(all_of(month_cols), ~ sum(.x, na.rm = TRUE), .names = "{.col}")) %>%
          ungroup() %>%
          pivot_longer(cols = -c(People.NodeID, Resource.Type), names_to = "Date", values_to = "Metric") %>%
          mutate(
            Date = str_replace(Date, "^X(\\d{1,2})\\.(\\d{1,2})\\.(\\d{2})$", "20\\3-\\1-\\2"),
            Date = ymd(Date),
            Date = ceiling_date(Date, "month") - days(1)
          ) %>%
          arrange(People.NodeID, Date) %>%
          filter(Date <= as.Date("2027-03-31"))
        
        PooledEmps <- PooledEmps %>% filter(Date <= as.Date("2027-03-31"))
        
        assignments_final <- assignments2 %>%
          rbind(.,PooledEmps) %>%
          group_by(People.NodeID,Resource.Type,Date) %>%
          summarise(Metric=sum(Metric), .groups = "drop")
        
      } else {
        assignments_final <- assignments %>%
          filter(Resource.Type %in% c('Contractor','Employee','Requisition')) %>%
          group_by(People.NodeID,Resource.Type) %>%
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
      }
      
      incProgress(0.60, detail = "Building demand")
      employees2 <- employees %>%
        mutate(Source='HRMS') %>%
        mutate(EmployeeContractor=ifelse(Workforce.Type %in% c('Contractor','Independent Contractor','Leased Labor'),'Contractor','Employee')) %>%
        select(all_of(cols),Source)
      
      reqs2 <- reqs %>%
        mutate(EmployeeContractor=ifelse(Workforce.Type %in% c('Contractor','Independent Contractor','Leased Labor'),'Contractor','Employee'),
               Skill.Domain='Not Applicable') %>%
        mutate(Source='Reqs') %>%
        select(all_of(cols),Source)
      
      emp_reqs <- rbind(employees2,reqs2)
      
      dates <- data.frame(IntervalDate=unique(assignments_final$Date))
      
      demand <- emp_reqs %>%
        cross_join(dates) %>%
        left_join(assignments_final, by = c("People.NodeID", "IntervalDate" = "Date")) %>%
        mutate(Metric = coalesce(Metric, 0)) %>%
        group_by(IntervalDate,Source,across(all_of(setdiff(cols, "People.NodeID")))) %>%
        summarise(Headcount=length(People.NodeID),Metric = sum(Metric, na.rm = TRUE), .groups = "drop") %>%
        arrange(IntervalDate, Source)
      
      position_fields <- setdiff(names(demand), c("IntervalDate","Metric"))
      positionKeyMapping <- demand %>%
        distinct(across(all_of(position_fields))) %>%
        arrange(across(all_of(position_fields))) %>%
        mutate(PositionKey = row_number())
      
      demand <- demand %>%
        left_join(positionKeyMapping, by = position_fields) %>%
        arrange(PositionKey, IntervalDate) %>%
        select(PositionKey, IntervalDate, Tier1=`Tier1.Direct.Indirect`, Source, EmployeeContractor, Headcount, Metric)
      
      # Scope filter right before the roll-up
      if (!is.null(scope_choice) && scope_choice != "Overall") {
        demand <- demand %>% filter(Tier1 == scope_choice)
      }
      
      incProgress(0.80, detail = "Rolling calculations")
      demand <- demand %>%
        group_by(PositionKey) %>%
        arrange(IntervalDate, .by_group = TRUE) %>%
        mutate(
          RollingAvg = slide_dbl(Metric, ~ mean(.x, na.rm = TRUE), .before = 0, .after = rollingMonths - 1, .complete = FALSE),
          Demand = if_else(RollingAvg - floor(RollingAvg) >= roundingThreshold, ceiling(RollingAvg), floor(RollingAvg))
        ) %>%
        ungroup() %>%
        group_by(PositionKey) %>%
        arrange(IntervalDate, .by_group = TRUE) %>%
        mutate(
          Transaction = if_else(row_number() == 1, Demand - Headcount, Demand - lag(Demand)),
          AdjHeadcount = Headcount + cumsum(Transaction),
          StartHeadcount = if_else(row_number() == 1, Headcount, lag(AdjHeadcount))
        ) %>%
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
      
      incProgress(0.95, detail = "Formatting output")
      
      list(
        summary = summary_by_date_simple,
        params = list(
          position_fields = cols,
          rollingMonths = rollingMonths,
          roundingThreshold = roundingThreshold,
          adjustedSupply = adjustedSupply,
          scope = scope_choice
        )
      )
    })
  }, ignoreInit = FALSE)
  
  # Live table (formatted)
  output$summary_by_date_simple_head <- renderTable({
    req(results())
    head(format_summary_table(results()$summary), 20)
  }, striped = TRUE, bordered = TRUE, spacing = "m")
  
  # ------------------ Saved Models (folder-per-model design) ------------------
  
  # Build a dataframe of saved models by scanning models/*/meta.json
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
        adjusted_supply = if (isTRUE(meta$adjustedSupply == 1)) "Adjusted" else "Non-Adjusted",
        rolling_months = meta$rollingMonths %||% NA,
        rounding_threshold = meta$roundingThreshold %||% NA,
        scope = meta$scope %||% "Overall",
        position_fields = paste(meta$position_fields %||% character(), collapse="; "),
        table_path = file.path(models_dir, id, "summary.csv")
      )
    })
    dplyr::bind_rows(out)
  }
  
  saved_models_data <- reactiveVal(scan_models())
  observeEvent(input$rescan_models, { saved_models_data(scan_models()) })
  
  # Save Model: create models/<id> with meta.json + summary.csv
  observeEvent(input$save_model, {
    req(results())
    tryCatch({
      id <- paste0("model_", format(Sys.time(), "%Y%m%dT%H%M%S", tz="UTC"))
      model_dir <- file.path(models_dir, id)
      dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
      
      # Write summary table (raw)
      readr::write_csv(results()$summary, file.path(model_dir, "summary.csv"))
      
      # Write meta
      meta <- list(
        id = id,
        timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz="UTC"),
        adjustedSupply = results()$params$adjustedSupply,
        rollingMonths = results()$params$rollingMonths,
        roundingThreshold = results()$params$roundingThreshold,
        scope = results()$params$scope,
        position_fields = results()$params$position_fields
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
  
  # Delete selected model (optional convenience)
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
          adjusted_supply,
          " | RM=", rolling_months,
          " | RT=", rounding_threshold,
          " | Scope=", scope,
          " | ", timestamp
        )
      ) %>%
      select(Model, timestamp, id)
    datatable(
      display,
      selection = "single",
      options = list(pageLength = 10, order = list(list(2, 'desc'))),
      rownames = FALSE
    )
  })
  
  # When a row is selected, show params & table
  observeEvent(input$saved_models_dt_rows_selected, {
    idx <- input$saved_models_dt_rows_selected
    df <- saved_models_data()
    if (length(idx) != 1 || nrow(df) < idx) return()
    sel <- df[idx, , drop = FALSE]
    
    # Params panel
    output$saved_params <- renderText({
      paste0(
        "Model ID: ", sel$id, "\n",
        "Timestamp: ", sel$timestamp, "\n",
        "Adjusted Supply: ", sel$adjusted_supply, "\n",
        "Rolling Months: ", sel$rolling_months, "\n",
        "Rounding Threshold: ", sel$rounding_threshold, "\n",
        "Scope: ", sel$scope, "\n",
        "Position Fields: ", sel$position_fields, "\n",
        "Table Path: ", sel$table_path
      )
    })
    
    # Table preview
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
