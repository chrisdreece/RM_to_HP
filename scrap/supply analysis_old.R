library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)


### 
employees <- read.csv('data//2025-06-16 Employees.csv')
reqs <- read.csv('data//2025-09-16 Requisitions.csv')
assignments <- read.csv('data//Aero Assignments_20250916.csv') %>%
  filter(Assignment.Status %in% c('Firm','High Potential'))

month_cols <- grep("^([A-Z][a-z]{2}\\.[0-9]{2})$", names(assignments), value = TRUE)
### which employeeIDs are at least 75% allocated in assignments??
assignments <- assignments %>%
  filter(Resource.Type %in% c('Contractor','Employee')) %>%
  group_by(EmployeeID) %>%
  summarise(across(all_of(month_cols), ~ sum(.x, na.rm = TRUE), .names = "{.col}")) %>%
  ungroup()

assignments <- assignments %>%
  pivot_longer(
    cols = -EmployeeID,
    names_to = "Date",
    values_to = "Metric"
  ) %>%
  # Convert "Jan.25" → "2025-01-01" etc.
  mutate(
    # Remove any trailing periods and handle both month and year
    Date = str_replace_all(Date, "\\.", " "),
    # Parse as a date (assumes year is 20xx)
    Date = my(Date),                 # lubridate::my parses month-year
    Date = ceiling_date(Date, "month") - days(1)  # optional: make it end-of-month
  ) %>%
  arrange(EmployeeID, Date) %>%
  filter(Date>='2025-09-30')

# params you can tweak
under_thresh <- 0.9
min_consec   <- 8

assignments <- assignments %>%
  arrange(EmployeeID, Date) %>%
  group_by(EmployeeID) %>%
  # mark under-allocation and build run IDs of TRUE/FALSE streaks
  mutate(
    under = Metric < under_thresh,
    run_id_all = dplyr::consecutive_id(under),
    run_id = if_else(under, run_id_all, NA_integer_)
  ) %>%
  ungroup() %>%
  # find any under-allocation run(s) with length >= min_consec
  group_by(EmployeeID, run_id) %>%
  summarise(
    is_under_run = all(!is.na(run_id)),
    run_len = dplyr::n(),
    start_date = dplyr::first(Date),
    .groups = "drop"
  ) %>%
  filter(is_under_run, run_len >= min_consec) %>%
  group_by(EmployeeID) %>%
  summarise(elim_start = min(start_date), .groups = "drop") %>%
  # join back and create a carry-forward elimination flag
  right_join(assignments, by = "EmployeeID") %>%
  arrange(EmployeeID, Date) %>%
  group_by(EmployeeID) %>%
  mutate(
    EliminationFlag = if_else(!is.na(elim_start) & Date >= elim_start, 1L, 0L)
  ) %>%
  ungroup()

### remove underallocated employess
assignments_emp <- assignments %>%
  filter(EliminationFlag!=1) %>%
  select(-c('EliminationFlag','elim_start'))

### 
employees <- employees %>%
  #filter(Employee.Status=='Active') %>%
  select(EmployeeID=Employee.ID,Employee.Status)

### combined
dates<-data.frame(IntervalDate=unique(assignments_emp$Date))

demand <- employees %>%
  cross_join(dates) %>%
  left_join(assignments_emp, by = c("EmployeeID","IntervalDate" = "Date")) %>%
  group_by(EmployeeID) %>%
  arrange(IntervalDate, .by_group = TRUE) %>%
  mutate(
    Reduction = as.integer(is.na(Metric))
  ) %>%
  # remove rows after the first Reduction == 1
  filter(cumsum(Reduction) <= 1) %>%
  ungroup() %>%
  select(-Metric)

employeeReductionSummary <- demand %>%
  group_by(IntervalDate) %>%
  summarise(Headcount=length(EmployeeID),Reductions=sum(Reduction)) %>%
  ungroup()

### remove underallocated reqs
table(assignments$Resource.Type)



### where are we losing demand because an assignment is not allocated?
  ### are there Employees in assignments who don't show up in active? 
  ### only 1:
missingEmps <- assignments %>%
  filter(!(EmployeeID %in% unique(employees$EmployeeID)))

  ### how about the assignments that haven't been allocated?





