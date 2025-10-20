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
  arrange(EmployeeID, Date)
