
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(slider)
library(gdata)

########### ADD IN UNMET DEMAND
assignments <- read.csv('data//Space & RMS Assignments_20250916.csv') %>%
  filter(Assignment.Status %in% c('Firm','High Potential') & Resource.Type!='Placeholder') %>%
  filter(Assignment.Org2=='Space')
month_cols <- grep("^X[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{2}$", names(assignments), value = TRUE)
### which employeeIDs are at least 75% allocated in assignments??
assignments <- assignments %>%
  filter(Resource.Type %in% c('Not Allocated')) %>%
  group_by(Assignment.Org4) %>%
  summarise(across(all_of(month_cols), ~ sum(.x, na.rm = TRUE), .names = "{.col}")) %>%
  ungroup()

unmetDemandByTier1 <- assignments %>%
  pivot_longer(
    cols = -c(Assignment.Org4),
    names_to = "Date",
    values_to = "Metric"
  ) %>%
  mutate(
    # Turn "X9.1.25" -> "2025-09-01"
    Date = str_replace(Date, "^X(\\d{1,2})\\.(\\d{1,2})\\.(\\d{2})$", "20\\3-\\1-\\2"),
    Date = lubridate::ymd(Date),
    Date = lubridate::ceiling_date(Date, "month") - lubridate::days(1)
  ) %>%
  arrange(Assignment.Org4, Date) %>%
  filter(Date <= as.Date("2027-03-31")) %>%
  rename('UnmetDemand'='Metric')

unmetDemandOverall <- unmetDemandByTier1 %>%
  group_by(Date) %>%
  summarise(UnmetDemand=sum(UnmetDemand)) %>%
  ungroup()

write.csv(unmetDemandByTier1,'UnmetDemand_Tier1.csv',row.names = FALSE)
write.csv(unmetDemandOverall,'UnmetDemand_Overall.csv',row.names = FALSE)
