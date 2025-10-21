library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(slider)
library(gdata)

### PARAMETERS
demandCols <- c('Assignment.Tier1DirectIndirect','Job.Discipline','Level','FLSA.Status','Leader.or.IC',
          'Title','Business.Unit','Department','MSA','Country','SWP.Strategic.Priority','Assignment.Org2','Assignment.Org5','Assignment.Org6',
          'Assignment.Org7','Assignment.Org8')
rollingMonths<-3
roundingThreshold<-.7

assignments <- read.csv('data//Space & RMS Assignments_20250916.csv') %>%
  filter(Assignment.Status %in% c('Firm','High Potential') & Resource.Type!='Placeholder') %>%
  filter(Assignment.Org2=='Space')  %>%
  ### Peter will change the value in the raw file, so you can remove this eventually
  mutate(Resource.Type=ifelse(Resource.Type=='Pooled Services','SWA',Resource.Type)) %>%
  rename('Assignment.Tier1DirectIndirect'='Assignment.Org4')

month_cols <- grep("^X[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{2}$", names(assignments), value = TRUE)

table(assignments$AG.Program.Type)

assignments <- assignments %>%
  filter(AG.Program.Type!='Pooled Services') %>%
  #filter(Resource.Type %in% c('Contractor','Employee','Requisition')) %>%
  group_by(across(all_of(demandCols))) %>%
  summarise(across(all_of(month_cols), ~ sum(.x, na.rm = TRUE), .names = "{.col}")) %>%
  ungroup() %>%
  mutate(PositionKey=row_number()) %>%
  select(PositionKey,Assignment.Tier1DirectIndirect,X9.1.25:X3.1.27)

assignments <- assignments %>%
  pivot_longer(
    cols = -c(PositionKey,Assignment.Tier1DirectIndirect),
    names_to = "Date",
    values_to = "Metric"
  ) %>%
  mutate(
    # Turn "X9.1.25" -> "2025-09-01"
    Date = str_replace(Date, "^X(\\d{1,2})\\.(\\d{1,2})\\.(\\d{2})$", "20\\3-\\1-\\2"),
    Date = lubridate::ymd(Date),
    Date = lubridate::ceiling_date(Date, "month") - lubridate::days(1)
  ) %>%
  arrange(PositionKey, Date) %>%
  filter(Date <= as.Date("2027-03-31")) %>%
  rename('IntervalDate'='Date')

assignments <- assignments %>% 
  group_by(PositionKey) %>%
  arrange(IntervalDate, .by_group = TRUE) %>%
  mutate(
    RollingAvg = slide_dbl(
      Metric,
      ~ mean(.x, na.rm = TRUE),       # define how to handle NAs
      .before = 0,                    # include current row
      .after  = rollingMonths - 1,    # look forward N-1 rows
      .complete = FALSE               # allow partial windows at the end
    )
  ) %>%
  mutate(
    Demand = if_else(
      RollingAvg - floor(RollingAvg) >= roundingThreshold,
      ceiling(RollingAvg),
      floor(RollingAvg)
    )
  ) %>%
  ungroup() 

# 2) Simple monthly rollup with starting/ending + category nets
demand <- assignments %>%
  ### OPTIONAL FILTER by Tier1 if use does not choose 'overall'
  ### filter(Assignment.Tier1DirectIndirect==[USER SELECTION]) %>%
  group_by(IntervalDate) %>%
  summarise(RawDemand=round(sum(Metric)),RoundedDemand=sum(Demand)) %>%
  mutate(LostDemand=RawDemand-RoundedDemand) %>%
  ungroup()

