library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)


#### NEED TO SHARE RULES FOR HOW LEVEL IS COMPUTED NUMERICALLY
#### NOTE 'REPRESENTED', etc are considered LEVEL 0 -- maybe this isn't right because
### it means Level 1s are considered over resourced relative to Represented


### NOTE THAT WE FILTER OUT ROWS WHERE PEOPLE NODE ID = 'data mismatch' or 'executive placeholder'
employees <- read.csv('data//2025-06-16 Employees.csv') %>%
  filter(!(PeopleNodeID) %in% c('Data Mismatch','Exec Placeholder')) %>%
  mutate(People.NodeID=as.integer(PeopleNodeID))

reqs <- read.csv('data//2025-09-16 Requisitions.csv')
assignments <- read.csv('data//Aero Assignments_20250916.csv') %>%
  filter(Assignment.Status %in% c('Firm','High Potential')) %>%
  mutate(LevelNum=ifelse(substr(Level,1,1)=='L',as.numeric(substr(Level,7,7)),0)) %>%
  ### Peter will change the value in the raw file, so you can remove this eventually
  mutate(Resource.Type=ifelse(Resource.Type=='Pooled Services','SWA',Resource.Type))

month_cols <- grep("^([A-Z][a-z]{2}\\.[0-9]{2})$", names(assignments), value = TRUE)

### create adjusted SWA assignments
SWA <- assignments %>%
  ### TEMPORARY ADJUSTEMENT
  filter(Pooled.Services!='Operations') %>%
  filter(Resource.Type %in% c('SWA')) %>%
  group_by(Pooled.Services) %>%
  summarise(across(all_of(month_cols), ~ sum(.x, na.rm = TRUE), .names = "{.col}")) %>%
  ungroup() %>%
  rename('Assignment.Group'='Pooled.Services') %>%
  pivot_longer(
    cols = all_of(month_cols),
    names_to = "Month",
    values_to = "Budget"
  ) %>%
  mutate(
    # "Jan.25" -> "Jan 25" -> Date
    Date = Month %>% str_replace_all("\\.", " ") %>% my(),
    # make it end-of-month (use first-of-month if you prefer: Date = floor_date(Date, "month"))
    Date = ceiling_date(Date, "month") - days(1)
  ) %>%
  select(Assignment.Group,Date,Budget)


PooledServiceUsage <- assignments %>%
  filter(AG.Program.Type=='Pooled Services') %>%
  group_by(Assignment.Group) %>%
  summarise(across(all_of(month_cols), ~ sum(.x, na.rm = TRUE), .names = "{.col}")) %>%
  ungroup()  %>%
  pivot_longer(
    cols = all_of(month_cols),
    names_to = "Month",
    values_to = "Allocated"
  ) %>%
  mutate(
    # "Jan.25" -> "Jan 25" -> Date
    Date = Month %>% str_replace_all("\\.", " ") %>% my(),
    # make it end-of-month (use first-of-month if you prefer: Date = floor_date(Date, "month"))
    Date = ceiling_date(Date, "month") - days(1)
  ) %>%
  select(Assignment.Group,Date,Allocated)

ratios <- left_join(PooledServiceUsage,SWA,by=c('Assignment.Group','Date')) %>%
  mutate(ratio=ifelse(Allocated!=0,Budget/Allocated,0)) %>%
  select(Assignment.Group,Date,ratio)

### used in QA below:
zeroAllocationCheck <- left_join(PooledServiceUsage,SWA,by=c('Assignment.Group','Date')) %>%
  filter(Allocated==0)

PooledEmps <- assignments %>%
  filter(AG.Program.Type=='Pooled Services') %>%
  select(People.NodeID,Assignment.Group,Resource.Type,all_of(month_cols)) %>%
  pivot_longer(
    cols = all_of(month_cols),
    names_to = "Month",
    values_to = "Value"
  ) %>%
  mutate(
    # "Jan.25" -> "Jan 25" -> Date
    Date = Month %>% str_replace_all("\\.", " ") %>% my(),
    # make it end-of-month (use first-of-month if you prefer: Date = floor_date(Date, "month"))
    Date = ceiling_date(Date, "month") - days(1)
  ) %>%
  select(People.NodeID, Assignment.Group,Resource.Type, Date, Value) %>%
  arrange(People.NodeID, Date) %>%
  left_join(.,ratios,by=c('Date','Assignment.Group')) %>%
  mutate(Metric=Value*ratio) %>%
  select(People.NodeID,Resource.Type,Date,Metric)

### check to make sure you've adjusted PooledEmps to match SWABudget, discounting the budget rows where there
  ### are 0 allocations:
sum(SWA$Budget)-sum(PooledEmps$Metric)==sum(zeroAllocationCheck$Budget)

### which employeeIDs are at least 75% allocated in assignments??
assignments <- assignments %>%
  filter(Resource.Type %in% c('Contractor','Employee','Requisition') & AG.Program.Type!='Pooled Services') %>%
  group_by(People.NodeID,Resource.Type) %>%
  summarise(across(all_of(month_cols), ~ sum(.x, na.rm = TRUE), .names = "{.col}")) %>%
  ungroup()

assignments <- assignments %>%
  pivot_longer(
    cols = -c(People.NodeID, Resource.Type),
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
  arrange(People.NodeID, Date) %>%
  filter(Date>='2025-09-30')

### tack on PooledEmps
### provisional step: remove pre-sept records
PooledEmps <- PooledEmps %>%
  filter(Date>='2025-09-30')
sum(PooledEmps$Metric)  

assignments <- assignments %>%
  rbind(.,PooledEmps) %>%
  group_by(People.NodeID,Resource.Type,Date) %>%
  summarise(Metric=sum(Metric)) %>%
  ungroup()


# params you can tweak
under_thresh <- 0.9
min_consec   <- 8

assignments <- assignments %>%
  arrange(People.NodeID, Date) %>%
  group_by(People.NodeID) %>%
  # mark under-allocation and build run IDs of TRUE/FALSE streaks
  mutate(
    under = Metric < under_thresh,
    run_id_all = dplyr::consecutive_id(under),
    run_id = if_else(under, run_id_all, NA_integer_)
  ) %>%
  ungroup() %>%
  # find any under-allocation run(s) with length >= min_consec
  group_by(People.NodeID, run_id) %>%
  summarise(
    is_under_run = all(!is.na(run_id)),
    run_len = dplyr::n(),
    start_date = dplyr::first(Date),
    .groups = "drop"
  ) %>%
  filter(is_under_run, run_len >= min_consec) %>%
  group_by(People.NodeID) %>%
  summarise(elim_start = min(start_date), .groups = "drop") %>%
  # join back and create a carry-forward elimination flag
  right_join(assignments, by = "People.NodeID") %>%
  arrange(People.NodeID, Date) %>%
  group_by(People.NodeID) %>%
  mutate(
    EliminationFlag = if_else(!is.na(elim_start) & Date >= elim_start, 1L, 0L)
  ) %>%
  ungroup()

### remove underallocations
assignments <- assignments %>%
  filter(EliminationFlag!=1) %>%
  select(-c('EliminationFlag','elim_start'))

### get maxLevel for assignment in data
MAX_LEVEL <- read.csv('data//Aero Assignments_20250916.csv') %>%
  filter(Assignment.Status %in% c('Firm','High Potential')) %>%
  mutate(LevelNum=ifelse(substr(Level,1,1)=='L',as.numeric(substr(Level,7,7)),0)) %>%
  group_by(People.NodeID) %>%
  summarise(maxLevel=max(LevelNum)) %>%
  ### Peter will change the value in the raw file, so you can remove this eventually
  ungroup()

assignments <- assignments %>%
  left_join(.,MAX_LEVEL,by='People.NodeID')

### 
cols<-c('People.NodeID','Level')

table(employees$Workforce.Type)

employees <- employees %>%
  mutate(Source=ifelse(Workforce.Type %in% c('Casual Employee','Full Time Employee','Part Time Employee'),'emp','cont')) %>%
  select(cols,Source)

reqs <- reqs %>%
  select(cols) %>%
  mutate(Source='req')
emp_reqs <- rbind(employees,reqs) %>%
  mutate(LevelNum=ifelse(substr(Level,1,1)=='L',as.numeric(substr(Level,7,7)),0)) 
  
### combined
dates<-data.frame(IntervalDate=unique(assignments$Date))

demand <- emp_reqs %>%
  cross_join(dates) %>%
  left_join(assignments, by = c("People.NodeID","IntervalDate" = "Date")) %>%
  group_by(People.NodeID) %>%
  arrange(IntervalDate, .by_group = TRUE) %>%
  mutate(
    Reduction = as.integer(is.na(Metric))
  ) %>%
  # remove rows after the first Reduction == 1
  filter(cumsum(Reduction) <= 1) %>%
  ungroup() %>%
  select(-Metric) %>%
  mutate(maxLevel=ifelse(is.na(maxLevel),0,maxLevel)) %>%
  mutate(EmpOverResourced=ifelse(LevelNum>maxLevel & Reduction!=1 & Source=='emp',1,0),
         ContractorOverResourced=ifelse(LevelNum>maxLevel & Reduction!=1 & Source=='cont',1,0),
         ReqOverResourced=ifelse(LevelNum>maxLevel & Reduction!=1 & Source=='req',1,0)) %>%
  mutate(empReduction=ifelse(Source=='emp' & Reduction==1,1,0),
         contractorReduction=ifelse(Source=='cont' & Reduction==1,1,0),
         reqReduction=ifelse(Source=='req' & Reduction==1,1,0))

demandSummary <- demand %>%
  group_by(IntervalDate) %>%
  summarise(Headcount=length(People.NodeID),
            EmpReductions=sum(empReduction),ContractorReductions=sum(contractorReduction),ReqReductions=sum(reqReduction),
            EmpOverResourced=sum(EmpOverResourced),ReqOverResourced=sum(ReqOverResourced)) %>%
  ungroup()

### remove underallocated reqs
table(assignments$Resource.Type)



### where are we losing demand because an assignment is not allocated?
  ### are there Employees in assignments who don't show up in active? 
  ### only 1:
missingEmps <- assignments %>%
  filter(!(EmployeeID %in% unique(employees$EmployeeID)))

  ### how about the assignments that haven't been allocated?





