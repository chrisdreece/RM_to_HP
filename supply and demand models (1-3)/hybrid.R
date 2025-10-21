library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(slider)
library(gdata)

### PARAMETERS
cols <- c('People.NodeID','Business.Area.Descr','Tier1.Direct.Indirect','Job.Discipline','Level','FLSA.Status','Leader.or.IC',
          'Title','Business.Unit','Department','MSA','Country','Skill.Domain','EmployeeContractor')
rollingMonths<-3
roundingThreshold<-.7
adjustedSupply<-0



### NOTE THAT WE FILTER OUT ROWS WHERE PEOPLE NODE ID = 'data mismatch' or 'executive placeholder'
skillDomain<-read.csv('data//skillDomain.csv') %>%
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
  ### Peter will change the value in the raw file, so you can remove this eventually
  mutate(Resource.Type=ifelse(Resource.Type=='Pooled Services','SWA',Resource.Type))

month_cols <- grep("^X[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{2}$", names(assignments), value = TRUE)
### which employeeIDs are at least 75% allocated in assignments??

if (adjustedSupply==1){
### CREATE ADJUSTED SWA ASSIGNMENTS
SWA <- assignments %>%
  # TEMPORARY ADJUSTMENT
  filter(Resource.Type %in% c("SWA")) %>%
  group_by(Pooled.Services) %>%
  summarise(across(all_of(month_cols), ~ sum(.x, na.rm = TRUE), .names = "{.col}")) %>%
  ungroup() %>%
  rename("Assignment.Group" = "Pooled.Services") %>%
  pivot_longer(
    cols = all_of(month_cols),
    names_to = "Month",
    values_to = "Budget"
  ) %>%
  mutate(
    # Convert "X9.1.25" → "2025-09-01"
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
  pivot_longer(
    cols = all_of(month_cols),
    names_to = "Month",
    values_to = "Allocated"
  ) %>%
  mutate(
    # convert "X9.1.25" → "2025-09-01"
    Date = str_replace(Month, "^X(\\d{1,2})\\.(\\d{1,2})\\.(\\d{2})$", "20\\3-\\1-\\2"),
    Date = ymd(Date),
    Date = ceiling_date(Date, "month") - days(1)
  ) %>%
  select(Assignment.Group, Date, Allocated)

ratios <- left_join(PooledServiceUsage,SWA,by=c('Assignment.Group','Date')) %>%
  mutate(ratio=ifelse(Allocated!=0 & !(is.na(Budget)),Budget/Allocated,0)) %>%
  select(Assignment.Group,Date,ratio)

### used in QA below:
zeroAllocationCheck <- left_join(PooledServiceUsage,SWA,by=c('Assignment.Group','Date')) %>%
  filter(Allocated==0 | is.na(Budget))


### NOTE you have some duplicate rows by People.NodeID here because there are 
### a few people that are in 2 Assignment Groups -- that's fine, you apply the relevant ratio to each group,
### then, you roll up by People.NodeID AFTER you bind to the regular assignments
PooledEmps <- assignments %>%
  filter(AG.Program.Type == "Pooled Services") %>%
  select(People.NodeID, Assignment.Group, Resource.Type, all_of(month_cols)) %>%
  pivot_longer(
    cols = all_of(month_cols),
    names_to = "Month",
    values_to = "Value"
  ) %>%
  mutate(
    # "X9.1.25" → "2025-09-01" → Date
    Date = str_replace(Month, "^X(\\d{1,2})\\.(\\d{1,2})\\.(\\d{2})$", "20\\3-\\1-\\2"),
    Date = ymd(Date),
    Date = ceiling_date(Date, "month") - days(1)
  ) %>%
  select(People.NodeID, Assignment.Group, Resource.Type, Date, Value) %>%
  arrange(People.NodeID, Date) %>%
  left_join(ratios, by = c("Date", "Assignment.Group")) %>%
  mutate(Metric = Value * ratio) %>%
  select(People.NodeID, Resource.Type, Date, Metric)

sum(SWA$Budget)-sum(PooledEmps$Metric)==sum(zeroAllocationCheck$Budget,na.rm=TRUE)

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
  mutate(
    # Turn "X9.1.25" -> "2025-09-01"
    Date = str_replace(Date, "^X(\\d{1,2})\\.(\\d{1,2})\\.(\\d{2})$", "20\\3-\\1-\\2"),
    Date = lubridate::ymd(Date),
    Date = lubridate::ceiling_date(Date, "month") - lubridate::days(1)
  ) %>%
  arrange(People.NodeID, Date) %>%
  filter(Date <= as.Date("2027-03-31"))

### tack on PooledEmps
PooledEmps <- PooledEmps %>%
  filter(Date<="2027-03-31")
sum(PooledEmps$Metric)  

assignments <- assignments %>%
  rbind(.,PooledEmps) %>%
  group_by(People.NodeID,Resource.Type,Date) %>%
  summarise(Metric=sum(Metric)) %>%
  ungroup()
} else {
  assignments <- assignments %>%
    filter(Resource.Type %in% c('Contractor','Employee','Requisition')) %>%
    group_by(People.NodeID,Resource.Type) %>%
    summarise(Level.Group = dplyr::first(Level.Group[!is.na(Level.Group)]),
              across(all_of(month_cols), ~ sum(.x, na.rm = TRUE), .names = "{.col}")) %>%
    ungroup() 
  
  assignments <- assignments %>%
    pivot_longer(
      cols = -c(People.NodeID, Resource.Type, Level.Group),
      names_to = "Date",
      values_to = "Metric"
    ) %>%
    mutate(
      # Turn "X9.1.25" -> "2025-09-01"
      Date = str_replace(Date, "^X(\\d{1,2})\\.(\\d{1,2})\\.(\\d{2})$", "20\\3-\\1-\\2"),
      Date = lubridate::ymd(Date),
      Date = lubridate::ceiling_date(Date, "month") - lubridate::days(1)
    ) %>%
    arrange(People.NodeID, Date) %>%
    filter(Date <= as.Date("2027-03-31"))
}

employees <- employees %>%
  mutate(Source='HRMS') %>%
  mutate(EmployeeContractor=ifelse(Workforce.Type %in% c('Contractor','Independent Contractor','Leased Labor'),'Contractor','Employee')) %>%
  select(all_of(cols),Source)

reqs <- reqs %>%
  mutate(EmployeeContractor=ifelse(Workforce.Type %in% c('Contractor','Independent Contractor','Leased Labor'),'Contractor','Employee'),
         Skill.Domain='Not Applicable') %>%
  mutate(Source='Reqs') %>%
  select(cols,Source)

emp_reqs <- rbind(employees,reqs)

### combined
dates<-data.frame(IntervalDate=unique(assignments$Date))

demand <- emp_reqs %>%
  cross_join(dates) %>%
  left_join(assignments, by = c("People.NodeID", "IntervalDate" = "Date")) %>%
  mutate(Metric = coalesce(Metric, 0)) %>%
  group_by(IntervalDate,Source,across(all_of(setdiff(cols, "People.NodeID")))) %>%
  summarise(Headcount=length(People.NodeID),Metric = sum(Metric, na.rm = TRUE), .groups = "drop") %>%
  arrange()

### some quick checks:
# zzz <- demand %>%
#   group_by(IntervalDate) %>%
#   summarise(Headcount=sum(Headcount),Metric=sum(Metric)) %>%
#   ungroup()
# zzz2 <- assignments %>%
#   group_by(Date) %>%
#   summarise(Metric=sum(Metric)) %>%
#   ungroup()
# zzz3 <- assignments %>%
#   filter(!(People.NodeID %in% unique(emp_reqs$People.NodeID))) %>%
#   group_by(Date) %>%
#   summarise(Metric=sum(Metric)) %>%
#   ungroup()

### replace position fields with positionkey mapping, set mapping aside
# 1) Define which columns define the "position"
position_fields <- setdiff(names(demand), c("IntervalDate","Metric"))
# 2) Make a deterministic lookup (stable across runs)
positionKeyMapping <- demand %>%
  distinct(across(all_of(position_fields))) %>%          # unique combos
  arrange(across(all_of(position_fields))) %>%           # make ordering deterministic
  mutate(PositionKey = row_number())                     # assign integer key
demand <- demand %>%
  left_join(.,positionKeyMapping, by = position_fields) %>%
  arrange(PositionKey,IntervalDate) %>%
  select(PositionKey,IntervalDate,Tier1=Tier1.Direct.Indirect,Source,EmployeeContractor,Headcount,Metric)

### take rolling average of allocation, then use as the basis for reductions
demand <- demand %>%
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

### create transactions
demand <- demand %>%
  group_by(PositionKey) %>%
  arrange(IntervalDate, .by_group = TRUE) %>%
  mutate(
    # hires(+)/seps(-) needed this month
    Transaction = if_else(
      row_number() == 1,
      Demand - Headcount,          # first month: reconcile from starting Headcount
      Demand - lag(Demand)         # thereafter: change vs. prior month's Demand
    ),
    # JUST A QA: Headcount after applying transactions sequentially
    AdjHeadcount = Headcount + cumsum(Transaction)
  ) %>%
  ungroup()

# 1) Compute StartHeadcount per PositionKey (prior month's AdjHeadcount; first row uses Headcount)
demand <- demand %>%
  group_by(PositionKey) %>%
  arrange(IntervalDate, .by_group = TRUE) %>%
  mutate(StartHeadcount = if_else(row_number() == 1, Headcount, lag(AdjHeadcount))) %>%
  ungroup()

# 2) Simple monthly rollup with starting/ending + category nets
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
