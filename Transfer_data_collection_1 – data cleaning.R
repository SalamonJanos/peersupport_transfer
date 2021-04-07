## Transfer_data_collection_1 – data cleaning


# loading packages
library(tidyverse)
library(lavaan)
library(haven)
library(lubridate)
library(anytime)


### ----------------------- I. CREATING ONE DATABASE from individual data files ---------------------------

## ------------------------- Removable columns --------------------------

remove_cols <- c("StartDate", "EndDate", "Status", "IPAddress", 
                 "Finished", 
                 
                 "RecipientLastName", "RecipientFirstName", "RecipientEmail", 
                 "ExternalReference", "LocationLatitude", "LocationLongitude", 
                 "DistributionChannel", "UserLanguage", "Intro_2",
                 
                 "Demo_timer_First_Click", "Demo_timer_Last_Click", "Demo_timer_Page_Submit", "Demo_timer_Click_Count",
                 "Work_demo_timer_First_Click", "Work_demo_timer_Last_Click", "Work_demo_timer_Page_Submit", "Work_demo_timer_Click_Count",
                 "T_info_timer_First_Click", "T_info_timer_Last_Click", "T_info_timer_Page_Submit", "T_info_timer_Click_Count",
                 "Pre1_timer_First_Click", "Pre1_timer_Last_Click", "Pre1_timer_Page_Submit", "Pre1_timer_Click_Count",
                 "Pre2_timer_First_Click", "Pre2_timer_Last_Click", "Pre2_timer_Page_Submit", "Pre2_timer_Click_Count",
                 "Pre3_timer_First_Click", "Pre3_timer_Last_Click", "Pre3_timer_Page_Submit", "Pre3_timer_Click_Count", 
                 "Outc_timer_First_Click", "Outc_timer_Last_Click", "Outc_timer_Page_Submit", "Outc_timer_Click_Count", 
                 "Mindset_timer_First_Click", "Mindset_timer_Last_Click", "Mindset_timer_Page_Submit", "Mindset_timer_Click_Count",
                 "Email_timer_First_Click", "Email_timer_Last_Click", "Email_timer_Page_Submit", "Email_timer_Click_Count"
)


## ---------------------- IMPORT DATA ------------------------------


# Import data of Company_1a
work_data_raw_C1a <- read_sav("data/Transfer_Company_1a.sav")

# Process data of Company_1a
work_data_C1a <-
  work_data_raw_C1a %>% 
  zap_labels() %>% 
  as_tibble() %>% 
  filter(., .$Progress > 85) %>%
  filter(., Outc_7 >= 1) %>%
  add_column(., Sample = "Company_1a", .before = 1) %>%
  rename(., Training_date = T_date) %>% 
  rename(., Training_topic = T_topic) %>% 
  rename(., Draw_Interview = Q45) %>% 
  rename(., Record_Date = RecordedDate) %>% 
  mutate(Record_Date=as.Date(Record_Date)) %>% 
  select(., -one_of(remove_cols))

# ----------------------------------------------------

# Import data of Company_2a
work_data_raw_C2a <- read_sav("data/Transfer_Company_2a.sav")

# Process data of Company_2a
work_data_C2a <-
  work_data_raw_C2a %>% 
  zap_labels() %>% 
  as_tibble() %>% 
  filter(., .$Progress > 85) %>%
  filter(., Outc_7 >= 1) %>%
  add_column(., Sample = "Company_2a", .before = 1) %>%
  rename(., Training_date = T_date) %>% 
  rename(., Training_topic = T_topic) %>% 
  rename(., Record_Date = RecordedDate) %>% 
  mutate(Record_Date=as.Date(Record_Date)) %>% 
  select(., -one_of(remove_cols))

# ---------------------------------------------

# Import data of Company_3a
work_data_raw_C3a <- read_sav("data/Transfer_Company_3a.sav")

# Process data of Company_3a
work_data_C3a <-
  work_data_raw_C3a %>% 
  zap_labels() %>% 
  as_tibble() %>% 
  filter(., .$Progress > 85) %>%
  filter(., Outc_7 >= 1) %>%
  add_column(., Sample = "Company_3a", .before = 1) %>%
  rename(., Training_date = T_date) %>% 
  rename(., Training_topic = T_topic) %>% 
  rename(., Record_Date = RecordedDate) %>% 
  mutate(Record_Date=as.Date(Record_Date)) %>% 
  select(., -one_of(remove_cols))

# ---------------------------------------------

# Import data of Company_4a
work_data_raw_C4a <- read_sav("data/Transfer_Company_4a.sav")

# Process data of Company_4a
work_data_C4a <-
  work_data_raw_C4a %>% 
  zap_labels() %>% 
  as_tibble() %>% 
  filter(., .$Progress > 85) %>%
  filter(., Outc_7 >= 1) %>%
  add_column(., Sample = "Company_4a", .before = 1) %>%
  rename(., Training_date = T_date) %>% 
  rename(., Training_topic = T_topic) %>% 
  rename(., Record_Date = RecordedDate) %>% 
  mutate(Record_Date=as.Date(Record_Date)) %>% 
  select(., -one_of(remove_cols))

# -----------------------------------------------------

# Import data of Company_4b
work_data_raw_C4b <- read_sav("data/Transfer_Company_4b.sav")

# Process data of Company_4b
work_data_C4b <-
  work_data_raw_C4b %>% 
  zap_labels() %>% 
  as_tibble() %>% 
  filter(., .$Progress > 85) %>%
  filter(., Outc_7 >= 1) %>%
  add_column(., Sample = "Company_4b", .before = 1) %>%
  rename(., Training_date = T_date) %>% 
  rename(., Training_topic = T_topic) %>% 
  rename(., Record_Date = RecordedDate) %>% 
  mutate(Record_Date=as.Date(Record_Date)) %>% 
  select(., -one_of(remove_cols))

# ---------------------------------------------

# Import data of Company_5a
work_data_raw_C5a <- read_sav("data/Transfer_Company_5a.sav")

# Process data of Company_5a
work_data_C5a <-
  work_data_raw_C5a %>% 
  zap_labels() %>% 
  as_tibble() %>% 
  filter(., .$Progress > 85) %>%
  filter(., Outc_7 >= 1) %>%
  add_column(., Sample = "Company_5a", .before = 1) %>%
  rename(., Training_date = T_date) %>% 
  rename(., Training_topic = T_topic) %>% 
  rename(., Draw_Interview = Q45) %>% 
  rename(., Record_Date = RecordedDate) %>% 
  mutate(Record_Date=as.Date(Record_Date)) %>% 
  select(., -one_of(remove_cols))

# ------------------------------------------

# Import data of Company_6a
work_data_raw_C6a <- read_sav("data/Transfer_Company_6a.sav")

# Process data of Company_6a
work_data_C6a <-
  work_data_raw_C6a %>% 
  zap_labels() %>% 
  as_tibble() %>% 
  filter(., .$Progress > 85) %>%
  filter(., Outc_7 >= 1) %>%
  add_column(., Sample = "Company_6a", .before = 1) %>%
  rename(., Training_date = T_date) %>% 
  rename(., Training_topic = T_topic) %>% 
  rename(., Draw_Interview = Q45) %>% 
  rename(., Record_Date = RecordedDate) %>% 
  mutate(Record_Date=as.Date(Record_Date)) %>% 
  select(., -one_of(remove_cols))

# ------------------------------------------

# Import data of Company_7a
work_data_raw_C7a <- read_sav("data/Transfer_Company_7a.sav")

# Process data of Company_7a
work_data_C7a <-
  work_data_raw_C7a %>% 
  zap_labels() %>% 
  as_tibble() %>% 
  filter(., .$Progress > 85) %>%
  filter(., Outc_7 >= 1) %>%
  add_column(., Sample = "Company_7a", .before = 1) %>%
  rename(., Training_date = T_date) %>% 
  rename(., Training_topic = T_topic) %>% 
  rename(., Draw_Interview = Q45) %>% 
  rename(., Record_Date = RecordedDate) %>% 
  mutate(Record_Date=as.Date(Record_Date)) %>% 
  select(., -one_of(remove_cols))

# --------------------------------------------------

# Import data of Company_8a
work_data_raw_C8a <- read_sav("data/Transfer_Company_8a.sav")

# Process data of Company_8a
work_data_C8a <-
  work_data_raw_C8a %>% 
  zap_labels() %>% 
  as_tibble() %>% 
  filter(., .$Progress > 85) %>%
  filter(., Outc_7 >= 1) %>%
  add_column(., Sample = "Company_8a", .before = 1) %>%
  rename(., Training_date = T_date) %>% 
  rename(., Training_topic = T_topic) %>% 
  rename(., Draw_Interview = Q45) %>% 
  rename(., Record_Date = RecordedDate) %>% 
  mutate(Record_Date=as.Date(Record_Date)) %>% 
  select(., -one_of(remove_cols))

# ---------------------------------------

# Import data of Company_9a
work_data_raw_C9a <- read_sav("data/Transfer_Company_9a.sav")

# Process data of Company_9a
work_data_C9a <-
  work_data_raw_C9a %>% 
  zap_labels() %>% 
  as_tibble() %>% 
  filter(., .$Progress > 85) %>%
  filter(., Outc_7 >= 1) %>%
  add_column(., Sample = "Company_9a", .before = 1) %>%
  rename(., Training_date = T_date) %>% 
  rename(., Training_topic = T_topic) %>% 
  rename(., Draw_Interview = Q45) %>% 
  rename(., Record_Date = RecordedDate) %>% 
  mutate(Record_Date=as.Date(Record_Date)) %>% 
  select(., -one_of(remove_cols))

# -----------------------------------------------------

# Import data of Company_10a
work_data_raw_C10a <- read_sav("data/Transfer_Company_10a.sav")

# Process data of Company_10a
work_data_C10a <-
  work_data_raw_C10a %>% 
  zap_labels() %>% 
  as_tibble() %>% 
  filter(., .$Progress > 85) %>%
  filter(., Outc_7 >= 1) %>%
  add_column(., Sample = "Company_10a", .before = 1) %>%
  rename(., Training_date = T_date) %>% 
  rename(., Training_topic = T_topic) %>% 
  rename(., Draw_Interview = Q45) %>% 
  rename(., Record_Date = RecordedDate) %>% 
  mutate(Record_Date=as.Date(Record_Date)) %>% 
  select(., -one_of(remove_cols))

# ---------------------

# Import data of Company_10b
work_data_raw_C10b <- read_sav("data/Transfer_Company_10b.sav")

# Process data of Company_10b
work_data_C10b <-
  work_data_raw_C10b %>% 
  zap_labels() %>% 
  as_tibble() %>% 
  filter(., .$Progress > 85) %>%
  filter(., Outc_7 >= 1) %>%
  add_column(., Sample = "Company_10b", .before = 1) %>%
  rename(., Training_date = T_date) %>% 
  rename(., Training_topic = T_topic) %>% 
  rename(., Draw_Interview = Q45) %>% 
  rename(., Record_Date = RecordedDate) %>% 
  mutate(Record_Date=as.Date(Record_Date)) %>% 
  select(., -one_of(remove_cols))

# ---------------------------------------



# create one data file from the 12 samples - bind rows
work_data_2 <- bind_rows(work_data_C1a, work_data_C2a, work_data_C3a,
                         work_data_C4a, work_data_C4b, work_data_C5a,
                         work_data_C6a, work_data_C7a, work_data_C8a,
                         work_data_C9a, work_data_C10a, work_data_C10b
)

# write_sav(work_data_2, "data/output-files/Transfer_datacollection1_onedata.sav", compress = FALSE)  



### ----------------------- II. DATA CLEANING ---------------------------


# work_data_2 <- read_sav("data/output-files/Transfer_datacollection1_onedata.sav")



# delete rows with NA, and rows with test/unusable data
work_data_3 <-
  work_data_2 %>%
  drop_na(., -c(JobRole_24_TEXT, Draw_Interview, Data_prot_3, Email, Result_quali, Q46, Ter_let)) %>%
  .[ grep("argdrg", .$Training_topic, invert = TRUE) , ] %>%
  .[ grep("teszt", .$Training_topic, invert = TRUE) , ] %>%
  .[ grep("Nem emlékszem", .$Training_topic, invert = TRUE) , ]



## --------------------------------------- cleaning training time data -----------------------------------------

work_data4 <- work_data_3 
work_data4$Training_date <- str_replace_all(
  work_data4$Training_date, # column we want to search
  c("január" = "01.", "február" = "02.", "március" = "03.", "márciusa" = "03.", "Március" = "03.",
    "április" = "04.", "\\d május" = "05.", "^május" = "2019.05.", "június" = "06.", "junius" = "06.", "július" = "07.", 
    "augusztus" = "08.", "szeptember" = "09.","október" = "10.","októver" = "10.", 
    "november" = "11.", "November" = "11.", "december" = "12.", "dec." = "12.", " " = "",
    "9-10-11." = "11.", 
    "2018.11.7-9" = "2018-11-09", "2018.10.3-4." = "2018-10-04", "2018.10.8-9" = "2018-10-09", 
    "2019.03.5-6." = "2019-03-06", "2019.05.8-10." = "2019-05-10.", 
    
    "2019.03.4.-5." = "2019-03-05",  
    "201806.,07." = "2018-06-07",    
    
    "2019.03.04-05" = "2019-03-05", "2018.09.05-06." = "2018-09-06", "2018.11.07-08." = "2018-11-08", "2018.12.05-07." = "2018-12-07", 
    "2019.04.02-03" = "2019-04-03", 
    "2018.12.05-11." = "2018-12-11", "2018.09.11-12." = "2018-09-12", "2018.09.17-18" = "2018-09-18", "2018.12.17-19" = "2018-12-19", 
    "2018.12.13-15." = "2018-12-15", "2018.09.27-28" = "2018-09-28", "2019.03.26-27" = "2019-03-27", "2019.01.24-25" = "2019-01-25",
    "2019.03.27-28" = "2019-03-28", "2019.02.11-13" = "2019-02-13", "2019.02.14-15" = "2019-02-15", "2019.04.28-29" = "2019-04-29",
    "2019.04.29-30" = "2019-04-30", "2019.04.04-05." = "2019-04-05", "2019.03.20-22" = "2019-03-22", "2019.04.25-26." = "2019-04-26",
    "2019,04,25-26" = "2019-04-26", "2019.02.06-08." = "2019-02-08", "2019.02.04-05." = "2019-02-05", "2019.03.25-27" = "2019-03-27",
    "2018.05.10-11" = "2018-05-11", "2019.04.08-09" = "2019-04-09", "2019.04.13-14." = "2019-04-14", "201811.22-23" = "2018-11-23",
    "2018.11.19-20." = "2018-11-20", "2018.11.21-22" = "2018-11-22", "2018.12.11-12" = "2018-12-12", "2019.12.12-13" = "2019-12-13",
    "2018.12.04-05." = "2018-12-05", "2018.12.18-19." = "2018-12-19", "2018.12.04-05" = "2018-12-05", "2018.12.05-06" = "2018-12-06",
    "2018.12.18-19" = "2018-12-19", "2019.04.01-02" = "2019-04-02", "2019.04.08.-09." = "2019-04-09", "2018.09.26-27." = "2018-09-27",
    "2018.09.18-19" = "2018-09-19", "2018.09.18-19." = "2018-09-19", "2018.09.26-27." = "2018-09-27", "2018.09.25-26." = "2018-09-26", 
    "2018.09.10-11." = "2018-09-11", "2018.10.03-04" = "2018-10-04", "2018.10.01-02" = "2018-10-02", "2019.04.03.-04." = "2019-04-04",
    "2018.03.05-06." = "2018-03-06", "2019.03.28-29" = "2019-03-29", "2019.03.28-29." = "2019-03-29", "2019.03.12-13." = "2019-03-13",
    "2019.03.19-20" = "2019-03-20", "2019.02.21-22." = "2019-02-22", "2019.03.05-06." = "2019-03-06", "2019.04.03-04" = "2019-04-04",
    "2019.04.16-17." = "2019-04-17", "2019.05.08-09." = "2019-05-09", 
    
    "2018-10-02-26" = "2018-10-26", "2018-10-02-26." = "2018-10-26", "2018-11-01.06" = "2018-11-06", # does not change it - I do not know why
    
    "2019.04.16_17." = "2019-04-17",
    
    "2018.10.02.-26" = "2018-10-26", "2018.10.08.-09." = "2018-10-09",     "2019-04-29.-30." = "2019-04-30",
    
    "2108.10.09-10." = "2018-10-10",
    
    "2019059-10" = "2019-05-10", "2019035-6" = "2019-03-06", "20190429-30" = "2019-04-30",
    
    "2018.0915" = "2018-09-15",
    "2019032728" = "2019-03-28",
    "2019.03025." = "2019-03-25",
    
    "2019.04.11,12" = "2019-04-12", 
    
    "2018.09-17-2018.09.18" = "2018-09-18", "2018.10.10-2018.10.11" = "2018-10-11", "2018.11.20-2018.11.24" = "2018-11-24", 
    "2019.02.04-2019.02.05" = "2019-02-05", "2018.11.19-2018.11.20" = "2018-11-20", "2018.10.03-2018.10.04" = "2018-10-04",
    "2018.10.09-2018.10.10" = "2018-10-10", "2019.02.21-2019.02.22" = "2019-02-22", "2019.03.28.-2019.03.29." = "2019-03-29",
    "2018.11.06.,2019.11.19.,2019.11.20." = "2018-11-06", "2018-11-208.11.20" = "2018-11-20",
    
    "19.04.20." = "2019-04-20", 
    
    "201903.a" = "2019-03-01", "201903Szolnok" = "2019-03-01", "2018mentori" = "2018-01-01", "2018.11.hó" = "2018-11-01",
    "2018.I.negyedév" = "2018-01-01", "2018.11.másodikfele" = "2018-11-15",
    
    "2018.12.4" = "2018-12-04", "2018.10.2." = "2018-10-02", 
    
    "2018.09.24-09.25." = "2018-09-25", "2018.11.26-11.28" = "2018-11-28", "2019.03.28-03.29" = "2019-03-29",
    
    "2018$" = "NA", "2018.$" = "NA","nemtudom" = "NA", "Szakmainapok" = "NA", "1hónap" = "NA", "passznegyedévenbelül" = "NA", "2hó" = "NA",
    
    
    # negative time between training date and response date --> 2019 changed to 2018    
    "2019-12-13" = "2018-12-13", "2019.05.22" = "2018-05-22", "2019.05.26" = "2018-05-26", 
    "2019.10." = "2018-10-01", "2019.11" = "2018-11-01", "2019.11.06" = "2018-11-06", "2019.12.12" = "2018-12-12" ))


# Process data of transfer database
work_data4b <-
  work_data4 %>%
  mutate(Training_date = stringr::str_replace_all(Training_date, '2018-10-02-26', '2018-10-26')) %>%
  mutate(Training_date = stringr::str_replace_all(Training_date, '2018-10-02-26.', '2018-10-26')) %>%
  mutate(Training_date = stringr::str_replace_all(Training_date, '2018-11-01.06', '2018-11-06')) %>%
  mutate(tdate = parse_date_time(Training_date, orders = c("dmy", "ymd", "d0mY", "0mY", "Y", "Ym0")))


#write_sav(work_data4b, "data/output-files/Transfer_datacollection1_clean-date_data.sav", compress = FALSE)  



### --------------------------- III. DATA PREPARATION FOR ANALYSIS -------------------------------


#work_data4b <- read_sav("data/output-files/Transfer_datacollection1_clean-date_data.sav")


## ---------------------------- data cleaning (filtering) ------------------------------------

# create time difference variable
work_data5 <- work_data4b %>%
  mutate(timediff = as.Date(Record_Date) - as.Date(tdate)) %>%
  select(., -contains("Training_date")) %>% 
  rename(., Training_date = tdate) %>% 
  mutate(timediff = as.Date(Record_Date) - as.Date(Training_date)) %>% 
  filter(timediff <= 365, timediff > 5)


## --------------------- creating finale database from data collection 1 ----------------------

# create data ready for analysis - removing email address
datacollection1_analysis <-
  work_data5 %>%
  select(., -contains("email"))

write_sav(datacollection1_analysis, "data/output-files/Transfer_data_collection_1.sav", compress = FALSE)  




