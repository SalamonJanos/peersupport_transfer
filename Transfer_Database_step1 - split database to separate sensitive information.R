

# The variables with sensitive information that could increase the risk of identification of respondents were either: 
# - Generalized: training topic and organizational level; 
# - Anonymized: company name and sector;
# - Removed: email address, training date, response date, job role, organization size, 
# level of education completed, total number of years at current workplace, and total number of years spent on the labor market




# Preparation
# loading packages
library(tidyverse)
library(haven)


# import database
work_data <- read_sav("sensitive_data/Transfer_onedata.sav")

# creating variables that can decrease the risk of identification, and will be necessary later in the analysis
#(1. accurate org. level could increase the risk of identification (especially in higher management positions))
#(2. record year variable is necessary for calculating respondents' age, while accurate date could increase the risk of identification)
work_data <- work_data %>% 
  mutate(Management = case_when(Org_level >= "5" ~ "0", # non-managers
                                Org_level <= "4" ~ "1", # managers
                                TRUE ~ as.character(Org_level))) %>% 
  mutate(RDate = format(as.Date(Record_Date),"%Y")) 



# selecting variables that may contain sensitive information 
sensitive_info <- c("Training_topic",
                    "Ter_let", "Q46", "Area", "Area_19_TEXT",
                    "T_emotion_text", "Result_quali", 
                    "T_techniques", "Other_feedback", "JobRole_24_TEXT",
                    
                    # variables that could increase the risk of identification
                    "Training_date2", "Record_Date", "JobRole", "Org_level", 
                    "original_date_string", "training_start", "training_end", "training_length",
                    
                    # variables with non-relevant info (not used in further analysis and just increase the risk of identification)
                    "Residence", "Relationship", "Education", 
                    "Current_Edu", "Current_job", "Work_total", "Org_size", "Colleagues", "Co_workers", "Training_total", 
                    "Progress", "Duration__in_seconds_", "Data_prot_3", "Draw_Interview", "Current_work")


# Separating sensitive data
work_data_sensitive <- work_data %>%
  select(c("ResponseId", one_of(sensitive_info)))

# Separating non-sensitive data for further analysis
work_data2 <- work_data %>% 
  select(., -one_of(sensitive_info))

colnames(work_data2)

# creating two separate databases
write_sav(work_data2, "data/Transfer_onedata_for_analysis.sav", compress = FALSE)
write_sav(work_data_sensitive, "sensitive_data/Transfer_onedata_sensitive_part.sav", compress = FALSE)



