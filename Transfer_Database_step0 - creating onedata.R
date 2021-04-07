# # Executive summary 

# Preparation
# loading packages
library(tidyverse)
library(haven)
library(stringr)
library(data.table)

## ---------------------- IMPORT DATA ------------------------------

# Import data of transfer database
data1_raw <- read_sav("sensitive_data/Transfer_data_collection_1.sav")
data1 <- data1_raw %>% 
  zap_labels() %>% 
  as_tibble() %>% 
  rename(., Current_Edu = Study) %>% 
  rename(., Training_date2 = Training_date) %>%
  add_column(., Study = "1", .before = 1) %>% 
  setnames(., old=c("Pre1_1", "Pre1_2", "Pre1_3", "Pre1_4", "Pre1_5", "Pre1_6", "Pre1_7", "Pre1_8", "Pre2_1", "Pre2_2", "Pre2_3",
                    "Pre2_4", "Pre2_5", "Pre2_6", "Pre2_7", "Pre2_8", "Pre2_9", "Pre2_10", "Pre2_11", "Pre2_12", "Pre3_1", "Pre3_2",
                    "Pre3_3", "Pre3_4", "Pre3_5", "Pre3_6", "Pre3_7", "Pre3_8", "Pre3_9", "Pre3_10", "Pre3_11", "Pre3_12", "Outc_1",
                    "Outc_2", "Outc_3", "Outc_4", "Outc_5", "Outc_6", "Outc_7"), 
           new=c("car11", "per12", "car13", "per14", "car15", "per16", "per17", "car18", "mot21", "uti22", "eff23", "eff24", "uti25",
                 "mot26", "uti27", "mot28", "eff29", "uti210", "eff211", "mot212", "sup31", "sup32", "pee33", "opp34", "pee35", "sup36",
                 "opp37", "pee38", "opp39", "sup310", "pee311", "opp312", 
                 "use1", "perf2", "use3", "perf4", "use5", "perf6", "use7")) %>% 
  setnames(., old=c("MS_1", "MS_2", "MS_3"), 
           new=c("ms1", "ms2", "ms3"))


data2_raw <- read_sav("sensitive_data/Transfer_data_collection_2.sav")
data2 <- data2_raw %>%
  zap_labels() %>% 
  as_tibble() %>% 
  add_column(., Study = "2", .before = 1) %>% 
  setnames(., old=c("Pre1_1", "Pre1_2", "Pre1_3", "Pre1_4", "Pre1_5", "Pre1_6", 
                    "Pre2_1", "Pre2_2", "Pre2_3", "Pre2_4", "Pre2_5", "Pre2_6", 
                    "Pre3_1", "Pre3_2", "Pre3_3", "Pre3_4", "Pre3_5", "Pre3_6", "Pre3_7", "Pre3_8", "Pre3_9", 
                    "Outc_1", "Outc_2", "Outc_3", "Outc_4", "Outc_5", "Outc_6", "Outc_7"), 
           new=c("car11", "per12", "per14", "car15", "per16", "car18", 
                 "eff23", "eff24", "mot26", "mot28", "eff211", "mot212", 
                 "sup31", "sup32", "pee33", "pee35", "sup36", "opp37", "opp39", "pee311", "opp312", 
                 "use1", "perf2", "use3", "perf4", "use5", "perf6", "use7")) %>% 
  setnames(., old=c("uwes_1", "uwes_2", "uwes_3", "uwes_4", "uwes_5", 
                    "uwes_6", "uwes_7", "uwes_8", "uwes_9"), 
           new=c("uwes1", "uwes2", "uwes3", "uwes4", "uwes5", 
                 "uwes6", "uwes7", "uwes8", "uwes9")) %>% 
  setnames(., old=c("jdr_1", "jdr_2", "jdr_3", "jdr_4", "jdr_5", "jdr_6", 
                    "jdr_7", "jdr_8", "jdr_9", "jdr_10", "jdr_11"), 
           new=c("jdr1", "jdr2", "jdr3", "jdr4", "jdr5", "jdr6", 
                 "jdr7", "jdr8", "jdr9", "jdr10", "jdr11")) %>% 
  setnames(., old=c("Stress_1", "Stress_2", "Stress_3", "Stress_4"), 
           new=c("stress1", "stress2", "stress3", "stress4")) %>% 
  setnames(., old=c("OC_1", "OC_2", "OC_3", "OC_4", "OC_5", "OC_6", 
                    "OC_7", "OC_8", "OC_9", "OC_10", "OC_11", "OC_12"), 
           new=c("oc1", "oc2", "oc3", "oc4", "oc5", "oc6", 
                 "oc7", "oc8", "oc9", "oc10", "oc11", "oc12")) %>% 
  setnames(., old=c("grit_1", "grit_2", "grit_3", "grit_4", "grit_5", "grit_6", "grit_7", "grit_8"), 
           new=c("grit1", "grit2", "grit3", "grit4", "grit5", "grit6", "grit7", "grit8")) %>% 
  setnames(., old=c("mastery_1", "mastery_2", "mastery_3", "mastery_4", "mastery_5"), 
           new=c("mastery1", "mastery2", "mastery3", "mastery4", "mastery5")) 


data1$Record_Date <- as.Date(data1$Record_Date)
data2$Record_Date <- as.Date(data2$Record_Date)

work_data <- plyr::rbind.fill(data1, data2)

write_sav(work_data, "sensitive_data/Transfer_onedata.sav", compress = FALSE)
