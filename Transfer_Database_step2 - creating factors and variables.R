
# Preparation
# loading packages
library(tidyverse)
library(haven)
library(stringr)
library(data.table)

# import database
work_data <- read_sav("data/Transfer_onedata_for_analysis.sav")


work_data$car11 <- as.numeric(work_data$car11)
work_data$uwes3 <- as.numeric(work_data$uwes3)

## creating factors from variables
work_data2 <-
  work_data %>% 
  mutate(careerg = (car11 + car15 + car18)/3,
         performg = (per12 + per14 + per16)/3,
         motivation = (mot26 + mot28 + mot212)/3,
         selfefficacy = (eff23 + eff24 + eff211)/3,
         supervisor = (sup31 + sup32 + sup36)/3,
         peer = (pee33 + pee35 + pee311)/3,
         opportunity = (opp37 + opp39 + opp312)/3,
         use = (use1 + use3 + use5 + use7)/4,
         perform = (perf2 + perf4 + perf6)/3,
         mindset = (ms1 + ms2 + ms3)/3,
         emotion = (T_emotion_1 + T_emotion_2)/2,
         vigor = (uwes1 + uwes2 + uwes5)/3,
         dedication = (uwes3 + uwes4 + uwes7)/3,
         absorption = (uwes6 + uwes8 + uwes9)/3,
         uwes_all = (vigor + dedication + absorption)/3,
         oc = (oc1 + oc2 + oc3 + oc4 + oc5 + oc6 + oc7 + oc8 + oc9 + oc10 + oc11 + oc12)/12,
         grit = (grit1 + grit2 + grit3 + grit4 + grit5 + grit6 + grit7 + grit8)/8,
         mastery = (mastery1 + mastery2 + mastery3 + mastery4 + mastery5)/5) %>% 
  mutate(stress1R = case_when(stress1 == 1 ~ 5,
                              stress1 == 2 ~ 4,
                              stress1 == 3 ~ 3,
                              stress1 == 4 ~ 2,
                              stress1 == 5 ~ 1,
                              TRUE ~ as.numeric(stress1))) %>% 
  mutate(stress4R = case_when(stress4 == 1 ~ 5,
                              stress4 == 2 ~ 4,
                              stress4 == 3 ~ 3,
                              stress4 == 4 ~ 2,
                              stress4 == 5 ~ 1,
                              TRUE ~ as.numeric(stress4))) %>%
  mutate(consistency_of_interest = (grit1 + grit3 + grit5 + grit6)/4,
         perseverance_of_effort = (grit2 + grit4 + grit7 + grit8)/4,
         job_resources = (jdr1 + jdr3 + jdr5 + jdr7 + jdr9 + jdr11)/6,
         job_demands = (jdr2 + jdr4 + jdr6 + jdr8 + jdr10)/5,
         stress = (stress1R + stress2 + stress3 + stress4R)/4,
         affective_comm = (oc2 + oc8 + oc10)/3,
         normative_comm = (oc3 + oc6 + oc12)/3,
         continu_comm_HSacri = (oc1 + oc7 + oc11)/3,
         continu_comm_LAlter = (oc4 + oc5 + oc9)/3)



## creating age variable from record year and Date of Birth 

# change birth dates to years
birth_df <- structure(list(V1 = c(6:100), V2 = c(2001:1907), 
                           .Names = c("V1", "V2"), class = "data.frame"))
work_data2$Birth <- birth_df$V2[match(as.character(work_data2$Birth), as.character(birth_df$V1))]

# count age from record year and birth year
work_data2 <- work_data2 %>%
  mutate(age = as.numeric(RDate) - as.numeric(Birth))

transfer_data <- work_data2 %>% 
  mutate(Company = case_when(str_detect(Sample, "Company_1[:lower:]") ~ "Company_1",
                             str_detect(Sample, "Company_2[:lower:]") ~ "Company_2",
                             str_detect(Sample, "Company_3[:lower:]") ~ "Company_3",
                             str_detect(Sample, "Company_4[:lower:]") ~ "Company_4",
                             str_detect(Sample, "Company_5[:lower:]") ~ "Company_5",
                             str_detect(Sample, "Company_6[:lower:]") ~ "Company_6",
                             str_detect(Sample, "Company_7[:lower:]") ~ "Company_7",
                             str_detect(Sample, "Company_8[:lower:]") ~ "Company_8",
                             str_detect(Sample, "Company_9[:lower:]") ~ "Company_9",
                             str_detect(Sample, "Company_10[:lower:]") ~ "Company_10",
                             str_detect(Sample, "Company_11[:lower:]") ~ "Company_11",
                             str_detect(Sample, "Company_12[:lower:]") ~ "Company_12",
                             str_detect(Sample, "Company_13[:lower:]") ~ "Company_13",
                             str_detect(Sample, "Company_14[:lower:]") ~ "Company_14"))

## filtering data
transfer_data2 <- transfer_data %>% 
  filter(timediff <= 160)



## join manually created, generalized training topics to transfer database
library(readxl)
topics <- read_excel("sensitive_data/training_topics.xlsx")
transfer_data3 <- inner_join(transfer_data2, topics, by = "ResponseId")



write_sav(transfer_data3, "data/Transfer_factors.sav", compress = FALSE)

