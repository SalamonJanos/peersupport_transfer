
# loading packages
library(haven)
library(tidyverse)

library(lme4) # for multilevel models


# import database
work_data <- read_sav("data/Transfer_factors.sav")


## -------------------------------------------- preparation for analysis --------------------------------------

# creating necessary variables for visualization
work_data2 <- work_data %>% 
  filter(Open_or_Closed_Skills == "Open") %>% 
  filter(timediff >= 13 & timediff <= 120) %>%
  mutate(Cohort = case_when(T_colleagues == "1" ~ "3", # all together
                            T_colleagues == "2" ~ "2", # some together
                            T_colleagues == "3" ~ "1", # some previously
                            T_colleagues == "4" ~ "0", # none
                            TRUE ~ as.character(T_colleagues)))


## ---------------------------------------- reporting descriptive statistics ----------------------------------------







