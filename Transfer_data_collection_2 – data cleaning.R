## Transfer_data_collection_2 – data cleaning


# loading packages
library(tidyverse)
library(lavaan)
library(haven)
library(anytime)
library(lubridate)


### ----------------------- I. CREATING ONE DATABASE from individual data files ---------------------------

# ## ---------------------------------- IMPORT DATA ---------------------------------


# Import data of Company_1b
work_data_raw_C1b <- read_sav("data/Transfer_Company_1b.sav")

# Process data of Company_1b
work_data_C1b <-
  work_data_raw_C1b %>% 
  rename(T_date_First = T_date_Meggy) %>% 
  rename(T_date_Last = T_date_Other) %>% 
  add_column(., Sample = "Company_1b", .before = 1)

# Create training date df
training_dates <-
  work_data_C1b %>%
  select(ResponseId, T_date_First:T_date_Last) %>% 
  mutate_if(is.labelled, as_factor) %>% 
  gather(var, original_date_string, -ResponseId, na.rm = TRUE) %>% 
  filter(original_date_string != "" & original_date_string != "Más dátum:") 

# Modify problematic strings
training_dates$original_date_string <- 
  str_replace_all(training_dates$original_date_string,
                  c("2018.11.12-14" = "2018/11/12 - 2018/11/14",
                    "2019/04$" = "2019/04/01", 
                    "2019. 04. 29. és 2019. 04. 30." = "2019/04/29 - 2019/04/30",
                    "2019/09/02-03" = "2019/09/02 - 2019/09/03"))

# Create multiple variables for training date df 
training_dates <-
  training_dates %>%
  separate(original_date_string, 
           into = c("training_start", "training_end"), 
           sep = "\\s*-\\s*", 
           fill = "left", 
           remove = FALSE) %>% 
  mutate_at(vars(training_start, training_end), ymd) %>% 
  mutate(training_length = as.numeric(training_end - training_start) + 1) %>% 
  mutate(training_length = if_else(is.na(training_length), 1, training_length)) %>% 
  select(-var)

# Create training topic df
training_topics <- work_data_C1b %>% 
  select(ResponseId, T_topic) %>% 
  mutate_if(is.labelled, as_factor) %>% 
  gather(var, Training_topic, -ResponseId, na.rm = TRUE) %>% 
  select(-var)

# Join all (original, training date, training topic) data frames
work_data_C1b <- work_data_C1b %>% 
  left_join(training_dates, by = "ResponseId") %>% 
  select(-(T_date_First:T_date_Last)) %>% 
  left_join(training_topics, by = "ResponseId") %>% 
  mutate(Training_topic = ifelse(Training_topic == "Egyéb:", T_topic_29_TEXT, Training_topic)) %>% 
  select(-(c(T_topic, T_topic_29_TEXT)))

## ----------------------------------------------------------------------------------


# Import data of Company_1c
work_data_raw_C1c <- read_sav("data/Transfer_Company_1c.sav")

# Process data of Company_1c
work_data_C1c <-
  work_data_raw_C1c %>% 
  rename(T_date_First = T_date_Meggy) %>% 
  rename(T_date_Last = T_date_Other) %>% 
  add_column(., Sample = "Company_1c", .before = 1)

# Create training date df
training_dates <-
  work_data_C1c %>%
  select(ResponseId, T_date_First:T_date_Last) %>% 
  mutate_if(is.labelled, as_factor) %>% 
  gather(var, original_date_string, -ResponseId, na.rm = TRUE) %>% 
  filter(original_date_string != "" & original_date_string != "Más dátum:") 

# Modify problematic strings
training_dates$original_date_string <- 
  str_replace_all(training_dates$original_date_string,
                  c("2019/03$" = "2019/03.01.",
                    "nov. 4-5" = "2019/11/04 - 2019/11/05", 
                    "2019.11.04-05" = "2019/11/04 - 2019/11/05"))

# Create multiple variables for training date df 
training_dates <-
  training_dates %>%
  separate(original_date_string, 
           into = c("training_start", "training_end"), 
           sep = "\\s*-\\s*", 
           fill = "left", 
           remove = FALSE) %>% 
  mutate_at(vars(training_start, training_end), ymd) %>% 
  mutate(training_length = as.numeric(training_end - training_start) + 1) %>% 
  mutate(training_length = if_else(is.na(training_length), 1, training_length)) %>% 
  select(-var)

# Create training topic df
training_topics <- work_data_C1c %>% 
  select(ResponseId, T_topic) %>% 
  mutate_if(is.labelled, as_factor) %>% 
  gather(var, Training_topic, -ResponseId, na.rm = TRUE) %>% 
  select(-var)

# Join all (original, training date, training topic) data frames
work_data_C1c <- work_data_C1c %>% 
  left_join(training_dates, by = "ResponseId") %>% 
  select(-(T_date_First:T_date_Last)) %>% 
  left_join(training_topics, by = "ResponseId") %>% 
  mutate(Training_topic = ifelse(Training_topic == "Egyéb:", T_topic_29_TEXT, Training_topic)) %>% 
  select(-(c(T_topic, T_topic_29_TEXT)))

## ----------------------------------------------------------------------------------


# Import data of Company_3b
work_data_raw_C3b <- read_sav("data/Transfer_Company_3b.sav")

# Process data of Company_3b
work_data_C3b <-
  work_data_raw_C3b %>% 
  rename(T_date_First = T_date_ActionL) %>% 
  rename(T_date_Last = T_date_Other) %>% 
  add_column(., Sample = "Company_3b", .before = 1)

# Create training date df
training_dates <-
  work_data_C3b %>%
  select(ResponseId, T_date_First:T_date_Last) %>% 
  mutate_if(is.labelled, as_factor) %>% 
  gather(var, original_date_string, -ResponseId, na.rm = TRUE) %>% 
  filter(original_date_string != "" & original_date_string != "Más dátum:") 

# Modify problematic strings
training_dates$original_date_string <- str_replace_all(training_dates$original_date_string, 
                                                       c("2019.05.$" = "2019.05.01.",
                                                         "2019.10$" = "2019/10/01", "2019/10/07-08" = "2019/10/07 - 2019/10/08"))

# Create multiple variables for training date df 
training_dates <-
  training_dates %>%
  separate(original_date_string, 
           into = c("training_start", "training_end"), 
           sep = "\\s*-\\s*", 
           fill = "left", 
           remove = FALSE) %>% 
  mutate_at(vars(training_start, training_end), ymd) %>% 
  mutate(training_length = as.numeric(training_end - training_start) + 1) %>% 
  mutate(training_length = if_else(is.na(training_length), 1, training_length)) %>% 
  select(-var)

# 2019.10
# 2019.05.
# 2019/10/07-08

# Create training topic df
training_topics <- work_data_C3b %>% 
  select(ResponseId, T_topic) %>% 
  mutate_if(is.labelled, as_factor) %>% 
  gather(var, Training_topic, -ResponseId, na.rm = TRUE) %>% 
  select(-var)

# Join all (original, training date, training topic) data frames
work_data_C3b <- work_data_C3b %>% 
  left_join(training_dates, by = "ResponseId") %>% 
  select(-(T_date_First:T_date_Last)) %>% 
  left_join(training_topics, by = "ResponseId") %>% 
  mutate(Training_topic = ifelse(Training_topic == "Egyéb:", T_topic_29_TEXT, Training_topic)) %>% 
  select(-(c(T_topic, T_topic_29_TEXT)))

## ----------------------------------------------------------------------------------


# Import data of Company 3b_Eng
work_data_raw_C3b_Eng <- read_sav("data/Transfer_Company_3b_Eng.sav")


# Process data of Company 3b_Eng
work_data_C3b_Eng <-
  work_data_raw_C3b_Eng %>% 
  rename(T_date_First = T_date_AgileP) %>% 
  rename(T_date_Last = T_date_Other) %>% 
  add_column(., Sample = "Company 3b_Eng", .before = 1)

# Create training date df
training_dates <-
  work_data_C3b_Eng %>%
  select(ResponseId, T_date_First:T_date_Last) %>% 
  mutate_if(is.labelled, as_factor) %>%
  gather(var, original_date_string, -ResponseId, na.rm = TRUE) %>% 
  filter(original_date_string != "" & original_date_string != "Más dátum:") %>% 
  separate(original_date_string, 
           into = c("training_start", "training_end"), 
           sep = "\\s*-\\s*", 
           fill = "left", 
           remove = FALSE) %>% 
  mutate_at(vars(training_start, training_end), ymd) %>% 
  mutate(training_length = as.numeric(training_end - training_start) + 1) %>% 
  mutate(training_length = if_else(is.na(training_length), 1, training_length)) %>% 
  select(-var)

# Other date:

# Create training topic df
training_topics <- work_data_C3b_Eng %>% 
  select(ResponseId, T_topic) %>% 
  mutate_if(is.labelled, as_factor) %>% 
  gather(var, Training_topic, -ResponseId, na.rm = TRUE) %>% 
  select(-var)

# Join all (original, training date, training topic) data frames
work_data_C3b_Eng <- work_data_C3b_Eng %>% 
  left_join(training_dates, by = "ResponseId") %>% 
  select(-(T_date_First:T_date_Last)) %>% 
  left_join(training_topics, by = "ResponseId") %>% 
  mutate(Training_topic = ifelse(Training_topic == "Egyéb:", T_topic_29_TEXT, Training_topic)) %>% 
  select(-(c(T_topic, T_topic_29_TEXT)))

## -------------------------------------------------------------------------------------


# Import data of Company_7b
work_data_raw_C7b <- read_sav("data/Transfer_Company_7b.sav")

# Process data of Company_7b
work_data_C7b <-
  work_data_raw_C7b %>%
  rename(T_date_First = T_date_DK) %>% 
  rename(T_date_Last = T_date_Other) %>% 
  rename(., Q111 = Q45) %>% 
  add_column(., Sample = "Company_7b", .before = 1)

# Create training date df
training_dates <-
  work_data_C7b %>%
  select(ResponseId, T_date_First:T_date_Last) %>% 
  mutate_if(is.labelled, as_factor) %>%
  gather(var, original_date_string, -ResponseId, na.rm = TRUE) %>% 
  filter(original_date_string != "" & original_date_string != "Más dátum:") %>% 
  separate(original_date_string, 
           into = c("training_start", "training_end"), 
           sep = "\\s*-\\s*", 
           fill = "left", 
           remove = FALSE) %>% 
  mutate_at(vars(training_start, training_end), ymd) %>% 
  mutate(training_length = as.numeric(training_end - training_start) + 1) %>% 
  mutate(training_length = if_else(is.na(training_length), 1, training_length)) %>% 
  select(-var)

# Create training topic df
training_topics <- work_data_C7b %>% 
  select(ResponseId, T_topic) %>% 
  mutate_if(is.labelled, as_factor) %>% 
  gather(var, Training_topic, -ResponseId, na.rm = TRUE) %>% 
  select(-var)

# Join all (original, training date, training topic) data frames
work_data_C7b <- work_data_C7b %>% 
  left_join(training_dates, by = "ResponseId") %>% 
  select(-(T_date_First:T_date_Last)) %>% 
  left_join(training_topics, by = "ResponseId") %>% 
  mutate(Training_topic = ifelse(Training_topic == "Egyéb:", T_topic_29_TEXT, Training_topic)) %>% 
  select(-(c(T_topic, T_topic_29_TEXT)))

## ----------------------------------------------------------------------------------


# Import data of Company_7c
work_data_raw_C7c <- read_sav("data/Transfer_Company_7c.sav")

# Process data of Company_7c
work_data_C7c <-
  work_data_raw_C7c %>% 
  rename(T_date_First = T_date_DK) %>% 
  rename(T_date_Last = T_date_Other) %>% 
  add_column(., Sample = "Company_7c", .before = 1)


# Create training date df
training_dates <-
  work_data_C7c %>%
  select(ResponseId, T_date_First:T_date_Last) %>% 
  mutate_if(is.labelled, as_factor) %>%
  gather(var, original_date_string, -ResponseId, na.rm = TRUE) %>% 
  filter(original_date_string != "" & original_date_string != "Más dátum:") %>% 
  separate(original_date_string, 
           into = c("training_start", "training_end"), 
           sep = "\\s*-\\s*", 
           fill = "left", 
           remove = FALSE) %>% 
  mutate_at(vars(training_start, training_end), ymd) %>% 
  mutate(training_length = as.numeric(training_end - training_start) + 1) %>% 
  mutate(training_length = if_else(is.na(training_length), 1, training_length)) %>% 
  select(-var)

# Create training topic df
training_topics <- work_data_C7c %>% 
  select(ResponseId, T_topic) %>% 
  mutate_if(is.labelled, as_factor) %>% 
  gather(var, Training_topic, -ResponseId, na.rm = TRUE) %>% 
  select(-var)

# Join all (original, training date, training topic) data frames
work_data_C7c <- work_data_C7c %>% 
  left_join(training_dates, by = "ResponseId") %>% 
  select(-(T_date_First:T_date_Last)) %>% 
  left_join(training_topics, by = "ResponseId") %>% 
  mutate(Training_topic = ifelse(Training_topic == "Egyéb:", T_topic_29_TEXT, Training_topic)) %>% 
  select(-(c(T_topic, T_topic_29_TEXT)))

## ----------------------------------------------------------------------------------


# Import data of Company_7d
work_data_raw_C7d <- read_sav("data/Transfer_Company_7d.sav")

# Process data of Company_7d
work_data_C7d <-
  work_data_raw_C7d %>% 
  rename(T_date_First = T_date_DK) %>% 
  rename(T_date_Last = T_date_Other) %>% 
  add_column(., Sample = "Company_7d", .before = 1)


# Create training date df
training_dates <-
  work_data_C7d %>%
  select(ResponseId, T_date_First:T_date_Last) %>% 
  mutate_if(is.labelled, as_factor) %>%
  gather(var, original_date_string, -ResponseId, na.rm = TRUE) %>% 
  filter(original_date_string != "" & original_date_string != "Más dátum:") %>% 
  separate(original_date_string, 
           into = c("training_start", "training_end"), 
           sep = "\\s*-\\s*", 
           fill = "left", 
           remove = FALSE) %>% 
  mutate_at(vars(training_start, training_end), ymd) %>% 
  mutate(training_length = as.numeric(training_end - training_start) + 1) %>% 
  mutate(training_length = if_else(is.na(training_length), 1, training_length)) %>% 
  select(-var)

# Create training topic df
training_topics <- work_data_C7d %>% 
  select(ResponseId, T_topic) %>% 
  mutate_if(is.labelled, as_factor) %>% 
  gather(var, Training_topic, -ResponseId, na.rm = TRUE) %>% 
  select(-var)

# Join all (original, training date, training topic) data frames
work_data_C7d <- work_data_C7d %>% 
  left_join(training_dates, by = "ResponseId") %>% 
  select(-(T_date_First:T_date_Last)) %>% 
  left_join(training_topics, by = "ResponseId") %>% 
  mutate(Training_topic = ifelse(Training_topic == "Egyéb:", T_topic_29_TEXT, Training_topic)) %>% 
  select(-(c(T_topic, T_topic_29_TEXT)))

## ----------------------------------------------------------------------------------


# Import data of Company_9b
work_data_raw_C9b <- read_sav("data/Transfer_Company_9b.sav")

# Process data of Company_9b
work_data_C9b <-
  work_data_raw_C9b %>% 
  rename(T_date_First = T_date_CsF) %>% 
  rename(T_date_Last = T_date_Other) %>% 
  add_column(., Sample = "Company_9b", .before = 1)

# Create training date df
training_dates <-
  work_data_C9b %>%
  select(ResponseId, T_date_First:T_date_Last) %>% 
  mutate_if(is.labelled, as_factor) %>%
  gather(var, original_date_string, -ResponseId, na.rm = TRUE) %>% 
  filter(original_date_string != "" & original_date_string != "Más dátum:") %>% 
  separate(original_date_string, 
           into = c("training_start", "training_end"), 
           sep = "\\s*-\\s*", 
           fill = "left", 
           remove = FALSE) %>% 
  mutate_at(vars(training_start, training_end), ymd) %>% 
  mutate(training_length = as.numeric(training_end - training_start) + 1) %>% 
  mutate(training_length = if_else(is.na(training_length), 1, training_length)) %>% 
  select(-var)

# Create training topic df
training_topics <- work_data_C9b %>% 
  select(ResponseId, T_topic) %>% 
  mutate_if(is.labelled, as_factor) %>% 
  gather(var, Training_topic, -ResponseId, na.rm = TRUE) %>% 
  select(-var)

# Join all (original, training date, training topic) data frames
work_data_C9b <- work_data_C9b %>% 
  left_join(training_dates, by = "ResponseId") %>% 
  select(-(T_date_First:T_date_Last)) %>% 
  left_join(training_topics, by = "ResponseId") %>% 
  mutate(Training_topic = ifelse(Training_topic == "Egyéb:", T_topic_29_TEXT, Training_topic)) %>% 
  select(-(c(T_topic, T_topic_29_TEXT)))

## ----------------------------------------------------------------------------------


# Import data of Company_11a
work_data_raw_C11a <- read_sav("data/Transfer_Company_11a.sav")

# Process data of Company_11a
work_data_C11a <-
  work_data_raw_C11a %>% 
  rename(T_date_First = T_date_TM_PM) %>% 
  rename(T_date_Last = T_date_Other) %>% 
  add_column(., Sample = "Company_11a", .before = 1)

# Create training date df
training_dates <-
  work_data_C11a %>%
  select(ResponseId, T_date_First:T_date_Last) %>% 
  mutate_if(is.labelled, as_factor) %>%
  gather(var, original_date_string, -ResponseId, na.rm = TRUE) %>% 
  filter(original_date_string != "" & original_date_string != "Más dátum:") %>% 
  separate(original_date_string, 
           into = c("training_start", "training_end"), 
           sep = "\\s*-\\s*", 
           fill = "left", 
           remove = FALSE) %>% 
  mutate_at(vars(training_start, training_end), ymd) %>% 
  mutate(training_length = as.numeric(training_end - training_start) + 1) %>% 
  mutate(training_length = if_else(is.na(training_length), 1, training_length)) %>% 
  select(-var)

# 2017.10

# Create training topic df
training_topics <- work_data_C11a %>% 
  select(ResponseId, T_topic) %>% 
  mutate_if(is.labelled, as_factor) %>% 
  gather(var, Training_topic, -ResponseId, na.rm = TRUE) %>% 
  select(-var)

# Join all (original, training date, training topic) data frames
work_data_C11a <- work_data_C11a %>% 
  left_join(training_dates, by = "ResponseId") %>% 
  select(-(T_date_First:T_date_Last)) %>% 
  left_join(training_topics, by = "ResponseId") %>% 
  mutate(Training_topic = ifelse(Training_topic == "Egyéb:", T_topic_29_TEXT, Training_topic)) %>% 
  select(-(c(T_topic, T_topic_29_TEXT)))

## ----------------------------------------------------------------------------------


# Import data of Company_12a
work_data_raw_C12a <- read_sav("data/Transfer_Company_12a.sav")

# Process data of Company_12a
work_data_C12a <-
  work_data_raw_C12a %>% 
  rename(T_date_First = T_date_Konfl) %>% 
  rename(T_date_Last = T_date_Other) %>% 
  add_column(., Sample = "Company_12a", .before = 1) %>% 
  .[ grep("rtzhrtzrt", .$T_topic_29_TEXT, invert = TRUE) , ]

# Create training date df
training_dates <-
  work_data_C12a %>%
  select(ResponseId, T_date_First:T_date_Last) %>% 
  mutate_if(is.labelled, as_factor) %>%
  gather(var, original_date_string, -ResponseId, na.rm = TRUE) %>% 
  filter(original_date_string != "" & original_date_string != "Más dátum:") %>% 
  separate(original_date_string, 
           into = c("training_start", "training_end"), 
           sep = "\\s*-\\s*", 
           fill = "left", 
           remove = FALSE) %>% 
  mutate_at(vars(training_start, training_end), ymd) %>% 
  mutate(training_length = as.numeric(training_end - training_start) + 1) %>% 
  mutate(training_length = if_else(is.na(training_length), 1, training_length)) %>% 
  select(-var)

# R_2yg9Pn9NtU6ixZP - "Nem emlékszem"

# Create training topic df
training_topics <- work_data_C12a %>% 
  select(ResponseId, T_topic) %>% 
  mutate_if(is.labelled, as_factor) %>% 
  gather(var, Training_topic, -ResponseId, na.rm = TRUE) %>% 
  select(-var)

# Join all (original, training date, training topic) data frames
work_data_C12a <- work_data_C12a %>% 
  left_join(training_dates, by = "ResponseId") %>% 
  select(-(T_date_First:T_date_Last)) %>% 
  left_join(training_topics, by = "ResponseId") %>% 
  mutate(Training_topic = ifelse(Training_topic == "Egyéb:", T_topic_29_TEXT, Training_topic)) %>% 
  select(-(c(T_topic, T_topic_29_TEXT)))

## ----------------------------------------------------------------------------------


# Import data of Company_12b
work_data_raw_C12b <- read_sav("data/Transfer_Company_12b.sav")

# Process data of Company_12b
work_data_C12b <-
  work_data_raw_C12b %>% 
  rename(T_date_First = T_date_Dont) %>% 
  rename(T_date_Last = T_date_Other) %>% 
  add_column(., Sample = "Company_12b", .before = 1) 


training_dates <-
  work_data_C12b %>%
  select(ResponseId, T_date_First:T_date_Last) %>% 
  mutate_if(is.labelled, as_factor) %>% 
  gather(var, original_date_string, -ResponseId, na.rm = TRUE) %>% 
  filter(original_date_string != "" & original_date_string != "Más dátum:") 

# Modify problematic strings
training_dates$original_date_string <- 
  str_replace_all(training_dates$original_date_string,
                  c("2019/10629" = "2019/10/29",
                    "2019/11/22-29" = "2019/11/22 - 2019/11/29"))

# Create multiple variables for training date df 
training_dates <-
  training_dates %>%
  separate(original_date_string, 
           into = c("training_start", "training_end"), 
           sep = "\\s*-\\s*", 
           fill = "left", 
           remove = FALSE) %>% 
  mutate_at(vars(training_start, training_end), ymd) %>% 
  mutate(training_length = as.numeric(training_end - training_start) + 1) %>% 
  mutate(training_length = if_else(is.na(training_length), 1, training_length)) %>% 
  select(-var)

# Create training topic df
training_topics <- work_data_C12b %>% 
  select(ResponseId, T_topic) %>% 
  mutate_if(is.labelled, as_factor) %>% 
  gather(var, Training_topic, -ResponseId, na.rm = TRUE) %>% 
  select(-var)

# Join all (original, training date, training topic) data frames
work_data_C12b <- work_data_C12b %>% 
  left_join(training_dates, by = "ResponseId") %>% 
  select(-(T_date_First:T_date_Last)) %>% 
  left_join(training_topics, by = "ResponseId") %>% 
  mutate(Training_topic = ifelse(Training_topic == "Egyéb:", T_topic_29_TEXT, Training_topic)) %>% 
  select(-(c(T_topic, T_topic_29_TEXT)))

## ----------------------------------------------------------------------------------


# Import data of Company_13a
work_data_raw_C13a <- read_sav("data/Transfer_Company_13a.sav")

# Process data of Company_13a
work_data_C13a <-
  work_data_raw_C13a %>% 
  rename(T_date_First = T_date_Kritik) %>% 
  rename(T_date_Last = T_date_Other) %>% 
  add_column(., Sample = "Company_13a", .before = 1)

# Create training date df
training_dates <-
  work_data_C13a %>%
  select(ResponseId, T_date_First:T_date_Last) %>% 
  mutate_if(is.labelled, as_factor) %>%
  gather(var, original_date_string, -ResponseId, na.rm = TRUE) %>% 
  filter(original_date_string != "" & original_date_string != "Más dátum:") %>% 
  separate(original_date_string, 
           into = c("training_start", "training_end"), 
           sep = "\\s*-\\s*", 
           fill = "left", 
           remove = FALSE) %>% 
  mutate_at(vars(training_start, training_end), ymd) %>% 
  mutate(training_length = as.numeric(training_end - training_start) + 1) %>% 
  mutate(training_length = if_else(is.na(training_length), 1, training_length)) %>% 
  select(-var)

# Create training topic df
training_topics <- work_data_C13a %>% 
  select(ResponseId, T_topic) %>% 
  mutate_if(is.labelled, as_factor) %>% 
  gather(var, Training_topic, -ResponseId, na.rm = TRUE) %>% 
  select(-var)

# Join all (original, training date, training topic) data frames
work_data_C13a <- work_data_C13a %>% 
  left_join(training_dates, by = "ResponseId") %>% 
  select(-(T_date_First:T_date_Last)) %>% 
  left_join(training_topics, by = "ResponseId") %>% 
  mutate(Training_topic = ifelse(Training_topic == "Egyéb:", T_topic_29_TEXT, Training_topic)) %>% 
  select(-(c(T_topic, T_topic_29_TEXT)))

## ----------------------------------------------------------------------------------


# Import data of Company_13b
work_data_raw_C13b <- read_sav("data/Transfer_Company_13b.sav")

# Process data of Company_13b
work_data_C13b <-
  work_data_raw_C13b %>% 
  rename(T_date_First = T_date_Kritik) %>% 
  rename(T_date_Last = T_date_Other) %>% 
  add_column(., Sample = "Company_13b", .before = 1)

# Create training date df
training_dates <-
  work_data_C13b %>%
  select(ResponseId, T_date_First:T_date_Last) %>% 
  mutate_if(is.labelled, as_factor) %>%
  gather(var, original_date_string, -ResponseId, na.rm = TRUE) %>% 
  filter(original_date_string != "" & original_date_string != "Más dátum:") %>% 
  separate(original_date_string, 
           into = c("training_start", "training_end"), 
           sep = "\\s*-\\s*", 
           fill = "left", 
           remove = FALSE) %>% 
  mutate_at(vars(training_start, training_end), ymd) %>% 
  mutate(training_length = as.numeric(training_end - training_start) + 1) %>% 
  mutate(training_length = if_else(is.na(training_length), 1, training_length)) %>% 
  select(-var)

# Create training topic df
training_topics <- work_data_C13b %>% 
  select(ResponseId, T_topic) %>% 
  mutate_if(is.labelled, as_factor) %>% 
  gather(var, Training_topic, -ResponseId, na.rm = TRUE) %>% 
  select(-var)

# Join all (original, training date, training topic) data frames
work_data_C13b <- work_data_C13b %>% 
  left_join(training_dates, by = "ResponseId") %>% 
  select(-(T_date_First:T_date_Last)) %>% 
  left_join(training_topics, by = "ResponseId") %>% 
  mutate(Training_topic = ifelse(Training_topic == "Egyéb:", T_topic_29_TEXT, Training_topic)) %>% 
  select(-(c(T_topic, T_topic_29_TEXT)))

## ----------------------------------------------------------------------------------


# Import data of Company_13c
work_data_raw_C13c <- read_sav("data/Transfer_Company_13c.sav")

# Process data of Company_13c
work_data_C13c <-
  work_data_raw_C13c %>% 
  rename(T_date_First = T_date_FelhatV) %>% 
  rename(T_date_Last = T_date_Other) %>% 
  add_column(., Sample = "Company_13c", .before = 1)

# Create training date df
training_dates <-
  work_data_C13c %>%
  select(ResponseId, T_date_First:T_date_Last) %>% 
  mutate_if(is.labelled, as_factor) %>%
  gather(var, original_date_string, -ResponseId, na.rm = TRUE) %>% 
  filter(original_date_string != "" & original_date_string != "Más dátum:") %>% 
  separate(original_date_string, 
           into = c("training_start", "training_end"), 
           sep = "\\s*-\\s*", 
           fill = "left", 
           remove = FALSE) %>% 
  mutate_at(vars(training_start, training_end), ymd) %>% 
  mutate(training_length = as.numeric(training_end - training_start) + 1) %>% 
  mutate(training_length = if_else(is.na(training_length), 1, training_length)) %>% 
  select(-var)

# Create training topic df
training_topics <- work_data_C13c %>% 
  select(ResponseId, T_topic) %>% 
  mutate_if(is.labelled, as_factor) %>% 
  gather(var, Training_topic, -ResponseId, na.rm = TRUE) %>% 
  select(-var)

# Join all (original, training date, training topic) data frames
work_data_C13c <- work_data_C13c %>% 
  left_join(training_dates, by = "ResponseId") %>% 
  select(-(T_date_First:T_date_Last)) %>% 
  left_join(training_topics, by = "ResponseId") %>% 
  mutate(Training_topic = ifelse(Training_topic == "Egyéb:", T_topic_29_TEXT, Training_topic)) %>% 
  select(-(c(T_topic, T_topic_29_TEXT)))

## ----------------------------------------------------------------------------------


# Import data of Company_14a
work_data_raw_C14a <- read_sav("data/Transfer_Company_14a.sav")

# Process data of Company_14a
work_data_C14a <-
  work_data_raw_C14a %>% 
  rename(T_date_First = T_date_Energia) %>% 
  rename(T_date_Last = T_date_Other) %>% 
  add_column(., Sample = "Company_14a", .before = 1)

# Create training date df
training_dates <-
  work_data_C14a %>%
  select(ResponseId, T_date_First:T_date_Last) %>% 
  mutate_if(is.labelled, as_factor) %>%
  gather(var, original_date_string, -ResponseId, na.rm = TRUE) %>% 
  filter(original_date_string != "" & original_date_string != "Más dátum:") %>% 
  separate(original_date_string, 
           into = c("training_start", "training_end"), 
           sep = "\\s*-\\s*", 
           fill = "left", 
           remove = FALSE) %>% 
  mutate_at(vars(training_start, training_end), ymd) %>% 
  mutate(training_length = as.numeric(training_end - training_start) + 1) %>% 
  mutate(training_length = if_else(is.na(training_length), 1, training_length)) %>% 
  select(-var)

# Create training topic df
training_topics <- work_data_C14a %>% 
  select(ResponseId, T_topic) %>% 
  mutate_if(is.labelled, as_factor) %>% 
  gather(var, Training_topic, -ResponseId, na.rm = TRUE) %>% 
  select(-var)

# Join all (original, training date, training topic) data frames
work_data_C14a <- work_data_C14a %>% 
  left_join(training_dates, by = "ResponseId") %>% 
  select(-(T_date_First:T_date_Last)) %>% 
  left_join(training_topics, by = "ResponseId") %>% 
  mutate(Training_topic = ifelse(Training_topic == "Egyéb:", T_topic_29_TEXT, Training_topic)) %>% 
  select(-(c(T_topic, T_topic_29_TEXT)))

## ----------------------------------------------------------------------------------------


## ----------------------------------- database for drawing ------------------------------ 

# create one data file from the 15 samples - bind rows
database_draw <- bind_rows(mutate_all(work_data_C7b, as.character), 
                           mutate_all(work_data_C7c, as.character), mutate_all(work_data_C7d, as.character), 
                           mutate_all(work_data_C1b, as.character), mutate_all(work_data_C1c, as.character),
                           mutate_all(work_data_C12a, as.character), mutate_all(work_data_C12b, as.character),
                           mutate_all(work_data_C14a, as.character), mutate_all(work_data_C11a, as.character),
                           mutate_all(work_data_C13a, as.character), mutate_all(work_data_C13b, as.character),
                           mutate_all(work_data_C13c, as.character),
                           mutate_all(work_data_C9b, as.character),
                           mutate_all(work_data_C3b, as.character), 
                           mutate_all(work_data_C3b_Eng, as.character))

draw_cols <- c("Sample", "Email", "Record_Date", "Data_prot_3", "Draw_Interview")

Draw_data <-
  database_draw %>%
  rename(., Draw_Interview = Q111) %>% 
  rename(., Record_Date = RecordedDate) %>% 
  mutate(openness = ifelse(Email != "" & Data_prot_3 == 1 & Draw_Interview == 1 | 
                             Email != "" & Data_prot_3 == 1 & Draw_Interview == 2, 1, 0)) %>%
  filter(openness == 1) %>%
  select(., one_of(draw_cols))

write_sav(Draw_data, "data/output-files/draw_2020-01-13.sav", compress = FALSE)



## ---------------------- database for analysis (only from the Hungarian survey)  ------------------------

# create one data file from the 14 samples - bind rows
work_data <- bind_rows(mutate_all(work_data_C7b, as.character), 
                       mutate_all(work_data_C7c, as.character), mutate_all(work_data_C7d, as.character), 
                       mutate_all(work_data_C1b, as.character), mutate_all(work_data_C1c, as.character),
                       mutate_all(work_data_C12a, as.character), mutate_all(work_data_C12b, as.character),
                       mutate_all(work_data_C14a, as.character), mutate_all(work_data_C11a, as.character),
                       mutate_all(work_data_C13a, as.character), mutate_all(work_data_C13b, as.character),
                       mutate_all(work_data_C13c, as.character),
                       mutate_all(work_data_C9b, as.character), 
                       mutate_all(work_data_C3b, as.character))


# write_sav(work_data, "data/output-files/Transfer_datacollection2_onedata.sav", compress = FALSE)



### --------------------------- II. DATA PREPARATION FOR ANALYSIS -------------------------------

# work_data <- read_sav("data/output-files/Transfer_datacollection2_onedata.sav")


# select non-necessary variables
timer_vars <- grep(x = names(work_data), pattern = "timer", value = TRUE)

remove_cols <- c("StartDate", "EndDate", "Status", "IPAddress", 
                 "Finished", 
                 "RecipientLastName", "RecipientFirstName", "RecipientEmail", 
                 "ExternalReference", "LocationLatitude", "LocationLongitude", 
                 "DistributionChannel", "UserLanguage", "Intro_2", timer_vars)

# create, rename and remove selected variables
work_data2 <- work_data %>% 
  filter(as.numeric(Progress) > 85) %>%
  filter(., as.numeric(Outc_7) >= 1) %>%
  rename(., Draw_Interview = Q111) %>% 
  rename(., Record_Date = RecordedDate) %>% 
  mutate(Record_Date=as.Date(Record_Date)) %>%
  mutate(timediff = as.Date(Record_Date) - as.Date(training_end)) %>% 
  select(-one_of(remove_cols))


# redefining variables class
i <- c(2, 6:11, 13:19, 22:30, 33:62, 64:112, 114, 115)

work_data2[ , i] <- apply(work_data2[ , i], 2,            # Specify own function within apply
                          function(x) as.numeric(x))


## ----------------------------------- change in filters --------------------------------------------

work_data_3 <- work_data2 %>% 
  filter(timediff <= 365, timediff > 5) 

datacollection2_analysis <- work_data_3 %>% 
  select(., -contains("email"))


write_sav(datacollection2_analysis, "data/output-files/Transfer_data_collection_2.sav", compress = FALSE)



