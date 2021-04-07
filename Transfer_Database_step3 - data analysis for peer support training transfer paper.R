
# loading packages
library(haven)
library(tidyverse)
library(table1) # to relabel for descriptive table 
library(flextable) # for making regression table and descriptive table publication ready
library(gtsummary) # for creating descriptive table (as_flex_table function)
library(lavaan) # for calculating Cronbach alpha (cfa function)
library(semTools)  # for calculating Cronbach alpha (reliability function)

library(lme4) # for multilevel models
library(performance) # for the assessment of regression models performance (check_model function)


# import database
work_data <- read_sav("data/Transfer_factors.sav")


## -------------------------------------------------- Preparation for analysis ---------------------------------------------------

# creating necessary variables for visualization
work_data2 <- work_data %>% 
  filter(Open_or_Closed_Skills == "Open") %>% 
  filter(timediff >= 13 & timediff <= 120) %>%
  mutate(Cohort = case_when(T_colleagues == "1" ~ "3", # all together
                            T_colleagues == "2" ~ "2", # some together
                            T_colleagues == "3" ~ "1", # some previously
                            T_colleagues == "4" ~ "0", # none
                            TRUE ~ as.character(T_colleagues)))


## ---------------------------------------------------- Demographics -----------------------------------------------------


# total number of respondents (before filters)
work_data %>% 
  summarise(n())

# total number of respondents (after filters)
work_data2 %>% 
  summarise(n())

# main info about respondents' age
work_data2 %>% 
  summarise(min(age),
            max(age),
            mean(age),
            sd(age))

# reporting most represented types of training programs (by number and percentage of topics)
work_data2 %>%
  group_by(topics) %>% 
  summarise(N = n(),
            Percent = n()/688*100) %>% 
  arrange(desc(Percent))



# creating new dataframe for creating descriptive table
work_data2b <- work_data2
work_data2b$Gender <- as.character(work_data2b$Gender)

# renaming variables for descriptive table
work_data2b$Cohort <- 
  factor(work_data2b$Cohort, levels=c(0,1,2,3),
         labels=c("none", 
                  "some previously",
                  "some together",
                  "all together"))

work_data2b %>%
  group_by(Cohort) %>% 
  summarise(N = n(),
            Percent = n()/688*100) %>% 
  arrange(desc(Percent))


label(work_data2b$timediff) <- "Mean Time lag (SD)"
label(work_data2b$age) <- "Mean Age (SD)"
label(work_data2b$Cohort) <- "Training Cohort Composition"
label(work_data2b$peer) <- "Mean Peer Support (SD)"
label(work_data2b$motivation) <- "Mean Motivation to Transfer (SD)"
label(work_data2b$use) <- "Mean Training Transfer – Use (SD)"



# change reference for descriptive table
work_data2b <- work_data2b %>% 
  mutate(Company2 = case_when(Company == "Company_1" ~ "01",
                              Company == "Company_2" ~ "02",
                              Company == "Company_3" ~ "03",
                              Company == "Company_4" ~ "04",
                              Company == "Company_5" ~ "05",
                              Company == "Company_6" ~ "06",
                              Company == "Company_7" ~ "07",
                              Company == "Company_8" ~ "08",
                              Company == "Company_9" ~ "09",
                              Company == "Company_10" ~ "10",
                              Company == "Company_11" ~ "11",
                              Company == "Company_12" ~ "12",
                              Company == "Company_13" ~ "13",
                              Company == "Company_14" ~ "14",
                              TRUE ~ as.character(Company)))

# select data for descriptive table
desc_data <- work_data2b %>% select(Company2, Gender, age, Management, timediff, 
                                    Cohort, peer, motivation, use)


# create descriptive table
flex_table2 <- 
  tbl_summary(
    desc_data,
    by = Company2, # split table by group
    value = list(Gender ~ "2",
                 Management ~ "1"),
    label = list(Gender ~ "Gender (Female)",
                 Management ~ "Organizational level (Manager)"),
    statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{p}%")) %>%
  modify_header(stat_by = c("**{level}**, N = {n}")) %>%
  modify_footnote(update = everything() ~ NA) %>% 
  add_overall(last = TRUE) %>% 
  modify_header(label = "Variables / Company codes") %>%  # update the column header
  as_flex_table() %>% 
  width(width = 0.55) %>% 
  add_footer_lines(c("Note. The table shows the descriptive statistics (means of continuous variables, and frequencies of categorical variables) by companies.  
                     Time Lag: Time Lag Between Training & DV Measure.")) %>% 
  fontsize(size = 11, part = "all") %>%
  font(fontname = "Times New Roman", part = "all")


# saving table to word file
save_as_docx("Table 1. Descriptive statistics" = flex_table2, path = "Descriptives table - peer support.docx")


## ---------------------------------------------------- 1. Preliminary analyses -----------------------------------------------------

## -------------------------- 1.1. Internal consistency (alpha and omega) calculation ---------------------------

# calculating Cronbach alpha for Peer Support
peer_factor <- '
peer_factor =~ pee33 + pee35 + pee311' 

fit_peer <- cfa(peer_factor, data = work_data2, estimator = 'MLR')
peer_rel <- as.data.frame(reliability(fit_peer))


# calculating Cronbach alpha for Motivation to Transfer
motivation_factor <- '
motiv_factor =~ mot26 + mot28 + mot212'

fit_motivation <- cfa(motivation_factor, data = work_data2, estimator = 'MLR')
motiv_rel <- as.data.frame(reliability(fit_motivation))


# calculating Cronbach alpha for Training Transfer
transfer_factor <- '
use_factor =~ use1 + use3 + use5 + use7'

fit_transf <- cfa(transfer_factor, data = work_data2, estimator = 'MLR')
transfer_rel <- as.data.frame(reliability(fit_transf))


reliabilities <- cbind(peer_rel, motiv_rel, transfer_rel)
reliabilities2 <- round(reliabilities[1,], 3)




## ---------------------------------------- 1.2. CORRELATION of MEASURED variables ------------------------------------

# 1.2.1. preparation for correlation table --------------------------------

# check variables normality
shapiro.test(work_data2$peer)
shapiro.test(work_data2$motivation)
shapiro.test(work_data2$use)

# significant Shapiro-Wilk normality tests --> Spearman correlations are necessary

corr_input <- c("Management", "timediff",
                "Cohort", "peer", 
                "motivation", "use")

work_data3 <- work_data2 %>% 
  mutate_at(vars(Cohort, Management), as.numeric)

corr_table <- work_data3 %>% 
  select(., one_of(corr_input))


# 1.2.2. function to create Spearman corr table ---------------------------
## function necessary for Spearman correlation table with stars indicating significance levels

# source of the following function: http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package
# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "***", ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 

# 1.2.3. calculating correlations and creating correlation matrix ---------

correlation_table <- corstars(corr_table, method = "spearman")

# adding column to be able to combine correlation table with descriptive table
correlation_table <- correlation_table %>%
  mutate(use = "")

# 1.2.4. calculating descriptives for final correlation table -------------

all_descriptives <- as.data.frame(psych::describe(work_data3[,corr_input], skew = TRUE))
descr1 <- all_descriptives[,c("mean", "sd")]
descr_table1 <- t(descr1)


# source of the following function: https://jeromyanglim.tumblr.com/post/50228877196/round-numbers-in-data-frame-that-contains-non
round_df <- function(x, digits) {
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

# rounding numbers
descr_table1_2 <- round_df(descr_table1, 2)


# 1.2.5. combining descriptives and correlation tables --------------------

combined_tables <- rbind(correlation_table, descr_table1_2)


row.names(combined_tables) <- c("1. Manager", 
                                "2. Time Lag",
                                "3. Cohort Composition", 
                                "4. Peer Support",  
                                "5. Motivation to Transfer", 
                                "6. Training Transfer – Use",
                                "Mean", "SD")


# setting row names to first column
combined_tables <- tibble::rownames_to_column(combined_tables, " ")

combined_tables <- combined_tables %>%  
  rename(
    "1" = Management,
    "2" = timediff,
    "3" = Cohort,
    "4" = peer,
    "5" = motivation,
    "6" = use)


# (Table) Descr stat and Spearman bivariate corr btw MEASURED vars --------

designed_table <- combined_tables %>% 
  flextable() %>% 
  hline(i = 6, part = "body", border = officer::fp_border()) %>% 
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  align(align = "left", part = "all") %>% 
  width(width = 0.59) %>% 
  #  set_table_properties(width = 1) %>% 
  add_footer_lines(c("Note. N=311, Manager (0 = Non-managers, 1 = Managers); Time Lag: Time Lag Between Training & DV Measure.", 
                     "* p < .05, ** p < .01, *** p < 0.001"))

# saving final table to word file
save_as_docx("Table 2. Descriptive statistics and Spearman bivariate correlations between variables" = designed_table, path = "correlation table - peer support.docx")


## ----------------------------------------------- 2. MAIN ANALYSIS ------------------------------------------

# 2.1. preparation for main analysis -------------------------------------------

# standardization of continuous and ordinal variables
work_data3 <- work_data3 %>% 
  mutate_at(list(std = ~scale(., center = TRUE, scale = TRUE)[, 1]), 
            .vars=c("timediff",
                    "Cohort", "peer",
                    "motivation", "use"))


# 2.2. Multilevel Modeling - Motivation to Transfer DV --------------------

# Baseline Model (Model 0)
model0_fit <- lmer(formula = motivation_std ~ 1 + 
                     (1|Company), 
                   data=work_data3,
                   REML = FALSE) #Maximum Likelihood

# Model 1
model1_fit <- lmer(formula = motivation_std ~ 1 + factor(Management) + 
                     (1|Company), 
                   data=work_data3,
                   REML = FALSE)

# Model 2
model2_fit <- lmer(formula = motivation_std ~ 1 + factor(Management) + timediff_std + 
                     (1|Company), 
                   data=work_data3,
                   REML = FALSE)

# Model 3
model3_fit <- lmer(formula = motivation_std ~ 1 + factor(Management) + timediff_std + 
                     peer_std + 
                     (1|Company), 
                   data=work_data3,
                   REML = FALSE)

# Model 4
model4_fit <- lmer(formula = motivation_std ~ 1 + factor(Management) + timediff_std + 
                     peer_std + Cohort_std + 
                     (1|Company), 
                   data=work_data3,
                   REML = FALSE)

# Model 5
model5_fit <- lmer(formula = motivation_std ~ 1 + factor(Management) + timediff_std + 
                     peer_std + Cohort_std + peer_std * Cohort_std +
                     (1|Company), 
                   data=work_data3,
                   REML = FALSE)
summary(model5_fit)


anova(model0_fit, model1_fit, model2_fit, model3_fit, model4_fit, model5_fit)

check_model(model5_fit)

library(jtools)
summ(model5_fit, cluster = "Company", digits = 3)











