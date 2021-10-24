
# loading packages
library(haven)
library(tidyverse)

library(table1) # to relabel for descriptive table 
library(flextable) # for making tables publication ready
library(gtsummary) # for creating descriptive table (as_flex_table function)
library(gtools) # for creating significance stars from p values (stars.pval function for correlation table)

library(processR) # for moderated mediation analysis using manifest variables
library(lavaan) # for moderated mediation analysis using latent variables
library(semTools) # for creating product indicators for the latent interactions


# import database
work_data <- read_sav("data/Transfer_factors.sav")


# Preparation for analysis ------------------------------------------------

# filtering relevant data and creating necessary variables for analysis
work_data2 <- work_data %>% 
  filter(Open_or_Closed_Skills == "Open") %>% 
  filter(timediff >= 13 & timediff <= 120) %>%
  mutate(Cohort = case_when(T_colleagues == "1" ~ 2L, # all together
                            T_colleagues == "3" ~ 1L, # some previously
                            T_colleagues == "2" ~ 1L, # some together
                            T_colleagues == "4" ~ 0L, # none
                            TRUE ~ NA_integer_))


# I. DESCRIPTIVE STATISTICS -----------------------------------------------

# total number of respondents (before filters)
work_data %>% 
  summarise(n())

# total number of respondents (after filters)
work_data2 %>% 
  summarise(n())

## Demographics
# main info about respondents' age
work_data2 %>% 
  summarise(min(age),
            max(age),
            mean(age),
            sd(age))

work_data2 %>% 
  group_by(Gender) %>%
  summarise(N = n(),
            Percent = n()/688*100)

work_data2 %>% 
  group_by(Management) %>%
  summarise(N = n(),
            Percent = n()/688*100)

# reporting most represented types of training programs (by number and percentage of topics)
work_data2 %>%
  group_by(topics) %>% 
  summarise(N = n(),
            Percent = n()/688*100) %>% 
  arrange(desc(Percent))

# reporting average peer support by cohort types
work_data2 %>% 
  summarise(mean(peer),
            sd(peer))

work_data2 %>% 
  group_by(Cohort) %>%
  summarise(mean(peer),
            sd(peer))



# creating new dataframe for descriptive table
work_data2b <- work_data2
work_data2b$Gender <- as.character(work_data2b$Gender)

# renaming variables for descriptive table
work_data2b$Cohort <- 
  factor(work_data2b$Cohort, levels=c(0,1,2),
         labels=c("None", 
                  "Some",
                  "Nearly All"))

work_data2b %>%
  group_by(Cohort) %>% 
  summarise(N = n(),
            Percent = n()/688*100) %>% 
  arrange(desc(Percent))



label(work_data2b$timediff) <- "Mean Time lag (SD)"
label(work_data2b$age) <- "Mean Age (SD)"
label(work_data2b$Cohort) <- "Coworkers' Participation"
label(work_data2b$peer) <- "Mean Peer Support (SD)"
label(work_data2b$motivation) <- "Mean Motivation to Transfer (SD)"
label(work_data2b$use) <- "Mean Perceived Training Transfer (SD)"



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
                     Time Lag: Time Lag Between Training & DV Measure (in days).")) %>% 
  fontsize(size = 11, part = "all") %>%
  font(fontname = "Times New Roman", part = "all")


# saving table to word file
save_as_docx("Table 1. Descriptive statistics by participating companies" = flex_table2, 
             path = "Table 1 - Descriptives table - peer support.docx")



# II. PRELIMINARY ANALYSIS ------------------------------------------------

# 1. Internal consistency (alpha) calculation -----------------------------
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
tr_factor =~ use1 + use3 + use5 + use7'

fit_transf <- cfa(transfer_factor, data = work_data2, estimator = 'MLR')
transfer_rel <- as.data.frame(reliability(fit_transf))


reliabilities <- cbind(peer_rel, motiv_rel, transfer_rel)
reliabilities2 <- round(reliabilities[1,], 3)



# 2. Standardized Parameter Estimates from the Preliminary Model ----------

# calculating composite reliability, McDonald's omega

# transfer model 1 reliability check
transfer_corr_model_rel <- '
# regressions
peers =~ p1*pee33 + p2*pee35 + p3*pee311

# Error Variance
pee33~~ep1*pee33
pee35~~ep2*pee35
pee311~~ep3*pee311

#Reliability
omega.p := 
((p1 + p2 + p3)^2) 
/ 
((p1 + p2 + p3)^2 + 
(ep1 + ep2 + ep3))

#Average Variance Extracted (AVE)
ave_p := 
((p1^2) + (p2^2) + (p3^2)) / 3


motiv =~ m1*mot26 + m2*mot28 + m3*mot212

# Error Variance
mot26~~em1*mot26
mot28~~em2*mot28
mot212~~em3*mot212

#Reliability
omega.m := 
((m1 + m2 + m3)^2) 
/ 
((m1 + m2 + m3)^2 + 
(em1 + em2 + em3))

#Average Variance Extracted (AVE)
ave_m := 
((m1^2) + (m2^2) + (m3^2)) / 3


transfer =~ t1*use1 + t2*use3 + t3*use5 + t4*use7

# Error Variance
use1~~et1*use1
use3~~et2*use3
use5~~et3*use5
use7~~et4*use7


#Reliability
omega.t := 
((t1 + t2 + t3 + t4)^2) 
/ 
((t1 + t2 + t3 + t4)^2 + 
(et1 + et2 + et3 + et4))

#Average Variance Extracted (AVE)
ave_t := 
((t1^2) + (t2^2) + (t3^2) + (t4^2)) / 4

Time =~ timediff
Mgnt =~ Management

# correlations
peers ~~ motiv
peers ~~ transfer
motiv ~~ transfer

peers ~~ Time
peers ~~ Mgnt
motiv ~~ Time
motiv ~~ Mgnt
transfer ~~ Time
transfer ~~ Mgnt

'

fit_transfer <- cfa(transfer_corr_model_rel, data = work_data2, estimator = 'MLR', std.lv = TRUE)
summary(fit_transfer, fit.measures = TRUE, standardized = TRUE, rsquare=T)



# Fit indices
round(fitMeasures(fit_transfer)[c("chisq.scaled", "df.scaled", "cfi.scaled", "tli.scaled",
                                  "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr", "aic", "bic")], 3)
round(fitMeasures(fit_transfer)["pvalue.scaled"], 3)



# Creating linear model for investigating potential problems of multicollinearity 
mod1_colltest <- lm(use ~ Cohort + peer + motivation, data = work_data2)

# Investigating multicollinearity
mod1_colltest %>% 
  vif()
# results show no problem of multicollinearity



# 3. Correlation of measured variables ------------------------------------

# 3.1. Preparation for correlation table ----------------------------------
# check variables normality
shapiro.test(work_data2$peer)
shapiro.test(work_data2$motivation)
shapiro.test(work_data2$use)

# significant Shapiro-Wilk normality tests --> Spearman correlations are necessary

corr_input <- c("Management", "timediff",
                "Cohort", "peer", 
                "motivation", "use")

work_data2 <- work_data2 %>% 
  mutate_at(vars(Management), as.numeric)


corr_table <- work_data2 %>% 
  select(., one_of(corr_input))


# 3.2. Function to create Spearman corr table ---------------------------
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

# 3.3. Calculating correlations and creating correlation matrix ---------

correlation_table <- corstars(corr_table, method = "spearman")

# adding column to be able to combine correlation table with descriptive table
correlation_table <- correlation_table %>%
  mutate(transfer = "")

# 3.4. Calculating descriptives for final correlation table -------------

all_descriptives <- as.data.frame(psych::describe(work_data2[,corr_input], skew = TRUE))
descr1 <- all_descriptives[,c("mean", "sd")]
# descr_table1 <- t(descr1)
descr_table1 <- descr1

# source of the following function: https://jeromyanglim.tumblr.com/post/50228877196/round-numbers-in-data-frame-that-contains-non
round_df <- function(x, digits) {
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

# rounding numbers
descr_table1_2 <- round_df(descr_table1, 2)


# 3.5. Combining descriptives and correlation tables --------------------

# combined_tables <- rbind(correlation_table, descr_table1_2)
combined_tables <- cbind(descr_table1_2, correlation_table)

row.names(combined_tables) <- c("1. Manager", 
                                "2. Time Lag",
                                "3. Coworkers' Participation", 
                                "4. Peer Support",
                                "5. Motivation to Transfer", 
                                "6. Perceived Training Transfer"#,
                                # "Mean", "SD"
                                )

# setting row names to first column
combined_tables <- tibble::rownames_to_column(combined_tables, " ")

# removing last (empty) column
combined_tables[,9] <- NULL

combined_tables <- combined_tables %>%  
  rename(
    "M" = mean,
    "SD" = sd,
    "1" = Management,
    "2" = timediff,
    "3" = Cohort,
    "4" = peer,
    "5" = motivation#,
    # "6" = use
    )


# 3.6. (Table) Descr stat and Spearman bivariate corr btw MEASURED vars --------

designed_table <- combined_tables %>% 
  flextable() %>% 
  hline(i = 6, part = "body", border = officer::fp_border()) %>% 
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  align(align = "left", part = "all") %>% 
  width(width = 0.59) %>% 
  #  set_table_properties(width = 1) %>% 
  add_footer_lines(c("Note. N=688, Manager (0 = Non-managers, 1 = Managers); Time Lag: Time Lag Between Training & DV Measure (in days).", 
                     "* p < .05, ** p < .01, *** p < 0.001"))

# saving final table to word file
save_as_docx("Table 2. Descriptive statistics and Spearman bivariate correlations between measured variables" = designed_table, path = "Table 2 - Correlation table - peer support - measured.docx")




# 4. Correlation of latent variables --------------------------------------

# 4.1. Correlation model --------------------------------

# transfer model 1 
transfer_corr_model <- '
# regressions
Peers =~ pee33 + pee35 + pee311
Motiv =~ mot26 + mot28 + mot212
Trans =~ use1 + use3 + use5 + use7

# correlations
Management ~~ timediff
Management ~~ Cohort
Management ~~ Peers
Management ~~ Motiv
Management ~~ Trans
timediff ~~ Cohort
timediff ~~ Peers
timediff ~~ Motiv
timediff ~~ Trans
Cohort ~~ Peers
Cohort ~~ Motiv
Cohort ~~ Trans
Peers ~~ Motiv
Peers ~~ Trans
Motiv ~~ Trans
'

# Heterotrait-monotrait (HTMT) ratio of correlations
htmt(transfer_corr_model, data = work_data2, sample.cov = NULL, missing = "listwise",
     ordered = NULL, absolute = TRUE)



fit_transf_corr <- sem(transfer_corr_model, data = work_data2, estimator = 'MLR')
sem_corr <- summary(fit_transf_corr, fit.measures = TRUE, standardized = TRUE, rsquare=T, ci = TRUE)
fit_corr <- round(fitMeasures(fit_transf_corr)[c("chisq.scaled", "df.scaled", "pvalue.scaled",
                                                 "cfi.scaled", "tli.scaled", "rmsea.scaled", 
                                                 "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", 
                                                 "srmr", "aic", "bic")], 3)

# 4.2. Creating dataframe for Table 1. (corr between latent vars) --------------

corr_estimates <- sem_corr$PE[11:25,c("lhs", "op", "rhs", "pvalue", "ci.lower", "ci.upper", "std.all")]
corr_estimates_nr <- sem_corr$PE[11:25,c("pvalue", "ci.lower", "ci.upper", "std.all")]
med_estimates_txt <- sem_corr$PE[11:25,c("lhs", "op", "rhs")]


# add significance stars from p values
corr_estimates_nr$sign <- stars.pval(corr_estimates_nr$pvalue)
corr_estimates_sign_nr <- corr_estimates_nr[1:15,"sign"]
corr_estimates_nr <- corr_estimates_nr[1:15,1:4]

# change class to numeric
# solution was found here: https://stackoverflow.com/questions/26391921/how-to-convert-entire-dataframe-to-numeric-while-preserving-decimals
corr_estimates_nr[] <- lapply(corr_estimates_nr, function(x) {
  if(is.character(x)) as.numeric(as.character(x)) else x
})
sapply(corr_estimates_nr, class)

# removing leading zeros in numbers
# solution was found here: https://stackoverflow.com/questions/53740145/remove-leading-zeros-in-numbers-within-a-data-frame
corr_estimates_nr2 <- data.frame(lapply(corr_estimates_nr, function(x) gsub("^0\\.", "\\.", gsub("^-0\\.", "-\\.", sprintf("%.3f", x)))), stringsAsFactors = FALSE)


# bind columns that contain text, modified numeric values and significance stars 
corr_estimates2 <- cbind(med_estimates_txt, corr_estimates_nr2, corr_estimates_sign_nr)

corr_estimates2 <- corr_estimates2 %>% 
  unite("95% CI", c(ci.lower, ci.upper), sep = ", ", remove = TRUE) %>% 
  unite("standardized", c(std.all, corr_estimates_sign_nr), sep = "", remove = TRUE)

corr_estimates2$`95% CI` <- corr_estimates2$`95% CI` %>%
  paste("[", ., "]")

corr_estimates3 <- corr_estimates2[1:15,c(1,3,6)]


# library(reshape2) used here for creating matrix from data frame columns
library(reshape2)
corr_estimates3$lhs <- factor(corr_estimates3$lhs, c("Management", "timediff", "Cohort", "Peers", "Motiv", "Trans"))
corr_estimates3$rhs <- factor(corr_estimates3$rhs, c("Management", "timediff", "Cohort", "Peers", "Motiv", "Trans"))
latent_corr <- acast(corr_estimates3, rhs~lhs, value.var="standardized")

latent_corr[4, 1] <- latent_corr[1, 4]
latent_corr[5, 1] <- latent_corr[1, 5]
latent_corr[4, 2] <- latent_corr[2, 4]
latent_corr[5, 2] <- latent_corr[2, 5]


per <- as.matrix(t(latent_corr[,3]))
rownames(per) <- c("Peers")

latent_corr1 <- latent_corr[1:3,]
latent_corr2 <- latent_corr[4:5,]
latent_corr3 <- rbind(latent_corr1, per, latent_corr2)

latent_corr4 <- latent_corr3[,1:2]
latent_corr5 <- latent_corr3[,3:5]
latent_corr6 <- rbind(latent_corr1, per, latent_corr2)


coh <- as.matrix(latent_corr[3,])
colnames(coh) <- c("Cohort")
coh1 <- as.matrix(coh[1:2, 1])
coh2 <- as.matrix(coh[3:5, 1])
coh_row <- as.matrix(NA)
rownames(coh_row) <- c("Cohort")
coh_new <- rbind(coh1, coh_row, coh2)
colnames(coh_new) <- c("Cohort")

latent_corr1 <- latent_corr3[1:6, 1:2]
latent_corr2 <- latent_corr3[1:6, 3:5]


latent_corr_tab <- cbind(latent_corr1, coh_new, latent_corr2)

latent_corr_tab2 <- as.data.frame(latent_corr_tab)
latent_corr_tab2[upper.tri(latent_corr_tab2)] <- NA
latent_corr_tab2[4, 4] <- NA


row.names(latent_corr_tab2) <- c("1. Management", "2. Time Lag", "3. Coworkers' participation", 
                                 "4. Peer Support", "5. Motivation to Transfer",
                                 "6. Perceived Training Transfer")

# setting row names to first column
latent_corr_tab2 <- tibble::rownames_to_column(latent_corr_tab2, " ")

# removing last (empty) column
latent_corr_tab2[,7] <- NULL

# renaming column names
latent_corr_tab2 <- latent_corr_tab2 %>%  
  rename(
    "1" = Management,
    "2" = timediff,
    "3" = Cohort,
    "4" = Peers,
    "5" = Motiv)


# 4.3. Table 1. Bivariate correlations between LATENT variables ----------------

# designing final correlation table
designed_corr_table <- latent_corr_tab2 %>% 
  flextable() %>% 
  # hline(i = 6, part = "body", border = officer::fp_border()) %>% 
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  align(align = "left", part = "all") %>% 
  # width(width = 0.59) %>% 
  autofit() %>% 
  #  set_table_properties(width = 1) %>% 
  add_footer_lines(c("Note. N = 688.", 
                     "* p < .05, ** p < .01, *** p < .001"))

# saving final table to word file
save_as_docx("Table 2. Bivariate correlations between latent variables" = designed_corr_table, 
             path = "Table 2 - Correlation table of latent variables.docx")



# III. MAIN ANALYSIS ------------------------------------------------------

# 1. Preparation for main analysis -------------------------------------------

# standardization of continuous and ordinal variables
work_data3 <- work_data2 %>% 
  mutate_at(list(std = ~scale(., center = TRUE, scale = TRUE)[, 1]), 
            .vars=c("Cohort", "peer", "motivation", "use"))

# selecting variables  
vars <- c("Cohort_std", "peer_std", "motivation_std", "use_std")

work_data4 <- work_data3 %>%
  select(c(one_of(vars)))


# 2. Moderated mediation analysis using manifest variables ----------------
# following the example from here: https://rpubs.com/cardiomoon/468602

labels=list(X="peer_std",M="motivation_std",Y="use_std",W="Cohort_std")
pmacroModel(8,labels=labels)
statisticalDiagram(8,labels=labels)

# Make Regression Equation
moderator=list(name="Cohort_std",site=list(c("a","c")))
equations=tripleEquation(labels=labels,moderator=moderator,mode=1)
cat(equations)

# Summary of Regression Models
fit=eq2fit(equations,data=work_data4)
modelsSummaryTable(fit,labels=labels)


# SEM Approach
# Make SEM equation
model=tripleEquation(labels=labels,moderator=moderator)
# cat(model)


# 2.1. Analysis -----------------------------------------------------------
semfit=sem(model=model,data=work_data4,se="boot",bootstrap=1000)

summary(semfit,standardized = TRUE, fit.measures = TRUE, ci = TRUE)

# Fit indices
round(fitMeasures(semfit)[c("chisq", "df", "cfi", "tli", "rmsea", "srmr", "aic", "bic")], 3)
round(fitMeasures(semfit)["pvalue"], 3)


# Estimates Table
estimatesTable(semfit, digits = 3)

# Correlation
corTable2(semfit)

# Mediation Summary
medSummaryTable(semfit)

# Moderated Mediation Summary
modmedSummaryTable(semfit,mod="Cohort_std", showP = TRUE, boot.ci.type = "bca.simple")
# The table summarizes the direct and indirect effect 
# when the moderator is mean, mean + sd and mean - sd.


# 2.2. Moderated mediation figure of manifest variables -------------------
# Statistical Model
statisticalDiagram(8,labels=labels,fit=semfit,whatLabel = "std", digits = 3)

dev.copy(png,'manifest_modmed_statisticalDiagram.png')
dev.off()


# Conditional Effect Plot
conditionalEffectPlot(semfit,data=work_data4,mod="Cohort_std")




# 3. Latent moderated mediation analysis ----------------------------------
# Peer Support -> MTT -> Transfer, all Moderated by Cohort


# 3.1. Create product indicators ------------------------------------------
# Double-mean centering / Product indicators of Cohort*Peer items (cp)
work_data4 <- indProd(work_data3, var1 = "Cohort", 
                      var2 = c("pee33", "pee35", "pee311"), 
                      match = FALSE, meanC = TRUE, residualC = TRUE, doubleMC = TRUE,
               namesProd = c("cp1", "cp2", "cp3"))


# 3.2. Define latent moderated mediation model ----------------------------

mod_1cont <- "
Peers =~ pee33 + pee35 + pee311
Motiv =~ mot26 + mot28 + mot212
Trans =~ use1 + use3 + use5 + use7
Time =~ timediff
Mgnt =~ Management

CP =~ cp1 + cp2 + cp3     #latent interaction of Cohort and Peer Support 

 # a path
 Motiv ~ a*Peers + z*Cohort_std + aMod*CP + Time + Mgnt

 # b path
 Trans ~ b*Motiv + Time + Mgnt

 # cp prime path
 Trans ~ cp*Peers + cpMod*CP

ab := a*b                                 # indirect effect of path a and path b (joint-significance)
c := cp + (a*b)                           # total effect without moderator's effects 
cMod := cpMod + aMod                      # moderator's effects on path a and path c
aModb := aMod*b                           # indirect effect of path b and moderated path a (joint-significance)
indirect := (a*b) + aMod                  # indirect effects with moderator's effects
total := c + cMod + aModb                 # total effect with moderator's effects
"


# 3.3. Fit latent model ---------------------------------------------------
fit_1cont <- sem(model = mod_1cont, data = work_data4, std.lv = TRUE, 
                 se = "bootstrap", bootstrap = 1000)

# 3.4. Results of latent model --------------------------------------------
# Summary
summary(fit_1cont, standardized = TRUE, fit.measures = TRUE, ci = TRUE)

# Fit indices
round(fitMeasures(fit_1cont)[c("chisq", "df", "cfi", "tli", "rmsea", "srmr", "aic", "bic")], 3)
round(fitMeasures(fit_1cont)["pvalue"], 3)


# For 95% bias-corrected bootstrapped confidence intervals (CIs)
med_estimates <- parameterestimates(fit_1cont, boot.ci.type = "bca.simple", standardized = TRUE)

med_estimatesb <- med_estimates[c(16:25, 54:59),c("lhs", "op", "rhs", "label", "est", "se", "pvalue", "ci.lower", "ci.upper", "std.all")]
med_estimates_nr2 <- med_estimates[c(16:25, 54:59),c("est", "se", "pvalue", "ci.lower", "ci.upper", "std.all")]
med_estimates_txt2 <- med_estimates[c(16:25, 54:59),c("lhs", "op", "rhs", "label")]

# add significance stars from p values
med_estimates_nr2$sign <- stars.pval(med_estimates_nr2$pvalue)
med_estimates_signb <- med_estimates_nr2[1:16,"sign"]
med_estimates_nr2 <- med_estimates_nr2[1:16,1:6]

# change class to numeric
# solution was found here: https://stackoverflow.com/questions/26391921/how-to-convert-entire-dataframe-to-numeric-while-preserving-decimals
med_estimates_nr2[] <- lapply(med_estimates_nr2, function(x) {
  if(is.character(x)) as.numeric(as.character(x)) else x
})
sapply(med_estimates_nr2, class)

# removing leading zeros in numbers
# solution was found here: https://stackoverflow.com/questions/53740145/remove-leading-zeros-in-numbers-within-a-data-frame
med_estimates_nr2b <- data.frame(lapply(med_estimates_nr2, function(x) gsub("^0\\.", "\\.", gsub("^-0\\.", "-\\.", sprintf("%.3f", x)))), stringsAsFactors = FALSE)


# bind columns that contain text, modified numeric values and significance stars 
med_estimates_2bx_cont <- cbind(med_estimates_txt2, med_estimates_nr2b, med_estimates_signb)

med_estimates_2bx_cont <- med_estimates_2bx_cont %>%
  unite("95% CI", c(ci.lower, ci.upper), sep = ", ", remove = TRUE) %>%
  unite("standardized", c(std.all, med_estimates_signb), sep = "", remove = TRUE)

med_estimates_2bx_cont$`95% CI` <- med_estimates_2bx_cont$`95% CI` %>% 
  paste("[", ., "]")



# 4. Figures --------------------------------------------------------------


# 4.1. Preparation for creating figures -----------------------------------

# install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
# fonts()

# Creating theme for figures
theme_transfer <- theme_classic() +
  theme(
    plot.title = element_text(size = 16,
                              face = "bold",
                              margin = margin(b = 35)),
    legend.title = element_text(size = 16, color = "#22292F", face = "bold"),
    legend.text = element_text(size = 14),
    plot.margin = unit(rep(1, 4), "cm"),
    axis.text = element_text(size = 14, color = "#22292F"),
    axis.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text.y = element_text(margin = margin(r = 5)),
    axis.text.x = element_text(margin = margin(t = 5)),
    plot.caption = element_text(size = 10, 
                                face = "italic",
                                color = "#606F7B",
                                margin = margin(t = 15)),
    axis.line = element_line(color = "#3D4852"),
    axis.ticks = element_line(color = "#3D4852"),
    panel.grid.major.y = element_line(color = "#DAE1E7"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.text.x = element_text(size = 12),
    text = element_text(family="Times New Roman")
  )


# Changing moderator's class for using it in the figure
work_data3 <- work_data3 %>% 
  mutate(Cohort2 = as.character(Cohort))



# 4.2. Creating figures ---------------------------------------------------
# Figure 1 - PeerSup on MTT, moderated by Colleagues' participation
work_data3 %>% 
  ggplot() +
  aes(x = peer, y = motivation, color = Cohort2, linetype = Cohort2) +
  stat_smooth(method = "lm", se = TRUE, size = 1.1, alpha = 0.15) +
  labs(x = "Peer support", y = "Motivation to Transfer") +
  scale_y_continuous(limits = c(1, 7), breaks = seq(1, 7, by = 1)) +
  scale_x_continuous(limits = c(1, 7), breaks = seq(1, 7, by = 1)) +
  theme_transfer +
  scale_colour_manual(values=c("#987654", "#000000", "#900009"),
                      name="Coworkers' Participation", 
                      breaks=c("0", "1", "2"), 
                      labels = c("None", "Some", "Nearly All")) +
  scale_linetype_manual(name="Coworkers' Participation", 
                        breaks=c("0", "1", "2"), 
                        labels = c("None", "Some", "Nearly All"),
                        values=c("solid", "dotted", "dashed", "longdash")) +
  theme(plot.title = element_text(margin = margin(b = 1)),
        legend.position = c(0.8, 0.25),
        legend.title = element_text(family="Times New Roman", size = 16))
        

ggsave(path = "plots", filename = "Figure_1 - PeerSup-Participation-Motivation.png", width = 9, height = 8)


