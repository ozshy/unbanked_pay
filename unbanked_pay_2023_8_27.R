### Unbanked_pay paper
# The code is partly based on the code written for an unpublished working paper: Atlanta Fed Policy Hub No. 2023_1. This code has been modified for a journal revision using the 2022 diary data (instead of the 2021 diary data)
#
### Instructions
# Parts of this code generates tables that are not used in the paper. However, you may need to run the entire code just to make sure that all variables are properly defined. 
#
### Contents of this code: Use bookmarks in RStudio. 

#
# The following packages are used:
#library(formattable)# has percent function
#library(plotrix)# weighted histograms
library(dplyr)
library(xtable)# for LaTeX tables
library(writexl)# export to Excel 
#library(ggplot2)
library(spatstat) # for weighted.median
library(sampleSelection)# Heckman 2-stage
library(mfx)
library(stargazer)# displays Heckman's 2-stage, does not work with mfx
library(texreg)# exports regression result to LaTeX (just like stargazer) or HTML (can be read by Excel or Word), works with mfx and Heckman (displays also the selection coefficients)
#library(regclass) # for confusion_matrix
#library(nnet) # for multinomial logit
#library(AER) # for p-values of nnet multinom logit coeftest(regression)
#library(rpart)# Decision trees
#library(rpart.plot)# Decision trees
#library(partykit)# modifies rpart tree plot # Decision trees
#library(performanceEstimation)# for SMOTE (balancing data by generating synthetic data classification tree) => hard to balance. Not used, bad results even when balanced.
#library(ROSE)# balancing data w.r.t. minority class (similar to SMOTE) => generates negative income. Do not use!
#library(smotefamily)# Also SMOTE, easier to use than the above package because it uses only 1 parameter to increase the minority class => Does NOT work with factors (only numeric)
#library(caret)
#library(e1071)# for audit function (tuning rpart, not used)
#library("randomForest")

setwd("~/Papers/unbanked_pay/unbanked_pay_coding")
dir()

### Reading RDS datasets
i1.df = readRDS("dcpc_2022_indlevel_public.rds")# indiv data
t1.df = readRDS("dcpc_2022_tranlevel_public.rds")# trans data
#d1.df = readRDS("dcpc_2021_daylevel_public.rds" )# day-level data Not used 

### Removing NAs of main variables
dim(i1.df)# num of respondents X num variables
i2.df = i1.df
#names(dplyr::select(i2.df, contains("id")))
#i2.df$id = i2.df$uasid# creating ID to be removed when using the public version of the data
length(unique(i2.df$id))# num resp

# Now using the full sample with all weights (instead of ind_weight)
names(dplyr::select(i2.df, contains("weight")))
nrow(subset(i2.df, is.na(ind_weight)))# num lost resp in small sample
nrow(subset(i2.df, is.na(ind_weight_all)))# full sample

# #remove NA without weights to obtain small sample (cancelled, now use the full sample)
# nrow(i2.df)
# str(i2.df$ind_weight)
# # removing missing weights (because this sample does not include the additional out of sample resp)
# nrow(subset(i2.df, is.na(ind_weight)))
# i3.df = subset(i2.df, !is.na(ind_weight))
# nrow(i3.df) # num resp
# sum(i3.df$ind_weight)# to be adjusted later, when needed. 

# Use full sample using ind_weight_all
i3.df = i2.df
# Remove NA: consumers with checking account
table(i3.df$chk_acnt_adopt, exclude = F)
# remove resp with NAs
i4.df = subset(i3.df, !is.na(chk_acnt_adopt))
length(unique(i4.df$id))# num resp
# Remove NA: consumers with savings account
table(i4.df$sav_acnt_adopt, exclude = F)
# remove resp with NAs
i5.df = subset(i4.df, !is.na(sav_acnt_adopt))
length(unique(i5.df$id))# num resp

# => i5.df is the base dataset with weights w5. I will have to further remove respondents for different tables depending on the NAs of the relevant variables. 
# rescale the weights to fit i5.df
nrow(i5.df)
sum(i5.df$ind_weight_all)
i5.df$w5 = nrow(i5.df)*i5.df$ind_weight_all/sum(i5.df$ind_weight_all)
sum(i5.df$w5)
head(i5.df$ind_weight_all)
head(i5.df$w5)# rescaled weights

# Start Table bank adoption ####
# start a table: banked/unbanked adoption of checking and savings accounts 
# This table is described in section 2 and is not displayed as a table. 

str(i5.df$chk_acnt_adopt)
str(i5.df$sav_acnt_adopt)

# num and frac resp with a checking account
(have_chk_num = sum(i5.df$chk_acnt_adopt))# unweighted
(have_chk_num_w = sum(subset(i5.df, chk_acnt_adopt==1)$w5))# weighted
(have_chk_frac = have_chk_num/nrow(i5.df))# unweighted
(have_chk_frac_w = have_chk_num_w/nrow(i5.df))# weighted

# num and frac resp with a saving account
(have_sav_num = sum(i5.df$sav_acnt_adopt))# unweighted
(have_sav_num_w = sum(subset(i5.df, sav_acnt_adopt==1)$w5))# weighted
(have_sav_frac = have_sav_num/nrow(i5.df))# unweighted
(have_sav_frac_w = have_sav_num_w/nrow(i5.df))# weighted

# num resp with both checking and saving account
(have_both_num = nrow(subset(i5.df, chk_acnt_adopt==1 & sav_acnt_adopt==1)))# unweighted
(have_both_num_w = sum(subset(i5.df, chk_acnt_adopt==1 & sav_acnt_adopt==1)$w5))# weighted
(have_both_frac = have_both_num/nrow(i5.df))# unweighted
(have_both_frac_w = have_both_num_w/nrow(i5.df))# weighted

# num banked resp
i5.df$banked = ifelse(i5.df$chk_acnt_adopt==1 | i5.df$sav_acnt_adopt==1, 1, 0)
(banked_num = sum(i5.df$banked))
(banked_num_w = sum(subset(i5.df, banked==1)$w5))# weighted
(banked_frac = banked_num/nrow(i5.df))
(banked_frac_w = banked_num_w/nrow(i5.df))# weighted

# num unbanked resp
i5.df$unbanked = ifelse(i5.df$chk_acnt_adopt==0 & i5.df$sav_acnt_adopt==0, 1, 0)
(unbanked_num = sum(i5.df$unbanked))
(unbanked_num_w = sum(subset(i5.df, unbanked==1)$w5))# weighted
(unbanked_frac = unbanked_num/nrow(i5.df))
(unbanked_frac_w = unbanked_num_w/nrow(i5.df))# weighted

# verify that num banked + num unbanked = num resp
nrow(i5.df)
banked_num+unbanked_num
banked_num_w+unbanked_num_w


# building a data frame (all weighted), 
# described at the beginning of Section 2 [not a formal table]
(own_var.vec = c("Banked", "Unbanked", "Have checking account", "Have savings account", "Have both checking and savings", "Number of respondents"))
#
(own_num_w.vec = c( banked_num_w, unbanked_num_w, have_chk_num_w, have_sav_num_w, have_both_num_w, nrow(i5.df)))
#
(own_frac_w.vec = 100*c(banked_frac_w, unbanked_frac_w, have_chk_frac_w, have_sav_frac_w, have_both_frac_w, 1))
#
(own.df = data.frame("Variable" = own_var.vec, Number = own_num_w.vec, Percentage = own_frac_w.vec))
#
#write.csv(own.df, "table2.csv", row.names = F) [Not used in the paper]

# This table (fraction of unbanked) is described in section 2 but not displayed as a table. Banked and unbanked are also displayed in Table 1 (summary stats)
# End Table bank adoption ####

# Start Table Demographics #### 
#[Not used in the paper]
names(dplyr::select(i5.df, contains("marital")))
table(i5.df$marital_status)
#
(banked_married_num = nrow(subset(i5.df, banked == 1 & marital_status %in% c(1,2))))
(banked_married_num_w = sum(subset(i5.df, banked == 1 & marital_status %in% c(1,2))$w5))
(banked_married_frac = banked_married_num/banked_num)
(banked_married_frac_w = banked_married_num_w/banked_num_w)
#
(unbanked_married_num = nrow(subset(i5.df, unbanked == 1 & marital_status %in% c(1,2))))
(unbanked_married_num_w = sum(subset(i5.df, unbanked == 1 & marital_status %in% c(1,2))$w5))
(unbanked_married_frac = unbanked_married_num/unbanked_num)
(unbanked_married_frac_w = unbanked_married_num_w/unbanked_num_w)
#

table(i5.df$race_white)
#
(banked_race_white_num = nrow(subset(i5.df, banked == 1 & race_white ==1)))
(banked_race_white_num_w = sum(subset(i5.df, banked == 1 & race_white ==1)$w5))
(banked_race_white_frac = banked_race_white_num/banked_num)
(banked_race_white_frac_w = banked_race_white_num_w/banked_num_w)
#
(unbanked_race_white_num = nrow(subset(i5.df, unbanked == 1 & race_white ==1)))
(unbanked_race_white_num_w = sum(subset(i5.df, unbanked == 1 & race_white ==1)$w5))
(unbanked_race_white_frac = unbanked_race_white_num/unbanked_num)
(unbanked_race_white_frac_w = unbanked_race_white_num_w/unbanked_num_w)
#
names(dplyr::select(i5.df, contains("income")))
head(i5.df$income_hh)
#
(banked_income_g25_num = nrow(subset(i5.df, banked == 1 & income_hh > 25000)))
(banked_income_g25_num_w = sum(subset(i5.df, banked == 1 & income_hh > 25000)$w5))
(banked_income_g25_frac = banked_income_g25_num/banked_num)
(banked_income_g25_frac_w = banked_income_g25_num_w/banked_num_w)
#
(unbanked_income_g25_num = nrow(subset(i5.df, unbanked == 1 & income_hh > 25000)))
(unbanked_income_g25_num_w = sum(subset(i5.df, unbanked == 1 & income_hh > 25000)$w5))
(unbanked_income_g25_frac = unbanked_income_g25_num/unbanked_num)
(unbanked_income_g25_frac_w = unbanked_income_g25_num_w/unbanked_num_w)
#
names(dplyr::select(i5.df, contains("employ")))
table(i5.df$work_employed)
#
(banked_employed_num = nrow(subset(i5.df, banked == 1 & work_employed==1)))
(banked_employed_num_w = sum(subset(i5.df, banked == 1 & work_employed==1)$w5))
(banked_employed_frac = banked_employed_num/banked_num)
(banked_employed_frac_w = banked_employed_num_w/banked_num_w)
#
(unbanked_employed_num = nrow(subset(i5.df, unbanked == 1 & work_employed==1)))
(unbanked_employed_num_w = sum(subset(i5.df, unbanked == 1 & work_employed==1)$w5))
(unbanked_employed_frac = unbanked_employed_num/unbanked_num)
(unbanked_employed_frac_w = unbanked_employed_num_w/unbanked_num_w)
#

names(dplyr::select(i5.df, contains("education")))
table(i5.df$highest_education)
#
(banked_hs_plus_num = nrow(subset(i5.df, banked == 1 & highest_education >= 9)))
(banked_hs_plus_num_w = sum(subset(i5.df, banked == 1 & highest_education >= 9)$w5))
(banked_hs_plus_frac = banked_hs_plus_num/banked_num)
(banked_hs_plus_frac_w = banked_hs_plus_num_w/banked_num_w)
#
(unbanked_hs_plus_num = nrow(subset(i5.df, unbanked == 1 & highest_education >= 9)))
(unbanked_hs_plus_num_w = sum(subset(i5.df, unbanked == 1 & highest_education >= 9)$w5))
(unbanked_hs_plus_frac = unbanked_hs_plus_num/unbanked_num)
(unbanked_hs_plus_frac_w = unbanked_hs_plus_num_w/unbanked_num_w)
#

(banked.vec = c(banked_married_frac_w, banked_race_white_frac_w, banked_income_g25_frac_w, banked_employed_frac_w, banked_hs_plus_frac_w))
#
(unbanked.vec = c(unbanked_married_frac_w, unbanked_race_white_frac_w, unbanked_income_g25_frac_w, unbanked_employed_frac_w, unbanked_hs_plus_frac_w))
#
char_var.vec = c("Married", "White", "HH_income>$25k", "Employed",  "High_school_and_higher")

# making the char (Figure 1 in 2016 paper) a data frame
(char.df = data.frame(Variable = char_var.vec, Banked = 100*banked.vec, Unbanked = 100*unbanked.vec))
#
# write.csv(char.df, "figure1.csv", row.names = F) #[NOT USED IN THE PAPER]

### Start table (some demographics) #[Not in paper]
nrow(i5.df)
sum(i5.df$w5)
#
banked_num
banked_num_w
sum(subset(i5.df, banked==1)$w5)
unbanked_num
unbanked_num_w
sum(subset(i5.df, banked==0)$w5)

# missing income values (num and weighgted)
(banked_miss_income_num = nrow(subset(i5.df, banked==1 & is.na(i5.df$income_hh))))
#
(banked_miss_income_num_w = sum(subset(i5.df, banked==1 & is.na(i5.df$income_hh))$w5))
#
(unbanked_miss_income_num = nrow(subset(i5.df, banked==0 & is.na(i5.df$income_hh))))
#
(unbanked_miss_income_num_w = sum(subset(i5.df, banked==0 & is.na(i5.df$income_hh))$w5))

(banked_income_0_25_num = nrow(subset(i5.df, banked == 1 & income_hh < 25000)))
(banked_income_0_25_num_w = sum(subset(i5.df, banked == 1 & income_hh < 25000)$w5))
(banked_income_0_25_frac = banked_income_0_25_num/(banked_num - banked_miss_income_num))
(banked_income_0_25_frac_w = banked_income_0_25_num_w/(banked_num_w - banked_miss_income_num_w))
#
(unbanked_income_0_25_num = nrow(subset(i5.df, unbanked == 1 & income_hh < 25000)))
(unbanked_income_0_25_num_w = sum(subset(i5.df, unbanked == 1 & income_hh < 25000)$w5))
(unbanked_income_0_25_frac = unbanked_income_0_25_num/(unbanked_num -unbanked_miss_income_num))
(unbanked_income_0_25_frac_w = unbanked_income_0_25_num_w/(unbanked_num_w -unbanked_miss_income_num_w))
#
(banked_income_25_50_num = nrow(subset(i5.df, banked == 1 & income_hh >= 25000 & income_hh < 50000)))
(banked_income_25_50_num_w = sum(subset(i5.df, banked == 1 & income_hh >= 25000 & income_hh < 50000)$w5))
(banked_income_25_50_frac = banked_income_25_50_num/(banked_num - banked_miss_income_num))
(banked_income_25_50_frac_w = banked_income_25_50_num_w/(banked_num_w -banked_miss_income_num_w))
#
(unbanked_income_25_50_num = nrow(subset(i5.df, unbanked == 1 & income_hh >= 25000 & income_hh < 50000)))
(unbanked_income_25_50_num_w = sum(subset(i5.df, unbanked == 1 & income_hh >= 25000 & income_hh < 50000)$w5))
(unbanked_income_25_50_frac = unbanked_income_25_50_num/(unbanked_num -unbanked_miss_income_num))
(unbanked_income_25_50_frac_w = unbanked_income_25_50_num_w/(unbanked_num_w -unbanked_miss_income_num_w))
#
(banked_income_50_75_num = nrow(subset(i5.df, banked == 1 & income_hh >= 50000 & income_hh < 75000)))
(banked_income_50_75_num_w = sum(subset(i5.df, banked == 1 & income_hh >= 50000 & income_hh < 75000)$w5))
(banked_income_50_75_frac = banked_income_50_75_num/(banked_num -banked_miss_income_num))
(banked_income_50_75_frac_w = banked_income_50_75_num_w/(banked_num_w -banked_miss_income_num_w))
#
(unbanked_income_50_75_num = nrow(subset(i5.df, unbanked == 1 & income_hh >= 50000 & income_hh < 75000)))
(unbanked_income_50_75_num_w = sum(subset(i5.df, unbanked == 1 & income_hh >= 50000 & income_hh < 75000)$w5))
(unbanked_income_50_75_frac = unbanked_income_50_75_num/(unbanked_num -unbanked_miss_income_num))
(unbanked_income_50_75_frac_w = unbanked_income_50_75_num_w/(unbanked_num_w -unbanked_miss_income_num_w))
#
(banked_income_75_100_num = nrow(subset(i5.df, banked == 1 & income_hh >= 75000 & income_hh < 100000)))
(banked_income_75_100_num_w = sum(subset(i5.df, banked == 1 & income_hh >= 75000 & income_hh < 100000)$w5))
(banked_income_75_100_frac = banked_income_75_100_num/(banked_num -banked_miss_income_num))
(banked_income_75_100_frac_w = banked_income_75_100_num_w/(banked_num_w -banked_miss_income_num_w))
#
(unbanked_income_75_100_num = nrow(subset(i5.df, unbanked == 1 & income_hh >= 75000 & income_hh < 100000)))
(unbanked_income_75_100_num_w = sum(subset(i5.df, unbanked == 1 & income_hh >= 75000 & income_hh < 100000)$w5))
(unbanked_income_75_100_frac = unbanked_income_75_100_num/(unbanked_num -unbanked_miss_income_num))
(unbanked_income_75_100_frac_w = unbanked_income_75_100_num_w/(unbanked_num_w -unbanked_miss_income_num_w))
#
(banked_income_100_inf_num = nrow(subset(i5.df, banked == 1 & income_hh >= 100000)))
(banked_income_100_inf_num_w = sum(subset(i5.df, banked == 1 & income_hh >= 100000)$w5))
(banked_income_100_inf_frac = banked_income_100_inf_num/(banked_num - banked_miss_income_num))
(banked_income_100_inf_frac_w = banked_income_100_inf_num_w/(banked_num_w -banked_miss_income_num_w))
#
(unbanked_income_100_inf_num = nrow(subset(i5.df, unbanked == 1 & income_hh >= 100000)))
(unbanked_income_100_inf_num_w = sum(subset(i5.df, unbanked == 1 & income_hh >= 100000)$w5))
(unbanked_income_100_inf_frac = unbanked_income_100_inf_num/(unbanked_num -unbanked_miss_income_num))
(unbanked_income_100_inf_frac_w = unbanked_income_100_inf_num_w/(unbanked_num_w -unbanked_miss_income_num_w))
#

(banked_income.vec = c(banked_income_0_25_frac_w, banked_income_25_50_frac_w, banked_income_50_75_frac_w, banked_income_75_100_frac_w, banked_income_100_inf_frac_w))
#
(unbanked_income.vec = c(unbanked_income_0_25_frac_w, unbanked_income_25_50_frac_w, unbanked_income_50_75_frac_w, unbanked_income_75_100_frac_w, unbanked_income_100_inf_frac_w))
#
(var_income.vec = c("<25k", ">=$25k-<$50k", ">=$50k-<$75k", ">=$75k-<$100k", ">=$100k"))

# finalizing table (not in paper)
(income.df = data.frame(Variable = var_income.vec, Banked = 100*banked_income.vec, Unbanked = 100*unbanked_income.vec))
# verify columns sum up to 100%
sum(income.df$Banked)
sum(income.df$Unbanked)
#
#write.csv(income.df, "figure2.csv", row.names = F) # [Not in the paper]

# Start Table 1 summary stat ####
# Start Table 1 in the paper: Summary stats (banking by demographics)
summary(i5.df$age)
quantile(i5.df$age, seq(0.05, 1, 0.05), na.rm = T)
range(i5.df$age, na.rm = T)
#
# Divide i5.df into 2 data frames and rescale the weights
i5_banked.df = subset(i5.df, banked==1)
i5_unbanked.df = subset(i5.df, unbanked==1)
# verify sum up
nrow(i5.df)
nrow(i5_banked.df)+nrow(i5_unbanked.df)
#
# Rescale weights for i5_banked.df and i5_unbanked.df
i5_banked.df$w5_banked = nrow(i5_banked.df)*i5_banked.df$w5/sum(i5_banked.df$w5)
nrow(i5_banked.df)
sum(i5_banked.df$w5_banked)
#
i5_unbanked.df$w5_unbanked = nrow(i5_unbanked.df)*i5_unbanked.df$w5/sum(i5_unbanked.df$w5)
nrow(i5_unbanked.df)
sum(i5_unbanked.df$w5_unbanked)

# demog age for the banked
nrow(subset(i5_banked.df, is.na(age)))# ignore
nrow(subset(i5_unbanked.df, is.na(age)))# ignore
#
(demog_banked_age_18_24_num = nrow(subset(i5_banked.df, age < 25)))
#
(demog_banked_age_18_24_frac = demog_banked_age_18_24_num/nrow(i5_banked.df))
#
(demog_banked_age_18_24_num_w = sum(subset(i5_banked.df, age < 25)$w5_banked))
#
(demog_banked_age_18_24_frac_w = demog_banked_age_18_24_num_w/nrow(i5_banked.df))
#
(demog_banked_age_25_34_num = nrow(subset(i5_banked.df, age >= 25 & age <= 34)))
#
(demog_banked_age_25_34_frac = demog_banked_age_25_34_num/nrow(i5_banked.df))
#
(demog_banked_age_25_34_num_w = sum(subset(i5_banked.df, age >= 25 & age <= 34)$w5_banked))
#
(demog_banked_age_25_34_frac_w = demog_banked_age_25_34_num_w/nrow(i5_banked.df))
#
(demog_banked_age_35_44_num = nrow(subset(i5_banked.df, age >= 35 & age <= 44)))
#
(demog_banked_age_35_44_frac = demog_banked_age_35_44_num/nrow(i5_banked.df))
#
(demog_banked_age_35_44_num_w = sum(subset(i5_banked.df, age >= 35 & age <= 44)$w5_banked))
#
(demog_banked_age_35_44_frac_w = demog_banked_age_35_44_num_w/nrow(i5_banked.df))
#
(demog_banked_age_45_54_num = nrow(subset(i5_banked.df, age >= 45 & age <= 54)))
#
(demog_banked_age_45_54_frac = demog_banked_age_45_54_num/nrow(i5_banked.df))
#
(demog_banked_age_45_54_num_w = sum(subset(i5_banked.df, age >= 45 & age <= 54)$w5_banked))
#
(demog_banked_age_45_54_frac_w = demog_banked_age_45_54_num_w/nrow(i5_banked.df))
#
(demog_banked_age_55_64_num = nrow(subset(i5_banked.df, age >= 55 & age <= 64)))
#
(demog_banked_age_55_64_frac = demog_banked_age_55_64_num/nrow(i5_banked.df))
#
(demog_banked_age_55_64_num_w = sum(subset(i5_banked.df, age >= 55 & age <= 64)$w5_banked))
#
(demog_banked_age_55_64_frac_w = demog_banked_age_55_64_num_w/nrow(i5_banked.df))
#
(demog_banked_age_65_and_older_num = nrow(subset(i5_banked.df, age >= 65)))
#
(demog_banked_age_65_and_older_frac = demog_banked_age_65_and_older_num/nrow(i5_banked.df))
#
(demog_banked_age_65_and_older_num_w = sum(subset(i5_banked.df, age >= 65)$w5_banked))
#
(demog_banked_age_65_and_older_frac_w = demog_banked_age_65_and_older_num_w/nrow(i5_banked.df))
#
# verify fractions sum up to 1
demog_banked_age_18_24_frac + demog_banked_age_25_34_frac + 
  demog_banked_age_35_44_frac + 
  demog_banked_age_45_54_frac + 
  demog_banked_age_55_64_frac +
  demog_banked_age_65_and_older_frac 
#
demog_banked_age_18_24_frac_w + demog_banked_age_25_34_frac_w + 
  demog_banked_age_35_44_frac_w + 
  demog_banked_age_45_54_frac_w + 
  demog_banked_age_55_64_frac_w +
  demog_banked_age_65_and_older_frac_w 

# demog age for the UNbanked
(demog_unbanked_age_18_24_num = nrow(subset(i5_unbanked.df, age < 25)))
#
(demog_unbanked_age_18_24_frac = demog_unbanked_age_18_24_num/nrow(i5_unbanked.df))
#
(demog_unbanked_age_18_24_num_w = sum(subset(i5_unbanked.df, age < 25)$w5_unbanked))
#
(demog_unbanked_age_18_24_frac_w = demog_unbanked_age_18_24_num_w/nrow(i5_unbanked.df))
#
(demog_unbanked_age_25_34_num = nrow(subset(i5_unbanked.df, age >= 25 & age <= 34)))
#
(demog_unbanked_age_25_34_frac = demog_unbanked_age_25_34_num/nrow(i5_unbanked.df))
#
(demog_unbanked_age_25_34_num_w = sum(subset(i5_unbanked.df, age >= 25 & age <= 34)$w5_unbanked))
#
(demog_unbanked_age_25_34_frac_w = demog_unbanked_age_25_34_num_w/nrow(i5_unbanked.df))
#
(demog_unbanked_age_35_44_num = nrow(subset(i5_unbanked.df, age >= 35 & age <= 44)))
#
(demog_unbanked_age_35_44_frac = demog_unbanked_age_35_44_num/nrow(i5_unbanked.df))
#
(demog_unbanked_age_35_44_num_w = sum(subset(i5_unbanked.df, age >= 35 & age <= 44)$w5_unbanked))
#
(demog_unbanked_age_35_44_frac_w = demog_unbanked_age_35_44_num_w/nrow(i5_unbanked.df))
#
(demog_unbanked_age_45_54_num = nrow(subset(i5_unbanked.df, age >= 45 & age <= 54)))
#
(demog_unbanked_age_45_54_frac = demog_unbanked_age_45_54_num/nrow(i5_unbanked.df))
#
(demog_unbanked_age_45_54_num_w = sum(subset(i5_unbanked.df, age >= 45 & age <= 54)$w5_unbanked))
#
(demog_unbanked_age_45_54_frac_w = demog_unbanked_age_45_54_num_w/nrow(i5_unbanked.df))
#
(demog_unbanked_age_55_64_num = nrow(subset(i5_unbanked.df, age >= 55 & age <= 64)))
#
(demog_unbanked_age_55_64_frac = demog_unbanked_age_55_64_num/nrow(i5_unbanked.df))
#
(demog_unbanked_age_55_64_num_w = sum(subset(i5_unbanked.df, age >= 55 & age <= 64)$w5_unbanked))
#
(demog_unbanked_age_55_64_frac_w = demog_unbanked_age_55_64_num_w/nrow(i5_unbanked.df))
#
(demog_unbanked_age_65_and_older_num = nrow(subset(i5_unbanked.df, age >= 65)))
#
(demog_unbanked_age_65_and_older_frac = demog_unbanked_age_65_and_older_num/nrow(i5_unbanked.df))
#
(demog_unbanked_age_65_and_older_num_w = sum(subset(i5_unbanked.df, age >= 65)$w5_unbanked))
#
(demog_unbanked_age_65_and_older_frac_w = demog_unbanked_age_65_and_older_num_w/nrow(i5_unbanked.df))
#
# verify fractions sum up to 1
demog_unbanked_age_18_24_frac + demog_unbanked_age_25_34_frac + 
  demog_unbanked_age_35_44_frac + 
  demog_unbanked_age_45_54_frac + 
  demog_unbanked_age_55_64_frac +
  demog_unbanked_age_65_and_older_frac 
#
demog_unbanked_age_18_24_frac_w + demog_unbanked_age_25_34_frac_w + 
  demog_unbanked_age_35_44_frac_w + 
  demog_unbanked_age_45_54_frac_w + 
  demog_unbanked_age_55_64_frac_w +
  demog_unbanked_age_65_and_older_frac_w 

# construct demog_age data frame [then row-bind to the grant demog data frame]
# construct variable names for first line and age range
(demog_age_var.vec = c("Number of respondents", "Age: 18-24", "Age: 25-34", "Age: 35-44", "Age: 45-54", "Age: 55-64", "Age: 65 and older"))
#
(demog_age.df = data.frame(Variable = demog_age_var.vec, Banked_weighted = c(banked_num_w, 100*demog_banked_age_18_24_frac_w, 100*demog_banked_age_25_34_frac_w, 100*demog_banked_age_35_44_frac_w, 100*demog_banked_age_45_54_frac_w, 100*demog_banked_age_55_64_frac_w, 100*demog_banked_age_65_and_older_frac_w), Unbanked_weighted = c(unbanked_num_w, 100*demog_unbanked_age_18_24_frac_w, 100*demog_unbanked_age_25_34_frac_w, 100*demog_unbanked_age_35_44_frac_w, 100*demog_unbanked_age_45_54_frac_w, 100*demog_unbanked_age_55_64_frac_w, 100*demog_unbanked_age_65_and_older_frac_w), Banked_unweighted = c(banked_num, 100*demog_banked_age_18_24_frac, 100*demog_banked_age_25_34_frac, 100*demog_banked_age_35_44_frac, 100*demog_banked_age_45_54_frac, 100*demog_banked_age_55_64_frac, 100*demog_banked_age_65_and_older_frac), Unbanked_unweighted = c(unbanked_num, 100*demog_unbanked_age_18_24_frac, 100*demog_unbanked_age_25_34_frac, 100*demog_unbanked_age_35_44_frac, 100*demog_unbanked_age_45_54_frac, 100*demog_unbanked_age_55_64_frac, 100*demog_unbanked_age_65_and_older_frac)))

# verify age columns sum up to 1
sum(demog_age.df$Banked_unweighted[2:7])
sum(demog_age.df$Unbanked_unweighted[2:7])
sum(demog_age.df$Banked_weighted[2:7])
sum(demog_age.df$Unbanked_weighted[2:7])

# demog gender
table(i5.df$gender)
nrow(subset(i5_banked.df, is.na(gender)))# ignore
nrow(subset(i5_unbanked.df, is.na(gender)))# ignore
# demog gender for the banked
(demog_banked_gender_female_num = nrow(subset(i5_banked.df, gender==0)))
#
(demog_banked_gender_female_frac = demog_banked_gender_female_num/nrow(i5_banked.df))
#
(demog_banked_gender_female_num_w = sum(subset(i5_banked.df, gender==0)$w5_banked))
#
(demog_banked_gender_female_frac_w = demog_banked_gender_female_num_w/nrow(i5_banked.df))
#
(demog_banked_gender_male_num = nrow(subset(i5_banked.df, gender==1)))
#
(demog_banked_gender_male_frac = demog_banked_gender_male_num/nrow(i5_banked.df))
#
(demog_banked_gender_male_num_w = sum(subset(i5_banked.df, gender==1)$w5_banked))
#
(demog_banked_gender_male_frac_w = demog_banked_gender_male_num_w/nrow(i5_banked.df))
# demog gender for the unbanked
(demog_unbanked_gender_female_num = nrow(subset(i5_unbanked.df, gender==0)))
#
(demog_unbanked_gender_female_frac = demog_unbanked_gender_female_num/nrow(i5_unbanked.df))
#
(demog_unbanked_gender_female_num_w = sum(subset(i5_unbanked.df, gender==0)$w5_unbanked))
#
(demog_unbanked_gender_female_frac_w = demog_unbanked_gender_female_num_w/nrow(i5_unbanked.df))
#
(demog_unbanked_gender_male_num = nrow(subset(i5_unbanked.df, gender==1)))
#
(demog_unbanked_gender_male_frac = demog_unbanked_gender_male_num/nrow(i5_unbanked.df))
#
(demog_unbanked_gender_male_num_w = sum(subset(i5_unbanked.df, gender==1)$w5_unbanked))
#
(demog_unbanked_gender_male_frac_w = demog_unbanked_gender_male_num_w/nrow(i5_unbanked.df))

(demog_gender_var.vec = c("Gender: Female"))
#
(demog_gender.df = data.frame(Variable = demog_gender_var.vec, Banked_weighted = c(100*demog_banked_gender_female_frac_w), Unbanked_weighted = c(100*demog_unbanked_gender_female_frac_w), Banked_unweighted = c(100*demog_banked_gender_female_frac), Unbanked_unweighted = c(100*demog_unbanked_gender_female_frac)))

# demog race
table(i5.df$race)
nrow(subset(i5_banked.df, is.na(race)))# ignore
nrow(subset(i5_unbanked.df, is.na(race)))# ignore

# demog race for the banked
(demog_banked_race_asian_num = nrow(subset(i5_banked.df, race==4)))
#
(demog_banked_race_asian_frac = demog_banked_race_asian_num/nrow(i5_banked.df))
#
(demog_banked_race_asian_num_w = sum(subset(i5_banked.df, race==4)$w5_banked))
#
(demog_banked_race_asian_frac_w = demog_banked_race_asian_num_w/nrow(i5_banked.df))
#
(demog_banked_race_black_num = nrow(subset(i5_banked.df, race==2)))
#
(demog_banked_race_black_frac = demog_banked_race_black_num/nrow(i5_banked.df))
#
(demog_banked_race_black_num_w = sum(subset(i5_banked.df, race==2)$w5_banked))
#
(demog_banked_race_black_frac_w = demog_banked_race_black_num_w/nrow(i5_banked.df))
#
(demog_banked_race_white_num = nrow(subset(i5_banked.df, race==1)))
#
(demog_banked_race_white_frac = demog_banked_race_white_num/nrow(i5_banked.df))
#
(demog_banked_race_white_num_w = sum(subset(i5_banked.df, race==1)$w5_banked))
#
(demog_banked_race_white_frac_w = demog_banked_race_white_num_w/nrow(i5_banked.df))
#
(demog_banked_race_other_num = nrow(subset(i5_banked.df, race %in% c(3,5,6))))
#
(demog_banked_race_other_frac = demog_banked_race_other_num/nrow(i5_banked.df))
#
(demog_banked_race_other_num_w = sum(subset(i5_banked.df, race %in% c(3,5,6))$w5_banked))
#
(demog_banked_race_other_frac_w = demog_banked_race_other_num_w/nrow(i5_banked.df))

# verify sum to 1
demog_banked_race_asian_frac + demog_banked_race_black_frac + demog_banked_race_white_frac + demog_banked_race_other_frac
#
demog_banked_race_asian_frac_w + demog_banked_race_black_frac_w + demog_banked_race_white_frac_w + demog_banked_race_other_frac_w

# demog race for the unbanked
(demog_unbanked_race_asian_num = nrow(subset(i5_unbanked.df, race==4)))
#
(demog_unbanked_race_asian_frac = demog_unbanked_race_asian_num/nrow(i5_unbanked.df))
#
(demog_unbanked_race_asian_num_w = sum(subset(i5_unbanked.df, race==4)$w5_unbanked))
#
(demog_unbanked_race_asian_frac_w = demog_unbanked_race_asian_num_w/nrow(i5_unbanked.df))
#
(demog_unbanked_race_black_num = nrow(subset(i5_unbanked.df, race==2)))
#
(demog_unbanked_race_black_frac = demog_unbanked_race_black_num/nrow(i5_unbanked.df))
#
(demog_unbanked_race_black_num_w = sum(subset(i5_unbanked.df, race==2)$w5_unbanked))
#
(demog_unbanked_race_black_frac_w = demog_unbanked_race_black_num_w/nrow(i5_unbanked.df))
#
(demog_unbanked_race_white_num = nrow(subset(i5_unbanked.df, race==1)))
#
(demog_unbanked_race_white_frac = demog_unbanked_race_white_num/nrow(i5_unbanked.df))
#
(demog_unbanked_race_white_num_w = sum(subset(i5_unbanked.df, race==1)$w5_unbanked))
#
(demog_unbanked_race_white_frac_w = demog_unbanked_race_white_num_w/nrow(i5_unbanked.df))
#
(demog_unbanked_race_other_num = nrow(subset(i5_unbanked.df, race %in% c(3,5,6))))
#
(demog_unbanked_race_other_frac = demog_unbanked_race_other_num/nrow(i5_unbanked.df))
#
(demog_unbanked_race_other_num_w = sum(subset(i5_unbanked.df, race %in% c(3,5,6))$w5_unbanked))
#
(demog_unbanked_race_other_frac_w = demog_unbanked_race_other_num_w/nrow(i5_unbanked.df))

# verify sum to 1
demog_unbanked_race_asian_frac + demog_unbanked_race_black_frac + demog_unbanked_race_white_frac + demog_unbanked_race_other_frac
#
demog_unbanked_race_asian_frac_w + demog_unbanked_race_black_frac_w + demog_unbanked_race_white_frac_w + demog_unbanked_race_other_frac_w

# demog race data frame to be appended from below
(demog_race_var.vec = c("Race: Asian", "Race: Black", "Race: White", "Race: Other/combination"))
#
(demog_race.df = data.frame(Variable = demog_race_var.vec, Banked_weighted = c(100*demog_banked_race_asian_frac_w, 100*demog_banked_race_black_frac_w, 100*demog_banked_race_white_frac_w, 100*demog_banked_race_other_frac_w), Unbanked_weighted = c(100*demog_unbanked_race_asian_frac_w, 100*demog_unbanked_race_black_frac_w, 100*demog_unbanked_race_white_frac_w, 100*demog_unbanked_race_other_frac_w), Banked_unweighted = c(100*demog_banked_race_asian_frac, 100*demog_banked_race_black_frac, 100*demog_banked_race_white_frac, 100*demog_banked_race_other_frac), Unbanked_unweighted = c(100*demog_unbanked_race_asian_frac, 100*demog_unbanked_race_black_frac, 100*demog_unbanked_race_white_frac, 100*demog_unbanked_race_other_frac)))

# start demog_Ethnicity
table(i5.df$hispaniclatino)
nrow(subset(i5_banked.df, is.na(hispaniclatino)))# ignore
nrow(subset(i5_unbanked.df, is.na(hispaniclatino)))# ignore
#
# demog hispaniclatino for the banked
(demog_banked_ethnicity_hispaniclatino_num = nrow(subset(i5_banked.df, hispaniclatino==1)))
#
(demog_banked_ethnicity_hispaniclatino_frac = demog_banked_ethnicity_hispaniclatino_num/nrow(i5_banked.df))
#
(demog_banked_ethnicity_hispaniclatino_num_w = sum(subset(i5_banked.df, hispaniclatino==1)$w5_banked))
#
(demog_banked_ethnicity_hispaniclatino_frac_w = demog_banked_ethnicity_hispaniclatino_num_w/nrow(i5_banked.df))

# demog hispaniclatino for the unbanked
(demog_unbanked_ethnicity_hispaniclatino_num = nrow(subset(i5_unbanked.df, hispaniclatino==1)))
#
(demog_unbanked_ethnicity_hispaniclatino_frac = demog_unbanked_ethnicity_hispaniclatino_num/nrow(i5_unbanked.df))
#
(demog_unbanked_ethnicity_hispaniclatino_num_w = sum(subset(i5_unbanked.df, hispaniclatino==1)$w5_unbanked))
#
(demog_unbanked_ethnicity_hispaniclatino_frac_w = demog_unbanked_ethnicity_hispaniclatino_num_w/nrow(i5_unbanked.df))

# construct demog_gender data frame [then row-bind to the grant demog data frame]
(demog_ethnicity_var.vec = c("Ethnicity: hispaniclatino"))
#
(demog_ethnicity.df = data.frame(Variable = demog_ethnicity_var.vec, Banked_weighted = c(100*demog_banked_ethnicity_hispaniclatino_frac_w), Unbanked_weighted = c(100*demog_unbanked_ethnicity_hispaniclatino_frac_w), Banked_unweighted = c(100*demog_unbanked_ethnicity_hispaniclatino_frac_w), Unbanked_unweighted = c(100*demog_unbanked_ethnicity_hispaniclatino_frac)))

# Start demog education banked
table(i5.df$highest_education)
nrow(subset(i5_banked.df, is.na(highest_education)))# ignore
nrow(subset(i5_unbanked.df, is.na(highest_education)))# ignore
#
(demog_banked_edu_less_than_high_school_num = nrow(subset(i5_banked.df, highest_education < 9)))
#
(demog_banked_edu_less_than_high_school_frac = demog_banked_edu_less_than_high_school_num/nrow(i5_banked.df))
#
(demog_banked_edu_less_than_high_school_num_w = sum(subset(i5_banked.df, highest_education < 9)$w5_banked))
#
(demog_banked_edu_less_than_high_school_frac_w = demog_banked_edu_less_than_high_school_num_w/nrow(i5_banked.df))
# high school == 9
(demog_banked_edu_high_school_num = nrow(subset(i5_banked.df, highest_education == 9)))
#
(demog_banked_edu_high_school_frac = demog_banked_edu_high_school_num/nrow(i5_banked.df))
#
(demog_banked_edu_high_school_num_w = sum(subset(i5_banked.df, highest_education == 9)$w5_banked))
#
(demog_banked_edu_high_school_frac_w = demog_banked_edu_high_school_num_w/nrow(i5_banked.df))
# Some college or associate degree == 10, 11, 12
(demog_banked_edu_some_college_or_associate_num = nrow(subset(i5_banked.df, highest_education %in% c(10, 11, 12))))
#
(demog_banked_edu_some_college_or_associate_frac = demog_banked_edu_some_college_or_associate_num/nrow(i5_banked.df))
#
(demog_banked_edu_some_college_or_associate_num_w = sum(subset(i5_banked.df, highest_education %in% c(10, 11, 12))$w5_banked))
#
(demog_banked_edu_some_college_or_associate_frac_w = demog_banked_edu_some_college_or_associate_num_w/nrow(i5_banked.df))
# College 13
(demog_banked_edu_college_num = nrow(subset(i5_banked.df, highest_education == 13)))
#
(demog_banked_edu_college_frac = demog_banked_edu_college_num/nrow(i5_banked.df))
#
(demog_banked_edu_college_num_w = sum(subset(i5_banked.df, highest_education == 13)$w5_banked))
#
(demog_banked_edu_college_frac_w = demog_banked_edu_college_num_w/nrow(i5_banked.df))
# Gradaute > 13
(demog_banked_edu_graduate_num = nrow(subset(i5_banked.df, highest_education > 13)))
#
(demog_banked_edu_graduate_frac = demog_banked_edu_graduate_num/nrow(i5_banked.df))
#
(demog_banked_edu_graduate_num_w = sum(subset(i5_banked.df, highest_education > 13)$w5_banked))
#
(demog_banked_edu_graduate_frac_w = demog_banked_edu_graduate_num_w/nrow(i5_banked.df))

# verify education banked sum up to 1
demog_banked_edu_less_than_high_school_frac + demog_banked_edu_high_school_frac + demog_banked_edu_some_college_or_associate_frac + demog_banked_edu_college_frac + demog_banked_edu_graduate_frac
#
demog_banked_edu_less_than_high_school_frac_w + demog_banked_edu_high_school_frac_w + demog_banked_edu_some_college_or_associate_frac_w + demog_banked_edu_college_frac_w + demog_banked_edu_graduate_frac_w

# demog education UNbanked
(demog_unbanked_edu_less_than_high_school_num = nrow(subset(i5_unbanked.df, highest_education < 9)))
#
(demog_unbanked_edu_less_than_high_school_frac = demog_unbanked_edu_less_than_high_school_num/nrow(i5_unbanked.df))
#
(demog_unbanked_edu_less_than_high_school_num_w = sum(subset(i5_unbanked.df, highest_education < 9)$w5_unbanked))
#
(demog_unbanked_edu_less_than_high_school_frac_w = demog_unbanked_edu_less_than_high_school_num_w/nrow(i5_unbanked.df))
# high school == 9
(demog_unbanked_edu_high_school_num = nrow(subset(i5_unbanked.df, highest_education == 9)))
#
(demog_unbanked_edu_high_school_frac = demog_unbanked_edu_high_school_num/nrow(i5_unbanked.df))
#
(demog_unbanked_edu_high_school_num_w = sum(subset(i5_unbanked.df, highest_education == 9)$w5_unbanked))
#
(demog_unbanked_edu_high_school_frac_w = demog_unbanked_edu_high_school_num_w/nrow(i5_unbanked.df))
# Some college or associate degree == 10, 11, 12
(demog_unbanked_edu_some_college_or_associate_num = nrow(subset(i5_unbanked.df, highest_education %in% c(10, 11, 12))))
#
(demog_unbanked_edu_some_college_or_associate_frac = demog_unbanked_edu_some_college_or_associate_num/nrow(i5_unbanked.df))
#
(demog_unbanked_edu_some_college_or_associate_num_w = sum(subset(i5_unbanked.df, highest_education %in% c(10, 11, 12))$w5_unbanked))
#
(demog_unbanked_edu_some_college_or_associate_frac_w = demog_unbanked_edu_some_college_or_associate_num_w/nrow(i5_unbanked.df))
# College 13
(demog_unbanked_edu_college_num = nrow(subset(i5_unbanked.df, highest_education == 13)))
#
(demog_unbanked_edu_college_frac = demog_unbanked_edu_college_num/nrow(i5_unbanked.df))
#
(demog_unbanked_edu_college_num_w = sum(subset(i5_unbanked.df, highest_education == 13)$w5_unbanked))
#
(demog_unbanked_edu_college_frac_w = demog_unbanked_edu_college_num_w/nrow(i5_unbanked.df))
# Gradaute > 13
(demog_unbanked_edu_graduate_num = nrow(subset(i5_unbanked.df, highest_education > 13)))
#
(demog_unbanked_edu_graduate_frac = demog_unbanked_edu_graduate_num/nrow(i5_unbanked.df))
#
(demog_unbanked_edu_graduate_num_w = sum(subset(i5_unbanked.df, highest_education > 13)$w5_unbanked))
#
(demog_unbanked_edu_graduate_frac_w = demog_unbanked_edu_graduate_num_w/nrow(i5_unbanked.df))

# verify education UNbanked sum up to 1
demog_unbanked_edu_less_than_high_school_frac + demog_unbanked_edu_high_school_frac + demog_unbanked_edu_some_college_or_associate_frac + demog_unbanked_edu_college_frac + demog_unbanked_edu_graduate_frac
#
demog_unbanked_edu_less_than_high_school_frac_w + demog_unbanked_edu_high_school_frac_w + demog_unbanked_edu_some_college_or_associate_frac_w + demog_unbanked_edu_college_frac_w + demog_unbanked_edu_graduate_frac_w


# construct demog_gender data frame [then row-bind to the grant demog data frame]
(demog_edu_var.vec = c("Education: Less than high school", "Education: High school", "Education: Some college or associate", "Education: College", "Education: Graduate"))
#
(demog_edu.df = data.frame(Variable = demog_edu_var.vec, Banked_weighted = c(100*demog_banked_edu_less_than_high_school_frac_w, 100*demog_banked_edu_high_school_frac_w, 100*demog_banked_edu_some_college_or_associate_frac_w, 100*demog_banked_edu_college_frac_w, 100*demog_banked_edu_graduate_frac_w), Unbanked_weighted = c(100*demog_unbanked_edu_less_than_high_school_frac_w, 100*demog_unbanked_edu_high_school_frac_w, 100*demog_unbanked_edu_some_college_or_associate_frac_w, 100*demog_unbanked_edu_college_frac_w, 100*demog_unbanked_edu_graduate_frac_w), Banked_unweighted = c(100*demog_banked_edu_less_than_high_school_frac, 100*demog_banked_edu_high_school_frac, 100*demog_banked_edu_some_college_or_associate_frac, 100*demog_banked_edu_college_frac, 100*demog_banked_edu_graduate_frac), Unbanked_unweighted = c(100*demog_unbanked_edu_less_than_high_school_frac, 100*demog_unbanked_edu_high_school_frac, 100*demog_unbanked_edu_some_college_or_associate_frac, 100*demog_unbanked_edu_college_frac, 100*demog_unbanked_edu_graduate_frac)))

# Start demog Marital status
table(i5.df$marital_status)
nrow(subset(i5_banked.df, is.na(marital_status)))# ignore
nrow(subset(i5_unbanked.df, is.na(marital_status)))# ignore
#
# Marital status: Married banked
(demog_banked_marital_status_married_num = nrow(subset(i5_banked.df, marital_status == 1)))
#
(demog_banked_marital_status_married_frac = demog_banked_marital_status_married_num/nrow(i5_banked.df))
#
(demog_banked_marital_status_married_num_w = sum(subset(i5_banked.df, marital_status==1)$w5_banked))
#
(demog_banked_marital_status_married_frac_w = demog_banked_marital_status_married_num_w/nrow(i5_banked.df))
# Marital status: Married unbanked
(demog_unbanked_marital_status_married_num = nrow(subset(i5_unbanked.df, marital_status == 1)))
#
(demog_unbanked_marital_status_married_frac = demog_unbanked_marital_status_married_num/nrow(i5_unbanked.df))
#
(demog_unbanked_marital_status_married_num_w = sum(subset(i5_unbanked.df, marital_status==1)$w5_unbanked))
#
(demog_unbanked_marital_status_married_frac_w = demog_unbanked_marital_status_married_num_w/nrow(i5_unbanked.df))

# construct a data frame to be appended to demog
(demog_marital_status_var.vec = c("Marital status: Married"))
#
(demog_marital_status.df = data.frame(Variable = demog_marital_status_var.vec, Banked_weighted = c(100*demog_banked_marital_status_married_frac_w), Unbanked_weighted = c(100*demog_unbanked_marital_status_married_frac_w), Banked_unweighted = c(100*demog_banked_marital_status_married_frac), Unbanked_unweighted = c(100*demog_unbanked_marital_status_married_frac)))

# Start demog employment status
nrow(subset(i5_banked.df, is.na(work_employed)))# ignore
#
table(i5.df$work_employed)
# employment: Married banked
(demog_banked_work_employed_employed_num = nrow(subset(i5_banked.df, work_employed == 1)))
#
(demog_banked_work_employed_employed_frac = demog_banked_work_employed_employed_num/nrow(i5_banked.df))
#
(demog_banked_work_employed_employed_num_w = sum(subset(i5_banked.df, work_employed==1)$w5_banked))
#
(demog_banked_work_employed_employed_frac_w = demog_banked_work_employed_employed_num_w/nrow(i5_banked.df))
# Marital status: work_employed unbanked
(demog_unbanked_work_employed_employed_num = nrow(subset(i5_unbanked.df, work_employed == 1)))
#
(demog_unbanked_work_employed_employed_frac = demog_unbanked_work_employed_employed_num/nrow(i5_unbanked.df))
#
(demog_unbanked_work_employed_employed_num_w = sum(subset(i5_unbanked.df, work_employed==1)$w5_unbanked))
#
(demog_unbanked_work_employed_employed_frac_w = demog_unbanked_work_employed_employed_num_w/nrow(i5_unbanked.df))

# construct a data frame to be appended to demog
(demog_work_employed_var.vec = c("Work_employed: Employed"))
#
(demog_work_employed.df = data.frame(Variable = demog_work_employed_var.vec, Banked_weighted = c(100*demog_banked_work_employed_employed_frac_w), Unbanked_weighted = c(100*demog_unbanked_work_employed_employed_frac_w), Banked_unweighted = c(100*demog_banked_work_employed_employed_frac), Unbanked_unweighted = c(100*demog_unbanked_work_employed_employed_frac)))

# Start demog home ownership status
table(i5.df$homeowner)
nrow(subset(i5_banked.df, is.na(homeowner)))# ignore
nrow(subset(i5_unbanked.df, is.na(homeowner)))# ignore
#
# demog home owner banked
(demog_banked_homeowner_own_num = nrow(subset(i5_banked.df, homeowner == 1)))
#
(demog_banked_homeowner_own_frac = demog_banked_homeowner_own_num/nrow(i5_banked.df))
#
(demog_banked_homeowner_own_num_w = sum(subset(i5_banked.df, homeowner==1)$w5_banked))
#
(demog_banked_homeowner_own_frac_w = demog_banked_homeowner_own_num_w/nrow(i5_banked.df))
# homeowner unbanked
(demog_unbanked_homeowner_own_num = nrow(subset(i5_unbanked.df, homeowner == 1)))
#
(demog_unbanked_homeowner_own_frac = demog_unbanked_homeowner_own_num/nrow(i5_unbanked.df))
#
(demog_unbanked_homeowner_own_num_w = sum(subset(i5_unbanked.df, homeowner==1)$w5_unbanked))
#
(demog_unbanked_homeowner_own_frac_w = demog_unbanked_homeowner_own_num_w/nrow(i5_unbanked.df))

# construct a data frame to be appended to demog
(demog_homeowner_var.vec = c("Homeowner: Own"))
#
(demog_homeowner.df = data.frame(Variable = demog_homeowner_var.vec, Banked_weighted = c(100*demog_banked_homeowner_own_frac_w), Unbanked_weighted = c(100*demog_unbanked_homeowner_own_frac_w), Banked_unweighted = c(100*demog_banked_homeowner_own_frac), Unbanked_unweighted = c(100*demog_unbanked_homeowner_own_frac)))

# Start demog urban category: 1=rural, 2=mixed, 3=urban
table(i5.df$urban_cat, exclude = F)
nrow(subset(i5_banked.df, is.na(urban_cat)))# ignore
nrow(subset(i5_unbanked.df, is.na(urban_cat)))# ignore
#
# demog urban category banked: rural
(demog_banked_rural_num = nrow(subset(i5_banked.df, urban_cat == 1)))
#
(demog_banked_rural_frac = demog_banked_rural_num/nrow(i5_banked.df))
#
(demog_banked_rural_num_w = sum(subset(i5_banked.df, urban_cat==1)$w5_banked))
#
(demog_banked_rural_frac_w = demog_banked_rural_num_w/nrow(i5_banked.df))
#
# demog urban category banked: urban
(demog_banked_urban_num = nrow(subset(i5_banked.df, urban_cat == 3)))
#
(demog_banked_urban_frac = demog_banked_urban_num/nrow(i5_banked.df))
#
(demog_banked_urban_num_w = sum(subset(i5_banked.df, urban_cat==3)$w5_banked))
#
(demog_banked_urban_frac_w = demog_banked_urban_num_w/nrow(i5_banked.df))
#
# demog urban category banked: mixed_urban
(demog_banked_mixed_urban_num = nrow(subset(i5_banked.df, urban_cat == 2)))
#
(demog_banked_mixed_urban_frac = demog_banked_mixed_urban_num/nrow(i5_banked.df))
#
(demog_banked_mixed_urban_num_w = sum(subset(i5_banked.df, urban_cat==2)$w5_banked))
#
(demog_banked_mixed_urban_frac_w = demog_banked_mixed_urban_num_w/nrow(i5_banked.df))
#
# demog urban category UNbanked: rural
(demog_unbanked_rural_num = nrow(subset(i5_unbanked.df, urban_cat == 1)))
#
(demog_unbanked_rural_frac = demog_unbanked_rural_num/nrow(i5_unbanked.df))
#
(demog_unbanked_rural_num_w = sum(subset(i5_unbanked.df, urban_cat==1)$w5_unbanked))
#
(demog_unbanked_rural_frac_w = demog_unbanked_rural_num_w/nrow(i5_unbanked.df))
#
# demog urban category banked: urban
(demog_unbanked_urban_num = nrow(subset(i5_unbanked.df, urban_cat == 3)))
#
(demog_unbanked_urban_frac = demog_unbanked_urban_num/nrow(i5_unbanked.df))
#
(demog_unbanked_urban_num_w = sum(subset(i5_unbanked.df, urban_cat==3)$w5_unbanked))
#
(demog_unbanked_urban_frac_w = demog_unbanked_urban_num_w/nrow(i5_unbanked.df))
#
# demog urban category banked: mixed_urban
(demog_unbanked_mixed_urban_num = nrow(subset(i5_unbanked.df, urban_cat == 2)))
#
(demog_unbanked_mixed_urban_frac = demog_unbanked_mixed_urban_num/nrow(i5_unbanked.df))
#
(demog_unbanked_mixed_urban_num_w = sum(subset(i5_unbanked.df, urban_cat==2)$w5_unbanked))
#
(demog_unbanked_mixed_urban_frac_w = demog_unbanked_mixed_urban_num_w/nrow(i5_unbanked.df))
#
# verify all sum up to 1
demog_banked_rural_frac + demog_banked_urban_frac + demog_banked_mixed_urban_frac
#
demog_banked_rural_frac_w + demog_banked_urban_frac_w + demog_banked_mixed_urban_frac_w
#
demog_unbanked_rural_frac + demog_unbanked_urban_frac + demog_unbanked_mixed_urban_frac
#
demog_unbanked_rural_frac_w + demog_unbanked_urban_frac_w + demog_unbanked_mixed_urban_frac_w
#

# construct a data frame to be appended to demog
(demog_urban_cat_var.vec = c("Urban_category: Rural", "Urban_category: Urban", "Urban_category: Mixed"))
#
(demog_urban_cat.df = data.frame(Variable = demog_urban_cat_var.vec, Banked_weighted = c(100*demog_banked_rural_frac_w, 100*demog_banked_urban_frac_w, 100*demog_banked_mixed_urban_frac), Unbanked_weighted = c(100*demog_unbanked_rural_frac_w, 100*demog_unbanked_urban_frac_w, 100*demog_unbanked_mixed_urban_frac_w), Banked_unweighted = c(100*demog_banked_rural_frac, 100*demog_banked_urban_frac, 100*demog_banked_mixed_urban_frac), Unbanked_unweighted = c(100*demog_unbanked_rural_frac, 100*demog_unbanked_urban_frac, 100*demog_unbanked_mixed_urban_frac)))
#
# verify columns sum up to 100
sum(demog_urban_cat.df$Banked_weighted)
sum(demog_urban_cat.df$Unbanked_weighted)
sum(demog_urban_cat.df$Banked_unweighted)
sum(demog_urban_cat.df$Unbanked_unweighted)
#

# Start demog median household size
table(i5.df$hh_size)
str(i5.df$hh_size)
# demog median HH size banked
(demog_banked_hh_size_med_w = weighted.median(i5_banked.df$hh_size, i5_banked.df$w5_banked, na.rm = T))
#
(demog_banked_hh_size_med = median(i5_banked.df$hh_size, na.rm = T))
# demog median HH size unbanked
(demog_unbanked_hh_size_med_w = weighted.median(i5_unbanked.df$hh_size, i5_unbanked.df$w5_unbanked, na.rm = T))
#
(demog_unbanked_hh_size_med = median(i5_unbanked.df$hh_size, na.rm = T))

# construct a data frame to be appended to demog
(demog_hhsize_var.vec = c("Household_size: Median"))
#
(demog_hhsize.df = data.frame(Variable = demog_hhsize_var.vec, Banked_weighted = c(demog_banked_hh_size_med_w), Unbanked_weighted = c(demog_unbanked_hh_size_med_w), Banked_unweighted = c(demog_banked_hh_size_med), Unbanked_unweighted = c(demog_unbanked_hh_size_med)))

# Start demog HH income banked
summary(i5.df$income_hh)
str(i5.df$income_hh)
nrow(subset(i5_banked.df, is.na(income_hh))) #=> num NA
nrow(subset(i5_unbanked.df, is.na(income_hh))) #=> num NA
# construct subset with no income_hh NA
i5_banked_income.df = subset(i5_banked.df, !is.na(income_hh))
#
i5_unbanked_income.df = subset(i5_unbanked.df, !is.na(income_hh))
#
nrow(i5_banked_income.df)
nrow(i5_unbanked_income.df)
nrow(i5_banked.df)
nrow(i5_unbanked.df)
#
(demog_banked_income_0_30_num = nrow(subset(i5_banked_income.df, income_hh < 30000)))
#
(demog_banked_income_0_30_frac = demog_banked_income_0_30_num/nrow(i5_banked_income.df))
#
(demog_banked_income_0_30_num_w = sum(subset(i5_banked_income.df, income_hh < 30000)$w5_banked))
#
(demog_banked_income_0_30_frac_w = demog_banked_income_0_30_num_w/nrow(i5_banked_income.df))
# 30-60k
(demog_banked_income_30_60_num = nrow(subset(i5_banked_income.df, income_hh >= 30000 & income_hh < 60000)))
#
(demog_banked_income_30_60_frac = demog_banked_income_30_60_num/nrow(i5_banked_income.df))
#
(demog_banked_income_30_60_num_w = sum(subset(i5_banked_income.df, income_hh >= 30000 & income_hh < 60000)$w5_banked))
#
(demog_banked_income_30_60_frac_w = demog_banked_income_30_60_num_w/nrow(i5_banked_income.df))
# 60-90k
(demog_banked_income_60_90_num = nrow(subset(i5_banked_income.df, income_hh >= 60000 & income_hh < 90000)))
#
(demog_banked_income_60_90_frac = demog_banked_income_60_90_num/nrow(i5_banked_income.df))
#
(demog_banked_income_60_90_num_w = sum(subset(i5_banked.df, income_hh >= 60000 & income_hh < 90000)$w5_banked))
#
(demog_banked_income_60_90_frac_w = demog_banked_income_60_90_num_w/nrow(i5_banked_income.df))
#  >= 90k
(demog_banked_income_90_inf_num = nrow(subset(i5_banked_income.df, income_hh >= 90000)))
#
(demog_banked_income_90_inf_frac = demog_banked_income_90_inf_num/nrow(i5_banked_income.df))
#
(demog_banked_income_90_inf_num_w = sum(subset(i5_banked_income.df, income_hh >= 90000)$w5_banked))
#
(demog_banked_income_90_inf_frac_w = demog_banked_income_90_inf_num_w/nrow(i5_banked_income.df))

# Verify sum to 1
demog_banked_income_0_30_frac + demog_banked_income_30_60_frac + demog_banked_income_60_90_frac + demog_banked_income_90_inf_frac
#
#demog income UNbanked
(demog_unbanked_income_0_30_num = nrow(subset(i5_unbanked_income.df, income_hh < 30000)))
#
(demog_unbanked_income_0_30_frac = demog_unbanked_income_0_30_num/nrow(i5_unbanked_income.df))
#
(demog_unbanked_income_0_30_num_w = sum(subset(i5_unbanked_income.df, income_hh < 30000)$w5_unbanked))
#
(demog_unbanked_income_0_30_frac_w = demog_unbanked_income_0_30_num_w/nrow(i5_unbanked_income.df))
# 30-60k
(demog_unbanked_income_30_60_num = nrow(subset(i5_unbanked_income.df, income_hh >= 30000 & income_hh < 60000)))
#
(demog_unbanked_income_30_60_frac = demog_unbanked_income_30_60_num/nrow(i5_unbanked_income.df))
#
(demog_unbanked_income_30_60_num_w = sum(subset(i5_unbanked_income.df, income_hh >= 30000 & income_hh < 60000)$w5_unbanked))
#
(demog_unbanked_income_30_60_frac_w = demog_unbanked_income_30_60_num_w/nrow(i5_unbanked_income.df))
# 60-90k
(demog_unbanked_income_60_90_num = nrow(subset(i5_unbanked_income.df, income_hh >= 60000 & income_hh < 90000)))
#
(demog_unbanked_income_60_90_frac = demog_unbanked_income_60_90_num/nrow(i5_unbanked_income.df))
#
(demog_unbanked_income_60_90_num_w = sum(subset(i5_unbanked_income.df, income_hh >= 60000 & income_hh < 90000)$w5_unbanked))
#
(demog_unbanked_income_60_90_frac_w = demog_unbanked_income_60_90_num_w/nrow(i5_unbanked_income.df))
#  >= 90k
(demog_unbanked_income_90_inf_num = nrow(subset(i5_unbanked_income.df, income_hh >= 90000)))
#
(demog_unbanked_income_90_inf_frac = demog_unbanked_income_90_inf_num/nrow(i5_unbanked_income.df))
#
(demog_unbanked_income_90_inf_num_w = sum(subset(i5_unbanked_income.df, income_hh >= 90000)$w5_unbanked))
#
(demog_unbanked_income_90_inf_frac_w = demog_unbanked_income_90_inf_num_w/nrow(i5_unbanked_income.df))

# Verify sum to 1
demog_unbanked_income_0_30_frac + demog_unbanked_income_30_60_frac + demog_unbanked_income_60_90_frac + demog_unbanked_income_90_inf_frac
#
demog_unbanked_income_0_30_frac_w + demog_unbanked_income_30_60_frac_w + demog_unbanked_income_60_90_frac_w + demog_unbanked_income_90_inf_frac_w 


# construct demog HH income data frame [then row-bind to the grant demog data frame]
(demog_income_var.vec = c("Household income: 0-30k", "Household income: 30-60k", "Household income: 60-90k", "Household income: 90k and higher"))
#
(demog_income.df = data.frame(Variable = demog_income_var.vec, Banked_weighted = c(100*demog_banked_income_0_30_frac_w, 100*demog_banked_income_30_60_frac_w, 100*demog_banked_income_60_90_frac_w, 100*demog_banked_income_90_inf_frac_w), Unbanked_weighted =  c(100*demog_unbanked_income_0_30_frac_w, 100*demog_unbanked_income_30_60_frac_w, 100*demog_unbanked_income_60_90_frac_w, 100*demog_unbanked_income_90_inf_frac_w), Banked_unweighted = c(100*demog_banked_income_0_30_frac, 100*demog_banked_income_30_60_frac, 100*demog_banked_income_60_90_frac, 100*demog_banked_income_90_inf_frac), Unbanked_unweighted = c(100*demog_unbanked_income_0_30_frac, 100*demog_unbanked_income_30_60_frac, 100*demog_unbanked_income_60_90_frac, 100*demog_unbanked_income_90_inf_frac)))

# Start demog num respondents
# demog num respondents banked
(demog_banked_resp_num = nrow(i5_banked.df))
#
(demog_banked_resp_frac = demog_banked_resp_num/nrow(i5.df))
#
(demog_banked_resp_num_w = sum(subset(i5_banked.df)$w5))
#
(demog_banked_resp_frac_w = demog_banked_resp_num_w/nrow(i5.df))
# num respondents unbanked
(demog_unbanked_resp_num = nrow(i5_unbanked.df))
#
(demog_unbanked_resp_frac = demog_unbanked_resp_num/nrow(i5.df))
#
(demog_unbanked_resp_num_w = sum(subset(i5_unbanked.df)$w5))
#
(demog_unbanked_resp_frac_w = demog_unbanked_resp_num_w/nrow(i5.df))

# construct a data frame to be appended to demog
(demog_resp_var.vec = c("Number of respondents", "Percentage of all respondents"))
#
(demog_resp.df = data.frame(Variable = demog_resp_var.vec, Banked_weighted = c(demog_banked_resp_num_w, 100*demog_banked_resp_frac_w), Unbanked_weighted = c(demog_unbanked_resp_num_w, 100*demog_unbanked_resp_frac_w), Banked_unweighted = c(demog_banked_resp_num, 100*demog_banked_resp_frac), Unbanked_unweighted = c(demog_unbanked_resp_num, 100*demog_unbanked_resp_frac)))

## combine all demog data frames into one
#
demog_resp.df
dim(demog_resp.df)
#
demog_age.df
dim(demog_age.df)
#
demog_gender.df
dim(demog_gender.df)
#
demog_race.df
dim(demog_race.df)
#
demog_ethnicity.df
dim(demog_ethnicity.df)
#
demog_edu.df
dim(demog_edu.df)
#
demog_marital_status.df
dim(demog_marital_status.df)
#
demog_income.df
dim(demog_income.df)
#
demog_work_employed.df
dim(demog_work_employed.df)
#
demog_hhsize.df
dim(demog_hhsize.df)
#
demog_homeowner.df
dim(demog_homeowner.df)

(demog.df = rbind(demog_resp.df, demog_age.df, demog_gender.df, demog_race.df, demog_ethnicity.df, demog_edu.df, demog_marital_status.df, demog_income.df, demog_work_employed.df, demog_hhsize.df, demog_homeowner.df, demog_urban_cat.df))
#
# Delete 3rd row (repetition of 1st row)
(demog2.df = demog.df[-3,])

#write.csv(demog.df, "table1.csv", row.names = F)
#
dim(demog2.df)
print(xtable(demog2.df, digits = 2), include.rownames = F, hline.after = c(0,2,8,9,13,14,19,20,24,25,26,27,30))

# End Table 1 summary stat ####
# End Table 1 in paper (sample statistics)

# Start mobile adopt ####
# Table is just described in the conclusion (not displayed in a table)
# code copied from unbanked_pay_2023_2_11.R. That code also have "funding" table
### Table on app adoption (with and without Zelle). "Table app" in Excel file. Some resemblance to Figure 7, but here apps are asked in detail. 
# verify weights
nrow(i5.df)
sum(i5.df$w5)
nrow(i5_banked.df)+nrow(i5_unbanked.df)
nrow(i5_banked.df)
sum(i5_banked.df$w5_banked)
nrow(i5_unbanked.df)
sum(i5_unbanked.df$w5_unbanked)
#
# Paypal adoption, paypal_adopt codebook p.316
(adopt_paypal_banked_w_frac = sum(i5_banked.df[i5_banked.df$paypal_adopt==1, ]$w5_banked, na.rm = T)/sum(i5_banked.df$w5_banked, na.rm = T) )
#
(adopt_paypal_unbanked_w_frac = sum(i5_unbanked.df[i5_unbanked.df$paypal_adopt==1, ]$w5_unbanked, na.rm = T)/sum(i5_unbanked.df$w5_unbanked, na.rm = T) )
#
(adopt_paypal_all_w_frac = sum(i5.df[i5.df$paypal_adopt==1, ]$w5, na.rm = T)/sum(i5.df$w5, na.rm = T) )
#
# Venmo adoption venmo_adopt, codebook p.358
(adopt_venmo_banked_w_frac = sum(i5_banked.df[i5_banked.df$venmo_adopt==1, ]$w5_banked, na.rm = T)/sum(i5_banked.df$w5_banked, na.rm = T) )
#
(adopt_venmo_unbanked_w_frac = sum(i5_unbanked.df[i5_unbanked.df$venmo_adopt==1, ]$w5_unbanked, na.rm = T)/sum(i5_unbanked.df$w5_unbanked, na.rm = T) )
#
(adopt_venmo_all_w_frac = sum(i5.df[i5.df$venmo_adopt==1, ]$w5, na.rm = T)/sum(i5.df$w5, na.rm = T) )
#
# other_nbops adoption other_nbops_adopt, codebook p.309
(adopt_other_nbops_banked_w_frac = sum(i5_banked.df[i5_banked.df$other_nbops_adopt==1, ]$w5_banked, na.rm = T)/sum(i5_banked.df$w5_banked, na.rm = T) )
#
(adopt_other_nbops_unbanked_w_frac = sum(i5_unbanked.df[i5_unbanked.df$other_nbops_adopt==1, ]$w5_unbanked, na.rm = T)/sum(i5_unbanked.df$w5_unbanked, na.rm = T) )
#
(adopt_other_nbops_all_w_frac = sum(i5.df[i5.df$other_nbops_adopt==1, ]$w5, na.rm = T)/sum(i5.df$w5, na.rm = T) )
#
# zelle adoption zelle_adopt, codebook p.378
(adopt_zelle_banked_w_frac = sum(i5_banked.df[i5_banked.df$zelle_adopt==1, ]$w5_banked, na.rm = T)/sum(i5_banked.df$w5_banked, na.rm = T) )
#
(adopt_zelle_unbanked_w_frac = sum(i5_unbanked.df[i5_unbanked.df$zelle_adopt==1, ]$w5_unbanked, na.rm = T)/sum(i5_unbanked.df$w5_unbanked, na.rm = T) )
#
(adopt_zelle_all_w_frac = sum(i5.df[i5.df$zelle_adopt==1, ]$w5, na.rm = T)/sum(i5.df$w5, na.rm = T) )
#
## start preparing for any of the above, creating a new variable "any" which includes Zelle adoption
i5_banked.df$any_adopt = ifelse(i5_banked.df$paypal_adopt==1 | i5_banked.df$venmo_adopt==1 | i5_banked.df$other_nbops_adopt==1 | i5_banked.df$zelle_adopt==1, 1, 0)
table(i5_banked.df$any_adopt, exclude = T)
(adopt_any_adopt_banked_w_frac = sum(i5_banked.df[i5_banked.df$any_adopt==1, ]$w5_banked, na.rm = T)/sum(i5_banked.df$w5_banked, na.rm = T) )
#
i5_unbanked.df$any_adopt = ifelse(i5_unbanked.df$paypal_adopt==1 | i5_unbanked.df$venmo_adopt==1 | i5_unbanked.df$other_nbops_adopt==1 | i5_unbanked.df$zelle_adopt==1, 1, 0)
table(i5_unbanked.df$any_adopt, exclude = T)
(adopt_any_adopt_unbanked_w_frac = sum(i5_unbanked.df[i5_unbanked.df$any_adopt==1, ]$w5_unbanked, na.rm = T)/sum(i5_unbanked.df$w5_unbanked, na.rm = T) )
#
i5.df$any_adopt = ifelse(i5.df$paypal_adopt==1 | i5.df$venmo_adopt==1 | i5.df$other_nbops_adopt==1 | i5.df$zelle_adopt==1, 1, 0)
table(i5.df$any_adopt, exclude = T)
(adopt_any_adopt_all_w_frac = sum(i5.df[i5.df$any_adopt==1, ]$w5, na.rm = T)/sum(i5.df$w5, na.rm = T) )
#
## start preparing for any of the above, creating a new variable "any_no_zelle" which exclude Zelle adoption
i5_banked.df$any_no_zelle_adopt = ifelse(i5_banked.df$paypal_adopt==1 | i5_banked.df$venmo_adopt==1 | i5_banked.df$other_nbops_adopt==1, 1, 0)
table(i5_banked.df$any_no_zelle_adopt, exclude = T)
(adopt_any_no_zelle_adopt_banked_w_frac = sum(i5_banked.df[i5_banked.df$any_no_zelle_adopt==1, ]$w5_banked, na.rm = T)/sum(i5_banked.df$w5_banked, na.rm = T) )
#
i5_unbanked.df$any_no_zelle_adopt = ifelse(i5_unbanked.df$paypal_adopt==1 | i5_unbanked.df$venmo_adopt==1 | i5_unbanked.df$other_nbops_adopt==1, 1, 0)
table(i5_unbanked.df$any_no_zelle_adopt, exclude = T)
(adopt_any_no_zelle_adopt_unbanked_w_frac = sum(i5_unbanked.df[i5_unbanked.df$any_no_zelle_adopt==1, ]$w5_unbanked, na.rm = T)/sum(i5_unbanked.df$w5_unbanked, na.rm = T) )
#
i5.df$any_no_zelle_adopt = ifelse(i5.df$paypal_adopt==1 | i5.df$venmo_adopt==1 | i5.df$other_nbops_adopt==1, 1, 0)
table(i5.df$any_no_zelle_adopt, exclude = T)
(adopt_any_no_zelle_adopt_all_w_frac = sum(i5.df[i5.df$any_no_zelle_adopt==1, ]$w5, na.rm = T)/sum(i5.df$w5, na.rm = T) )
#
## Construct the app_adopt data frame
(app_adopt_variable.vec = c("paypal_adopt", "venmo_adopt", "other_nbops_adopt", "zelle_adopt", "any_with_zelle", "any_without_zelle"))
#
(app_adopt_all.vec = c(adopt_paypal_all_w_frac, adopt_venmo_all_w_frac, adopt_other_nbops_all_w_frac, adopt_zelle_all_w_frac, adopt_any_adopt_all_w_frac, adopt_any_no_zelle_adopt_all_w_frac))
#
(app_adopt_banked.vec = c(adopt_paypal_banked_w_frac, adopt_venmo_banked_w_frac, adopt_other_nbops_banked_w_frac, adopt_zelle_banked_w_frac, adopt_any_adopt_banked_w_frac, adopt_any_no_zelle_adopt_banked_w_frac))
#
(app_adopt_unbanked.vec = c(adopt_paypal_unbanked_w_frac, adopt_venmo_unbanked_w_frac, adopt_other_nbops_unbanked_w_frac, adopt_zelle_unbanked_w_frac, adopt_any_adopt_unbanked_w_frac, adopt_any_no_zelle_adopt_unbanked_w_frac))
#
(app_adopt.df = data.frame(Variable = app_adopt_variable.vec, All = app_adopt_all.vec, Banked = app_adopt_banked.vec, Unbanked = app_adopt_unbanked.vec ))
#
#write.csv(app_adopt.df, "app_adopt.csv", row.names = F)
# End mobile adopt ####

# Start data unbanked regressions ####
# Start Table: Demog regressions [Not in the paper]
## Construct factor variables
i6.df = i5.df
table(i6.df$banked)
str(i6.df$age)
i6.df$age_factor = NA #construct age factors
i6.df$age_factor[i6.df$age < 25] = "Age_18_25"
i6.df$age_factor[i6.df$age >= 25 & i6.df$age < 34] = "Age_25_34"
i6.df$age_factor[i6.df$age >= 35 & i6.df$age < 44] = "Age_35_44"
i6.df$age_factor[i6.df$age >= 45 & i6.df$age < 54] = "Age_45_55"
i6.df$age_factor[i6.df$age >= 55 & i6.df$age < 64] = "Age_55_64"
i6.df$age_factor[i6.df$age >= 65] = "Age_65_and_older"
table(i6.df$age_factor)
sum(table(i6.df$age_factor))
nrow(i6.df)
i6.df$age_factor = as.factor(i6.df$age_factor)
str(i6.df$age_factor)
table(i6.df$age_factor)
i6.df$age_factor = relevel(i6.df$age_factor, ref = "Age_35_44") # reference age
levels(i6.df$age_factor)

#
str(i6.df$gender)
table(i6.df$gender)
i6.df$gender_factor = NA #construct age factors
i6.df$gender_factor[i6.df$gender==0] = "Gender_female"
i6.df$gender_factor[i6.df$gender==1] = "Gender_male"
table(i6.df$gender_factor)
sum(table(i6.df$gender_factor))
nrow(i6.df)
str(i6.df$gender_factor)
i6.df$gender_factor = as.factor(i6.df$gender_factor)
i6.df$gender_factor = relevel(i6.df$gender_factor, ref = "Gender_male") # reference male
levels(i6.df$gender_factor)

#
str(i6.df$race)
table(i6.df$race)
i6.df$race_factor = NA #construct age factors
i6.df$race_factor[i6.df$race==1] = "Race_white"
i6.df$race_factor[i6.df$race==2] = "Race_black"
i6.df$race_factor[i6.df$race==4] = "Race_asian"
i6.df$race_factor[i6.df$race %in% c(3,5,6)] = "Race_other"
table(i6.df$race_factor)
sum(table(i6.df$race_factor))
nrow(i6.df)
str(i6.df$race_factor)
i6.df$race_factor = as.factor(i6.df$race_factor)
i6.df$race_factor = relevel(i6.df$race_factor, ref = "Race_white")# white is reference
levels(i6.df$race_factor)

#
str(i6.df$hispaniclatino)
table(i6.df$hispaniclatino)
i6.df$ethnicity_factor = NA #construct age factors
i6.df$ethnicity_factor[i6.df$hispaniclatino==1] = "ethnicity_hispaniclatino"
i6.df$ethnicity_factor[i6.df$hispaniclatino==0] = "ethnicity_not_hispaniclatino"
table(i6.df$ethnicity_factor)
sum(table(i6.df$ethnicity_factor))
nrow(i6.df)
str(i6.df$ethnicity_factor)
i6.df$ethnicity_factor = as.factor(i6.df$ethnicity_factor)
i6.df$ethnicity_factor = relevel(i6.df$ethnicity_factor, ref = "ethnicity_not_hispaniclatino")# non-latino is reference
levels(i6.df$ethnicity_factor)

# Changing education factor 
str(i6.df$highest_education)
table(i6.df$highest_education, exclude = T)
i6.df$edu_factor = NA #construct age factors
i6.df$edu_factor[i6.df$highest_education < 9] = "Less_than_high_school"
i6.df$edu_factor[i6.df$highest_education == 9] = "High_school"
i6.df$edu_factor[i6.df$highest_education %in% c(10, 11, 12)] = "Some_college_or_associate"
#i6.df$edu_factor[i6.df$highest_education == 13] = "Education_college"
i6.df$edu_factor[i6.df$highest_education >= 13] = "College_and_higher"
#
#i6.df$edu_factor[i6.df$highest_education < 13] = "Education_no_college_degree"
#i6.df$edu_factor[i6.df$highest_education > 13] = "Education_graduate"
table(i6.df$edu_factor)
sum(table(i6.df$edu_factor))
nrow(i6.df)
str(i6.df$edu_factor)
i6.df$edu_factor = as.factor(i6.df$edu_factor)
#i6.df$edu_factor = relevel(i6.df$edu_factor, ref = "Education_college")# college education is reference
#i6.df$edu_factor = relevel(i6.df$edu_factor, ref = "Education_high_school")# HS education is reference
levels(i6.df$edu_factor)
#levels(i6.df$edu_factor) = c("Education_no_college_degree", "Education_college_and_higher")

#
str(i6.df$marital_status)
table(i6.df$marital_status)
i6.df$marital_factor = NA #construct marital factors
i6.df$marital_factor[i6.df$marital_status == 1] = "Marital_status_married"
i6.df$marital_factor[i6.df$marital_status > 1] = "Marital_status_other"
table(i6.df$marital_factor)
sum(table(i6.df$marital_factor))
nrow(i6.df)
str(i6.df$marital_factor)
i6.df$marital_factor = as.factor(i6.df$marital_factor)
i6.df$marital_factor = relevel(i6.df$marital_factor, ref = "Marital_status_other") #not married is reference
levels(i6.df$marital_factor)

#
str(i6.df$income_hh)# used in the regression instead of the income_factor constructed below
summary(i6.df$income_hh)
i6.df$income_factor = NA #construct HH income factors
i6.df$income_factor[i6.df$income_hh  < 30000] = "HH_income_less_than_30k"
i6.df$income_factor[i6.df$income_hh  >= 30000 & i6.df$income_hh < 60000] = "HH_income_30k_60k"
i6.df$income_factor[i6.df$income_hh  >= 60000 & i6.df$income_hh < 90000] = "HH_income_60k_90k"
i6.df$income_factor[i6.df$income_hh  >= 90000] = "HH_income_90k_and_higher"
table(i6.df$income_factor, exclude = T)
sum(table(i6.df$income_factor))
nrow(i6.df)
str(i6.df$income_factor)
i6.df$income_factor = as.factor(i6.df$income_factor)
#i6.df$income_factor = relevel(i6.df$income_factor, ref = "HH_income_60k_90k")# reference variable to be changed later on
levels(i6.df$income_factor)
levels(i6.df$income_factor) = c("HH_income_less_than_30k",  "HH_income_30k_60k", "HH_income_60k_90k",  "HH_income_90k_and_higher")
table(i6.df$income_factor, exclude = T)
sum(table(i6.df$income_factor, exclude = T))
#
str(i6.df$work_employed)
table(i6.df$work_employed)
i6.df$work_factor = NA #construct marital factors
i6.df$work_factor[i6.df$work_employed == 1] = "Work_employed"
i6.df$work_factor[i6.df$work_employed == 0] = "Work_not_employed"
table(i6.df$work_factor)
sum(table(i6.df$edu_factor))
nrow(i6.df)
str(i6.df$work_factor)
i6.df$work_factor = as.factor(i6.df$work_factor)
i6.df$work_factor = relevel(i6.df$work_factor, ref = "Work_not_employed")
levels(i6.df$work_factor)

#
str(i6.df$hh_size)
table(i6.df$hh_size)
#
str(i6.df$homeowner)
table(i6.df$homeowner)
i6.df$homeowner_factor = NA #construct marital factors
i6.df$homeowner_factor[i6.df$homeowner == 1] = "Homeowner_yes"
i6.df$homeowner_factor[i6.df$homeowner == 0] = "Homeowner_no"
table(i6.df$homeowner_factor)
sum(table(i6.df$homeowner_factor))
nrow(i6.df)
str(i6.df$homeowner_factor)
i6.df$homeowner_factor = as.factor(i6.df$homeowner_factor)
i6.df$homeowner_factor = relevel(i6.df$homeowner_factor, ref = "Homeowner_no")
levels(i6.df$homeowner_factor)

# new variable: income_hh/10k. New dataset for regression and RF only
i6.df$income_hh_div_10k = i6.df$income_hh/10000
summary(i6.df$income_hh_div_10k)
nrow(subset(i6.df, income_hh >=  1000000))
#
#summary(i6.df$age)
#nrow(subset(i6.df, age >=  100))
#summary(i6.df$age)
#nrow(subset(i6.df, age >=  100))
#
i6_reg.df = subset(i6.df, income_hh < 1000000)# => it also removes 82 resp with income_hh NAs
dim(i6_reg.df)
length(unique(i6.df$id))
# restricting to regression variables only
i6_reg.df = subset(i6_reg.df, select = c(unbanked, age,  gender_factor, race_factor, ethnicity_factor, edu_factor,  marital_factor, income_hh_div_10k, work_factor, hh_size,  homeowner_factor, id))
dim(i6_reg.df)
#
str(i6_reg.df)
i6_reg.df$unbanked = factor(i6_reg.df$unbanked)
names(i6_reg.df)
# End data unbanked regressions ####
# Not in the paper 

# Start reasons for unbanked ####
### In section 2.1 (reasons for being unbanked, listed not in a table). These are weighted.
dim(i5_unbanked.df)
table(i5_unbanked.df$pa002)
sum(table(i5_unbanked.df$pa002))
sum(i5_unbanked.df$w5)# more than nrow. 
#
(reason_1_w_frac = sum(subset(i5_unbanked.df, pa002 == 1)$w5)/sum(i5_unbanked.df$w5))
#
(reason_2_w_frac = sum(subset(i5_unbanked.df, pa002 == 2)$w5)/sum(i5_unbanked.df$w5))
#
(reason_3_w_frac = sum(subset(i5_unbanked.df, pa002 == 3)$w5)/sum(i5_unbanked.df$w5))
#
(reason_4_w_frac = sum(subset(i5_unbanked.df, pa002 == 4)$w5)/sum(i5_unbanked.df$w5))
#
(reason_5_w_frac = sum(subset(i5_unbanked.df, pa002 == 5)$w5)/sum(i5_unbanked.df$w5))
#
(reason_6_w_frac = sum(subset(i5_unbanked.df, pa002 == 6)$w5)/sum(i5_unbanked.df$w5))
#
(reason_7_w_frac = sum(subset(i5_unbanked.df, pa002 == 7)$w5)/sum(i5_unbanked.df$w5))
# check sums to 1
reason_1_w_frac + reason_2_w_frac + reason_3_w_frac + reason_4_w_frac + reason_5_w_frac + reason_6_w_frac + reason_7_w_frac

# constructing data frame for reasons to be unbanked (section 2.1 in the paper, not in a table)
reason_var.vec = c("Unbanked reason 1", "Unbanked reason 2", "Unbanked reason 3", "Unbanked reason 4", "Unbanked reason 5", "Unbanked reason 6", "Unbanked reason 7")
#
(reason.df = data.frame(Unbanked_reason = reason_var.vec, Percentage_of_unbanked = c(100*reason_1_w_frac, 100*reason_2_w_frac, 100*reason_3_w_frac, 100*reason_4_w_frac, 100*reason_5_w_frac, 100*reason_6_w_frac, 100*reason_7_w_frac)))
# check sums to 100
sum(reason.df$Percentage_of_unbanked)
#
# Spelling out the reasons from code book pa002 on p.392
(reason_spell_var.vec = c("I don’t write enough checks to make it worthwhile", "The minimum balance is too high", "I don’t like dealing with banks", "The fees and service charges are too high", "No bank has convenient hours or location", "No bank will give me a checking account", "Other (explain)"))
#
(reason_spell.df = data.frame(Unbanked_reason = reason_spell_var.vec, Percentage_of_unbanked = round(c(100*reason_1_w_frac, 100*reason_2_w_frac, 100*reason_3_w_frac, 100*reason_4_w_frac, 100*reason_5_w_frac, 100*reason_6_w_frac, 100*reason_7_w_frac),1)))
# sort in descending order
dim(reason_spell.df)
names(reason_spell.df)
(reason_spell_sort.df = reason_spell.df[order(-reason_spell.df$Percentage_of_unbanked), ])
#
#write.csv(reason.df, "figure3.csv", row.names = F)
# End reasons for unbanked ####

# Start Table 2(top) pay methods ####
### Start Table 2: fraction of payment methods by number (volume) (banked vs unbanked
# using the transaction data for the first time.
dim(t1.df)
table(i5.df$banked)
table(i5.df$unbanked)
# splitting i5.df into banked and unbanked data frames
banked.df = subset(i5.df, banked==1)
nrow(banked.df)
length(unique(banked.df$id))
#
unbanked.df = subset(i5.df, unbanked==1)
nrow(unbanked.df)
length(unique(unbanked.df$id))

# merging indiv data into trans data
dim(banked.df)
names(banked.df) #=> missing ind_weight => define banked2.df
banked2.df = subset(i5.df, select = c(id, banked, ind_weight_all))
dim(banked2.df)
banked2.df = data.frame(banked2.df)
head(banked2.df)

# remove transactions made in 9/29, 9/30, 11/1, 11/2 (otherwise, average monthly num of payments will be underestimated)
table(t1.df$date)
length(unique(t1.df$id))
t2.df = subset(t1.df, date >= "2022-10-01" & date <= "2022-10-31")
table(t2.df$date)
length(unique(t2.df$id))
length(unique(t2.df$id))-length(unique(t1.df$id))# num resp lost
nrow(t2.df) - nrow(t1.df)# num trans lost
#
length(unique(banked2.df$id))

#merge the individual df into transactions df, include only ID who are in the transactions df.
m1.df = merge(t2.df, banked2.df, by = "id", all.x = T, all.y = F)
dim(m1.df)
length(unique(m1.df$id))
length(unique(subset(m1.df, banked==1)$id))# num banked
length(unique(subset(m1.df, banked==0)$id))# num unbanked

# # remove transactions made in 9/29, 9/30, 11/1, 11/2
# nrow(m1.df)# num trans including September and November
# table(m1.df$date)
# #
# m2.df = subset(m1.df, date >= "2022-10-01" & date <= "2022-10-31")
# m2.df = subset(m1.df, date >= "2022-10-01")
m2.df = m1.df
table(m2.df$date)

nrow(subset(m2.df, payment==1))# num of payments
table(m2.df$payment==1)
nrow(subset(m2.df, payment==1 & bill==1)) # num bill payments

# PI used [we use pi= 1:8]
table(m2.df$pi)
100*(prop.table(table(m2.df$pi)))

#0 - Multiple payment methods
#1 - Cash
#2 - Check
#3 - Credit card
#4 - Debit card
#5 - Prepaid/gift/EBT card
#6 - Bank account number payment
#7 - Online banking bill payment
#8 - Money order
#9 - Traveler's check
#10 - PayPal
#11 - Account-to-account transfer

# NOTE: I keep all PI although I am using only pi=1:7. This is to keep the number of respondents who did not make any payment. 

# rescaling weights
names(dplyr::select(m2.df, contains("weight")))# not clear why it does not work
nrow(m2.df[is.na(m2.df$ind_weight_all), ])#=> 1 ID does not have weight (6 payments)
nrow(banked2.df[is.na(banked2.df$ind_weight_all), ])# => this ID is in the transaction data but not in the individual data, that's why it does not have weight.
sum(m2.df$ind_weight_all)# NA because of missing 1 weight
sum(m2.df$ind_weight_all, na.rm = T)
nrow(m2.df)
m3.df = m2.df[!is.na(m2.df$ind_weight_all), ]
nrow(m3.df)
m3.df$t_weight = nrow(m3.df)* m3.df$ind_weight_all/sum(m3.df$ind_weight_all)
sum(m3.df$t_weight)
nrow(m3.df)

# Fraction of cash payments in 8 PI payments (banked vs unbanked)
(cash_banked_frac_w = sum(subset(m3.df, banked==1 & pi==1)$t_weight)/sum(subset(m3.df, banked==1 & pi %in% 1:8)$t_weight))
#
(cash_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==1)$t_weight)/sum(subset(m3.df, banked==0 & pi %in% 1:8)$t_weight))
#
# Fraction of check payments in 8 PI payments (banked vs unbanked)
(check_banked_frac_w = sum(subset(m3.df, banked==1 & pi==2)$t_weight)/sum(subset(m3.df, banked==1 & pi %in% 1:8)$t_weight))
#
(check_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==2)$t_weight)/sum(subset(m3.df, banked==0 & pi %in% 1:8)$t_weight))
#
# Fraction of credit card payments in 8 PI payments (banked vs unbanked)
(credit_banked_frac_w = sum(subset(m3.df, banked==1 & pi==3)$t_weight)/sum(subset(m3.df, banked==1 & pi %in% 1:8)$t_weight))
#
(credit_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==3)$t_weight)/sum(subset(m3.df, banked==0 & pi %in% 1:8)$t_weight))
#
# Fraction of debit card payments in 8 PI payments (banked vs unbanked)
(debit_banked_frac_w = sum(subset(m3.df, banked==1 & pi==4)$t_weight)/sum(subset(m3.df, banked==1 & pi %in% 1:8)$t_weight))
#
(debit_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==4)$t_weight)/sum(subset(m3.df, banked==0 & pi %in% 1:8)$t_weight))
#
# Fraction of prepaid card payments in 8 PI payments (banked vs unbanked)
(prepaid_banked_frac_w = sum(subset(m3.df, banked==1 & pi==5)$t_weight)/sum(subset(m3.df, banked==1 & pi %in% 1:8)$t_weight))
#
(prepaid_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==5)$t_weight)/sum(subset(m3.df, banked==0 & pi %in% 1:8)$t_weight))
#
# Fraction of BANP payments in 8 PI payments (banked vs unbanked)
(banp_banked_frac_w = sum(subset(m3.df, banked==1 & pi==6)$t_weight)/sum(subset(m3.df, banked==1 & pi %in% 1:8)$t_weight))
#
(banp_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==6)$t_weight)/sum(subset(m3.df, banked==0 & pi %in% 1:8)$t_weight))
#
# Fraction of OBBP payments in 8 PI payments (banked vs unbanked)
(obbp_banked_frac_w = sum(subset(m3.df, banked==1 & pi==7)$t_weight)/sum(subset(m3.df, banked==1 & pi %in% 1:8)$t_weight))
#
(obbp_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==7)$t_weight)/sum(subset(m3.df, banked==0 & pi %in% 1:8)$t_weight))
#
# Fraction of money order payments in 8 PI payments (banked vs unbanked)
(money_order_banked_frac_w = sum(subset(m3.df, banked==1 & pi==8)$t_weight)/sum(subset(m3.df, banked==1 & pi %in% 1:8)$t_weight))
#
(money_order_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==8)$t_weight)/sum(subset(m3.df, banked==0 & pi %in% 1:8)$t_weight))

# summarizing 
(pi_banked_frac.vec = c(cash_banked_frac_w, check_banked_frac_w, credit_banked_frac_w, debit_banked_frac_w, prepaid_banked_frac_w, banp_banked_frac_w, obbp_banked_frac_w, money_order_banked_frac_w))
#
(pi_unbanked_frac.vec = c(cash_unbanked_frac_w, check_unbanked_frac_w, credit_unbanked_frac_w, debit_unbanked_frac_w, prepaid_unbanked_frac_w, banp_unbanked_frac_w, obbp_unbanked_frac_w, money_order_unbanked_frac_w))
#
(pi_var.vec = c("Cash (%)", "Check (%)", "Credit card (%)", "Debit card (%)", "Prepaid/gift/EBT card (%)", "Bank account number payment (%)", "Online banking bill payment (%)", "Money order (%)"))

# payments by number (volume)  (banked vs unbanked, part of Table 2)
(pi.df = data.frame(Payment_method = pi_var.vec, Banked = 100*pi_banked_frac.vec, Unbanked = 100* pi_unbanked_frac.vec))
# verify columns sum up to 1
sum(pi.df$Banked)
sum(pi.df$Unbanked)

#write.csv(pi.df, "hub_fig_2.csv", row.names = F)

### Start fraction of payment method by value (banked vs unbanked) Second part of Table 2

(total_amount_banked_w =sum(subset(m3.df, banked==1 & pi %in% 1:8)$t_weight*subset(m3.df, banked==1 & pi %in% 1:8)$amnt))
#
(total_amount_unbanked_w =sum(subset(m3.df, banked==0 & pi %in% 1:8)$t_weight*subset(m3.df, banked==0 & pi %in% 1:8)$amnt))

(cash_amount_banked_frac_w = sum(subset(m3.df, banked==1 & pi==1)$t_weight * subset(m3.df, banked==1 & pi==1)$amnt)/total_amount_banked_w)
#
(cash_amount_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==1)$t_weight * subset(m3.df, banked==0 & pi==1)$amnt)/total_amount_unbanked_w)
#
(check_amount_banked_frac_w = sum(subset(m3.df, banked==1 & pi==2)$t_weight * subset(m3.df, banked==1 & pi==2)$amnt)/total_amount_banked_w)
#
(check_amount_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==2)$t_weight * subset(m3.df, banked==0 & pi==2)$amnt)/total_amount_unbanked_w)
#
(credit_amount_banked_frac_w = sum(subset(m3.df, banked==1 & pi==3)$t_weight * subset(m3.df, banked==1 & pi==3)$amnt)/total_amount_banked_w)
#
(credit_amount_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==3)$t_weight * subset(m3.df, banked==0 & pi==3)$amnt)/total_amount_unbanked_w)
#
(debit_amount_banked_frac_w = sum(subset(m3.df, banked==1 & pi==4)$t_weight * subset(m3.df, banked==1 & pi==4)$amnt)/total_amount_banked_w)
#
(debit_amount_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==4)$t_weight * subset(m3.df, banked==0 & pi==4)$amnt)/total_amount_unbanked_w)
#
(prepaid_amount_banked_frac_w = sum(subset(m3.df, banked==1 & pi==5)$t_weight * subset(m3.df, banked==1 & pi==5)$amnt)/total_amount_banked_w)
#
(prepaid_amount_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==5)$t_weight * subset(m3.df, banked==0 & pi==5)$amnt)/total_amount_unbanked_w)
#
(banp_amount_banked_frac_w = sum(subset(m3.df, banked==1 & pi==6)$t_weight * subset(m3.df, banked==1 & pi==6)$amnt)/total_amount_banked_w)
#
(banp_amount_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==6)$t_weight * subset(m3.df, banked==0 & pi==6)$amnt)/total_amount_unbanked_w)
#
(obbp_amount_banked_frac_w = sum(subset(m3.df, banked==1 & pi==7)$t_weight * subset(m3.df, banked==1 & pi==7)$amnt)/total_amount_banked_w)
#
(obbp_amount_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==7)$t_weight * subset(m3.df, banked==0 & pi==7)$amnt)/total_amount_unbanked_w)
#
(money_order_amount_banked_frac_w = sum(subset(m3.df, banked==1 & pi==8)$t_weight * subset(m3.df, banked==1 & pi==8)$amnt)/total_amount_banked_w)
#
(money_order_amount_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==8)$t_weight * subset(m3.df, banked==0 & pi==8)$amnt)/total_amount_unbanked_w)

# finalizing fraction of payment method by value (banked vs unbanked)
(pi_amount_banked_frac.vec = c(cash_amount_banked_frac_w, check_amount_banked_frac_w, credit_amount_banked_frac_w, debit_amount_banked_frac_w, prepaid_amount_banked_frac_w, banp_amount_banked_frac_w, obbp_amount_banked_frac_w, money_order_amount_banked_frac_w))
#
(pi_amount_unbanked_frac.vec = c(cash_amount_unbanked_frac_w, check_amount_unbanked_frac_w, credit_amount_unbanked_frac_w, debit_amount_unbanked_frac_w, prepaid_amount_unbanked_frac_w, banp_amount_unbanked_frac_w, obbp_amount_unbanked_frac_w, money_order_amount_unbanked_frac_w))
#
(pi_amount_var.vec = c("Cash (%)", "Check (%)", "Credit card (%)", "Debit card (%)", "Prepaid/gift/EBT card (%)", "Bank account number payment (%)", "Online banking bill payment (%)", "Money order (%)"))

(pi_amount.df = data.frame(Payment_method = pi_amount_var.vec, Banked = 100*pi_amount_banked_frac.vec, Unbanked = 100* pi_amount_unbanked_frac.vec))
# verify columns sum up to 1
sum(pi_amount.df$Banked)
sum(pi_amount.df$Unbanked)
#write.csv(pi_amount.df, "hub_fig_3.csv", row.names = F)

## Combining fraction of payment method by number and by value (banked vs. unbanked). Table 2 in paper
dim(pi.df)
dim(pi_amount.df)
#
(pi_vol_val.df = cbind(pi.df, pi_amount.df[,-1]))
#
dim(pi_vol_val.df)
print(xtable(pi_vol_val.df, digits = 1), include.rownames = F, hline.after = c(0))

#write.csv(pi_vol_val.df, "pi_vol_val.csv", row.names = F)

# End Table 2(top) pay methods ####

# Start Table 2(bottom) pay methods bills ####
# Start table 3 (bills by number and value): Table 3 in paper
# construct new variable: weighted*amount
nrow(m3.df)
sum(m3.df$t_weight)
m3.df$weighted_amnt = m3.df$t_weight * m3.df$amnt# new variable
sum(m3.df$amnt)
sum(m3.df$weighted_amt)
#
(cash_bill_banked_frac_w = sum(subset(m3.df, banked==1 & pi==1 & bill==1)$t_weight)/sum(subset(m3.df, banked==1 & pi %in% 1:8 & bill==1)$t_weight))
#
(cash_bill_val_banked_frac_w = sum(subset(m3.df, banked==1 & pi==1 & bill==1)$weighted_amnt)/sum(subset(m3.df, banked==1 & pi %in% 1:8 & bill==1)$weighted_amnt))
#
(cash_bill_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==1 & bill==1)$t_weight)/sum(subset(m3.df, banked==0 & pi %in% 1:8 & bill==1)$t_weight))
#
(cash_bill_val_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==1 & bill==1)$weighted_amnt)/sum(subset(m3.df, banked==0 & pi %in% 1:8 & bill==1)$weighted_amnt))
#
(check_bill_banked_frac_w = sum(subset(m3.df, banked==1 & pi==2 & bill==1)$t_weight)/sum(subset(m3.df, banked==1 & pi %in% 1:8 & bill==1)$t_weight))
#
(check_bill_val_banked_frac_w = sum(subset(m3.df, banked==1 & pi==2 & bill==1)$weighted_amnt)/sum(subset(m3.df, banked==1 & pi %in% 1:8 & bill==1)$weighted_amnt))
#
(check_bill_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==2 & bill==1)$t_weight)/sum(subset(m3.df, banked==0 & pi %in% 1:8 & bill==1)$t_weight))
#
(check_bill_val_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==2 & bill==1)$weighted_amnt)/sum(subset(m3.df, banked==0 & pi %in% 1:8 & bill==1)$weighted_amnt))
#
(credit_bill_banked_frac_w = sum(subset(m3.df, banked==1 & pi==3 & bill==1)$t_weight)/sum(subset(m3.df, banked==1 & pi %in% 1:8 & bill==1)$t_weight))
#
(credit_bill_val_banked_frac_w = sum(subset(m3.df, banked==1 & pi==3 & bill==1)$weighted_amnt)/sum(subset(m3.df, banked==1 & pi %in% 1:8 & bill==1)$weighted_amnt))
#
(credit_bill_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==3 & bill==1)$t_weight)/sum(subset(m3.df, banked==0 & pi %in% 1:8 & bill==1)$t_weight))
#
(credit_bill_val_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==3 & bill==1)$weighted_amnt)/sum(subset(m3.df, banked==0 & pi %in% 1:8 & bill==1)$weighted_amnt))
#
(debit_bill_banked_frac_w = sum(subset(m3.df, banked==1 & pi==4 & bill==1)$t_weight)/sum(subset(m3.df, banked==1 & pi %in% 1:8 & bill==1)$t_weight))
#
(debit_bill_val_banked_frac_w = sum(subset(m3.df, banked==1 & pi==4 & bill==1)$weighted_amnt)/sum(subset(m3.df, banked==1 & pi %in% 1:8 & bill==1)$weighted_amnt))
#
(debit_bill_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==4 & bill==1)$t_weight)/sum(subset(m3.df, banked==0 & pi %in% 1:8 & bill==1)$t_weight))
#
(debit_bill_val_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==4 & bill==1)$weighted_amnt)/sum(subset(m3.df, banked==0 & pi %in% 1:8 & bill==1)$weighted_amnt))
#
(prepaid_bill_banked_frac_w = sum(subset(m3.df, banked==1 & pi==5 & bill==1)$t_weight)/sum(subset(m3.df, banked==1 & pi %in% 1:8 & bill==1)$t_weight))
#
(prepaid_bill_val_banked_frac_w = sum(subset(m3.df, banked==1 & pi==5 & bill==1)$weighted_amnt)/sum(subset(m3.df, banked==1 & pi %in% 1:8 & bill==1)$weighted_amnt))
#
(prepaid_bill_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==5 & bill==1)$t_weight)/sum(subset(m3.df, banked==0 & pi %in% 1:8 & bill==1)$t_weight))
#
(prepaid_bill_val_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==5 & bill==1)$weighted_amnt)/sum(subset(m3.df, banked==0 & pi %in% 1:8 & bill==1)$weighted_amnt))
#
(banp_bill_banked_frac_w = sum(subset(m3.df, banked==1 & pi==6 & bill==1)$t_weight)/sum(subset(m3.df, banked==1 & pi %in% 1:8 & bill==1)$t_weight))
#
(banp_bill_val_banked_frac_w = sum(subset(m3.df, banked==1 & pi==6 & bill==1)$weighted_amnt)/sum(subset(m3.df, banked==1 & pi %in% 1:8 & bill==1)$weighted_amnt))
#
(banp_bill_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==6 & bill==1)$t_weight)/sum(subset(m3.df, banked==0 & pi %in% 1:8 & bill==1)$t_weight))
#
(banp_bill_val_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==6 & bill==1)$weighted_amnt)/sum(subset(m3.df, banked==0 & pi %in% 1:8 & bill==1)$weighted_amnt))
#
(obbp_bill_banked_frac_w = sum(subset(m3.df, banked==1 & pi==7 & bill==1)$t_weight)/sum(subset(m3.df, banked==1 & pi %in% 1:8 & bill==1)$t_weight))
#
(obbp_bill_val_banked_frac_w = sum(subset(m3.df, banked==1 & pi==7 & bill==1)$weighted_amnt)/sum(subset(m3.df, banked==1 & pi %in% 1:8 & bill==1)$weighted_amnt))
#
(obbp_bill_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==7 & bill==1)$t_weight)/sum(subset(m3.df, banked==0 & pi %in% 1:8 & bill==1)$t_weight))
#
(obbp_bill_val_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==7 & bill==1)$weighted_amnt)/sum(subset(m3.df, banked==0 & pi %in% 1:8 & bill==1)$weighted_amnt))
#
(money_order_bill_banked_frac_w = sum(subset(m3.df, banked==1 & pi==8 & bill==1)$t_weight)/sum(subset(m3.df, banked==1 & pi %in% 1:8 & bill==1)$t_weight))
#
(money_order_bill_val_banked_frac_w = sum(subset(m3.df, banked==1 & pi==8 & bill==1)$weighted_amnt)/sum(subset(m3.df, banked==1 & pi %in% 1:8 & bill==1)$weighted_amnt))
#
(money_order_bill_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==8 & bill==1)$t_weight)/sum(subset(m3.df, banked==0 & pi %in% 1:8 & bill==1)$t_weight))
#
(money_order_bill_val_unbanked_frac_w = sum(subset(m3.df, banked==0 & pi==8 & bill==1)$weighted_amnt)/sum(subset(m3.df, banked==0 & pi %in% 1:8 & bill==1)$weighted_amnt))

# finalizing bills by number
(pi_bill_num_banked_frac.vec = c(cash_bill_banked_frac_w, check_bill_banked_frac_w, credit_bill_banked_frac_w, debit_bill_banked_frac_w, prepaid_bill_banked_frac_w, banp_bill_banked_frac_w, obbp_bill_banked_frac_w, money_order_bill_banked_frac_w))
# banked by value
(pi_bill_val_banked_frac.vec = c(cash_bill_val_banked_frac_w, check_bill_val_banked_frac_w, credit_bill_val_banked_frac_w, debit_bill_val_banked_frac_w, prepaid_bill_val_banked_frac_w, banp_bill_val_banked_frac_w, obbp_bill_val_banked_frac_w, money_order_bill_val_banked_frac_w))
#
(pi_bill_num_unbanked_frac.vec = c(cash_bill_unbanked_frac_w, check_bill_unbanked_frac_w, credit_bill_unbanked_frac_w, debit_bill_unbanked_frac_w, prepaid_bill_unbanked_frac_w, banp_bill_unbanked_frac_w, obbp_bill_unbanked_frac_w, money_order_bill_unbanked_frac_w))
# unbanked by value
(pi_bill_val_unbanked_frac.vec = c(cash_bill_val_unbanked_frac_w, check_bill_val_unbanked_frac_w, credit_bill_val_unbanked_frac_w, debit_bill_val_unbanked_frac_w, prepaid_bill_val_unbanked_frac_w, banp_bill_val_unbanked_frac_w, obbp_bill_val_unbanked_frac_w, money_order_bill_val_unbanked_frac_w))
#
(pi_bill_var.vec = c("Cash (%)", "Check (%)", "Credit card (%)", "Debit card (%)", "Prepaid/gift/EBT card (%)", "Bank account number payment (%)", "Online banking bill payment (%)", "Money order (%)"))

(pi_bill.df = data.frame(Payment_method = pi_bill_var.vec, Banked_number = 100*pi_bill_num_banked_frac.vec, Unbanked_number = 100* pi_bill_num_unbanked_frac.vec, Banked_value = 100*pi_bill_val_banked_frac.vec, Unbanked_value = 100* pi_bill_val_unbanked_frac.vec))

# check sum up to 1
sum(pi_bill.df$Banked_number)
sum(pi_bill.df$Unbanked_number)
sum(pi_bill.df$Banked_value)
sum(pi_bill.df$Unbanked_value)

dim(pi_bill.df)# Bill payments, Table 3 in paper
print(xtable(pi_bill.df, digits = 1), include.rownames = F, hline.after = c(0))

#write.csv(pi_bill.df, "pi_bill.csv", row.names = F)
# End Table 2(bottom) pay methods bills ####


# Start cash regression ####
i7.df = subset(i6.df, select=c("id", "unbanked", "age_factor", "gender_factor", "race_factor", "ethnicity_factor", "edu_factor", "marital_factor", "income_factor", "work_factor", "homeowner_factor", "income_hh_div_10k"))
#
length(unique(i7.df$id))
dim(i7.df)
#
str(i7.df)
i7.df$unbanked = factor(i7.df$unbanked)
i7.df$income_factor = factor(i7.df$income_factor)
#
table(i7.df$unbanked)
table(i7.df$age_factor)
table(i7.df$gender_factor)
table(i7.df$race_factor)
table(i7.df$ethnicity_factor)
table(i7.df$edu_factor)
table(i7.df$marital_factor)
table(i7.df$income_factor)
table(i7.df$income_factor)
table(i7.df$work_factor)
table(i7.df$homeowner_factor)
summary(i7.df$income_hh_div_10k)

names(t2.df)
table(t2.df$date)
length(unique(t2.df$id))
dim(t2.df)

# merge the individual df into transactions df, include only ID who are in the transactions df. 
m10.df = merge(t2.df, i7.df, by = "id", all.x = T, all.y = F)
dim(m10.df)
length(unique(m10.df$id))
length(unique(subset(m10.df, unbanked==1)$id))
length(unique(subset(m10.df, unbanked==0)$id))

# remove resp with missing unbanked status
nrow(subset(m10.df, is.na(m10.df$unbanked)))
length(unique((subset(m10.df, is.na(m10.df$unbanked))$id)))
m11.df = subset(m10.df, !is.na(m10.df$unbanked))
dim(m11.df)
length(unique(m11.df$id))
length(unique(subset(m11.df, unbanked==1)$id))
length(unique(subset(m11.df, unbanked==0)$id))

# remove trans with missing pi
nrow(subset(m11.df, is.na(m11.df$pi)))
m12.df = (subset(m11.df, !is.na(m11.df$pi)))
length(unique(m12.df$id))
length(unique(subset(m12.df, unbanked==1)$id))
length(unique(subset(m12.df, unbanked==0)$id))
# frac of unbanked 
length(unique(subset(m12.df, unbanked==1)$id))/length(unique(m12.df$id))

# Reduce to 8 only payment methods
#0 - Multiple payment methods
#1 - Cash
#2 - Check
#3 - Credit card
#4 - Debit card
#5 - Prepaid/gift/EBT card
#6 - Bank account number payment
#7 - Online banking bill payment
#8 - Money order
#9 - Traveler's check
#10 - PayPal
#11 - Account-to-account transfer
table(m12.df$pi)
m13.df = subset(m12.df, pi %in% c(1:8))# restrict to 8 payment methods
table(m13.df$pi)
length(unique(m13.df$id))
length(unique(subset(m13.df, unbanked==1)$id))
length(unique(subset(m13.df, unbanked==0)$id))

# Construct a dependent variable "cash" =1 if paid cash, =0 non-cash payment
table(m13.df$pi, exclude = T)
m13.df$cash = ifelse(m13.df$pi==1, 1, 0)
table(m13.df$cash)
str(m13.df$cash)# leave it as numerical (although turning into a factor shouldn't matter)

# renaming variables for nicer regression display
m14.df = m13.df
table(m14.df$unbanked, exclude = T)
m14.df$unbanked_factor = factor(ifelse(m14.df$unbanked==1, "Unbanked_yes", "Unbanked_no"))
table(m14.df$unbanked_factor)

table(m14.df$bill)# is it a bill payment?
m14.df$bill_factor = factor(ifelse(m14.df$bill==1, "Bill_yes", "Bill_no"))
table(m14.df$unbanked_factor)

#
table(m14.df$edu_factor)
m14.df$edu_factor = factor(m14.df$edu_factor, levels = c("Less_than_high_school", "High_school", "Some_college_or_associate", "College_and_higher"))
table(m14.df$edu_factor)
m14.df$edu_factor = relevel(m14.df$edu_factor, "College_and_higher")

# Construct amnt_factor variable to indicate thresholds of payment amounts
m15.df = m14.df
nrow(subset(m15.df, is.na(amnt)))# verify no missing amnt
hist(subset(m15.df, amnt<=100 & pi==1)$amnt)
m15.df$amnt_factor = NA
#
m15.df$amnt_factor =  factor(case_when(m15.df$amnt <= 10 ~ "Amount_0_10",
  m15.df$amnt > 10 & m15.df$amnt <= 20 ~ "Amount_10_20",
  m15.df$amnt > 20 & m15.df$amnt <= 40 ~ "Amount_20_40",
  m15.df$amnt > 40 & m15.df$amnt <= 100 ~ "Amount_40_100",
  m15.df$amnt > 100 ~ "Amount_more_than_100"))
#
table(m15.df$amnt_factor, exclude = T)
table(m15.df$amnt_factor, m15.df$cash)
#
levels(m15.df$amnt_factor)
m15.df$amnt_factor = relevel(m15.df$amnt_factor, ref = "Amount_more_than_100")
levels(m15.df$amnt_factor)
table(m15.df$amnt_factor, m15.df$cash)

# # Restricting to in-person payments only (see regression below that include "in-person") [not used in the paper, see below]
# table(m15.df$in_person, exclude = T)
# length(unique(m15.df$id))
# length(unique(subset(m15.df, unbanked=="Unbanked_yes")$id))
# #
# m16.df = subset(m15.df, in_person==1)
# dim(m16.df)
# length(unique(m16.df$id))
# length(unique(subset(m16.df, unbanked=="Unbanked_yes")$id))
# # fraction of unbanked
# length(unique(subset(m16.df, unbanked=="Unbanked_yes")$id))/length(unique(m16.df$id))
# 
# # setting reference variable for income_factor
# table(m16.df$income_factor, exclude = T)
# m16.df$income_factor = relevel(m16.df$income_factor, "HH_income_90k_and_higher")
# 
# # cash regression model 1 (full regression)
# (cash_model1 = formula(cash~ unbanked + amnt_factor + age_factor + gender_factor +  race_factor + ethnicity_factor + edu_factor + marital_factor + income_factor + work_factor + homeowner_factor))
# #
# (cash_reg_output1 = probitmfx(cash_model1, data=m16.df))
# 
# # cash regression model 2 (unbanked removed)
# (cash_model2 = formula(cash~ amnt_factor + age_factor + gender_factor +  race_factor + ethnicity_factor + edu_factor + marital_factor + income_factor + work_factor + homeowner_factor))
# #
# (cash_reg_output2 = probitmfx(cash_model2, data=m16.df))
# 
# # cash regression model 3 (unbanked, in-person, income_factor => no demographics)
# (cash_model3 = formula(cash~ unbanked + amnt_factor ))
# #
# (cash_reg_output3 = probitmfx(cash_model3, data=m16.df))
# #
# texreg(list(cash_reg_output1, cash_reg_output2, cash_reg_output3))
# 
# # Display 3 regressions in LaTeX
# texreg(list(cash_reg_output1, cash_reg_output2, cash_reg_output3))
# 

# WITHOUT restricting to in-person payments only (see regression above restricted "in-person")
m17.df = m15.df# recall, m16.df is restricted to in-person only
table(m17.df$in_person, exclude = T)
# remove missing in_person payments <=> not needed, the regression will do that
#m18.df = subset(m17.df, !is.na(m17.df$in_person))
#table(m18.df$in_person, exclude = T)
m18.df = m17.df
str(m18.df$in_person)
m18.df$in_person_factor = factor(ifelse(m18.df$in_person==1, "In_person_yes", "In_person_no"))
table(m18.df$in_person_factor, exclude = T)
#
length(unique(m18.df$id))
length(unique(subset(m18.df, unbanked_factor=="Unbanked_yes")$id))
#correlation between unbanked and in-person
cor(as.numeric(m18.df$unbanked), as.numeric(m14.df$in_person),  use = "complete.obs")

# setting reference variable for income_factor
table(m18.df$income_factor, exclude = T)
m18.df$income_factor = relevel(m18.df$income_factor, "HH_income_90k_and_higher")
#
# m16.df = subset(m15.df, in_person==1)
# dim(m16.df)
# length(unique(m16.df$id))
# length(unique(subset(m16.df, unbanked=="Unbanked_yes")$id))
# # fraction of unbanked
# length(unique(subset(m16.df, unbanked=="Unbanked_yes")$id))/length(unique(m16.df$id))

# cash regression model 1 (full regression)
(cash_model1 = formula(cash~ unbanked_factor + in_person_factor + amnt_factor + bill_factor + age_factor + gender_factor +  race_factor + ethnicity_factor + edu_factor + marital_factor + income_factor + work_factor + homeowner_factor))
#
(cash_reg_output1 = probitmfx(cash_model1, data=m18.df))

# cash regression model 2 (unbanked removed)
(cash_model2 = formula(cash~ in_person_factor +amnt_factor + bill_factor + age_factor + gender_factor +  race_factor + ethnicity_factor + edu_factor + marital_factor + income_factor + work_factor + homeowner_factor))
#
(cash_reg_output2 = probitmfx(cash_model2, data=m18.df))

# cash regression model 3 (unbanked, in-person, income_factor => no demographics)
(cash_model3 = formula(cash~ unbanked_factor + in_person_factor + amnt_factor + bill_factor ))
#
(cash_reg_output3 = probitmfx(cash_model3, data=m18.df))

texreg(list(cash_reg_output1, cash_reg_output2, cash_reg_output3), single.row = T)

# Misc calculations ####
#0 - Multiple payment methods
#1 - Cash
#2 - Check
#3 - Credit card
#4 - Debit card
#5 - Prepaid/gift/EBT card
#6 - Bank account number payment
#7 - Online banking bill payment
#8 - Money order

#Num payments by unbanked
nrow(subset(m3.df, banked==0 & pi %in% 1:8))
#
#out of these num bill payments by unbanked
nrow(subset(m3.df, banked==0 & pi %in% 1:8 & bill==1))
#
# Dist of payment methods for all unbanked payments
table(subset(m3.df, banked==0 & pi %in% 1:8)$pi)
# Dist of payment methods for bills paid by unbanked
table(subset(m3.df, banked==0 & pi %in% 1:8 & bill==1)$pi)


# End cash regression ####
# End of code for the paper J Econ & Fin ####


