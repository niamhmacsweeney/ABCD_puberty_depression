#title: "PREP_PDS"
#author: "Niamh MacSweeney"
#date: "19/07/2021"

#### INTRODUCTION
#The purpose of this script is to:
#tidy and score the pubertal development scale (PDS) data (caregiver report)
#to prepare a final, clean, sample for use in models. 


#### SETUP
#Load libraries needed and set working directory.

library(tidyr)
library(dplyr)
library(tidyverse)
library(psych)
library(ggplot2)
library(ggstatsplot)
library(GGally)
library(stringr)
library(outliers)
library(jtools) #great package for summarising regression results
library(readr)
library(gridExtra)
setwd("/Volumes/GenScotDepression/data/abcd/release2.0.1/iii.data/Physical_Health/")
getwd()

####LOAD DATA
# Load PDS untidied data. Use list.files() to list all files in wd.
df <- readRDS("abcd_ppdms01.rds")

#check import and preview data
head(df, n=10) #first few observations
str(df)  #types of variables
names(df) #List variables in dataframe

#examine data format by group=sex
describeBy(df, group=df$pubertal_sex_p) 


#reduce to baseline data only, N=11,875
df <- subset(df,eventname=="baseline_year_1_arm_1")


#### CLEAN DATA
#STEP 1: Column formatting

#Convert age in months to years, save new variable
pds_cg <- df %>%
  mutate(
    age_years = interview_age/12 
  )
colnames(pds_cg) #check new variable exists

#Change column names to avoid confusion: "gender" is alias for "sex of subject" Responses: Male, Female, Other, Not Reported.
pds_cg <- pds_cg %>% 
  rename(
    sex_of_subject = gender) #change  "gender" to "sex_of_subject"

#STEP 2: Check missing values, mismatch between sex of subject and pubertal sex.

#Check number of participants by sex_grouping
pds_cg %>% 
  dplyr::count(sex_of_subject) # "" = 6

#Remove 6 people with blank (i.e.,"") data for sex_of_subject
pds_cg <-  pds_cg[!(is.na(pds_cg$sex_of_subject) | pds_cg$sex_of_subject==""), ] # displays all rows, which are not NA or blank values 
pds_cg %>% dplyr::count(sex_of_subject) #check that they have been removed
  
# Check for missing values for pubertal sex and remove
pds_cg %>%
  dplyr::count(pubertal_sex_p) # NA = 5
pds_cg <-  pds_cg[!(is.na(pds_cg$pubertal_sex_p) | pds_cg$pubertal_sex_p=="NA"), ] # displays all rows, which are not NA or blank values 
pds_cg %>% dplyr::count(pubertal_sex_p) #check that they have been removed

#Check mismatches between sex and pubertal sex
table(pds_cg$sex_of_subject,pds_cg$pubertal_sex_p) #mismatches = 63

#Remove mismatches
#Note: for pubertal sex, 1=Male 2=Female
pds_cg <- pds_cg[-c(which(pds_cg$sex_of_subject == "M" & pds_cg$pubertal_sex_p == 2), 
which(pds_cg$sex_of_subject == "F" & pds_cg$pubertal_sex_p == 1)),]

#Re-run descriptives to check it worked
table(pds_cg$sex_of_subject,pds_cg$pubertal_sex_p) # M = 6142, F = 5659, Total N = 11,801

#Check that males have answered male items, females answered female items
#Note: pds_m5 and pds_m4 mean should equal 0 for females, and pds_f4 and pds_f5b mean should equal 0 for males. 
describeBy(pds_cg, group=pds_cg$sex_of_subject)

#Note: NDAR_INV5D3CU8ML has male sex_of_subject but female answers -- we will remove from dataset
c(which(pds_cg$sex_of_subject == "M" & !is.na(pds_cg$pds_f4_p)))
pds_cg=pds_cg[-c(which(pds_cg$sex_of_subject == "M" & !is.na(pds_cg$pds_f4_p))),]

#Final sample for PDS caregiver report: MALES = 6141, FEMALES = 5659, TOTAL N = 11800
xtabs(~pds_cg$sex_of_subject+pds_cg$pubertal_sex_p)


#STEP 3: Prepare data for scoring - check cleaning worked and remove "I don't know = "999" values.

#What do caregivers report? Spanish vs. English responses per sex. 
pds_cg %>%
  dplyr::count(pds_select_language___1, pubertal_sex_p)

#Double check tables for PDS variables. 
#Note: there are NO female responses for male items, and NO male responses for female items -----> CLEANING WORKED
#lapply - could use here... 

table(pds_cg$pds_1_p,pds_cg$pubertal_sex_p)
table(pds_cg$pds_2_p,pds_cg$pubertal_sex_p)
table(pds_cg$pds_3_p,pds_cg$pubertal_sex_p)
table(pds_cg$pds_m5_p,pds_cg$pubertal_sex_p)
table(pds_cg$pds_m4_p,pds_cg$pubertal_sex_p)
table(pds_cg$pds_f4_p,pds_cg$pubertal_sex_p)
table(pds_cg$pds_f5b_p,pds_cg$pubertal_sex_p)
table(pds_cg$pds_f6_p,pds_cg$pubertal_sex_p) #see age of menarche out of interest- not used in PDS total score. 

#PDS Recode: Change "999 = I don't know" to NA for these variables only as it is not numerically meaningful. 
pds_cg$pds_1_p[pds_cg$pds_1_p== 999] <- NA
pds_cg$pds_2_p[pds_cg$pds_2_p== 999] <- NA
pds_cg$pds_3_p[pds_cg$pds_3_p== 999] <- NA
pds_cg$pds_m5_p[pds_cg$pds_m5_p== 999] <- NA
pds_cg$pds_m4_p[pds_cg$pds_m4_p== 999] <- NA
pds_cg$pds_f4_p[pds_cg$pds_f4_p== 999] <- NA
pds_cg$pds_f5b_p[pds_cg$pds_f5b_p== 999] <- NA

#Double check tables again to see 999 has been changed to NA. 
table(pds_cg$pds_1_p,pds_cg$pubertal_sex_p)
table(pds_cg$pds_2_p,pds_cg$pubertal_sex_p)
table(pds_cg$pds_3_p,pds_cg$pubertal_sex_p)
table(pds_cg$pds_m5_p,pds_cg$pubertal_sex_p)
table(pds_cg$pds_m4_p,pds_cg$pubertal_sex_p)
table(pds_cg$pds_f4_p,pds_cg$pubertal_sex_p)
table(pds_cg$pds_f5b_p,pds_cg$pubertal_sex_p)


##EXAMINE DATA

#STEP 1: Examine correlation of raw data for PDS caregiver report
#Split df by sex and examine valid PDS variables for each.
pds_cg_m <- filter(pds_cg, pubertal_sex_p== 1)
pds_cg_f <- filter(pds_cg, pubertal_sex_p== 2)

#Group male PDS correlations with age
keeps_pds_cg_m=c("pds_1_p", "pds_2_p", "pds_3_p", "pds_m5_p", "pds_m4_p", "age_years")
df_m_all_corr =ggpairs(pds_cg_m, keeps_pds_cg_m, title = "Within PDS Males")
df_m_all_corr

#Group female PDS correlations with age
keeps_pds_cg_f=c("pds_1_p", "pds_2_p", "pds_3_p", "pds_f4_p", "pds_f5b_p", "age_years")
df_f_all_corr =ggpairs(pds_cg_f, keeps_pds_cg_f, title = "Within PDS Females")
df_f_all_corr

#STEP 2: GENERATE PDS TOTAL SCORE (5 OUT OF 5 VARIABLES NEEDED)

#Sum PDS scores across rows for males. Then change female values for this field to NA
pds_cg$pds_tot_m <-rowSums(pds_cg[,c("pds_1_p", "pds_2_p", "pds_3_p", "pds_m5_p", "pds_m4_p")], na.rm=T)
pds_cg$pds_tot_m[pds_cg$pds_tot_m==0] <-NA
pds_cg$pds_tot_m[pds_cg$pubertal_sex_p==2] <- NA

#Sum PDS scores across rows for females. Then change male values for this field to NA
pds_cg$pds_tot_f <-rowSums(pds_cg[,c("pds_1_p", "pds_2_p", "pds_3_p", "pds_f4_p", "pds_f5b_p")], na.rm=T)
pds_cg$pds_tot_f[pds_cg$pds_tot_f==0] <-NA
pds_cg$pds_tot_f[pds_cg$pubertal_sex_p==1] <- NA

#Count number of Qs per subject in males that are not NA/DK
pds_cg$pds_m_Qcount_p <- 
rowSums(!is.na(pds_cg[,c("pds_1_p", "pds_2_p", "pds_3_p","pds_m5_p", "pds_m4_p")]))

#Change female values to NA for this Qcount_m col.
pds_cg$pds_m_Qcount_p[pds_cg$pubertal_sex_p==2] <- NA 

#Count number of Qs per subject in females that are not NA/DK
pds_cg$pds_f_Qcount_p <- 
rowSums(!is.na(pds_cg[,c("pds_1_p", "pds_2_p", "pds_3_p", "pds_f4_p", "pds_f5b_p")]))

#Change male values to NA for this Qcount_f col.
pds_cg$pds_f_Qcount_p[pds_cg$pubertal_sex_p==1] <- NA 

#This will give total number of NAs in males per pds column
apply(subset(pds_cg, pubertal_sex_p==1)[,c("pds_1_p", "pds_2_p", "pds_3_p","pds_m5_p", "pds_m4_p")],
      2, function(x) sum(is.na(x)))
#And total number of NAs in females per pds column
apply(subset(pds_cg, pubertal_sex_p==2)[,c("pds_1_p", "pds_2_p", "pds_3_p", "pds_f4_p", "pds_f5b_p")], 
      2, function(x) sum(is.na(x)))

#If any NAs for males. Then code to invalid.
pds_cg$pds_males_valid_for_pt <- NA
pds_cg$pds_males_valid_for_pt <- ifelse(pds_cg$pubertal_sex_p==1 
      & pds_cg$pds_m_Qcount_p==5,1,0)  #5 denotes the number of PDS variables needed, 1=valid, 0= invalid.

#If any NAs for females. Then code to invalid.
pds_cg$pds_females_valid_for_pt <- NA
pds_cg$pds_females_valid_for_pt <- ifelse(pds_cg$pubertal_sex_p==2 
      & pds_cg$pds_f_Qcount_p==5,1,0)  #5 denotes the number of PDS variables needed, 1=valid, 0= invalid. 

#Count who is valid based on sex
xtabs(~pds_cg$pds_males_valid_for_pt+pds_cg$pubertal_sex_p)
xtabs(~pds_cg$pds_females_valid_for_pt+pds_cg$pubertal_sex_p)

#Count who is valid in total
pds_cg %>% dplyr::count(pds_males_valid_for_pt)
pds_cg %>% dplyr::count(pds_females_valid_for_pt)


#STEP 3: SELECT VALID IDS AND CHECK FOR OUTLIERS BEFORE GENERATING PUBERTAL TIMING SCORE

#ONLY for valid PDS total calculate the following meaningful PDS total to use in linear model.  

#Calculate PDS total for males for only those with valid data (see definition of valid above)
pds_cg$pds_total_males_p <- pds_cg$pds_tot_m
pds_cg$pds_total_males_p[pds_cg$pds_males_valid_for_pt==0] <- NA

#Calculate PDS total for females for only those with valid data (see definition of valid above)
pds_cg$pds_total_females_p <- pds_cg$pds_tot_f
pds_cg$pds_total_females_p[pds_cg$pds_females_valid_for_pt==0] <- NA

#Create column for PDS total for males and females together
pds_cg <- pds_cg %>% 
  mutate(pds_tot_all = if_else(is.na(pds_tot_m), pds_tot_f, pds_tot_m))


#Generate new dataframe with complete cases for pubertal timing --- N=10,964
#This will allow us to save the linear model outputs as a new column. 

pds_timing <- pds_cg %>% select(src_subject_id, eventname, sex_of_subject , pubertal_sex_p, age_years,
                                pds_males_valid_for_pt , pds_females_valid_for_pt, 
                                pds_total_males_p, pds_total_females_p, pds_tot_all)

#Remove participants with invalid data for generating pubertal timing score. 
pds_timing <- pds_timing[-c(which(pds_timing$sex_of_subject == "M" & pds_timing$pds_males_valid_for_pt == 0), 
                    which(pds_timing$sex_of_subject == "F" & pds_timing$pds_females_valid_for_pt == 0)),]

####Inspect data

#distribution

raw_plot <- pds_timing %>%
  ggplot(aes(x = pds_tot_all)) +
  geom_histogram(color="black",fill="deepskyblue4") +
  facet_wrap(~ sex_of_subject, scales="free")
raw_plot


#Check for Outliers (±5 SD from mean)
#Note: We are defining outliers as ±5SD from mean as with a sample this large (N= ~10,000), 
# we should expect some responses greater than 3SD (commonly used outlier cutoff) if the data are really normally distributed. 
#As sensitivity analysis, we will run models with and without ouliers removed for PDS.
#this will be for males only as no outlier detected for females. 

#Males
outl_pds_m_upper <- (mean(pds_timing$pds_total_males_p, na.rm=T) + 5*(sd(pds_timing$pds_total_males_p, na.rm=T))) # = 16.45
outl_pds_m_lower <- (mean(pds_timing$pds_total_males_p, na.rm=T) - 5*(sd(pds_timing$pds_total_males_p, na.rm=T))) # = -2.06

#Plot outliers. Focus on +5SD from mean as negative values that are -5SD aren't valid here. 
hist(pds_timing$pds_total_males_p, main="", xlab="PDS TOTAL_MALES")
abline(v=outl_pds_m_upper,col="red")
abline(v=outl_pds_m_lower,col="red")


outl_pds_m <- pds_timing %>% 
  filter(pds_total_males_p > outl_pds_m_upper) #There are 10 outlier values

#create new variable with outliers removed for later sensitivity analysis
#duplicate pds_total_males and name is pds_tot_m_no_outl
pds_timing <- pds_timing %>% 
  mutate(pds_tot_m_no_outl = pds_total_males_p) 

#Change outlier values for pds_tot_m_no_outl to NA
#tried to do with dplyr syntax below and couldn't get working
pds_timing$pds_tot_m_no_outl[pds_timing$pds_tot_m_no_out > outl_pds_m_upper] <- NA

#dplyr syntax
#pds_timing <- pds_timing %>% 
  #mutate(pds_tot_m_no_outl = na_if(pds_tot_m_no_outl, pds_total_males_p > outl_pds_m_upper))

#check it worked - it did! 
outl_pds_m_check <- pds_timing %>% 
  filter(pds_tot_m_no_outl > outl_pds_m_upper)

#Females
outl_pds_f_upper <- (mean(pds_timing$pds_total_females_p, na.rm=T) + 5*(sd(pds_timing$pds_total_females_p, na.rm=T))) # = 21.87
outl_pds_f_lower <- (mean(pds_timing$pds_total_females_p, na.rm=T) - 5*(sd(pds_timing$pds_total_females_p, na.rm=T))) # = -4.13

#Plot outliers. Focus on +5SD from mean as negative values that are -5SD aren't valid here. 
hist(pds_timing$pds_total_females_p, main="", xlab="PDS TOTAL_FEMALES")
abline(v=outl_pds_f_upper,col="red")
abline(v=outl_pds_f_lower,col="red")

#There are no outlier values for females at threshold of 5SD. 


#another method using dplyr
#pds_out_m_tmp <- pds_timing %>% 
#select(pds_total_males_p) %>% 
#filter(mean(pds_total_males_p) + 5*(sd(pds_total_males_p)) | mean(pds_total_males_p) - 5*(sd(pds_total_males_p))) %>% 
# summarise_each(funs(mean(., na.rm = TRUE)))


#STEP 4: GENERATE PUBERTAL TIMING SCORE 
#PDS total score is regressed on age for males and females separately.
#standardised residual obtained is used as the pubertal timing score. 

#### Male pubertal timing linear model ####
#model summary
pt_m_lm <- lm(pds_total_males_p~age_years, na.action=na.exclude, data = pds_timing) # create linear model, and specify na.exclude so that df size stays the same.
summ(pt_m_lm,
     robust = TRUE, #robust standard error = HC3 as per sandwich package
     scale = TRUE,  #scaled beta used
     confint = TRUE, digits = 3) #get confidence intervals. 95% CI by default 

#obtain standardised residual = measure of pubertal timing. Save as new variable. 
#Note: the resid function in lm allows us to have the desired NA values for female rows. 

pds_timing$pt_m <- resid(pt_m_lm)
summary(pds_timing$pt_m)

#for sensitivity analysis with outliers removed
pt_m_lm_no_outl <- lm(pds_tot_m_no_outl~age_years, na.action=na.exclude, data = pds_timing) # create linear model, and specify na.exclude so that df size stays the same.
summ(pt_m_lm,
     robust = TRUE, #robust standard error = HC3 as per sandwich package
     scale = TRUE,  #scaled beta used
     confint = TRUE, digits = 3) #get confidence intervals. 95% CI by default 

#obtain standardised residual = measure of pubertal timing. Save as new variable. 
#Note: the resid function in lm allows us to have the desired NA values for female rows. 

pds_timing$pt_m_no_outl <- resid(pt_m_lm)
summary(pds_timing$pt_m_no_outl)


#### Female pubertal timing linear model ####
#model summary
pt_f_lm <- lm(pds_total_females_p~age_years, na.action=na.exclude, data = pds_timing) # create linear model

summ(pt_f_lm,
     robust = TRUE, #robust standard error = HC3 as per sandwich package
     scale = TRUE,  #scaled beta used
     confint = TRUE, digits = 3) #get confidence intervals. 95% CI by default )

#obtain standardised residual = measure of pubertal timing. Save as new variable. 
pds_timing$pt_f <- resid(pt_f_lm)
summary(pds_timing$pt_f)

#Create a single pubertal timing measure combining males (with outliers) and females 
#Use ifelse to specify that NAs values should be replaced by non-NA value from other column. 
pds_timing <- pds_timing %>% 
  mutate(pt_all = if_else(is.na(pt_m), pt_f, pt_m))

#### Residual plot analysis #####
#examine plots to check to validity of the assumptions (i.e., residuals vs. leverage; residuals vs. fitted; Normal Q-Q; scale-location)
#This allows us to assess the quality of the regression
#see https://data.library.virginia.edu/diagnostic-plots/ for guidance on plot interpretation. 

#Check male plots - looks like assumptions have been met. 
plot(pt_m_lm)

#Check female plots - looks like assumptions have been met. 
plot(pt_f_lm)


#### EXPORT CLEANED DATA

#Change working directory and save clean file there. 
#Note: clean files should be saved in data folder in ABCD_puberty_RR project folder.
setwd("/Volumes/GenScotDepression/users/niamh/puberty_ABCD/ABCD_puberty_RR/data")
saveRDS(pds_timing,"pubertal_timing_cleaned.rds")

#read in new clean file to check export worked okay. 
pds_timing_check_rds <- read_rds("pubertal_timing_cleaned.rds")


####------------ END OF MAIN SCRIPT -----------------####

####NEXT STEPS:
#pds_timing file to be used in glm models
#remember to conduct sensitivity analyses when running models to check for male outlier effect. 

 

#STEP X: GENERATE PDS AVERAGE SCORE --- do at later stage


#STEP X: GENERATE PDS GONADAL AND ADRENAL SCORE --- do at a later stage. 


