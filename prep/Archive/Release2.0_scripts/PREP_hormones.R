#title: "PREP_hormones"
#author: "Niamh MacSweeney"
#date: "12/03/2021"

#### Introduction
#The purpose of this script is to tidy and quality control the pubertal hormone data. 

#### SETUP
#Load libraries needed (the packages vector will install anything you need if you don't have it already) and set working directory.

packages =  c('tidyr','dplyr', 'ggplot2','psych', 'ggstatsplot', 
              'jtools', 'readr', 'gridExtra')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())), 
                   repos = "http://cran.us.r-project.org")
}


setwd("/Volumes/GenScotDepression/data/abcd/release2.0.1/iii.data/Physical_Health/")
getwd()

####LOAD DATA, REDUCE VARIABLES, CREATE PUBERTY DATAFRAME, TIDY VARIABLES
# Load rds files that include variables needed. 
hormones <- readRDS("abcd_hsss01.rds") #salimetric hormones
hormone_notes <- readRDS("sph01.rds") #notes on hormone data

#Reduce to baseline data only. N= 11875
hormones <- subset(hormones, eventname=="baseline_year_1_arm_1")
hormone_notes <- subset(hormone_notes, visit=="baseline_year_1_arm_1")

#Merge and create pubertal hormone master dataframe. 
#drop repeated columns from hormone_notes before merge
hormone_notes <- hormone_notes %>% select(-interview_date, -interview_age, -gender, -visit)
#merge hormones and hormone_notes to form df
ph_df <- merge(hormones, hormone_notes, by="src_subject_id") 
colnames(ph_df)


#Change column names to keep consistent across project.
#Convert age in months to years, save new variable
ph_df <- ph_df %>%
  mutate(
    age_years = interview_age/12 
  )

#Change column name of "gender" to "sex of subject". Responses: Male, Female, Other, Not Reported. Gender = alias for sex of subject. 
ph_df <- ph_df %>% 
  rename(
    sex_of_subject = gender) #change  "gender" to "sex_of_subject"


####INSPECT DATA

#STEP1: Check for and remove blank and missing values

ph_df %>% 
  dplyr::count(sex_of_subject) # N=6 blanks present
ph_df <-  ph_df[!(is.na(ph_df$sex_of_subject) | ph_df$sex_of_subject==""), ] #Remove blank values (N=11875 --> N=11869)

ph_df %>% 
  dplyr::count(hormone_sal_sex) # N=5 NAs now present (after removing blanks for sex_of_subject)
ph_df <-  ph_df[!(is.na(ph_df$hormone_sal_sex) | ph_df$hormone_sal_sex==""), ] #Remove NAs (N=11869 --> 11864)

table(ph_df$sex_of_subject, ph_df$hormone_sal_sex) #Check missing values have been removed and now check for mismatches. 


#STEP2: Check whether sex_of_subject and hormone_sal_sex' match. Remove data that is unusable. (N=11864 --> N=11771)
ph_df %>% 
  dplyr::count(hormone_sal_sex)

#Remove participants if following conditions are met:
#12 male tubes miscaslsified as female, 21 female tubes misclassified as male ---> REMOVE (N=33)
# 57 unable to complete sample, 18 refused to give sample, 6 sample not collected/other ---> REMOVE (N=81)
ph_df  <- ph_df[-c(which(ph_df$sex_of_subject == "M" & ph_df$hormone_sal_sex == 1), #male sex, female labelled tube
             which(ph_df$sex_of_subject =="F" & ph_df$hormone_sal_sex == 2), #female sex, male labelled tube
             which(ph_df$hormone_sal_sex == 3), #participant unable to complete sample 
             which(ph_df$hormone_sal_sex == 4), #participant/parent refused sample:
             which(ph_df$hormone_sal_sex == 5)),] # sample not collected
          

####FILTER DATA

#First check number of NAs to get estimate of complete cases
#Count number of participant with NAs for three hormone mean scores
sum(is.na(ph_df$hormone_scr_dhea_mean)) #NAs= 6667; complete cases= 5083
sum(is.na(ph_df$hormone_scr_ert_mean)) #NAs= 6751; complete cases= 4999
sum(is.na(ph_df$hormone_scr_hse_mean)) #NAs= 9588; complete cases = 2162 (females only)


#Then use the filter scheme that used by Herting & Uban et al., (2021) to check records for any RA saliva collection notes.
#If true, then flag the record. 
#Then check flagged records and see if the Salimetrics value is out of range per hormone.
#If yes, then change value to NA, else keep the existing values for each replicate.
#Finally, average the two replicates into a new field.
#Note: variable hormon_sal_notes_y___1 is not included here because it is a summary measure, we are interested in the items independently. 


#STEP1: Count number of participants with flagged records.
#Method 1: based on yes/no concerns about sample. 
#10,015 = No concerns(1): N= 10,015, Concerns(0): N= 1735
#Could remove all sample with any flagged records - this would be the most conservative approach. 
ph_df %>% 
  dplyr::count(hormon_sal_notes_y___1)

#Method 2: Or could look at number flagged cases and see whether they are in range for each hormone — we will use this approach.
ph_df$hormone_notes_sum <- as.numeric(ph_df$hormon_sal_notes_y___2) + 
  as.numeric(ph_df$hormon_sal_notes_y___3) +
  as.numeric(ph_df$hormon_sal_notes_y___4) +
  as.numeric(ph_df$hormon_sal_notes_y___5) +
  as.numeric(ph_df$hormon_sal_notes_y___6)

summary(ph_df$hormone_notes_sum) #check min and max.Values should be within 0-5 range.

rownums <- which(ph_df$hormone_notes_sum >= 1) #get the row numbers that have at least 1 flagged record. N=1943

#STEP 2: Filter data to remove participants with unusable hormone levels (i.e., values outside range) for each hormone separately.
#Note: DHEA, Testosterone, and Estradiol Ranges, respectively, as per Herting and Uban et al. (2021)
#calibrator ranges (10.2–1,000 pg/ml; 6.1–600 pg/ml; 1–32 pg/ml); 
#lower limits of sensitivity (5 pg/ml; 1 pg/ml; 0.1 pg/ml),


####DHEA

#Generate new columns for filtered data
ph_df$filtered_dhea <- NA
ph_df$filtered_dhea_rep1 <- ph_df$hormone_scr_dhea_rep1
ph_df$filtered_dhea_rep2 <- ph_df$hormone_scr_dhea_rep2

#Is the replicate too low for detection (e.g., nds)? 
#If Yes (val=1), then change to 0 so that it will be removed a later "value range" QC step.
ph_df %>% 
  dplyr::count(hormone_scr_dhea_rep1_nd) #R1: N=29

ph_df %>% 
  dplyr::count(hormone_scr_dhea_rep2_nd) #R2: N=41

ph_df$filtered_dhea[which(ph_df$hormone_scr_dhea_rep1_nd == 1)] <- 0 #change any nds=1 values to 0 for R1
ph_df$filtered_dhea[which(ph_df$hormone_scr_dhea_rep2_nd == 1)] <- 0 #change any nds=1 values to 0 for R2
rownums_rep1 <- which(ph_df$hormone_scr_dhea_rep1 < 5 | ph_df$hormone_scr_dhea_rep1 > 1000) #isolate R1 rows that are outside range.
rownums_rep2 <- which(ph_df$hormone_scr_dhea_rep2 < 5 | ph_df$hormone_scr_dhea_rep2 > 1000) #repeat for R2
ph_df$filtered_dhea_rep1[rownums[which(rownums %in% rownums_rep1)]] <- NA #change out of range R1 values to NA
ph_df$filtered_dhea_rep2[rownums[which(rownums %in% rownums_rep2)]] <- NA #repeat for R2

#average two replicates to new variable
ph_df$filtered_dhea <- apply(ph_df[, c("filtered_dhea_rep1", "filtered_dhea_rep2")], 1, function(x) mean(x, na.rm=T)) 

sum(is.na(ph_df$filtered_dhea)) #NAs= 6619. Complete cases = 5131 (Remember to use total N=11750 to calc complete cases) 
#Same as Herting & Uban QC. 

#check distribution - data is skewed, as expected, should I log-transform?

ggplot(data=ph_df, aes(x= filtered_dhea)) +
  geom_histogram(fill = "deepskyblue4",
                 bins =30) +
  ggtitle("Distribution of Filtered DHEA")


#To Do: breakdown of QC steps to get to final DHEA value. 

####TESTOSTERONE 

#Generate new columns for filtered data
ph_df$filtered_testosterone <- NA
ph_df$filtered_testosterone_rep1 <- ph_df$hormone_scr_ert_rep1
ph_df$filtered_testosterone_rep2 <- ph_df$hormone_scr_ert_rep2

#Is the replicate too low for detection (e.g., nds)? 
#If Yes (val=1), then change to 0 so that it will be removed a later "value range" QC step.
ph_df %>% 
  dplyr::count(hormone_scr_ert_rep1_nd) #R1: N=0

ph_df %>% 
  dplyr::count(hormone_scr_ert_rep2_nd) #R2: N=2

ph_df$filtered_testosterone[which(ph_df$hormone_scr_ert_rep1_nd == 1)] <- 0 #change any nds=1 values to 0 for R1
ph_df$filtered_testosterone[which(ph_df$hormone_scr_ert_rep2_nd == 1)] <- 0 #change any nds=1 values to 0 for R2
rownums_rep1 <- which(ph_df$hormone_scr_ert_rep1 < 1 | ph_df$hormone_scr_ert_rep1 > 600) #isolate R1 rows that are outside range.
rownums_rep2 <- which(ph_df$hormone_scr_ert_rep2 < 1 | ph_df$hormone_scr_ert_rep2 > 600) #repeat for R2
ph_df$filtered_testosterone_rep1[rownums[which(rownums %in% rownums_rep1)]] <- NA #change out of range R1 values to NA
ph_df$filtered_testosterone_rep2[rownums[which(rownums %in% rownums_rep2)]] <- NA #repeat for R2

#average two replicates to new variable
ph_df$filtered_testosterone <- apply(ph_df[, c("filtered_testosterone_rep1", "filtered_testosterone_rep2")], 1, function(x) mean(x, na.rm=T)) 

sum(is.na(ph_df$filtered_testosterone)) #NAs = 6747 Complete cases= 5003 (same as H&U 2021)

#Check distribution - data is skewed, as expected. Should I log-transform?

ggplot(data=ph_df, aes(x= filtered_testosterone)) +
  geom_histogram(fill = "deepskyblue4",
                 bins =30) +
  ggtitle("Distribution of Filtered Testosterone")


####ESTRADIOL

#Generate new columns for filtered data
ph_df$filtered_estradiol <- NA
ph_df$filtered_estradiol_rep1 <- ph_df$hormone_scr_hse_rep1
ph_df$filtered_estradiol_rep2 <- ph_df$hormone_scr_hse_rep2

#Is the replicate too low for detection (e.g., nds)? 
#If Yes (val=1), then change to 0 so that it will be removed a later "value range" QC step.
ph_df %>% 
  dplyr::count(hormone_scr_hse_rep1_nd) #R1: N=10

ph_df %>% 
  dplyr::count(hormone_scr_hse_rep2_nd) #R2: N=9

ph_df$filtered_estradiol[which(ph_df$hormone_scr_hse_rep1_nd == 1)] <- 0 #change any nds=1 values to 0 for R1
ph_df$filtered_estradiol[which(ph_df$hormone_scr_hse_rep2_nd == 1)] <- 0 #change any nds=1 values to 0 for R2
rownums_rep1 <- which(ph_df$hormone_scr_hse_rep1 < 0.1 | ph_df$hormone_scr_hse_rep1 > 32) #isolate R1 rows that are outside range.
rownums_rep2 <- which(ph_df$hormone_scr_hse_rep2 < 0.1 | ph_df$hormone_scr_hse_rep2 > 32) #repeat for R2
ph_df$filtered_estradiol_rep1[rownums[which(rownums %in% rownums_rep1)]] <- NA #change out of range R1 values to NA
ph_df$filtered_estradiol_rep2[rownums[which(rownums %in% rownums_rep2)]] <- NA #repeat for R2

#average two replicates to new variable
ph_df$filtered_estradiol <- apply(ph_df[, c("filtered_estradiol_rep1", "filtered_estradiol_rep2")], 1, function(x) mean(x, na.rm=T)) 

sum(is.na(ph_df$filtered_estradiol)) #NAs = 9562 Complete cases= 2188 (same as H&U 2021) 

#Check distribution - skewed as expected. 

ph_df %>%
  filter(sex_of_subject == "F") %>% 
  ggplot(aes(x= filtered_estradiol)) +
  geom_histogram(fill = "deepskyblue4",
                 bins =30) +
  ggtitle("Distribution of Filtered Estradiol")

#### EXPORT CLEANED DATA
#We now have single value for each hormone plus, hormone sex and sex at birth. 

ph_df <- ph_df %>% select("src_subject_id", "eventname", "filtered_dhea", "filtered_estradiol", "filtered_testosterone", 
                             "hormone_sal_sex", "sex_of_subject")

#Change working directory and save clean file there. 
#Note: clean files should be saved in data folder in ABCD_puberty_RR project folder. #GETTING ERROR WHEN I TRY TO OPEN FILE IN R. 
setwd("/Volumes/GenScotDepression/users/niamh/puberty_ABCD/ABCD_puberty_RR/data")
saveRDS(ph_df,"hormone_cleaned.rds")

#read in new clean file to check export worked okay. 
hormone_df <- read_rds("hormone_cleaned.rds")

