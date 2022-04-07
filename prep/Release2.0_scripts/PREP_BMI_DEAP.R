#title: "PREP_COVS"
#author:"Niamh MacSweeney"
#date: "08/07/2021

#### INTRODUCTION
#The purpose of this script is to tidy and quality control the covariates for our analysis.
#BMI, age, race/ethnicity, family income. 

#This data frame was downloaded via the DEAP portal which works off the most recent ABCD data release (3.0). 
#Therefore, there is a slightly discrepancy in the total N for baseline data (N= 11,878) compared to 2.0 release (N=11,875)
#This should not be an issue when the data is merged by participant ID for main analysis but good to note. 


#### SETUP
#load libraries
library(psych)
library(tidyverse)

setwd("/Volumes/GenScotDepression/users/niamh/puberty_ABCD/ABCD_puberty_depression/DEAP_data")
getwd()

####LOAD DATA AND REDUCE TO BASELINE

covs_df <- readRDS("BMI_demographic_vars.rds")

covs_df<- subset(covs_df,event_name=="baseline_year_1_arm_1")

###BMI 

summary(covs_df$anthro_bmi_calc) #Note everyone in normal range! 



#NOTE:
#height and weight measurements are in inches and lbs. Convert to m and kg to calculate BMI.
#for the purposes of this study, a continuous measure of BMI will be used.
#Check for extreme values. 

str(ant_df) #check data classification to see if they are correct. 

###NOTE correct classifications: interview age(months) = interger; src_subject_id, eventname, sex = factor; BMI measurements= numeric. 

#Reclassify relevant variables
ant_df %>% 
  mutate(src_subject_id=as.factor(src_subject_id),
         eventname=as.factor(eventname),
         anthro_timestamp=as.factor(anthro_timestamp),
         anthroheightcalc=as.numeric(anthroheightcalc),
         anthroweightcalc=as.numeric(anthroweightcalc)
        )

str(ant_df) #check classification again

#Select variables needed
#anthroheightcalc = Standing Height Average
#anthroweightcalc = Measured Weight Average

colnames(ant_df)

bmi_vars <- ant_df %>% 
  select(src_subject_id, eventname, anthroheightcalc, anthroweightcalc)


#reduce to baseline data only, N=11,875
bmi_vars<- subset(bmi_vars,eventname=="baseline_year_1_arm_1")

####BMI CALCULATION PREP

#Convert inches to m. 1 inch = 0.0254m
# d(m) = d(inch) × 0.0254

bmi_vars <- bmi_vars %>% 
  mutate(
    anthro_height_calc_m = anthroheightcalc*0.0254
  )

#Convert lbs to kgs. 1lb = 0.45359237 kg

bmi_vars <- bmi_vars %>% 
  mutate(
    anthro_weight_calc_kg = anthroweightcalc*0.45359237
  )

colnames(bmi_vars) #check new columns exist 

#remove missing values. Go from N=11,875 to 11,864. (NA: N=11)
bmi_vars <- subset(bmi_vars, 
                   !is.na(src_subject_id) & 
                     !is.na(eventname) &
                     !is.na(anthro_height_calc_m) &
                     !is.na(anthro_weight_calc_kg))

#Check that weight and height values makes sense
summary(bmi_vars$anthro_height_calc_m) #min = 0, max = 2.083m ---> need to remove extremes or unfeasible measures
summary(bmi_vars$anthro_weight_calc_kg) #min =0, max = 123.38kg ---> need to remove extremes or unfeasible measures

#Let's explore some of the extreme values, e.g., where height >2 metres

height_rm <- bmi_vars[(is.na(bmi_vars$anthro_height_calc_m) | bmi_vars$anthro_height_calc_m>2),]
#participants NDAR_INVXAUM8TGK and NDAR_INVM48HLU0M have heights above 2m but weight (in kg) = 26.76 and and 25.4 respectively.


#Plot data to check distribution
#Height Histogram
qplot(bmi_vars$anthro_height_calc_m, geom="histogram")
#Height Boxplot
boxplot(bmi_vars$anthro_height_calc_m, data=bmi_vars)

#Weight Histogram
qplot(bmi_vars$anthro_weight_calc_kg, geom="histogram", binwidth=5)

#Weight Boxplot
boxplot(bmi_vars$anthro_weight_calc_kg, data=bmi_vars)

qplot(bmi_vars$anthro_height_calc_m,
      geom="histogram",
      binwidth=5,  
      main="Histogram for Height", 
      xlab="Height",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      ....=c(20,50))

#Remove implausible values (e.g., height= 0m; weight= 0kg)
#to get ID for implausible values use:

height_rm <- bmi_vars[(is.na(bmi_vars$anthro_height_calc_m) | bmi_vars$anthro_height_calc_m>2),]

bmi_vars <-  bmi_vars[!(is.na(bmi_vars$anthro_height_calc_m) | bmi_vars$anthro_height_calc_m==0), ] #remove NDAR_INV46P89PTE who has height and weight = 0

#re-run summary stats
summary(bmi_vars$anthro_height_calc_m) #still some extreme/implausible values. Remove values that are +- 5 SDs from mean. 
summary(bmi_vars$anthro_weight_calc_kg)

#Remove outliers

height_outliers <- which(bmi_vars$anthro_height_calc_m > (mean(anthro_height_calc_m) + 5*sd(anthro_height_calc_m))
                             & bmi_vars$anthro_height_calc_m < (mean(anthro_height_calc_m) - 5*sd(anthro_height_calc_m)))
  
  
height_outliers <- bmi_vars %>% 
  filter(anthro_height_calc_m > (mean(anthro_height_calc_m) + 3*sd(anthro_height_calc_m))
  & anthro_height_calc_m < (mean(anthro_height_calc_m) - 3*sd(anthro_height_calc_m)))

rownums_rep1 <- which(ph_df$hormone_scr_hse_rep1 < 0.1 | ph_df$hormone_scr_hse_rep1 > 32) 
height_rmv_outliers <-
  bmi_vars

remove_outliers_1 <- 
  bmi_vars$anthro_height_calc_m[bmi_vars$anthro_height_calc_m > (mean(bmi_vars$anthro_height_calc_m) + 3*sd(bmi_vars$anthro_height_calc_m))
& bmi_vars$anthro_height_calc_m < (mean(bmi_vars$anthro_height_calc_m) - 3*sd(bmi_vars$anthro_height_calc_m))]

###TO-Do 
#Outlier removal not working at the moment. Need to distinguish between implausible values and extreme values. 
#Will do next week. 
df1 = df %>%
  group_by(element) %>%
  filter(!(abs(value - median(value)) > 2*sd(value))) %>%
  summarise_each(funs(mean), value)

remove_outliers_1 <- 
  x[x > (mean(x) - 3*sd(x)) & 
      x < (mean(x) + 3*sd(x))]

#Calculate BMI 
bmi_vars <- bmi_vars %>%
  mutate(
    bmi = anthro_weight_calc_kg/anthro_height_calc_m^2 
  )
 
colnames(bmi_vars) #check new variable has been created


summary(bmi_vars$bmi)
describeBy(bmi_vars$bmi)
  
      
  