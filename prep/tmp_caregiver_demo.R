#title: "tmp_caregiver_breakdown"
#author: "Niamh MacSweeney"
#date: "04/06/2021"

#### Introduction
#The purpose of this script is to  look at the demographic data for caregivers 

#### SETUP
#Load libraries needed and set working directory.

library(tidyr)
library(dplyr)
library(tidyverse)

setwd("/Volumes/GenScotDepression/data/abcd/release2.0.1/iii.data/Culture_and_Environment/")
getwd()

#Load data
cg_demo <- readRDS("crpbi01.rds")
head(cg_demo)

cg_demo <- subset(cg_demo, eventname=="baseline_year_1_arm_1") #baseline only 

cg_demo %>% 
  dplyr::count(crpbi_studycaregiver_id)