#title: "PREP_imaging"
#author: "Niamh MacSweeney" Credit to Gladi Thng and Xueyi Shen for template scripts. 
#date: "17/05/2021"

#### Introduction
#The purpose of this script is to quality control and process the ABCD imaging data (cortical, diffusion/white matter, subcortical)

#Helpful Documents:

#Follow the QC criteria recommended by ABCD in NDA 2.0.1 Imaging Instrument Release Notes: https://nda.nih.gov/study.html?id=634
#Refer to the sMRI and dMRI release notes, also found at https://nda.nih.gov/study.html?id=634
#Data dictionaries (located in same folder as corresponding data) are also useful for understanding variables. 
#Hagler et al., 2019 Neuroimage paper: https://www.sciencedirect.com/science/article/pii/S1053811919306822

#Helpful link for grep syntax: https://biocorecrg.github.io/CRG_RIntroduction/regular-expressions-to-find-more-flexible-patterns.html

#### SETUP
#Load libraries needed and set working directory.

library(tidyverse)
library(dplyr)
library(broom)
library(ggplot2)


setwd("/Volumes/GenScotDepression/data/abcd/release2.0.1/iii.data/MRI_QC")

#STEP:1 LOAD DATA AND TIDY DATA
#Need satisfactory T1 raw images, Freesurfer (FS) and DTI QC, reduce to baseline data only.

qc.t1 <- readRDS("mriqcrp102.rds") %>% filter(eventname == "baseline_year_1_arm_1") #t1 image QC file, baseline only --- N=11871
qc.fs <- readRDS("freesqc01.rds")  %>% filter(eventname == "baseline_year_1_arm_1") #freesurfer QC file, baseline only --- N=11556
qc.criteria <- full_join(qc.t1,qc.fs) #merge two dataframes.

#select IDs that passed QC for T1 FS and DTI
#field name: iqc_t1_ok_ser = T1: Number of series that are complete and passed QC. Valid IDs: iqc_t1_ok_ser > 0
#field name: fsqc_qc = FS QC score. Valid IDs: fsqc_qc = 1
#field name: iqc_dmri_ok_ser = DTI: Number of series that are complete and passed QC. Valid IDs: iqc_dmri_ok_ser > 0

t1.ids.pass <- qc.criteria %>% 
  select(src_subject_id,iqc_t1_ok_ser,fsqc_qc) %>%  
  filter(iqc_t1_ok_ser >0) %>% # t1 raw scans that passed QC
  filter(fsqc_qc==1) # passed freesurfer QC
t1.ids.pass <- t1.ids.pass$src_subject_id # make new column for valid t1 IDs --- N =11,556

dti.ids.pass <- qc.criteria %>% 
  select(src_subject_id,iqc_dmri_ok_ser,iqc_t1_ok_ser,fsqc_qc) %>% 
  filter(iqc_dmri_ok_ser>0) %>% #DTI scans that passed QC
  filter(iqc_t1_ok_ser>0) %>% 
  filter(fsqc_qc==1) 
dti.ids.pass <- dti.ids.pass$src_subject_id #make new column 


#STEP 2: PROCESS IMAGING DATA

#### Cortical Measures #####

#Using cortical volume, surface area, thickness and sulcal depth. 

#Change working directory as sMRI data is different folder to QC data. 

setwd("/Volumes/GenScotDepression/data/abcd/release2.0.1/iii.data/MRI_T_roi")

abcd.smrip101 <- readRDS("abcd_smrip101.rds") #read in data file.

abcd.cortical<- abcd.smrip101 %>%  #make df name more intuitive
  filter(eventname == "baseline_year_1_arm_1") %>% #baseline only --- N= 11534
  .[,grep("subject|thick_cdk|sulc_cdk|area_cdk|vol_cdk", colnames(.))] #extract variables of interest using grep (expression including... thick_cdk etc.)

#Rename columns to make more intuitive
colnames(abcd.cortical)=gsub('smri_','',colnames(abcd.cortical))
colnames(abcd.cortical)=gsub('_cdk_','.APARC.',colnames(abcd.cortical)) #APARC = cortical parcellation in FS syntax
colnames(abcd.cortical)=gsub('thick.','thk.',colnames(abcd.cortical))
colnames(abcd.cortical)=gsub('area.','sa.',colnames(abcd.cortical))

#NOTE TO SELF: SORT OUT ICV LATER. 
##Create ICV and remove wholeb and csf 
#abcd.cortical$ICV_derived = abcd.cortical$vol.ASEG.wholeb+abcd.cortical$vol.ASEG.csf
#abcd.cortical$ICV_ASEG = abcd.cortical$vol.ASEG.intracranialv
#abcd.subcort=abcd.subcort[,!grepl('vol.ASEG.intracranialv',colnames(abcd.subcort))]


#rearrange col names so that hemisphere (lh or rh or bl (bilateral - have one measure)) is indicated as the start of col name
colnames(abcd.cortical)[grep('lh$',colnames(abcd.cortical))]=paste0('lh.',gsub('lh$','',colnames(abcd.cortical)))[grep('lh$',colnames(abcd.cortical))]
colnames(abcd.cortical)[grep('rh$',colnames(abcd.cortical))]=paste0('rh.',gsub('rh$','',colnames(abcd.cortical)))[grep('rh$',colnames(abcd.cortical))]
colnames(abcd.cortical)[grep('^vol.|^sa.|^sulc.|^thk.',colnames(abcd.cortical))]=paste0('bl.',colnames(abcd.cortical))[grep('^vol.|^sa.|^sulc.|^thk.',colnames(abcd.cortical))]

#We are only interested in overall total values for this analysis so remove totals for lh and rh
cols.omit <- abcd.cortical %>% .[grep(c("^rh.+\\.total$|^lh.+\\.total$|^rh.+\\.mean$|^lh.+\\.mean$"),colnames(.))] 
abcd.cortical <- abcd.cortical[,!colnames(abcd.cortical) %in% colnames(cols.omit)]

#select IDs that passed QC check ----  N=10754
abcd.cortical.final <- abcd.cortical %>% 
  .[.$src_subject_id %in% t1.ids.pass,] 

colnames(abcd.cortical.final) #inspect data; 277 variables.

#check distribution for global (total) vol and sa. 
abcd.cortical.final %>%
  select(contains(".total")) %>% 
  gather() %>% 
  ggplot(aes(as.numeric(value)))+
  facet_wrap(~ key, scales = "free") +
  geom_histogram(fill = "deepskyblue4") #Looks fine. 




#check distribution for global (mean) sulc and thk. 
abcd.cortical.final %>%
  select(contains(".mean")) %>% 
  gather() %>% 
  ggplot(aes(as.numeric(value)))+
  facet_wrap(~ key, scales = "free") +
  geom_histogram(fill = "deepskyblue4") #Looks fine. 


#### DTI/White matter Measures ####

#Note to self: check how tracks are being defined/ 

#Using fractional anisotropy and mean diffusivity 

abcd_dti_p101 <- readRDS("abcd_dti_p101.rds")

abcd.dti<- abcd_dti_p101 %>% 
  filter(eventname == "baseline_year_1_arm_1") %>% #reduce to baseline --- N = 11,400
  .[,grep("subject|_dtifa_|_dtimd_", colnames(.))] %>% #select FA and MD cols
  .[,grep('subject|fiberat',colnames(.))]  #include DTI atlas tract adjustment



#tidyup column names
colnames(abcd.dti)=gsub('dmri_','',colnames(abcd.dti))
colnames(abcd.dti)=gsub('fiberat_','',colnames(abcd.dti))


colnames(IM.dat)[grep('\\.all&rh$',colnames(IM.dat))]=
  paste0('rhtotal.',gsub('\\.all|rh$','',colnames(IM.dat)))[grep('\\.all|rh$',colnames(IM.dat))]

#rearrange col names as done for cortical measures
colnames(abcd.dti)[grep('lh$',colnames(abcd.dti))]=paste0('lh.',gsub('lh$','',colnames(abcd.dti)))[grep('lh$',colnames(abcd.dti))]
colnames(abcd.dti)[grep('rh$',colnames(abcd.dti))]=paste0('rh.',gsub('rh$','',colnames(abcd.dti)))[grep('rh$',colnames(abcd.dti))]
colnames(abcd.dti)[grep('\\_all',colnames(abcd.dti))]=paste0('bl.',colnames(abcd.dti))[grep('\\_all',colnames(abcd.dti))]
colnames(abcd.dti)[grep('^dti',colnames(abcd.dti))]=paste0('bl.',colnames(abcd.dti))[grep('^dti',colnames(abcd.dti))]

#remove measures that don't include corpus callosum
cols.omit <- abcd.dti %>% .[grep("_allfcc$|_allfib$",colnames(.))] 
abcd.dti <- abcd.dti[,!colnames(abcd.dti) %in% colnames(cols.omit)]

abcd.dti.final<- abcd.dti %>% 
  .[.$src_subject_id %in% dti.ids.pass,]

colnames(abcd.dti.final)

abcd.dti.final %>%
  select(ends_with("allfibers"))%>% 
  gather() %>% 
  ggplot(aes(as.numeric(value)))+
  facet_wrap(~ key, scales = "free") +
  geom_histogram(fill = "deepskyblue4") #highly skewed -> so remove indiv that exceed +-5sd

abcd.dti.final <- filter(abcd.dti.final,!is.na(bl.dtifa_allfibers))
abcd.dti.final[abs(scale(abcd.dti.final$bl.dtifa_allfibers))>5,grep('dtifa',colnames(abcd.dti.final))]=NA
abcd.dti.final[abs(scale(abcd.dti.final$bl.dtimd_allfibers))>5,grep('dtimd',colnames(abcd.dti.final))]=NA
#check dist again -> looks fine

###NOTE: not including subcortical measures in Puberty RR but processing here for future reference. 

abcd.smrip201 <- readRDS(paste0(getwd(),"/abcd_smrip201.rds"))

abcd.subcort <- abcd.smrip201 %>% 
  #filter(eventname == "baseline_year_1_arm_1") %>% #for future release
  .[,grep("subject|smri_vol_scs_", colnames(.))] %>%   #select smri vol data
  .[,grep("subject|aa|amygdala|caudate|hpus|pallidum|putamen|tp|cranial|subcortical",colnames(.))] #select columns that overlap with enigma data

colnames(abcd.subcort)=gsub('smri_','',colnames(abcd.subcort))
colnames(abcd.subcort)=gsub('_scs_','.ASEG.',colnames(abcd.subcort))

# create ICV and remove wholeb, csf
# abcd.subcort$ICV_derived = abcd.subcort$vol.ASEG.wholeb+abcd.subcort$vol.ASEG.csf
abcd.subcort$ICV_ASEG =abcd.subcort$vol.ASEG.intracranialv
abcd.subcort=abcd.subcort[,!grepl('vol.ASEG.intracranialv',colnames(abcd.subcort))]

colnames(abcd.subcort)[grep('l$|lh$',colnames(abcd.subcort))]=paste0('lh.',gsub('l$|lh$','',colnames(abcd.subcort)))[grep('l$|lh$',colnames(abcd.subcort))]
colnames(abcd.subcort)[grep('r$|rh$',colnames(abcd.subcort))]=paste0('rh.',gsub('r$|rh$','',colnames(abcd.subcort)))[grep('r$|rh$',colnames(abcd.subcort))]
colnames(abcd.subcort)[grep('^vol.',colnames(abcd.subcort))]=paste0('bl.',colnames(abcd.subcort))[grep('^vol.',colnames(abcd.subcort))]

#select those that passed QC check 
abcd.subcort.final<- abcd.subcort %>% 
  .[.$src_subject_id %in% t1.ids.pass,]  #%>%  #all individuals

#check distribution
abcd.subcort.final %>%
  select(!starts_with("src"))%>% 
  gather() %>% 
  ggplot(aes(as.numeric(value)))+
  facet_wrap(~ key, scales = "free") +
  geom_histogram(fill = "deepskyblue4") 

colnames(abcd.subcort.final)

#Save RDS 
saveRDS(abcd.subcort.final,"ABCD_release201_Subcortical_QCed_AllSubjects.rds")
saveRDS(abcd.cortical.final,"ABCD_release201_Cortical_QCed_AllSubjects.rds")
saveRDS(abcd.dti.final,"ABCD_release201_DTI_QCed_AllSubjects.rds")

