#code for generating unrelated sample in ABCD

#Assign random number to each family ID. Set random

df$random <- NA #generate new column of NAs
unrel_df <- data.frame() #make new df

#iterate through family IDs using for loop (this will take a few minutes)
# set.seed(2507) #to make sure random sample is the same random sample each time
unique_fam_id <- unique(df$rel_family_id)
for (i in unique_fam_id[1:length(unique_fam_id)]){

  set.seed(2507)

  #subset each family and assign random number to each individual within each family
  df$random[df$rel_family_id == i] <- sample(nrow(df[df$rel_family_id == i,]),nrow(df[df$rel_family_id == i,]), replace= FALSE)

  #select individual with highest number for unrelated dataframe
  unrel_individual <- df[df$rel_family_id == i,][which.max(df$random[df$rel_family_id == i]),]

  #append unrelated individual to unrelated dataframe
  unrel_df <- rbind(unrel_df, unrel_individual)

}

# i <- 8781 #for trouble shooting.
nrow(unrel_df) #check that N= 9850
length(unique(unrel_df$rel_family_id)) #check that no. of unique IDs is correct.

#Check that set.seed worked so that we get the same IDs each timescript is run

test_set_seed <- unrel_df$src_subject_id #make new vector of un_rel individual ids

#the rerun df$random lines + for loop, without clearing global environment
#check if new participant id list matches test_set_seed

all(unrel_df$src_subject_id == test_set_seed) #it worked!