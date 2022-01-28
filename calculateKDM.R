######### set working directory ########
setwd("./")

##########required to install two packages ##########
library(dplyr)
library(devtools)
library(mice)

################ set path to kdm R code #########
source("./kdm_calc.R") 

################ data from 2017, clean by LiRui #####################
# Cleaned data not uploaded
data <- read.csv("./BioAgeBGE_matched.csv", header=T)
skin <- read.csv("./2017SkinPercentile.csv")

##############gather all markers ################
heart_age <- c("ECG.Diastolic_pressure.mmHg.", "ECG.Heart_rate.times.min.", "ECG.P.R_interval", "ECG.QRS_width.ms.", "ECG.QT_interval.ms.", "ECG.QTc_interval.ms.", "ECG.Systolic_pressure.mmHg.", "Health.Fasting_blood_glucose.mmol.L.", "Health.High.density_lipoprotein.mmol.L.", "Health.Low.density_lipoprotein.mmol.L.", "Health.Total_cholesterol.mmol.L.", "Health.Triglyceride.mmol.L.", "Phy.Step_index")

immune_age <- c("Health.Monocyte_percentage...", "Health.Platelet_distribution_width.fL.", "Health.Platelet_volume_ratio...", "Hormone.12.Deoxycortisol_test.ng.mL.", "Hormone.Serum_cortisone_test_value.ng.mL.", "Hormone.Serum_hydrocortisone_test_value.ng.mL.")

kidney_age <- c("Health.Serum_creatinine.¦Ìmol.L.", "Health.Serum_uric_acid.¦Ìmol.L.", "Hormone.12.Deoxycorticosterone_Test_Value.ng.mL.", "Hormone.Corticosterone_test_value.ng.mL.")

liver_age <- c("Amino.1.Methylhistidine.¦Ìmol.L.", "Amino.3.methylhistidine.¦Ìmol.L.", "Amino.Arginine.¦Ìmol.L.", "Amino.Argininosuccinic_acid.¦Ìmol.L.", "Amino.Citrulline.¦Ìmol.L.", "Amino.Cystine.¦Ìmol.L.", "Amino.Ethanolamine.¦Ìmol.L.", "Amino.Glutamate.¦Ìmol.L.", "Amino.Glycine.¦Ìmol.L.", "Amino.Isoleucine.¦Ìmol.L.", "Amino.L.homocitrulline.¦Ìmol.L.", "Amino.Lysine.¦Ìmol.L.", "Amino.Methionine.¦Ìmol.L.", "Amino.Phenylalanine.¦Ìmol.L.", "Amino.Phosphoethanolamine.¦Ìmol.L.", "Amino.Phosphoserine.¦Ìmol.L.", "Amino.Sarcosine.¦Ìmol.L.", "Amino.Serine.¦Ìmol.L.", "Amino.Taurine.¦Ìmol.L.", "Amino.Threonine.¦Ìmol.L.", "Amino.Tryptophan.¦Ìmol.L.", "Amino.Tyrosine.¦Ìmol.L.", "Amino.¦Á.aminoadipic_acid.¦Ìmol.L.", "Amino.¦Á.aminobutyric_Acid.¦Ìmol.L.", "Amino.¦Â.Alanine.¦Ìmol.L.", "Health.Albumin_concentration.g.L.", "Health.Indirect_bilirubin.¦Ìmol.L.", "Health.Serum_alanine_aminotransferase.U.L.", "Health.Serum_aspartate_aminotransferase.U.L.", "Health.¦Ã.glutamyl_transpeptidase.U.L.")

nutrition_age <- c("Microelement.Arsenic.¦Ìg.L.", "Microelement.Copper.mg.L.", "Microelement.Lead.¦Ìg.L.", "Microelement.Mercury.¦Ìg.L.", "Microelement.Selenium.¦Ìg.L.", "Vitamin.25.hydroxy_D.ng.mL.", "Vitamin.25.hydroxyD2.ng.mL.", "Vitamin.5.methyltetrahydrofolate.ng.mL.", "Vitamin.A.ng.mL.", "Vitamin.B1.ng.mL.", "Vitamin.B5.ng.mL.", "Vitamin.E.ng.mL.", "Vitamin.Pyridoxine.ng.mL.")

hormone_age <- c("Hormone.18.hydroxyprogesterone_test_value.ng.mL.", "Hormone.Androstenedione_test_value.ng.mL.", "Hormone.Progesterone.ng.mL.", "Hormone.Serum_dehydroepiandrosterone.ng.mL.", "Hormone.Serum_estradiol_test_value.ng.mL.", "Hormone.Serum_estrone_test_value.ng.mL.", "Hormone.Serum_testosterone_test_value.ng.mL.")

skin_age <- c("Skin.Number_of_brown_spots_on_frontal_face", "Skin.Number_of_brown_spots_on_side_face", "Skin.Number_of_pores_on_frontal_face", "Skin.Number_of_pores_on_side_face", "Skin.Number_of_porphyrins_on_frontal_face", "Skin.Number_of_porphyrins_on_side_face", "Skin.Number_of_red_areas_on_frontal_face", "Skin.Number_of_red_areas_on_side_face", "Skin.Number_of_spots_on_frontal_face", "Skin.Number_of_spots_on_side_face", "Skin.Number_of_textures_on_frontal_face", "Skin.Number_of_textures_on_side_face", "Skin.Number_of_UV_spots_on_frontal_face", "Skin.Number_of_UV_spots_on_side_face", "Skin.Number_of_wrinkles_on_frontal_face", "Skin.Number_of_wrinkles_on_side_face")

sport_bodycom <- c("Inbody.Basal_metabolic_rate.kcal.d.", "Inbody.Body_fat_rate...", "Inbody.Inorganic_salt.kg.", "Inbody.Protein.kg.", "Inbody.Total_body_water.L.", "Phy.Reaction_time.s.", "Phy.Sit.up.Push_up_Test", "Phy.Vertical_jump_Test", "Phy.Vital_capacity.mL.")

gut_meta_male <- c("Meta.Alistipes_onderdonkii", "Meta.Bacteroides_coprocola", "Meta.Bacteroides_nordii", "Meta.Bacteroides_uniformis", "Meta.Bifidobacterium_animalis", "Meta.Bifidobacterium_longum", "Meta.Clostridium_asparagiforme", "Meta.Clostridium_citroniae", "Meta.Clostridium_hathewayi", "Meta.Collinsella_aerofaciens", "Meta.Dorea_longicatena", "Meta.Enterobacter_aerogenes", "Meta.Gemella_unclassified", "Meta.Holdemania_filiformis", "Meta.Klebsiella_pneumoniae", "Meta.Lachnospiraceae_bacterium_3_1_46FAA", "Meta.Megamonas_unclassified", "Meta.Mitsuokella_multacida", "Meta.Odoribacter_splanchnicus", "Meta.Pantoea_unclassified", "Meta.Parabacteroides_merdae", "Meta.Ruminococcus_obeum", "Meta.Ruminococcus_sp_5_1_39BFAA", "Meta.Streptococcus_australis", "Meta.Streptococcus_salivarius", "Meta.Streptococcus_thermophilus", "Meta.Subdoligranulum_unclassified", "Meta.Weissella_confusa")

gut_meta_femal <- c("Meta.Actinobacillus_unclassified", "Meta.Alistipes_shahii", "Meta.Bacteroides_eggerthii", "Meta.Bacteroides_salyersiae", "Meta.Bacteroides_sp_3_1_19", "Meta.Bacteroides_uniformis", "Meta.Clostridium_citroniae", "Meta.Clostridium_leptum", "Meta.Clostridium_nexile", "Meta.Comamonas_unclassified", "Meta.Coprobacillus_unclassified", "Meta.Eubacterium_eligens", "Meta.Haemophilus_parainfluenzae", "Meta.Haemophilus_sputorum", "Meta.Leuconostoc_lactis", "Meta.Prevotella_copri", "Meta.Roseburia_unclassified", "Meta.Streptococcus_australis", "Meta.Streptococcus_thermophilus", "Meta.Subdoligranulum_unclassified")

all_markers <- c(heart_age, immune_age, kidney_age, liver_age, nutrition_age, hormone_age, skin_age, sport_bodycom, gut_meta_femal, gut_meta_male)

female <- subset(data, data$gender == "f")
male <- subset(data, data$gender == "m")

train <- kdm_calc(female, biomarkers=all_markers)

train <- kdm_calc(male, biomarkers=all_markers)

cardioageNoNA <- read.csv("CardioAgeNoNA.csv", header=T)
female <- subset(cardioageNoNA, cardioageNoNA$gender == "f")
train <- kdm_calc(female, biomarkers=heart_age)

##############seperate data by gender ###################
female <- subset(data, data$gender == "f")
male <- subset(data, data$gender == "m")

for (r in 5:ncol(female)) {
  correlation <- cor(female[, r], female$age)
  lm_age[r-4, 11] <- correlation 
}

#########define featues for cardiovascular age #########
heart_age <- c("ECG.Diastolic_pressure.mmHg.", "ECG.Heart_rate.times.min.", "ECG.P.R_interval", "ECG.QRS_width.ms.", "ECG.QT_interval.ms.", "ECG.QTc_interval.ms.", "ECG.Systolic_pressure.mmHg.", "Health.Fasting_blood_glucose.mmol.L.", "Health.High.density_lipoprotein.mmol.L.", "Health.Low.density_lipoprotein.mmol.L.", "Health.Total_cholesterol.mmol.L.", "Health.Triglyceride.mmol.L.", "Phy.Step_index")
heart_age <- c(  "Health.Fasting_blood_glucose.mmol.L.", "Health.High.density_lipoprotein.mmol.L.", "Health.Low.density_lipoprotein.mmol.L.", "Health.Total_cholesterol.mmol.L.", "Health.Triglyceride.mmol.L.")

heart_data <- select(data, "BGEID", "age", "gender", all_of(heart_age))

############use kdm to train model and calculate kdm biological ages
train <- kdm_calc(female, biomarkers=heart_age)
pred<- kdm_calc(female, biomarkers=heart_age, fit=train$fit)

############### generate results ###################
results <- pred$data
rownames(results) <- results$LY_OUTER

kdm <- cbind(results$LY_OUTER, results$age, results$kdm)
write.table(kdm, file="femal_cardio_age.txt")

####################### Start the calculations ####################
trainData <- read.csv("./BioAgeBGE_matched.csv", header=T)
celfull <- read.csv("./Hebe.csv", header=T)

#B1_celfull <- read.csv("./Hebe_batch1.csv", header=T)
#celfull<- B1_celfull
#vascular_age <- read.csv("./vascular.results.results.csv", header=T)

###################### Standardize data #############################


preproc1 <- preProcess(celfull[, 5:45], method=c("center", "scale"))
norm1 <- predict(preproc1, celfull[, 5:45])
temp1 <- cbind(norm1, celfull[, 1:4]) 
celfull <- temp1

preproc2 <- preProcess(trainData[, 7:1059], method=c("center", "scale"))
norm2 <- predict(preproc2, trainData[, 7:1059])
temp2 <- cbind(norm2, trainData[, 1:6]) 
trainData <- temp2

############### heart age ################

heart_age <- c("ECG.Diastolic_pressure.mmHg.", "ECG.Systolic_pressure.mmHg.", "Health.Fasting_blood_glucose.mmol.L.", "Health.High.density_lipoprotein.mmol.L.", "Health.Low.density_lipoprotein.mmol.L.", "Health.Total_cholesterol.mmol.L.", "Health.Triglyceride.mmol.L.")


train_heart_age <- select(trainData, "OUTER_CUSTOMER_ID", "age", "ECG.Diastolic_pressure.mmHg.", "ECG.Systolic_pressure.mmHg.", "Health.Fasting_blood_glucose.mmol.L.", "Health.High.density_lipoprotein.mmol.L.", "Health.Low.density_lipoprotein.mmol.L.", "Health.Total_cholesterol.mmol.L.", "Health.Triglyceride.mmol.L.")
test_heart_age <- select(celfull, "OUTER_CUSTOMER_ID", "age", "ECG.Diastolic_pressure.mmHg.", "ECG.Systolic_pressure.mmHg.", "Health.Fasting_blood_glucose.mmol.L.", "Health.High.density_lipoprotein.mmol.L.", "Health.Low.density_lipoprotein.mmol.L.", "Health.Total_cholesterol.mmol.L.", "Health.Triglyceride.mmol.L.")

combined <- rbind(train_heart_age, test_heart_age)

############### perform imputation ################

combined_impute <- mice(combined[, 2:9], meth='pmm', seed=0)
completed <- complete(combined_impute, 3)
all_data <- cbind(combined[, 1], completed)

#all_data <- combined
################# train KDM model ########################

heart_age <- c("ECG.Diastolic_pressure.mmHg.", "ECG.Systolic_pressure.mmHg.", "Health.Fasting_blood_glucose.mmol.L.", "Health.High.density_lipoprotein.mmol.L.", "Health.Low.density_lipoprotein.mmol.L.", "Health.Total_cholesterol.mmol.L.", "Health.Triglyceride.mmol.L.")
sub <- c( "Health.Fasting_blood_glucose.mmol.L.", "Health.High.density_lipoprotein.mmol.L.", "Health.Low.density_lipoprotein.mmol.L.")
train <- kdm_calc(all_data[1:3270, ], biomarkers=heart_age)
pred<- kdm_calc(all_data[3270:3279, ], biomarkers=sub, fit=train$fit)


results <- pred$data
hebe <- tail(results, n=6)
write.table(hebe_results, file="./CardiovascularAge.txt")


############################ Immune Age #####################

immune_age <- c("Health.Monocyte_percentage...", "Health.Platelet_distribution_width.fL.", "Health.Platelet_volume_ratio...", "Hormone.12.Deoxycortisol_test.ng.mL.", "Hormone.Serum_cortisone_test_value.ng.mL.", "Hormone.Serum_hydrocortisone_test_value.ng.mL.")


train_immune_age <- select(trainData, "OUTER_CUSTOMER_ID", "age", "Health.Monocyte_percentage...", "Health.Platelet_distribution_width.fL.", "Health.Platelet_volume_ratio...")
test_immune_age <- select(celfull, "OUTER_CUSTOMER_ID", "age", "Health.Monocyte_percentage...", "Health.Platelet_distribution_width.fL.", "Health.Platelet_volume_ratio...")


combined <- rbind(train_immune_age, test_immune_age)

############### perform imputation ################

combined_impute <- mice(combined[, 2:5], meth='pmm', seed=0)
completed <- complete(combined_impute, 3)
all_data <- cbind(combined[, 1], completed)

#all_data <- combined
################# train KDM model ########################

immune_age <- c("Health.Monocyte_percentage...", "Health.Platelet_distribution_width.fL.", "Health.Platelet_volume_ratio...")

train <- kdm_calc(all_data, agevar='age', biomarkers=immune_age)
pred<- kdm_calc(all_data, agevar='age', biomarkers=immune_age, fit=train$fit)
results <- pred$data
hebe <- tail(results, n=6)
write.table(hebe_results, file="./CardiovascularAge.txt")
