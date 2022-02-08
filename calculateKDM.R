######### Set working directory #########
setwd("./")

######### Required packages #########
library(dplyr)
library(devtools)
library(mice)

######### Set path to kdm R code #########
source("./kdm_calc.R") 

######### Load data cleaned by Rui Li #########
# No cleaned data uploaded
data <- read.csv("./cleaned_data.csv", header=T)

######### Select markers #########
heart_age <- c("ECG.Diastolic_pressure.mmHg.", "ECG.Heart_rate.times.min.", "ECG.P.R_interval", "ECG.QRS_width.ms.", "ECG.QT_interval.ms.", "ECG.QTc_interval.ms.", "ECG.Systolic_pressure.mmHg.", "Health.Fasting_blood_glucose.mmol.L.", "Health.High.density_lipoprotein.mmol.L.", "Health.Low.density_lipoprotein.mmol.L.", "Health.Total_cholesterol.mmol.L.", "Health.Triglyceride.mmol.L.", "Phy.Step_index")

immune_age <- c("Health.Monocyte_percentage...", "Health.Platelet_distribution_width.fL.", "Health.Platelet_volume_ratio...", "Hormone.12.Deoxycortisol_test.ng.mL.", "Hormone.Serum_cortisone_test_value.ng.mL.", "Hormone.Serum_hydrocortisone_test_value.ng.mL.")

kidney_age <- c("Health.Serum_creatinine.μmol.L.", "Health.Serum_uric_acid.μmol.L.", "Hormone.12.Deoxycorticosterone_Test_Value.ng.mL.", "Hormone.Corticosterone_test_value.ng.mL.")

liver_age <- c("Amino.1.Methylhistidine.μmol.L.", "Amino.3.methylhistidine.μmol.L.", "Amino.Arginine.μmol.L.", "Amino.Argininosuccinic_acid.μmol.L.", "Amino.Citrulline.μmol.L.", "Amino.Cystine.μmol.L.", "Amino.Ethanolamine.μmol.L.", "Amino.Glutamate.μmol.L.", "Amino.Glycine.μmol.L.", "Amino.Isoleucine.μmol.L.", "Amino.L.homocitrulline.μmol.L.", "Amino.Lysine.μmol.L.", "Amino.Methionine.μmol.L.", "Amino.Phenylalanine.μmol.L.", "Amino.Phosphoethanolamine.μmol.L.", "Amino.Phosphoserine.μmol.L.", "Amino.Sarcosine.μmol.L.", "Amino.Serine.μmol.L.", "Amino.Taurine.μmol.L.", "Amino.Threonine.μmol.L.", "Amino.Tryptophan.μmol.L.", "Amino.Tyrosine.μmol.L.", "Amino.α.aminoadipic_acid.μmol.L.", "Amino.α.aminobutyric_Acid.μmol.L.", "Amino.β.Alanine.μmol.L.", "Health.Albumin_concentration.g.L.", "Health.Indirect_bilirubin.μmol.L.", "Health.Serum_alanine_aminotransferase.U.L.", "Health.Serum_aspartate_aminotransferase.U.L.", "Health.γ.glutamyl_transpeptidase.U.L.")

nutrition_age <- c("Microelement.Arsenic.μg.L.", "Microelement.Copper.mg.L.", "Microelement.Lead.μg.L.", "Microelement.Mercury.μg.L.", "Microelement.Selenium.μg.L.", "Vitamin.25.hydroxy_D.ng.mL.", "Vitamin.25.hydroxyD2.ng.mL.", "Vitamin.5.methyltetrahydrofolate.ng.mL.", "Vitamin.A.ng.mL.", "Vitamin.B1.ng.mL.", "Vitamin.B5.ng.mL.", "Vitamin.E.ng.mL.", "Vitamin.Pyridoxine.ng.mL.")

hormone_age <- c("Hormone.18.hydroxyprogesterone_test_value.ng.mL.", "Hormone.Androstenedione_test_value.ng.mL.", "Hormone.Progesterone.ng.mL.", "Hormone.Serum_dehydroepiandrosterone.ng.mL.", "Hormone.Serum_estradiol_test_value.ng.mL.", "Hormone.Serum_estrone_test_value.ng.mL.", "Hormone.Serum_testosterone_test_value.ng.mL.")

skin_age <- c("Skin.Number_of_brown_spots_on_frontal_face", "Skin.Number_of_brown_spots_on_side_face", "Skin.Number_of_pores_on_frontal_face", "Skin.Number_of_pores_on_side_face", "Skin.Number_of_porphyrins_on_frontal_face", "Skin.Number_of_porphyrins_on_side_face", "Skin.Number_of_red_areas_on_frontal_face", "Skin.Number_of_red_areas_on_side_face", "Skin.Number_of_spots_on_frontal_face", "Skin.Number_of_spots_on_side_face", "Skin.Number_of_textures_on_frontal_face", "Skin.Number_of_textures_on_side_face", "Skin.Number_of_UV_spots_on_frontal_face", "Skin.Number_of_UV_spots_on_side_face", "Skin.Number_of_wrinkles_on_frontal_face", "Skin.Number_of_wrinkles_on_side_face")

sport_bodycom <- c("Inbody.Basal_metabolic_rate.kcal.d.", "Inbody.Body_fat_rate...", "Inbody.Inorganic_salt.kg.", "Inbody.Protein.kg.", "Inbody.Total_body_water.L.", "Phy.Reaction_time.s.", "Phy.Sit.up.Push_up_Test", "Phy.Vertical_jump_Test", "Phy.Vital_capacity.mL.")

gut_meta_male <- c("Meta.Alistipes_onderdonkii", "Meta.Bacteroides_coprocola", "Meta.Bacteroides_nordii", "Meta.Bacteroides_uniformis", "Meta.Bifidobacterium_animalis", "Meta.Bifidobacterium_longum", "Meta.Clostridium_asparagiforme", "Meta.Clostridium_citroniae", "Meta.Clostridium_hathewayi", "Meta.Collinsella_aerofaciens", "Meta.Dorea_longicatena", "Meta.Enterobacter_aerogenes", "Meta.Gemella_unclassified", "Meta.Holdemania_filiformis", "Meta.Klebsiella_pneumoniae", "Meta.Lachnospiraceae_bacterium_3_1_46FAA", "Meta.Megamonas_unclassified", "Meta.Mitsuokella_multacida", "Meta.Odoribacter_splanchnicus", "Meta.Pantoea_unclassified", "Meta.Parabacteroides_merdae", "Meta.Ruminococcus_obeum", "Meta.Ruminococcus_sp_5_1_39BFAA", "Meta.Streptococcus_australis", "Meta.Streptococcus_salivarius", "Meta.Streptococcus_thermophilus", "Meta.Subdoligranulum_unclassified", "Meta.Weissella_confusa")

gut_meta_femal <- c("Meta.Actinobacillus_unclassified", "Meta.Alistipes_shahii", "Meta.Bacteroides_eggerthii", "Meta.Bacteroides_salyersiae", "Meta.Bacteroides_sp_3_1_19", "Meta.Bacteroides_uniformis", "Meta.Clostridium_citroniae", "Meta.Clostridium_leptum", "Meta.Clostridium_nexile", "Meta.Comamonas_unclassified", "Meta.Coprobacillus_unclassified", "Meta.Eubacterium_eligens", "Meta.Haemophilus_parainfluenzae", "Meta.Haemophilus_sputorum", "Meta.Leuconostoc_lactis", "Meta.Prevotella_copri", "Meta.Roseburia_unclassified", "Meta.Streptococcus_australis", "Meta.Streptococcus_thermophilus", "Meta.Subdoligranulum_unclassified")

all_markers <- c(heart_age, immune_age, kidney_age, liver_age, nutrition_age, hormone_age, skin_age, sport_bodycom, gut_meta_femal, gut_meta_male)

######### Seperate data by gender #########
female <- subset(data, data$gender == "f")
male <- subset(data, data$gender == "m")

for (r in 5:ncol(female)) {
  correlation <- cor(female[, r], female$age)
  lm_age[r-4, 11] <- correlation 
}

######### Use kdm to train model and calculate kdm biological ages #########
train <- kdm_calc(female, biomarkers=heart_age)
pred <- kdm_calc(female, biomarkers=heart_age, fit=train$fit)

######### Generate results #########
results <- pred$data
rownames(results) <- results$LY_OUTER

kdm <- cbind(results$LY_OUTER, results$age, results$kdm)
write.table(kdm, file="female_cardio_age.txt")
