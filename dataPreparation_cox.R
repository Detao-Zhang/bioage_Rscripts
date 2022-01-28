setwd("./NHANES")

#install.packages('devtools')
#install.packages("remotes")
#remotes::install_github("bjb40/bioage")
library(devtools)
#install.packages("dplyr")
library(dplyr)
source("./kdm_calc.R") 

#install.packages(c("survival", "survminer"))

library("survival")
library("survminer")

#setwd("/Users/liyan/Documents/BGI/BioAge/NHANES")

########load cardio data ###############
data <- read.csv("cardio.csv", header=T)

bp <- read.csv("./all.DPX.csv", header=T)

sub <- subset(data, data$age >=20)
x <- c(1:55080)

########################### load TCHOL Data #######################

library(foreign)

CHOL99 <- read.xport("./LAB13_99_00.XPT")
CHOL01 <- read.xport("./L13_B_01_02.XPT")
CHOL03 <- read.xport("./L13_C_03_04.XPT")
CHOL05 <- read.xport("./TCHOL_D_05_06.XPT")
CHOL07 <- read.xport("./TCHOL_E_07_08.XPT")
CHOL09 <- read.xport("./TCHOL_F_09_10.XPT")
CHOL11 <- read.xport("./TCHOL_G_11_12.XPT")
CHOL13 <- read.xport("./TCHOL_H_13_14.XPT")
CHOL15 <- read.xport("./TCHOL_I_15_16.XPT")
CHOL17 <- read.xport("./P_TCHOL_17_20.XPT")



TCHOL <- rbind (cbind(CHOL99$SEQN, CHOL99$LBXTC), cbind(CHOL01$SEQN, CHOL01$LBXTC), cbind(CHOL03$SEQN, CHOL03$LBXTC), cbind(CHOL05$SEQN, CHOL05$LBXTC), cbind(CHOL07$SEQN, CHOL07$LBXTC), cbind(CHOL09$SEQN, CHOL09$LBXTC), cbind(CHOL11$SEQN, CHOL11$LBXTC), cbind(CHOL13$SEQN, CHOL13$LBXTC), cbind(CHOL15$SEQN, CHOL15$LBXTC), cbind(CHOL17$SEQN, CHOL17$LBXTC))
colnames(TCHOL) <- c("SEQN", "LBXTC")



######## calculate sample level missing ########
for(i in c(1:55080))
{ x[i] = sum(is.na(sub[i, 1:8]))
  sub[i, 9] = x[i]
}

                 
nonmissCardio <- subset(sub, sub[, 9] == 0)

heart <- merge(nonmissCardio, bp, by="SEQN")

############ load death record data ##########

dod <- read.csv("merged.Death.csv")

total <- merge(heart, dod, by="SEQN")

newtotal <- merge(total, TCHOL, by="SEQN")

female <- subset(newtotal, newtotal$gender ==2)
train <- kdm_calc(female, biomarkers=c('Health.Triglyceride.mmol.L.', 'Health.Low.density_lipoprotein.mmol.L.', 'Health.Fasting_blood_glucose.mmol.L.', 'Health.High.density_lipoprotein.mmol.L.', 'BPXSY', 'BPXDI', 'LBXTC'))
pred<- kdm_calc(female, biomarkers=c('Health.Triglyceride.mmol.L.', 'Health.Low.density_lipoprotein.mmol.L.', 'Health.Fasting_blood_glucose.mmol.L.', 'Health.High.density_lipoprotein.mmol.L.', 'BPXSY', 'BPXDI', 'LBXTC'), fit=train$fit)
results_female <- pred$data

male <- subset(newtotal, newtotal$gender ==1)
train <- kdm_calc(male, biomarkers=c('Health.Triglyceride.mmol.L.', 'Health.Low.density_lipoprotein.mmol.L.', 'Health.Fasting_blood_glucose.mmol.L.', 'Health.High.density_lipoprotein.mmol.L.', 'BPXSY', 'BPXDI', 'LBXTC'))
pred<- kdm_calc(male, biomarkers=c('Health.Triglyceride.mmol.L.', 'Health.Low.density_lipoprotein.mmol.L.', 'Health.Fasting_blood_glucose.mmol.L.', 'Health.High.density_lipoprotein.mmol.L.', 'BPXSY', 'BPXDI', 'LBXTC'), fit=train$fit)
results_male <- pred$data

results <- rbind(results_female, results_male)

results[, 23] <- results$kdm/results$age
CardioCox <- results

heart_disease <- subset(CardioCox, CardioCox$ucod_leading ==1 || CardioCox$mortsta == 0) 

res.cox <- coxph(Surv(as.numeric(CardioCox$permth_int), as.numeric(CardioCox$mortstat)) ~   CardioCox$gender + CardioCox$age  + CardioCox[, 23]) 
summary(res.cox)

################# cox regression using single feature ##################

res.cox <- coxph(Surv(as.numeric(CardioCox$permth_int), as.numeric(CardioCox$mortstat)) ~   CardioCox$gender + CardioCox$age  + CardioCox$Health.Fasting_blood_glucose.mmol.L.) 
summary(res.cox)

res.cox <- coxph(Surv(as.numeric(heart_disease$permth_int), as.numeric(heart_disease$mortstat)) ~   heart_disease$gender + heart_disease$age  + heart_disease[, 23]) 
summary(res.cox)


res.cox <- coxph(Surv(as.numeric(heart_disease$permth_int), as.numeric(heart_disease$mortstat)) ~   heart_disease$gender + heart_disease$age  + heart_disease$LBXTC)
summary(res.cox)

res.cox <- coxph(Surv(as.numeric(heart_disease$permth_int), as.numeric(heart_disease$mortstat)) ~   heart_disease$gender + heart_disease$age  + heart_disease$V23)
summary(res.cox)


res.cox <- coxph(Surv(as.numeric(CardioCox$permth_int), as.numeric(CardioCox$mortstat)) ~   CardioCox$gender + CardioCox$age  + CardioCox$kdm) 

middleAge <- subset(results, results$age >45 & results$age <75)
res.cox <- coxph(Surv(as.numeric(middleAge$permth_int), as.numeric(middleAge$mortstat)) ~   middleAge$gender + middleAge$age  + middleAge$kdm)

res.cox <- coxph(Surv(as.numeric(middleAge$permth_int), as.numeric(middleAge$mortstat)) ~   middleAge$gender + middleAge$age  + middleAge[, 19])

####################### only consider people aged between 45~75 ##############

middleAgeCardio <- subset(newtotal, newtotal$age >45 && newtotal$age <75)

train <- kdm_calc(middleAgeCardio, biomarkers=c('Health.Triglyceride.mmol.L.', 'Health.Low.density_lipoprotein.mmol.L.', 'Health.Fasting_blood_glucose.mmol.L.', 'Health.High.density_lipoprotein.mmol.L.', 'BPXSY', 'BPXDI', 'LBXTC'))
pred<- kdm_calc(middleAgeCardio, biomarkers=c('Health.Triglyceride.mmol.L.', 'Health.Low.density_lipoprotein.mmol.L.', 'Health.Fasting_blood_glucose.mmol.L.', 'Health.High.density_lipoprotein.mmol.L.', 'BPXSY', 'BPXDI', 'LBXTC'), fit=train$fit)
results <- pred$data


results[, 23] <- results$kdm/results$age
CardioCox <- results

heart_disease <- subset(CardioCox, CardioCox$ucod_leading ==1 || CardioCox$mortsta == 0) 

CardioCox <- heart_disease

for(i in c("Health.Triglyceride.mmol.L.", "Health.Low.density_lipoprotein.mmol.L.", "Health.Fasting_blood_glucose.mmol.L.", "Health.High.density_lipoprotein.mmol.L.", "BPXSY", "BPXDI", "LBXTC", "V23", "BioAgeIndex"))
{
  print(i)
  res.cox <- coxph(Surv(as.numeric(CardioCox$permth_int), as.numeric(CardioCox$mortstat)) ~   CardioCox$gender + CardioCox$age  + CardioCox[, i]) 
  print(summary(res.cox))
}

res.cox <- coxph(Surv(as.numeric(CardioCox$permth_int), as.numeric(CardioCox$mortstat)) ~   CardioCox$gender + CardioCox$age  + CardioCox[, 23]) 
summary(res.cox)

res.cox <- coxph(Surv(as.numeric(CardioCox$permth_int), as.numeric(CardioCox$mortstat)) ~   CardioCox$gender + CardioCox$age  + CardioCox$Health.Triglyceride.mmol.L.) 
summary(res.cox)

res.cox <- coxph(Surv(as.numeric(CardioCox$permth_int), as.numeric(CardioCox$mortstat)) ~   CardioCox$gender + CardioCox$age  + CardioCox$Health.Low.density_lipoprotein.mmol.L.) 
summary

res.cox <- coxph(Surv(as.numeric(CardioCox$permth_int), as.numeric(CardioCox$mortstat)) ~   CardioCox$gender + CardioCox$age  + CardioCox$V23)
summary(res.cox)

res.cox <- coxph(Surv(as.numeric(CardioCox$permth_int), as.numeric(CardioCox$mortstat)) ~   CardioCox$gender + CardioCox$age  + CardioCox$LBXTC) 
summary(res.cox)


for(i in c(1:dim(CardioCox)[1]))
{
  if(is.na(CardioCox[i, 23]))
  {
    next;
  }else if(CardioCox[i, 23] > 1)
  {
    CardioCox[i, 24] = 1
  }else
  {
    CardioCox[i, 24] = 0
  }
}

################################################################################################################################################
########################## 
heart_disease <- subset(CardioCox, CardioCox$ucod_leading ==1 || CardioCox$mortsta == 0) 
CardioCox <- heart_disease

for(i in c(1:dim(CardioCox)[1]))
{
  if(is.na(CardioCox[i, 20]))
  {
    next;
  }else if(CardioCox[i, 20] > 220)
  {
    CardioCox[i, 24] = 1
  }else
  {
    CardioCox[i, 24] = 0
  }
}

colnames(CardioCox)[24] <- "TC"
#plot(survfit(Surv(as.numeric(CardioCox$permth_int), as.numeric(CardioCox$mortstat)) ~  CardioCox[, 24]))

res.cox <- coxph(Surv(as.numeric(CardioCox$permth_int), as.numeric(CardioCox$mortstat)) ~   CardioCox$gender + CardioCox$age  + CardioCox$LBXTC) 
summary(res.cox)

CardioCox_female <- subset(CardioCox, CardioCox$gender == 1 & CardioCox$age > 75 )

res.cox <- coxph(Surv(as.numeric(CardioCox_female$permth_int), as.numeric(CardioCox_female$mortstat)) ~    CardioCox_female$age  + CardioCox_female$LBXTC) 
summary(res.cox)


ggsurvplot(
  fit = survfit(Surv(as.numeric(CardioCox_female$permth_int), as.numeric(CardioCox_female$mortstat)) ~  CardioCox_female$TC , data=CardioCox_female), ylim=c(0.75, 1)
)

res.cox <- coxph(Surv(as.numeric(CardioCox$permth_int), as.numeric(CardioCox$mortstat)) ~   CardioCox$gender + CardioCox$age  + CardioCox_female$TC) 
summary(res.cox)



for(i in c(1:dim(CardioCox)[1]))
{
  if(is.na(CardioCox[i, 23]))
  {
    next;
  }else if(CardioCox[i, 23] > 1)
  {
    CardioCox[i, 24] = 1
  }else
  {
    CardioCox[i, 24] = 0
  }
}

#plot(survfit(Surv(as.numeric(CardioCox$permth_int), as.numeric(CardioCox$mortstat)) ~  CardioCox[, 24]))
colnames(CardioCox)[24] <- "BioAgeIndex"

CardioCox_female <- subset(CardioCox, CardioCox$gender == 1 )

ggsurvplot(
  fit = survfit(Surv(as.numeric(CardioCox_female$permth_int), as.numeric(CardioCox_female$mortstat)) ~  CardioCox_female$BioAgeIndex , data=CardioCox_female), ylim=c(0.75, 1)
)
cox <- coxph(Surv(as.numeric(CardioCox_female$permth_int), as.numeric(CardioCox_female$mortstat)) ~  CardioCox_female$BioAgeIndex , data=CardioCox_female)
ggforest(cox, data=CardioCox_female)

res.cox <- coxph(Surv(as.numeric(CardioCox$permth_int), as.numeric(CardioCox$mortstat)) ~   CardioCox$gender + CardioCox$age  + CardioCox_female$BioAgeIndex) 
summary(res.cox)


########################################################################################################################



for(i in c(1:dim(CardioCox)[1]))
{
  if(is.na(CardioCox[i, 3]))
  {
    next;
  }else if(CardioCox[i, 3] >= 2.26)
  {
    CardioCox[i, 24] = 1
  }else
  {
    CardioCox[i, 24] = 0
  }
}

colnames(CardioCox)[24] <- "TG"
#plot(survfit(Surv(as.numeric(CardioCox$permth_int), as.numeric(CardioCox$mortstat)) ~  CardioCox[, 24]))

CardioCox_female <- subset(CardioCox, CardioCox$gender == 2 )

ggsurvplot(
  fit = survfit(Surv(as.numeric(CardioCox_female$permth_int), as.numeric(CardioCox_female$mortstat)) ~  CardioCox_female$TG , data=CardioCox_female), ylim=c(0.75, 1)
)

res.cox <- coxph(Surv(as.numeric(CardioCox$permth_int), as.numeric(CardioCox$mortstat)) ~   CardioCox$gender + CardioCox$age  + CardioCox_female$TG) 
summary(res.cox)

cox <- coxph(Surv(as.numeric(CardioCox_female$permth_int), as.numeric(CardioCox_female$mortstat)) ~  CardioCox_female$BioAgeIndex , data=CardioCox_female)
ggforest(cox, data=CardioCox_female)



######################## Staff Family members ##############################

data <- read.csv("./StaffFamilyManCardio.csv", header=T)
markers <- c("Fasting_blood_glucose", "Triglyceride", "High-density_lipoprotein", "Low-density_lipoprotein", "Total_cholesterol", "Diastolic_pressure")

train <- kdm_calc(data, biomarkers=c('Fasting_blood_glucose', 'Triglyceride', 'High.density_lipoprotein', 'Low.density_lipoprotein', 'Total_cholesterol', 'Diastolic_pressure'), s_ba2=30)
pred<- kdm_calc(data, biomarkers=c('Fasting_blood_glucose', 'Triglyceride', 'High.density_lipoprotein', 'Low.density_lipoprotein', 'Total_cholesterol', 'Diastolic_pressure'), fit=train$fit, s_ba2=30)
results_male <- pred$data
plot(results_male$kdm, results_male$age)
reg <- glm(results_male$manY ~ results_male$kdm , family = "binomial")
summary(reg)

reg <- glm(results_male$manY ~ results_male$age , family = "binomial")
summary(reg)

reg <- glm(results_male$manY ~ results_male$age + results_male$kdm , family = "binomial")
summary(reg)



######################## plot ###########################################
result <- read.table("cox_regression.txt", header=T)
yAxis <- 9:1

p <- ggplot(result, aes(x = exp.coef., y=marker)) 
p + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_point(size = 3.5, color = "orange", shape=15) +
  geom_errorbarh(aes(xmax = upper95, xmin = lower95), size = .5, height = .2, color = "gray50")+
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  annotate("text", x=2.8, y=yAxis, label = result$Pvalue)+
  ylab("")+
  xlab("HR")


OrganAgeCompare <- read.table("OrganAgeCompare.txt", header=T)
yAxis <- 5:1

p <- ggplot(result, aes(x = exp.coef., y=marker)) 
p + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_point(size = 3.5, color = "orange", shape=15) +
  geom_errorbarh(aes(xmax = upper95, xmin = lower95), size = .5, height = .2, color = "gray50")+
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  annotate("text", x=2.8, y=yAxis, label = result$Pvalue)+
  ylab("")+
  xlab("HR")

