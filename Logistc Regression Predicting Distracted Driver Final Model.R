#SLOGISTIC REGRESSION WITH DISTRACTED DRIVER AS OUTCOME USING VIF SIG VARS

library(ggplot2)
library(dplyr)
library(tidyr)
accidents <- read.table("/users/Marilyn/ACC_AUX.csv", header=TRUE, sep=",")
str(accidents)

#convert variables from integers to factors
accidents$alcohol_fac <- factor(accidents$A_POSBAC)
accidents$DriverUn25_fac <- factor(accidents$A_D16_24)
accidents$DOW_fac <- factor(accidents$A_DOW)
accidents$HitRun_fac <- factor(accidents$A_HR)
accidents$Interstate_fac <- factor(accidents$A_INTER)
accidents$Intersection_fac <- factor(accidents$A_INTSEC)
accidents$LargeTruck_fac <- factor(accidents$A_LT)
accidents$ManCol_fac <- factor(accidents$A_MANCOL)
accidents$motorcycle_fac <- factor(accidents$A_MC)
accidents$pedestrian_fac <- factor(accidents$A_PED)
accidents$biker_fac <- factor(accidents$A_PEDAL)
accidents$RD_fac <- factor(accidents$A_RD)
accidents$Region_fac <- factor(accidents$A_REGION)
accidents$roll_fac <- factor(accidents$A_ROLL)
accidents$RuralUrb_fac <- factor(accidents$A_RU)
accidents$speeding_fac <- factor(accidents$A_SPCRA)
accidents$time_fac <- factor(accidents$A_TOD)
accidents$Over65_fac <- factor(accidents$A_D65PLS)
accidents$Distracted_fac <- factor(accidents$A_DIST)
accidents$Drowsy_fac <- factor(accidents$A_DROWSY)
accidents$RoadClass_fac <- factor(accidents$A_ROADFC)

#convert distracted driver to dummy variable where distracted is 1.
accidents$Distracted_bin<-ifelse(accidents$Distracted_fac=="1",1,0)
table(accidents$Distracted_bin)
#convert back to factor for logistic regression model
accidents$Distracted_bin <- factor(accidents$Distracted_bin)


#convert variables from factors to dummies
#only keep vars sig from VIF checks
accidents$Alcohol_Yes <-ifelse(accidents$alcohol_fac=="1",1,0)
table(accidents$Alcohol_Yes)

accidents$DriverUn25_fac <- factor(accidents$A_D16_24)
accidents$DriverUn25_Yes <-ifelse(accidents$DriverUn25_fac=="1",1,0)
table(accidents$DriverUn25_Yes)

table(accidents$ManCol_fac)
accidents$ManCol_fac <- factor(accidents$A_MANCOL)
accidents$ManCol_NoOthCar <-ifelse(accidents$ManCol_fac=="1",1,0)
table(accidents$ManCol_NoOthCar)

accidents$ManCol_fac <- factor(accidents$A_MANCOL)
accidents$ManCol_RearEnd <-ifelse(accidents$ManCol_fac=="2",1,0)
table(accidents$ManCol_RearEnd)

accidents$motorcycle_fac <- factor(accidents$A_MC)
accidents$motorcycle_Yes <-ifelse(accidents$motorcycle_fac=="1",1,0)
table(accidents$motorcycle_Yes)

accidents$RD_fac <- factor(accidents$A_RD)
accidents$RoadDep_Yes <-ifelse(accidents$RD_fac=="1",1,0)
table(accidents$RoadDep_Yes)

accidents$roll_fac <- factor(accidents$A_ROLL)
accidents$Roll_Yes <-ifelse(accidents$roll_fac=="1",1,0)
table(accidents$Roll_Yes)

accidents$speeding_fac <- factor(accidents$A_SPCRA)
accidents$speeding_Yes <-ifelse(accidents$speeding_fac=="1",1,0)
table(accidents$speeding_Yes)

accidents$time_fac <- factor(accidents$A_TOD)
accidents$daytime <-ifelse(accidents$time_fac=="1",1,0)
table(accidents$daytime)

accidents$Region_fac <- factor(accidents$A_REGION)
accidents$Region_midAtlantic <-ifelse(accidents$Region_fac=="3",1,0)
table(accidents$Region_midAtlantic)

accidents$Region_fac <- factor(accidents$A_REGION)
accidents$Region_SouthEast <-ifelse(accidents$Region_fac=="4",1,0)
table(accidents$Region_SouthEast)

accidents$Region_fac <- factor(accidents$A_REGION)
accidents$Region_NorMidwest <-ifelse(accidents$Region_fac=="5",1,0)
table(accidents$Region_NorMidwest)

accidents$Region_fac <- factor(accidents$A_REGION)
accidents$Region_CoNvNdSdWyUt <-ifelse(accidents$Region_fac=="8",1,0)
table(accidents$Region_CoNvNdSdWyUt)

accidents$Region_fac <- factor(accidents$A_REGION)
accidents$Region_AZ_CA_HI <-ifelse(accidents$Region_fac=="9",1,0)
table(accidents$Region_AZ_CA_HI)

accidents$Region_fac <- factor(accidents$A_REGION)
accidents$Region_PacificNW <-ifelse(accidents$Region_fac=="10",1,0)
table(accidents$Region_PacificNW)


#daytime & rear end interaction
accidents$daytime_rearend <-ifelse(accidents$ManCol_RearEnd=="1" & accidents$daytime=="1" ,1,0)
table(accidents$daytime_rearend)

table(accidents$ManCol_RearEnd, accidents$daytime)


#SUBSET DATA of signifcant variables from inital Logistic regression
View(accidents)
ACCdd <- accidents[c(60:76)]
str(ACCdd)

#split into testing and training sets

set.seed(123)
samp <- sample(nrow(ACCdd), 0.7 * nrow(ACCdd))
train <- ACCdd[samp, ]
test <- ACCdd[-samp, ]


#TRAINING DATASET
# Logistic regression on Distracted drivers with VIF chosen dummy vars 
LRdistracted <- glm(Distracted_bin ~ ManCol_NoOthCar+motorcycle_Yes+RoadDep_Yes+
                      Roll_Yes+speeding_Yes+DriverUn25_Yes+Alcohol_Yes+daytime+
                      ManCol_RearEnd+Region_midAtlantic+
                      Region_SouthEast+
                      Region_NorMidwest+
                      Region_CoNvNdSdWyUt+
                      Region_AZ_CA_HI+
                      Region_PacificNW + daytime_rearend, family=binomial, data=train)

#model summary
summary(LRdistracted)

#odds ratios
exp(cbind(Odds_and_OR=coef(LRdistracted), confint(LRdistracted)))


#check anova 
anova(LRdistracted, test="Chisq")

#get pseudo r squared
library(pscl)
pR2(LRdistracted)


#TESTING DATASET
# Logistic regression on Distracted drivers with VIF chosen dummy vars 
LRdistractedTest <- glm(Distracted_bin ~ ManCol_NoOthCar+motorcycle_Yes+RoadDep_Yes+
                          Roll_Yes+speeding_Yes+DriverUn25_Yes+Alcohol_Yes+daytime+
                          ManCol_RearEnd+Region_midAtlantic+
                          Region_SouthEast+
                          Region_NorMidwest+
                          Region_CoNvNdSdWyUt+
                          Region_AZ_CA_HI+
                          Region_PacificNW + daytime_rearend, family=binomial, data=test)

#model summary
summary(LRdistractedTest)

#odds ratios
exp(cbind(Odds_and_OR=coef(LRdistractedTest), confint(LRdistractedTest)))

#check anova 
anova(LRdistractedTest, test="Chisq")

#get pseudo r squared
library(pscl)
pR2(LRdistractedTest)


#check prediction accuracy 

library(caret)

# Select testing results
#real result from testing dataset
real_results <- subset(test,select=c(1))


# Predict results
results_prob <- predict(LRdistracted,test,type='response')

# If prob > 0.5 then 1, else 0
results <- ifelse(results_prob > 0.5,1,0)

# Quick check
head(results)
head(results_prob)

# Check accuracy
misClasificError <- mean(real_results != results)
print(paste('Accuracy',1-misClasificError))

# Confusion matrix
confusionMatrix(data=results, reference=real_results$Distracted_bin)


