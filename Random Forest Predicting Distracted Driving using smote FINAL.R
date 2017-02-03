##RANDOM FOREST predicting Distracted Drivers using smote

library(DMwR)
library(randomForest)
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

#leave as numeric for smote to work
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


#SUBSET DATA of signifcant variables from Logistic regression
View(accidents)
ACCdd <- accidents[c(60:75)]
str(ACCdd)

#LOOK AT OUTCOME VARIABLES

table(ACCdd$Distracted_bin)

prop.table(table(ACCdd$Distracted_bin))

#BEFORE BUILDING MOODEL, SEPARATE THE DATA INTO TESTING AND TRAINING SETS
#Training model

library(caret)
set.seed(1234)
splitIndex <- createDataPartition(ACCdd$Distracted_bin, p = .70,
                                  list = FALSE,
                                  times = 1)
trainSplit <- ACCdd[ splitIndex,]
testSplit <- ACCdd[-splitIndex,]

prop.table(table(trainSplit$Distracted_bin))

prop.table(table(testSplit$Distracted_bin))


#train a random forest model on trainSplit and predict Distracted_bin on testSplit

ctrl <- trainControl(method = "cv", number = 5)
rfddmodel <- train(Distracted_bin ~ ., data = trainSplit, method = "rf",
                   trControl = ctrl)

predictors <- names(trainSplit)[names(trainSplit) != 'Distracted_bin']
pred <- predict(rfddmodel$finalModel, testSplit[,predictors])

#evaluate model

#CHECK ACCURACY OF PREDICTION

#testSplit
table(testSplit$Distracted_bin, pred)

(8691+0)/nrow(testSplit)

#need to rerun the model on with numeric outcome to test this
library(pROC)
auc <- roc(testSplit$Distracted_bin, pred)
print(auc)

plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)

#replciate with SMOTE

#create extra positive observations on distracted driving using smote
library(DMwR)
trainSplit$Distracted_bin <- as.factor(trainSplit$Distracted_bin)
trainSplit <- SMOTE(Distracted_bin ~ ., trainSplit, perc.over = 100, perc.under=200)
#trainSplit$Distracted_bin <- as.numeric(trainSplit$Distracted_bin)

#check distracted is back to factor
str(trainSplit)

prop.table(table(trainSplit$Distracted_bin))

#train a model using the smote training set and predict on the same non smote testing set as used before 

rfddmodel <- train(Distracted_bin ~ ., data = trainSplit, method = "rf",
                   trControl = ctrl)

predictors <- names(trainSplit)[names(trainSplit) != 'Distracted_bin']
pred <- predict(rfddmodel$finalModel, testSplit[,predictors])


#CHECK ACCURACY OF PREDICTION
table(testSplit$Distracted_bin, pred)

(5812+491)/nrow(testSplit)


#need to rerun the model on with numeric outcome to test this
library(pROC)
auc <- roc(testSplit$Distracted_bin, pred)
print(auc)

plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
abline(h=1,col='blue',lwd=2)
abline(h=0,col='red',lwd=2)
