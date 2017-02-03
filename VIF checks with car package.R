#VIF checks
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
accidents$RoadDep_fac <- factor(accidents$A_RD)
accidents$Region_fac <- factor(accidents$A_REGION)
accidents$roll_fac <- factor(accidents$A_ROLL)
accidents$RuralUrb_fac <- factor(accidents$A_RU)
accidents$speeding_fac <- factor(accidents$A_SPCRA)
accidents$time_fac <- factor(accidents$A_TOD)
accidents$Over65_fac <- factor(accidents$A_D65PLS)
accidents$Distracted_fac <- factor(accidents$A_DIST)
accidents$Drowsy_fac <- factor(accidents$A_DROWSY)

table(accidents$Distracted_fac) 

accidents$Distracted_bin<-ifelse(accidents$Distracted_fac=="1",1,0)
table(accidents$Distracted_bin)
accidents$Distracted_bin <- factor(accidents$Distracted_bin)


#convert to dummy vairables
table(accidents$alcohol_fac)

accidents$alcohol_fac <- factor(accidents$A_POSBAC)

accidents$Alcohol_Yes <-ifelse(accidents$alcohol_fac=="1",1,0)
table(accidents$Alcohol_Yes)

accidents$Alcohol_No <-ifelse(accidents$alcohol_fac=="2",1,0)
table(accidents$Alcohol_No)

accidents$DriverUn25_fac <- factor(accidents$A_D16_24)
accidents$DriverUn25_Yes <-ifelse(accidents$DriverUn25_fac=="1",1,0)
table(accidents$DriverUn25_Yes)

accidents$HitRun_fac <- factor(accidents$A_HR)
accidents$HitRun_Yes <-ifelse(accidents$HitRun_fac=="1",1,0)
table(accidents$HitRun_Yes)

accidents$Intersection_fac <- factor(accidents$A_INTSEC)
accidents$Intersection_Yes <-ifelse(accidents$Intersection_fac=="1",1,0)
table(accidents$Intersection_Yes)

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

#Reduce dataset to only variables needed
ACCvif <- accidents[c(59:67, 69:78)]

str(ACCvif)

#load car package
library(car)

#convert distracted back to factor
ACCvif$Distracted_bin <- factor(ACCvif$Distracted_bin)

#fit log reg model
fitVif <- glm(Distracted_bin ~., family=binomial, data=ACCvif)

summary(fitVif)

# Evaluate Collinearity
vif(fitVif) # variance inflation factors 
sqrt(vif(fitVif)) > 2 # problem?


