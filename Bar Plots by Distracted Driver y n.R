
##EXPLORATORY ANALYSIS - BAR PLOTS WITH DISTRACTED DRIVER AS OUTCOME##
getwd()
setwd("/Users/Marilyn")


library(aod)
library(Rcpp)

library(ggplot2)
library(dplyr)
library(tidyr)
accidents <- read.table("ACC_AUX.csv", header=TRUE, sep=",")
str(accidents)

#convert variables to factors
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



#Drunk Driver by DISTRACTED DRIVER
ggplot(accidents, aes(x=accidents$alcohol_fac, fill=accidents$Distracted_fac)) +
  geom_bar(position="dodge")



## DRIVER AGE UNDER 25 (CAT 1) BY DISTRACTED DRIVER
ggplot(accidents, aes(x=accidents$DriverUn25_fac , fill=accidents$Distracted_fac)) +
  geom_bar(position="dodge")



## DAY OF WEEK (WEEKDAY, WEEKEND, UNKNOWN) BY DISTRACTED DRIVER
ggplot(accidents, aes(x=accidents$DOW_fac  , fill=accidents$Distracted_fac)) +
  geom_bar(position="dodge")


## HIT AND RUN (YES, NO) BY DISTRACTED DRIVER
ggplot(accidents, aes(x=accidents$HitRun_fac, fill=accidents$Distracted_fac)) +
  geom_bar(position="dodge")


## INTERSTATE (YES, NO, UNKNONW) BY DISTRACTED DRIVER
ggplot(accidents, aes(x=accidents$Interstate_fac, fill=accidents$Distracted_fac)) +
  geom_bar(position="dodge")


## INTERSECTION (YES, NO, UNKNONW) BY DISTRACTED
ggplot(accidents, aes(x=accidents$Intersection_fac, fill=accidents$Distracted_fac)) +
  geom_bar(position="dodge")


## LARGE TRUCK (YES, NO) BY DISTRACTED DRIVER
ggplot(accidents, aes(x=accidents$LargeTruck_fac, fill=accidents$Distracted_fac)) +
  geom_bar(position="dodge")


## MANOR OF COLLISION (7 CATEGORIES) BY DISTRACTED DRIVER
ggplot(accidents, aes(x=accidents$ManCol_fac , fill=accidents$Distracted_fac)) +
  geom_bar(position="dodge")


## MOTORCYCLE (YES, NO) BY RURAL DISTRACTED DRIVER
ggplot(accidents, aes(x=accidents$motorcycle_fac, fill=accidents$Distracted_fac)) +
  geom_bar(position="dodge")


## PEDESTRIAN (YES, NO) BY DISTRACTED DRIVER
ggplot(accidents, aes(x=accidents$pedestrian_fac , fill=accidents$Distracted_fac)) +
  geom_bar(position="dodge")


## CYCLISTS (YES, NO) BY DISTRACTED DRIVER 
ggplot(accidents, aes(x=accidents$biker_fac, fill=accidents$Distracted_fac)) +
  geom_bar(position="dodge")


## ROAD DEPARTURE (YES, NO) BY DISTRACTED DRIVER
ggplot(accidents, aes(x=accidents$RoadDep_fac , fill=accidents$Distracted_fac)) +
  geom_bar(position="dodge")


## REGION (10 REGIONS) BY DISTRACTED DRIVER
ggplot(accidents, aes(x=accidents$Region_fac  , fill=accidents$Distracted_fac)) +
  geom_bar(position="dodge")


## ROAD DEPARTURE (YES, NO) BY RURAL URBAN UNKNNOWN
ggplot(accidents, aes(x=accidents$RoadDep_fac , fill=accidents$RuralUrb_fac)) +
  geom_bar(position="dodge")

## ROLL OVER (YES, NO) BY DISTRACTED DRIVER
ggplot(accidents, aes(x=accidents$roll_fac, fill=accidents$Distracted_fac)) +
  geom_bar(position="dodge")



## SPEEDING (YES, NO) BY DISRACTED DRIVER
ggplot(accidents, aes(x=accidents$speeding_fac , fill=accidents$Distracted_fac)) +
  geom_bar(position="dodge")


## TIME OF DAY (YES, NO) BY DISTRACTED DRIVER
ggplot(accidents, aes(x=accidents$time_fac, fill=accidents$Distracted_fac)) +
  geom_bar(position="dodge")


## DRIVER OVER 65 (YES, NO) BY DISTRACTED DRIVER
ggplot(accidents, aes(x=accidents$Over65_fac, fill=accidents$Distracted_fac)) +
  geom_bar(position="dodge")


## DROWSY DRIVER (YES, NO) BY DISTRACTED DRIVER
ggplot(accidents, aes(x=accidents$Drowsy_fac, fill=accidents$Distracted_fac)) +
  geom_bar(position="dodge")


## RURAL URBAN(YES, NO, UKN) BY DISTRACTED DRIVER
ggplot(accidents, aes(x=accidents$RuralUrb_fac, fill=accidents$Distracted_fac)) +
  geom_bar(position="dodge")

