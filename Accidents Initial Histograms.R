#Accidents histograms
library(ggplot2)

accidents <- read.csv(file="ACC_AUX.CSV", header=TRUE, sep=",")
str(accidents)

#Crash type
ggplot(accidents, aes(x=A_CT)) +
  geom_histogram()

ggplot(accidents, aes(x=A_D16_24)) +
  geom_histogram()

ggplot(accidents, aes(x=A_D21_24)) +
  geom_histogram()

ggplot(accidents, aes(x=A_D65PLS)) +
  geom_histogram()

ggplot(accidents, aes(x=A_DOW)) +
  geom_histogram()

ggplot(accidents, aes(x=A_DROWSY)) +
  geom_histogram()

ggplot(accidents, aes(x=A_HR)) +
  geom_histogram()

ggplot(accidents, aes(x=A_INTER)) +
  geom_histogram()

ggplot(accidents, aes(x=A_INTSEC)) +
  geom_histogram()

ggplot(accidents, aes(x=A_JUNC)) +
  geom_histogram()

ggplot(accidents, aes(x=A_LT)) +
  geom_histogram()

ggplot(accidents, aes(x=A_MANCOL)) +
  geom_histogram()

ggplot(accidents, aes(x=A_MC)) +
  geom_histogram()

ggplot(accidents, aes(x=A_PED)) +
  geom_histogram()

ggplot(accidents, aes(x=A_PEDAL)) +
  geom_histogram()

ggplot(accidents, aes(x=A_POLPUR)) +
  geom_histogram()

ggplot(accidents, aes(x=A_POSBAC)) +
  geom_histogram()

ggplot(accidents, aes(x=A_RD)) +
  geom_histogram()

ggplot(accidents, aes(x=A_REGION)) +
  geom_histogram()

ggplot(accidents, aes(x=A_RELRD)) +
  geom_histogram()

ggplot(accidents, aes(x=A_ROADFC)) +
  geom_histogram()

ggplot(accidents, aes(x=A_ROLL)) +
  geom_histogram()

ggplot(accidents, aes(x=A_RU)) +
  geom_histogram()

ggplot(accidents, aes(x=A_SPCRA)) +
  geom_histogram()

ggplot(accidents, aes(x=A_TOD)) +
  geom_histogram()



#for fun
ggplot(accidents, aes(x=FATALS, y=A_REGION, col=A_MANCOL)) +
  geom_point(shape=1, size=4)

ggplot(accidents, aes(x = FATALS, y = A_REGION, fill = A_MANCOL)) +
  geom_point(shape = 16, size = 1, alpha=0.6)

ggplot(accidents, aes(x = A_REGION, y = A_MANCOL, col = A_DOW)) +
  geom_jitter() +
  facet_grid(. ~ A_ROLL)

ggplot(accidents, aes(x = A_REGION, y = A_POSBAC, col =  A_D16_24)) +
  geom_jitter() +
  facet_grid(. ~ A_RU)

