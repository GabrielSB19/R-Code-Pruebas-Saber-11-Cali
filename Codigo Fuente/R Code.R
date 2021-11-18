library(tidyverse)
library(lubridate)
library(xts)
library(qrmdata)
library(qrmtools)
library(psych)
library(gmodels)
library(MASS)
library(survival)
library(fitdistrplus)
library(lmtest)
library (fdth)
library(readxl)
library(ggplot2)
library(PASWR2)
library(lattice)
library(descr)
library(openxlsx)
library(dplyr)

BaseData <- read_excel("")

BaseDataBecados <- filter(BaseData, ESTU_GENERACIONE == "GENERACION E - EXCELENCIA NACIONAL")
BaseDataNoBecados <- filter(BaseData, ESTU_GENERACIONE == "NO")

# Gender exploration
PieGender <- factor(BaseDataBecados$ESTU_GENERO, labels = c("Femenino", "Masculino"))
Genero <- table(PieGender)
pie(Genero, main = "Becados por genero", col = c("purple", "orange"), clockwise = TRUE)
legend("topright", c("Feminio", "Masculino"), cex = 1.5, fill = c("purple", "orange"))
Genero.TFrecuenciaGenero <- freq(ESTU_GENERO, plot = FALSE)
Genero.TFrecuenciaGenero

#Kind College exploration
PieKindC <- factor(BaseDataBecados$COLE_NATURALEZA, labels = c("No oficial", "oficial"))
KindCollage <- table(PieKindC)
pie(KindCollage, main = "Becados segun el tipo de colegio", col = c("green", "yellow"), clockwise = TRUE)
legend("topright", c("No oficial", "Oficial"), cex = 1.5, fill = c("green", "yellow"))
KindCollage.TFrecuenciaKindC <- freq(COLE_NATURALEZA, plot = FALSE)
KindCollage.TFrecuenciaKindC

#GPS College exploration
PieGpsC <- factor(BaseDataBecados$COLE_AREA_UBICACION, labels = c("Urbano", "Rural"))
GpsCollage <- table(PieGpsC)
pie(GpsCollage, main = "Becados por ubicacion del colegio", col = c("red", "blue", clockwise = TRUE))
legend("topright", c("Urbano", "Rural"), cex = 1.5, fill = c("red", "blue"))
GpsCollage.TFrecuenciaGPSC <- freq(COLE_AREA_UBICACION, plot = FALSE)
GpsCollage.TFrecuenciaGPSC

#Stratum B exploration
BarStratumB <- factor(BaseDataBecados$FAMI_ESTRATOVIVIENDA, labels = c("Estrato 1", "Estrato 2", "Estrato 3" , "Estrato 4-6"))
StratumBecados <- table(BarStratumB)
barplot(StratumBecados, main = "Obtenci?n de becas por estratos")
StratumBecados.TFrecuenciaStr <- freq(FAMI_ESTRATOVIVIENDA, plot = FALSE)
StratumBecados.TFrecuenciaStr

#Stratum NB exploration
BarStratumNB <- factor(BaseDataNoBecados$FAMI_ESTRATOVIVIENDA, labels = c("Estrato 1", "Estrato 2", "Estrato 3" , "Estrato 4-6"))
StratumNBecados <- table(BarStratumNB)
barplot(StratumNBecados, main = "No obtenci?n de becas por estratos")
StratumNBecados.TFrecuenciaStr <- freq(FAMI_ESTRATOVIVIENDA, plot = FALSE)
StratumNBecados.TFrecuenciaStr

#Internet exploration
BarInternet <- factor(BaseDataBecados$ESTU_DEDICACIONINTERNET, labels = c("30 minutos o menos", "Entre 1 y 3 horas", "Entre 30 y 60 min", "Mas de tres horas", "No navega Internet"))
Internet <- table(BarInternet)
barplot(Internet, main = "Dedicacion de internet diaria de los estudiantes becados")
Internet.TFrecuenciaI <- freq(ESTU_DEDICACIONINTERNET, plot = FALSE)
Internet.TFrecuenciaI

#Read exploration
BarRead <- factor(BaseDataBecados$ESTU_DEDICACIONLECTURADIARIA, labels = c("30 minutos o menos", "Entre 1 y 2 horas", "Entre 30 y 60 min", "Mas de 2 horas", "No lee"))
Read <- table(BarRead)
barplot(Read, main = "Dedicaci?n de lectura diaria")
Read.TFrecuenciaR <- freq(ESTU_DEDICACIONLECTURADIARIA, plot = FALSE)
Read.TFrecuenciaR

#Hypotheses for the average
LCData <- BaseDataNoBecados[c(10)]
t.test(LCData, mu = 62, alternative = "less")

#Hypotheses for the proportion
PData <- BaseData[c(29)]
PDataSucces <- filter(PData, ESTU_GENERACIONE2 == 1)
prop.test(x = nrow(PDataSucces), n = nrow(PData), p = 0.15, alternative = "less", conf.level = 0.95, correct = FALSE)

<<<<<<< HEAD
#ANOVA Which stratum belong to the students who obtained a better score on the English test?
attach(BaseData)
boxplot(PUNT_INGLES ~ FAMI_ESTRATOVIVIENDA) #Delete atypical data
anova<- aov(lm(PUNT_INGLES ~FAMI_ESTRATOVIVIENDA))
summary(anova)
TukeyHSD(anova )
=======
#Hypotheses for Independent samples
E1Data <- filter(BaseData, FAMI_ESTRATOVIVIENDA == "Estrato 1")
E3Data <- filter(BaseData, FAMI_ESTRATOVIVIENDA == "Estrato 3")
t.test(x = E1Data$PUNT_GLOBAL, y = E3Data$PUNT_GLOBAL, mu = 0, alternative = "greater", sigma.x = sd(E1Data$PUNT_GLOBAL),
       sigma.y = sd(E3Data$PUNT_GLOBAL), conf.level = 0.95)

#Hypotheses for dependent samples












>>>>>>> 8073c73ca50e81900a09b5e8cc9ea2f98e81852b

