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

BaseData <- read_excel("Escritorio/ICESI/Cuarto Semestre de Ingenieria de sistemas/Talleres/Inferencia/R-workSpace/ProyectoFinalInfe/R-Code-Pruebas-Saber-11-Cali/Base de Datos/Saber_11__2019-2 Trabajo Final.xlsx")

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

#Hypotheses for the average
LCData <- BaseDataNoBecados[c(10)]
t.test(LCData, mu = 62, alternative = "less")

#Hypotheses for the proportion
PData <- BaseData[c(29)]
PDataSucces <- filter(PData, ESTU_GENERACIONE2 == 1)
prop.test(x = nrow(PDataSucces), n = nrow(PData), p = 0.15, alternative = "less", conf.level = 0.95, correct = FALSE)

#ANOVA Which stratum belong to the students who obtained a better score on the English test?
attach(BaseData)
boxplot(PUNT_INGLES ~ FAMI_ESTRATOVIVIENDA) #Delete atypical data
anova<- aov(lm(PUNT_INGLES ~FAMI_ESTRATOVIVIENDA))
summary(anova)
TukeyHSD(anova )

