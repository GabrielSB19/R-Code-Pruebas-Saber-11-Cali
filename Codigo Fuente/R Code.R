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
library(parameters)

BaseData <- read_excel("C:/Users/usuario/OneDrive - Universidad Icesi (@icesi.edu.co)/Escritorio/Universidad/Semestre 4/Inferencia Estadistica/Proyecto final Inferencia/Base de Datos/Saber_11__2019-2 Trabajo Final.xlsx")

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
StratumBecados <- freq(BaseDataBecados$FAMI_ESTRATOVIVIENDA, plot = TRUE, main = "Obtencion de becas por estrato")
StratumBecados

#Stratum NB exploration
StratumNBecados <- freq(BaseDataNoBecados$FAMI_ESTRATOVIVIENDA, plot = TRUE, main = "No obtención de becas por estratos")
StratumNBecados

#Internet exploration
BarInternet <- factor(BaseDataBecados$ESTU_DEDICACIONINTERNET, labels = c("30 minutos o menos", "Entre 1 y 3 horas", "Entre 30 y 60 min", "Mas de tres horas", "No navega Internet"))
Internet <- table(BarInternet)
barplot(Internet, main = "Dedicacion de internet diaria de los estudiantes becados")
Internet.TFrecuenciaI <- freq(BaseDataBecados$ESTU_DEDICACIONINTERNET, plot = FALSE)
Internet.TFrecuenciaI

#Read exploration
BarRead <- factor(BaseDataBecados$ESTU_DEDICACIONLECTURADIARIA, labels = c("30 minutos o menos", "Entre 1 y 2 horas", "Entre 30 y 60 min", "Mas de 2 horas", "No lee"))
Read <- table(BarRead)
barplot(Read, main = "Dedicacion de lectura diaria")
Read.TFrecuenciaR <- freq(BaseDataBecados$ESTU_DEDICACIONLECTURADIARIA, plot = FALSE)
Read.TFrecuenciaR

#Critical reading score data
summary(BaseDataBecados$PUNT_LECTURA_CRITICA)
boxplot(BaseDataBecados$PUNT_LECTURA_CRITICA, horizontal = TRUE, main = "Lectura Critica, Becados (Datos atipicos)")
DataLCWO <- filter(BaseDataBecados, PUNT_LECTURA_CRITICA < 81)
summary(DataLCWO$PUNT_LECTURA_CRITICA)
boxplot(DataLCWO$PUNT_LECTURA_CRITICA, horizontal = TRUE, main = "Lectura critica, Becados (Sin Datos atipicos)")


#Natural score data
summary(BaseDataBecados$PUNT_C_NATURALES)
boxplot(BaseDataBecados$PUNT_C_NATURALES, horizontal = TRUE, main = "Ciencias naturales, Becados (Datos atipicos)")
DataCNWO <- filter(BaseDataBecados, PUNT_C_NATURALES > 62)
summary(DataCNWO$PUNT_C_NATURALES)
boxplot(DataCNWO$PUNT_C_NATURALES, horizontal = TRUE, main = "Ciencias naturales, Becados (Sin datos atipicos")

#Mathematics score data
summary(BaseDataBecados$PUNT_MATEMATICAS)
boxplot(BaseDataBecados$PUNT_MATEMATICAS, horizontal = TRUE, main = "Matematicas, Becados (Datos atipicos)")
DataMWO <- filter(BaseDataBecados, PUNT_MATEMATICAS < 86)
summary(DataMWO$PUNT_MATEMATICAS)
boxplot(DataMWO$PUNT_MATEMATICAS, horizontal = TRUE, main = "Matematicas, Becados (Sin datos atipicos")

#Social score data
summary(BaseDataBecados$PUNT_SOCIALES_CIUDADANAS)
boxplot(BaseDataBecados$PUNT_SOCIALES_CIUDADANAS, horizontal = TRUE, main = "Ciencias Sociales, Becados (Datos atipicos)")
DataCSWO <- filter(BaseDataBecados, PUNT_SOCIALES_CIUDADANAS < 83)
summary(DataCSWO$PUNT_SOCIALES_CIUDADANAS)
boxplot(DataCSWO$PUNT_SOCIALES_CIUDADANAS, horizontal = TRUE, main = "Ciencias Sociales, Becados (Sin datos atipicos")

#English score data
summary(BaseDataBecados$PUNT_INGLES)
boxplot(BaseDataBecados$PUNT_INGLES, horizontal = TRUE, main = "Ingles, Becados (Datos atipicos)")
DataEWO <- filter(BaseDataBecados, PUNT_INGLES < 91)
summary(DataEWO$PUNT_INGLES)
boxplot(DataEWO$PUNT_INGLES, horizontal = TRUE, main = "Ingles, Becados (Sin datos atipicos)")

#Global score data
summary(BaseDataBecados$PUNT_GLOBAL)
boxplot(BaseDataBecados$PUNT_GLOBAL, horizontal = TRUE, main = "Puntaje global, Becados (Datos atipicos)")
DataGWO <- filter(BaseDataBecados, PUNT_GLOBAL < 380)
summary(DataGWO$PUNT_GLOBAL)
boxplot(DataGWO$PUNT_GLOBAL, horizontal = TRUE, main = "Puntaje global, Becados (Sin datos atipicos)")

#Hypotheses for the average
t.test(BaseDataNoBecados$PUNT_LECTURA_CRITICA, mu = 62, alternative = "less", conf.level = 0.95)

#Hypotheses for the proportion
PDataSucces <- filter(BaseData, ESTU_GENERACIONE2 == 1)
prop.test(x = nrow(PDataSucces), n = nrow(BaseData), p = 0.15, alternative = "less", conf.level = 0.95, correct = FALSE)


#Hypotheses for Independent samples
E1Data <- filter(BaseData, FAMI_ESTRATOVIVIENDA == "Estrato 1")
E3Data <- filter(BaseData, FAMI_ESTRATOVIVIENDA == "Estrato 3")
var.test(x = E1Data$PUNT_GLOBAL, y = E3Data$PUNT_GLOBAL, conf.level = 0.95)
t.test(x = E1Data$PUNT_GLOBAL, y = E3Data$PUNT_GLOBAL, mu = 0, alternative = "less", sigma.x = sd(E1Data$PUNT_GLOBAL),
       sigma.y = sd(E3Data$PUNT_GLOBAL), conf.level = 0.95)

#Hypotheses for dependent samples
t.test(x = BaseData$PUNT_MATEMATICAS, y = BaseData$PUNT_INGLES, paired = TRUE, alternative = "greater", conf.level = 0.95)


#ANOVA Which stratum belong to the students who obtained a better score on the English test?
AnovaWOAtypical <- filter(BaseData, PUNT_INGLES < 79 & PUNT_INGLES > 22)
boxplot(AnovaWOAtypical$PUNT_INGLES ~ AnovaWOAtypical$FAMI_ESTRATOVIVIENDA, xlab = "Estratos", ylab = "Puntaje ingles")
anova<- aov(lm(PUNT_INGLES ~FAMI_ESTRATOVIVIENDA))
summary(anova)
TukeyHSD(anova) 










