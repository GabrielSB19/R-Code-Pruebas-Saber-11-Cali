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

BaseData <- read_excel("C:/Users/usuario/OneDrive - Universidad Icesi (@icesi.edu.co)/Escritorio/Universidad/Semestre 4/Inferencia Estadistica/Proyecto final Inferencia/Base de Datos/Saber_11__2019-2 Trabajo Final.xlsx")

BaseDataBecados <- filter(BaseData, ESTU_GENERACIONE == "GENERACION E - EXCELENCIA NACIONAL")
BaseDataNoBecados <- filter(BaseData, ESTU_GENERACIONE == "NO")

# Gender exploration
PieGender <- factor(BaseDataBecados$ESTU_GENERO, labels = c("Femenino", "Masculino"))
Genero <- table(PieGender)
pie(Genero, main = "Becados por genero", col = c("purple", "orange"), clockwise = TRUE)
legend("topright", c("Feminio", "Masculino"), cex = 1.5, fill = c("purple", "orange"))
Genero.TFrecuenciaGenero <- freq(ESTU_GENERO, plot = FALSE)
Genero.TFrecuenciaGenero.

#Hypotheses for the average
LCData <- BaseDataNoBecados[c(10)]
t.test(LCData, mu = 62, alternative = "less")







