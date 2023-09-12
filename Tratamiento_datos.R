#                   Tratamiento datos por Areas Generadoras 
#------------------------------------------------------------------------------.
# Paqueterias ------------------------------------------------------------------
rm(list=ls())

library(tidyverse)
library(readxl)
library(leaps)
library(SciViews)
library(dplyr)
library(tidyverse) 

#Carga de datos 
Datos <- read_xlsx("C:/Users/diego/OneDrive/Escritorio/Modelo_SAT/SAT/Modelo PCA/Datos_actualizados_areas.xlsx", sheet = "Final")


#Y's: 2015-Julio 2021, X's: 2015-Febrero 2021
Datos <- Datos %>% 
          filter(Fecha > "2014-12-01" &  Fecha <="2022-04-01")

#Quitamos importaciones y desempleo eua si no lo tenemos
Datos <- Datos[,-c(57,58), drop = FALSE]


#Convertimos a ln 
Datos_ln <- ln(Datos[,2:61])

# #Quitamos los NA's 
# Datos_ln[is.na(Datos_ln)] = 0

#Conseguimos Lags
lag1_X <- lag(Datos_ln)
lag1_X <- rename_with(lag1_X, ~paste0("l1_",.) )
lag2_X <- lag(Datos_ln, n = 2)
lag2_X <- rename_with(lag2_X, ~paste0("l2_",.) )
lag3_X <- lag(Datos_ln, n = 3)
lag3_X <- rename_with(lag3_X, ~paste0("l3_",.) )
#Dataframe
Datos_s <- cbind(Datos_ln, lag1_X[,10:60], lag2_X[,10:60], lag3_X[,10:60])



