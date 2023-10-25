library(dplyr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(stringr)
library(randomForest)
library(glm)
library(MASS)
library(Metrics)
library(glmnet)

setwd("D:/Usuarios/rrrei/Desktop/doctorado") # casa
setwd("/home/ruben/Trabajo/modelo_predictivo/2paper/") # GO-lab
list.files()

e <- 2.7182818284590452353602874713527


data_ine <- read.delim("data_ine_v2.csv", sep = ",", encoding = "latin1")
data_ine <- data_ine[-which(is.na(data_ine$Total)),]
