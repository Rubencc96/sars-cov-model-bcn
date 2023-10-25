library(ggplot2)
library(readxl)
library(stringr)
library(randomForest)
library(glm)
library(MASS)
library(Metrics)
library(glmnet)
library(tidyverse)
library(naniar)

setwd("D:/Usuarios/rrrei/Desktop/doctorado") # casa
setwd("/home/ruben/Trabajo/modelo_predictivo/2paper/") # GO-lab
list.files()

e <- 2.7182818284590452353602874713527


data_ine <- read.delim("data_ine_v2.csv", sep = ",", encoding = "latin1")
data_ine <- data_ine[-which(is.na(data_ine$Total)),]


## Intermediate data load
municipios_meta <- read_excel("datos_municipios_sarsaigua_v2.xlsx", sheet = "Municipios")

## Clinical data load
hosp_meta <- read_delim("regiones_hosp.csv")
data_hosp <- read_delim("Registre_de_casos_de_COVID-19_a_Catalunya_per__rea_b_sica_de_salut__ABS__i_sexe.csv")
data_edar <- read_delim("release.csv")

### Data cleaning and wrangling ###
data_edar$Fecha <-as.Date(str_split_fixed(data_edar$`id mostra`, "-", 2)[,2],
                          format = "%Y-%m-%d")
data_edar$week <- strftime(data_edar$Fecha, format = "%Y %V")


data_hosp2 <- data_hosp %>% 
  mutate(Fecha = as.Date(TipusCasData, format = "%d/%m/%Y")) %>%
  mutate(week = strftime(Fecha, format = "%Y %V")) %>%
  select(week, ABSDescripcio, NumCasos) %>%
  group_by(week, ABSDescripcio) %>%
  summarise(casos = sum(NumCasos))

colnames(data_hosp2) <- c("Week", "Ubicacion", "Casos")

data_hosp3 <- data_hosp2 %>% 
  mutate(Mun = str_split_fixed(Ubicacion, "\\s{0,1}\\d|\\s-|-\\d|\\s[a-zA-Z]{1}$|-[a-zA-Z]{1}$|/", 2)[,1]) %>% 
  group_by(Week, Mun) %>% 
  summarise(Ncasos = sum(Casos)) %>%
  mutate(Year = str_split_fixed(Week, " ", 2)[,2])

# Cambio de los datos de municipio para el merge con el data_hosp
# También voy a mergear con los datos del ine y así luego el merge va
# Con el número de habitantes
municipios_meta$Municipio <- toupper(municipios_meta$Municipio)
data_ine$Municipio <- toupper(data_ine$Municipio)


mun_meta_ine <- merge(municipios_meta, data_ine, by = "Municipio", all.x = T)
colnames(mun_meta_ine) <- c("Mun", "Codigo", "EDAR", "Year", "nHab", "CP")
mun_meta_ine <- select(mun_meta_ine, Mun, EDAR, Year, nHab)

## Ahora la unión con las EDAR que esto va a ser lo gracioso
data_merged_clean$EDAR <- str_replace_all(data_merged_clean$EDAR, ",", " ")
data_edar$depuradora <- str_replace_all(data_edar$depuradora, "_", " ")
data_edar_red <- data_edar[,c(2,3,4,5,6, 7, 8, 11, 12)]
colnames(data_edar_red) <- c("EDAR", "N1", "N2",
                             "IP4", "E", "caudal",
                             "lluvia", "Fecha", "Week")
data_edar_red <- data_edar_red %>%
  arrange(Fecha) %>%
  mutate(Week = factor(Week, levels = unique(data_edar_red$Week)))
## Antes de la unión voy a crear variables de ventana deslizante, por si sirven de
## algo vamos por que uf
## Cada rolling var tendrá que ser por municipio, que es lo gracioso

### Primero datos EDAR

# Voy a hacer también el LOESS de los datos, por si aca

edars <- unique(data_edar_red$EDAR)

new_edar_dfs <- list()
for (i in seq(edars)){
  edar <- edars[i]
  subdata <- filter(data_edar_red, EDAR == edar)
  model_n1 <- loess(N1 ~ Fecha, data = subdata)
  new_edar_dfs[i] <- subdata
}

gg_miss_upset(subdata)
vis_miss(data_edar_red)
