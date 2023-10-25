library(dplyr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(stringr)
library(randomForest)
library(glm)
library(MASS)
library(Metrics)

setwd("D:/Usuarios/rrrei/Desktop/doctorado") # casa
setwd("/home/ruben/Trabajo/modelo_predictivo/2paper/") # GO-lab
load("RData")
list.files()

e <- 2.7182818284590452353602874713527

## INE data load
# ine <- c("Barcelona.csv", "Girona.csv", "Lleida.csv", "Tarragona.csv")
# for (i in seq(ine)){
#   if (i == 1){
#     ine_df_list <- list()
#     ine_df <- read.delim(ine[i], sep = ";", encoding = "latin1")
#     ine_df <- ine_df %>%
#       mutate(CP = str_split_fixed(Municipios, " ", 2)[,1],
#              Municipio = str_split_fixed(Municipios, " ", 2)[,2]) %>% 
#       select(!(Municipios))
#     ine_df_list[[i]] <-  ine_df
#   } else {
#     ine_df <- read.delim(ine[i], sep = ";", encoding = "latin1")
#     ine_df <- ine_df %>%
#       mutate(CP = str_split_fixed(Municipios, " ", 2)[,1],
#              Municipio = str_split_fixed(Municipios, " ", 2)[,2]) %>% 
#       select(!(Municipios))
#     ine_df_list[[i]] <-  ine_df
#   }
# }
# 
# data_ine <- list_rbind(ine_df_list)
# write.table(data_ine, "data_ine.csv", sep = ",", 
#             col.names = T, row.names = F, quote = F,
#             fileEncoding = "latin1")

data_ine <- read.delim("data_ine_v2.csv", sep = ",", encoding = "latin1")
data_ine <- data_ine[-which(is.na(data_ine$Total)),]
## EDAR (SARS-CoV-2) data load
edar_meta <- read_excel("datos_municipios_sarsaigua.xlsx", sheet = "Edar")

## Intermediate data load
municipios_meta <- read_excel("datos_municipios_sarsaigua_v2.xlsx", sheet = "Municipios")

## Clinical data load
hosp_meta <- read_delim("regiones_hosp.csv")
data_hosp <- read_delim("Registre_de_casos_de_COVID-19_a_Catalunya_per__rea_b_sica_de_salut__ABS__i_sexe.csv")
data_edar <- read_delim("release.csv")

### Data cleaning and wrangling ###
data_edar$Fecha <-as.Date(str_split_fixed(data_edar$`id mostra`, "-", 2)[,2],
                          format = "%Y-%m-%d")
data_edar$week <- strftime(data_edar$Fecha, format = "%V %Y")

# Lo primero va a ser que voy a tratar de reducir el df de datos hosp
# para reducir el tiempo de cuantificación

data_hosp2 <- data_hosp %>% 
  mutate(Fecha = as.Date(TipusCasData, format = "%d/%m/%Y")) %>%
  mutate(week = strftime(Fecha, format = "%V %Y")) %>%
  select(week, ABSDescripcio, NumCasos) %>%
  group_by(week, ABSDescripcio) %>%
  summarise(casos = sum(NumCasos))

colnames(data_hosp2) <- c("Week", "Ubicacion", "Casos")
  
## regex test ##
# prueba <- c("hola 2-C", "bcn - 24", "bcn 5-e", "hola 2", "Llobregat-Llomera", "Reus-4", "Terrasa A", "Terrasa-B", "coso/ay")
# str_split_fixed(prueba, "\\s{0,1}\\d|\\s-|-\\d|\\s[a-zA-Z]{1}$|-[a-zA-Z]{1}$|/", 2)[,1]
################

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

data_merged <- left_join(data_hosp3, mun_meta_ine, by = c("Mun", "Year"))
data_merged_clean <- data_merged[-which(is.na(data_merged$EDAR)),]

## Ahora la unión con las EDAR que esto va a ser lo gracioso

data_merged_clean$EDAR <- str_replace_all(data_merged_clean$EDAR, ",", " ")
data_edar$depuradora <- str_replace_all(data_edar$depuradora, "_", " ")

data_edar_red <- data_edar[,c(2,3,4,5,6, 7, 8, 11, 12)]

colnames(data_edar_red) <- c("EDAR", "N1", "N2",
                             "IP4", "E", "caudal",
                             "lluvia", "Fecha", "Week")

model_data <- inner_join(data_merged_clean,
           data_edar_red,
           by = c("EDAR", "Week"))

ggplot(model_data) + 
  geom_point(aes(x = Ncasos, y = N1), color = "red") +
  geom_point(aes(x = Ncasos, y = N2), color = "blue") +
  geom_point(aes(x = Ncasos, y = IP4), color = "green") +
  geom_point(aes(x = Ncasos, y = E), color = "#B4234F")



model_data %>% pivot_longer(!c(Week, Mun, EDAR, Year, Fecha), names_to = "Variable") %>%
  ggplot(aes(x = Variable, y = value)) + 
           geom_boxplot() + 
           facet_grid(.~Variable, scales = "free_y")


model_data2 <- model_data %>% 
  mutate(
    N1_log = log(N1),
    N2_log = log(N2),
    IP4_log = log(IP4),
    E_log = log(E)
  ) %>%
  select(!c(N1,N2,IP4,E))

model_data2 %>% pivot_longer(!c(Week, Mun, EDAR, Year, Fecha), names_to = "Variable") %>%
  ggplot(aes(x = Variable, y = value)) + 
  geom_boxplot() + 
  facet_grid(.~Variable, scales = "free_y")  

model_data2 %>% pivot_longer(!c(Week, Mun, EDAR, Year, Fecha), names_to = "Variable") %>%
  filter(Variable == "E_log") %>%
  ggplot(aes(x = value)) + 
  geom_density(color = "black", fill = "lightblue", alpha = .5)

model_data3 <- model_data %>% 
  mutate(
    N1_norm = N1/nHab,
    N2_norm = N2/nHab,
    IP4_norm = IP4/nHab,
    E_norm = E/nHab
  ) %>%
  select(!c(N1,N2,IP4,E))


model_data3 %>% pivot_longer(!c(Week, Mun, EDAR, Year, Fecha), names_to = "Variable") %>%
  filter(Variable == "N1_norm") %>%
  ggplot(aes(x = value)) + 
  geom_density(color = "black", fill = "lightblue", alpha = .5)

model_data4 <- model_data %>% 
  mutate(
    N1_norm = log(N1 + 1/nHab),
    N2_norm = log(N2+1/nHab),
    IP4_norm = log(IP4+1/nHab),
    E_norm = log(E+1/nHab),
    Ncasos_norm = log(Ncasos+1)
  ) 
  select(!c(N1,N2,IP4,E))


model_data4 %>% pivot_longer(!c(Week, Mun, EDAR, Year, Fecha), names_to = "Variable") %>%
  filter(Variable == "N1") %>%
  ggplot(aes(x = log(value))) + 
  geom_density(color = "black", fill = "lightblue", alpha = .5)



model_data5 <- model_data4 %>%
  select( Ncasos_norm, N1_norm) %>%
  na.omit()

seed = 1234

training_indices <- sample(nrow(model_data5), nrow(model_data5)*.5)
training_data <- model_data5[training_indices, ]
testing_data <- model_data5[-training_indices, ]


rf.fit <- randomForest(formula = Ncasos_norm ~ N1_norm, data = training_data,
                       ntree = 1000, importance = T)

predictions <- predict(rf.fit, newdata = testing_data)
rmse <- rmse(predictions, testing_data$Ncasos_norm) # 1.34
r2 <- cor(predictions, testing_data$Ncasos_norm)^2

e**rmse
rmse

model_data6 <- model_data4 %>%
  replace(is.na(.), 0)

training_indices <- sample(nrow(model_data5), nrow(model_data6)*.8)
training_data <- model_data6[training_indices, ]
testing_data <- model_data6[-training_indices, ]

rf2.fit <- randomForest(formula = Ncasos_norm ~ N1_norm + N2_norm + IP4_norm + E_norm + 
                          N1_norm:lluvia + N2_norm:lluvia + IP4_norm:lluvia + E_norm:lluvia +
                          N1_norm:caudal + N2_norm:caudal + IP4_norm:caudal + E_norm:caudal,
                        data = training_data,
                        ntree = 1000, 
                        importance = T)

predictions <- predict(rf.fit, newdata = testing_data)
rmse <- rmse(predictions, testing_data$Ncasos_norm)
r2 <- cor(predictions, testing_data$Ncasos_norm)^2

rmse_casos <- e**rmse
r2

### Visualización de la importancia de cada variable

ImpData <- as.data.frame(importance(rf2.fit))
ImpData$Var.Names <- row.names(ImpData)

ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )


testing_data$prediction = predictions
testing_data$denorm_pred = e**predictions

ggplot(data = testing_data, aes(x = prediction, y = Ncasos_norm)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F) + 
  annotate("text", x = min(testing_data$prediction), y = max(testing_data$Ncasos_norm),
            label = paste0("R² = ", round(r2, 2))) +
  theme_bw()

r22 = cor(testing_data$denorm_pred, testing_data$Ncasos)**2
ggplot(data = testing_data, aes(x = denorm_pred, y = Ncasos)) + 
  geom_point() +
  geom_smooth(method = "lm", se = F) + 
  annotate("text", x = min(testing_data$denorm_pred), y = max(testing_data$Ncasos),
           label = paste0("R² = ", round(r22, 2))) +
  theme_bw()

save.image()


# 23/10

library(glmnet)

training_indices <- sample(nrow(model_data6), nrow(model_data6)*.8)
training_data <- model_data6[training_indices, ]
testing_data <- model_data6[-training_indices, ]



lambdas <- 10^seq(2, -3, by = -.1)
cv_ridge <- cv.glmnet(x, y_train, alpha = 0, lambda = lambdas)

## que le peten a esto, voy a intentar hacer algo de viz

library(ggridges)
top_mun <- model_data %>% arrange(desc(nHab))
top_mun_list = unlist(unique(data.frame(top_mun[,"Mun", drop = T])))[1:10]

model_data_viz <- model_data[model_data$Mun %in% top_mun_list,] 
model_data_viz <- model_data_viz %>% arrange(desc(nHab))
model_data_viz$Mun = factor(model_data_viz$Mun, levels = unique(model_data_viz$Mun))

ggplot(data = model_data_viz, aes(x = Fecha, y = Mun, group = Mun, height = log(E+1), fill = Mun)) +
  geom_density_ridges(stat = "identity")

save.image(file = "RData2310")
