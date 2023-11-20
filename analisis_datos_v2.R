library(ggplot2)
library(readxl)
library(stringr)
library(randomForest)
library(glm)
library(MASS)
library(Metrics)
library(glmnet)
library(tidyverse)
library(dplyr)
library(naniar)
library(zoo)
library(ggridges)
library(cowplot)
library(GGally)

setwd("D:/Usuarios/rrrei/Desktop/doctorado") # casa
setwd("/home/ruben/Trabajo/modelo_predictivo/2paper/git/sars-cov-model-bcn") # GO-lab
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
  mutate(Year = str_split_fixed(Week, " ", 2)[,1])


## Ahora la unión con las EDAR que esto va a ser lo gracioso
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


data_edar_red$sars2 <- 
  pmax(
       data_edar_red$N2,
       data_edar_red$IP4,
       data_edar_red$E,
       na.rm = T)

data_edar_red$sars <- 
  pmax(
    data_edar_red$N1,
    data_edar_red$N2,
    data_edar_red$IP4,
    data_edar_red$E,
    na.rm = T)

data_edar_red$sars2[which(is.na(data_edar_red$sars2))] = 0


new_edar_dfs <- list()
for (i in seq(edars)){
  edar <- edars[i]

  subdata <- filter(data_edar_red, EDAR == edar) %>%
    mutate(n1_fill = na.approx(N1),
           f_num = as.numeric(Fecha))
  model_n1 <- loess(n1_fill ~ f_num, data = subdata, span = .10)
  # Dejo N1 por si acaso
  subdata <- subdata %>%
    mutate(n1_loess = predict(model_n1),
           n1_loess = pmax(n1_loess, 0),
           n1_loess_15 = rollmean(n1_loess, k = 2, fill = NA),
           n1_loess_30 = rollmean(n1_loess, k = 4, fill = NA))
  model_sars <- loess(sars ~ f_num, data = subdata, span = .1)
  model_sars2 <- loess(sars2 ~ f_num, data = subdata, span = .1)
  # sars 1 (n1,n2,ip4,e)
  subdata <- subdata %>%
    mutate(sars_loess = predict(model_sars),
           sars_loess = pmax(sars_loess, 0),
           sars_loess_15 = rollmean(sars_loess, k = 2, fill = NA),
           sars_loess_30 = rollmean(sars_loess, k = 4, fill = NA))
  # SARS 2 (n2, ip4, e)
  subdata <- subdata %>%
    mutate(sars2_loess = predict(model_sars2),
           sars2_loess = pmax(sars2_loess, 0),
           sars2_loess_15 = rollmean(sars2_loess, k = 2, fill = NA),
           sars2_loess_30 = rollmean(sars2_loess, k = 4, fill = NA))

  new_edar_dfs[[i]] <- subdata
}

data_sars <- list_rbind(new_edar_dfs)

# gg_miss_upset(test)
# vis_miss(test)
# vis_miss(data_edar_red)


Muns <- unique(data_hosp3$Mun)

# Por algún motivo la semana 2022 52 está por ahí suelta, la voy a quitar
data_hosp3 <- data_hosp3[data_hosp3$Week != "2022 52",]
data_hosp3$Week <- factor(data_hosp3$Week, levels = unique(data_hosp3$Week))
Weeks_df <- data.frame(unique(data_hosp3$Week))
names(Weeks_df) <- "Week"
hosp_df_list <- list()
for (i in seq(Muns)){
  mun <- Muns[i]
  subdata <- filter(data_hosp3, Mun == mun)
  subdata_weeks <- left_join(Weeks_df, subdata, by = "Week")
  subdata_weeks <- subdata_weeks %>%
    mutate(Ncasos_lead = lead(Ncasos),
           Ncasos_lag = lag(Ncasos),
           Ncasos_lagroll_15 = rollmeanr(Ncasos_lag, k = 2, fill = NA),
           Ncasos_lagroll_30 = rollmeanr(Ncasos_lag, k = 4, fill = NA))
  subdata_weeks$Mun <- mun
  
  hosp_df_list[[i]] <- subdata_weeks
}

data_hosp4 <- list_rbind(hosp_df_list)
data_hosp4 %>% arrange(Week)


# Cambio de los datos de municipio para el merge con el data_hosp
# También voy a mergear con los datos del ine y así luego el merge va
# Con el número de habitantes
municipios_meta$Municipio <- toupper(municipios_meta$Municipio)
data_ine$Municipio <- toupper(data_ine$Municipio)


mun_meta_ine <- merge(municipios_meta, data_ine, by = "Municipio", all.x = T)
colnames(mun_meta_ine) <- c("Mun", "Codigo", "EDAR", "Year", "nHab", "CP")
mun_meta_ine <- select(mun_meta_ine, Mun, EDAR, Year, nHab)

data_merged <- left_join(data_hosp4, mun_meta_ine, by = c("Mun", "Year"))
data_merged_clean <- data_merged[-which(is.na(data_merged$EDAR)),]

model_data <- inner_join(data_merged_clean,
                         data_sars,
                         by = c("EDAR", "Week"))

model_data$Month = strftime(model_data$Fecha, format = "%m")

model_data_trim1 <- model_data[,c("Mun","Ncasos_lead", "Ncasos_lagroll_15", "Ncasos_lagroll_30",
                                  "nHab", "caudal", "lluvia", 
                                  "sars_loess", "sars_loess_15", "sars_loess_30", "Month")]
model_data_trim1 <- model_data_trim1 %>% na.omit(model_data_trim1)

training_indices <- sample(nrow(model_data_trim1), nrow(model_data_trim1)*.8)
training_data <- model_data_trim1[training_indices, ]
testing_data <- model_data_trim1[-training_indices, ]

rf.fit <- randomForest(formula = Ncasos_lead ~ (Ncasos_lagroll_15 + Ncasos_lagroll_30 + 
                                                sars_loess + sars_loess_15 + sars_loess_30):nHab +
                                                Month + caudal + lluvia,
                       data = training_data,
                       ntree = 1000, importance = T)

predictions <- predict(rf.fit, newdata = testing_data)

rmse_m <- rmse(testing_data$Ncasos_lead, predictions)
rmse_m
MAE_m <- mae(testing_data$Ncasos_lead, predictions) # MAE
MAE_m
MSE_m <- mse(testing_data$Ncasos_lead, predictions)
MSE_m
r2 <- cor(predictions, testing_data$Ncasos_lead)^2
r2



# y_test_mean = mean(testing_data$Ncasos_lead)
# tss = sum((testing_data$Ncasos_lead - y_test_mean)^2)
# rss = sum((testing_data$Ncasos_lead - predictions)^2)
# rsq = 1 - (rss/tss)

testing_data$predictions = predictions


ggplot(testing_data, aes(x = log(predictions), y = log(Ncasos_lead))) + 
  geom_point() + 
  geom_smooth(method = "lm")


ImpData <- as.data.frame(importance(rf.fit))
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

ggplot(ImpData, aes(x = Var.Names, y = `%IncMSE`, fill = IncNodePurity)) + 
  geom_bar(stat = "identity") +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="right",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

varImpPlot(rf.fit,
           sort = T,
           main="Variable Importance Plot")



# prueba con modelo con logtransform

model_data_trim2 <- model_data_trim1 %>%
  mutate(Ncasos_lead = log(Ncasos_lead),
         Ncasos_lagroll_15 = log(Ncasos_lagroll_15+1),
         Ncasos_lagroll_30 = log(Ncasos_lagroll_30+1),
         sars_loess = log(sars_loess+1),
         sars_loess_15 = log(sars_loess_15+1),
         sars_loess_30 = log(sars_loess_30+1))

training_indices <- sample(nrow(model_data_trim2), nrow(model_data_trim2)*.8)
training_data <- model_data_trim2[training_indices, ]
testing_data <- model_data_trim2[-training_indices, ]

rflog.fit <- randomForest(formula = Ncasos_lead ~ (Ncasos_lagroll_15 + Ncasos_lagroll_30 + 
                                                  sars_loess + sars_loess_15 + sars_loess_30):nHab +
                         Month + caudal + lluvia,
                       data = training_data,
                       ntree = 1000, importance = T)

predictions <- predict(rflog.fit, newdata = testing_data)

rmse_m <- rmse(testing_data$Ncasos_lead, predictions)
rmse_m
MAE_m <- mae(testing_data$Ncasos_lead, predictions) # MAE
MAE_m
MSE_m <- mse(testing_data$Ncasos_lead, predictions)
MSE_m
r2 <- cor(predictions, testing_data$Ncasos_lead)^2
r2


testing_data$predictions = predictions

### FIGURAS PERFORMANCE MODELO
F3A <- ggplot(testing_data, aes(x = predictions, y = Ncasos_lead)) + 
  geom_point() + 
  geom_smooth(method = "lm")

F3B <- ggplot(testing_data, aes(x = predictions, y = Ncasos_lead - predictions)) + 
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  coord_cartesian(ylim = c(-5, 5))

F3 <- plot_grid(F3A, F3B,
                labels = c("A", "B"))

ImpData <- as.data.frame(importance(rf.fit))
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

ggplot(ImpData, aes(x = reorder(Var.Names,`%IncMSE`), y = `%IncMSE`, fill = IncNodePurity)) + 
  geom_bar(stat = "identity") +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="right",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )


top_mun <- model_data %>% arrange(desc(nHab))
top_mun_list = unlist(unique(data.frame(top_mun[,"Mun", drop = T])))[1:5]
bottom_mun <- model_data %>% arrange(nHab)
bottom_mun_list = unlist(unique(data.frame(bottom_mun[,"Mun", drop = T])))[1:5]
selected_mun <- c(top_mun_list, bottom_mun_list)
model_data_viz <- model_data[model_data$Mun %in% selected_mun,] 
model_data_viz <- model_data_viz %>% arrange(desc(nHab))
model_data_viz$Mun = factor(model_data_viz$Mun, levels = unique(model_data_viz$Mun))



## FIGURA 1 ## 
F1A <- ggplot(data = model_data_viz, aes(x = Fecha, y = fct_rev(Mun), 
                                  group = Mun, height = log(sars_loess+1), 
                                  fill = log(nHab))) +
  geom_density_ridges_gradient(stat = "identity", scale = 1.5) +
  scale_fill_viridis_b(name = "Population", option = "C") + 
  guides(fill = FALSE)
F1B <- ggplot(data = model_data_viz, aes(x = Fecha, y = fct_rev(Mun), 
                                  group = Mun, height = log(Ncasos+1), 
                                  fill = log(nHab))) +
  geom_density_ridges_gradient(stat = "identity", scale = 1.5) +
  scale_fill_viridis_b(name = "Population", option = "C") +
  guides(fill = FALSE)

F1 <- plot_grid(F1A, F1B, labels = c("A", "B"))

# FIGURA sup
vis_miss(data_edar_red)
vis_miss(data_hosp4)

ggplot(model_data, aes(x = log(sars_loess+1), y = log(Ncasos_lead+1))) +
  geom_point()


# FIGURA 2

model_data_pairs <- model_data[,c("Ncasos", "sars_loess")]
model_data_pairs <- model_data_pairs %>%
  mutate(Ncasos_log = log(Ncasos+1),
         sars_loess_log = log(sars_loess+1))

F2A1 <- ggplot(model_data_pairs, aes(x = sars_loess)) +
  geom_density(color = "black", fill = "lightblue")
F2A2 <- ggplot(model_data_pairs, aes(x = Ncasos)) +
  geom_density(color = "black", fill = "lightblue")
F2A3 <- ggplot(model_data_pairs, aes(x = sars_loess, y = Ncasos)) +
  geom_point(alpha = .1)
F2A <- plot_grid(F2A1, F2A2, F2A3,
          nrow = 1)


F2B1 <- ggplot(model_data_pairs, aes(x = sars_loess_log)) +
  geom_density(color = "black", fill = "lightblue")
F2B2 <- ggplot(model_data_pairs, aes(x = Ncasos_log)) +
  geom_density(color = "black", fill = "lightblue")
F2B3 <- ggplot(model_data_pairs, aes(x = sars_loess_log, y = Ncasos_log)) +
  geom_point(alpha = .1)
F2B <- plot_grid(F2B1, F2B2, F2B3, 
                 nrow = 1)

F2 <- plot_grid(F2A,F2B,
                ncol = 1,
                labels = c("No transform", "Log transform"))

###





































# neural net
library(neuralnet)
model_data_trim3 <- model_data_trim1 %>% mutate(Month_n = as.numeric(Month)) 
training_indices <- sample(nrow(model_data_trim3), nrow(model_data_trim3)*.8)
training_data <- model_data_trim3[training_indices, ]
testing_data <- model_data_trim3[-training_indices, ]
nn <- neuralnet(formula = Ncasos_lead ~ Ncasos_lagroll_15 + Ncasos_lagroll_30 + 
                sars_loess + sars_loess_15 + sars_loess_30 + nHab +
                Month_n + caudal + lluvia,
                data = training_data,
                hidden = c(5,3),
                linear.output = TRUE)

nn.pr <- compute(nn, testing_data)
predictions <- nn.pr$net.result
rmse_m <- rmse(testing_data$Ncasos_lead, predictions)
rmse_m
MAE_m <- mae(testing_data$Ncasos_lead, predictions) # MAE
MAE_m
MSE_m <- mse(testing_data$Ncasos_lead, predictions)
MSE_m
r2 <- cor(predictions, testing_data$Ncasos_lead)^2
r2




testing_data$predictions = predictions
plot(nn)

ggplot(testing_data, aes(x = predictions, y = Ncasos_lead)) + 
  geom_point() + 
  geom_smooth(method = "lm")


ImpData <- as.data.frame(importance(rf.fit))
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


# xgboost
library(xgboost)

train_x <- data.matrix(training_data[, !names(training_data) %in% c("Mun", "Ncasos_lead")])
train_y <- data.matrix(training_data[, "Ncasos_lead"])
 
test_x <- data.matrix(testing_data[, !names(testing_data) %in% c("Mun", "Ncasos_lead")])
test_y <- data.matrix(testing_data[, "Ncasos_lead"])

xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

watchlist = list(train=xgb_train, test=xgb_test)

model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 1)

pred_y = predict(model, xgb_test)

mean((test_y - pred_y)^2) #mse - Mean Squared Error

caret::RMSE(test_y, pred_y) #rmse - Root Mean Squared Error


importance_matrix <- xgb.importance(names(train_x), model = model)
xgb.plot.importance(importance_matrix)
