#----- 1. ÉTAPES PRÉLIMINAIRES ----------

#----- 1.1. Charger les packages -----

library(readxl)
library(randomForest)
library(zoo)
library(ggplot2)





#----- 1.2. Charger les données -----

data <- read_excel("data/donnees.xlsx")

noms <- c("date",
          "ipc_energie",
          "tx_eurdoll",
          "c_charbon",
          "c_brent",
          "c_gaz",
          "c_uranium",
          "ipi",
          "co2",
          "temperature",
          "saison",
          "cons_elec",
          "prod_elec_nuc",
          "prod_elec_fos",
          "prod_elec_enr",
          "ipc_transport")

names(data) <- noms





#----- 1.5. Attribuer le bon format -----

str(data)

data$date <- as.yearmon(data$date, "%Y-%m")
data$saison <- as.factor(data$saison)

str(data)





#----- 1.4. Transformer en différence première -----

data_infl <- data

data_infl$ipc_energie <- c(NA, diff(data_infl$ipc_energie))

data_infl <- data_infl[-1, ]  # retirer la première ligne devenue NA

names(data_infl)[names(data_infl) == "ipc_energie"] <- "infl_energie"





#----- 1.5. Séparer train (2010-2022) et test (2023)

data_train <- data_infl[1:155,] # [2010-2022]
data_test <- data_infl[156:167,] # 2023





#----- 2. MÉTHODE 1 : RANDOM FOREST "CLASSIQUE" ----------

#----- 2.1. Entraîner le modèle -----

#----- 2.1.1. Entraîner ---

model_rf_class <- randomForest(infl_energie ~ . - date, 
                               data = data_train, 
                               importance = TRUE) # connaître l'importance de chaque variable



#----- 2.1.2. Visualiser l'importance des variables ---

# Numérique

importance(model_rf_class)


# Graphiques

varImpPlot(model_rf_class)





#----- 2.2. Prédire avec le modèle -----

#----- 2.2.1. Prédire sur les données de test (2023) ---

pred_rf_class <- predict(model_rf_class, newdata = data_test)



#----- 2.2.2. Comparer les valeurs prédites et réelles ---

# Numérique

result_rf_class <- data.frame(date = data_test$date,
                              infl_reelle = data_test$infl_energie,
                              infl_predite = pred_rf_class)

print(result_rf_class)



# Graphique

ggplot(result_rf_class, aes(x = date)) +
  geom_line(aes(y = infl_reelle, color = "Inflation réelle"), size = 1) +  # Courbe des données réelles
  geom_line(aes(y = infl_predite, color = "Prédictions RF"), size = 1, linetype = "dashed") +  # Courbe des prédictions
  scale_color_manual(values = c("Inflation réelle" = "blue", "Prédictions RF" = "red")) +
  labs(title = "Comparaison des valeurs réelles et prédites de l'inflation", 
       x = "Date", 
       y = "Inflation", 
       color = "Légende") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#----- 2.3. Évaluer le modèle ----------

# Calculer l'erreur quadratique moyenne

mse_rf_class <- mean((result_rf_class$infl_predite - result_rf_class$infl_reelle)^2)
mse_rf_class



# Calculer le RMSE

rmse_rf_class <- sqrt(mse_rf_class)
rmse_rf_class

