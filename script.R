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

set.seed(123)

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










#----- 3. MÉTHODE 2 : RANDOM FOREST "PONDÉRÉ" ----------

#----- 3.1. Entraîner le modèle -----

#----- 3.1.1. Définir les poids ---

poids <- exp(-(as.numeric(data_train$date) - max(as.numeric(data_train$date))) / 365)  # Exponentiellement dégressif



#----- 3.1.2. Entraîner ---

set.seed(123)

model_rf_pond <- randomForest(infl_energie ~ . - date, 
                              data = data_train,
                              weights = poids,
                              importance = TRUE)



#----- 3.1.3. Visualiser l'importance des variables ---

# Numérique

importance(model_rf_pond)


# Graphiques

varImpPlot(model_rf_pond)





#----- 3.2. Prédire avec le modèle -----

#----- 3.2.1. Prédire sur les données de test (2023) ---

pred_rf_pond <- predict(model_rf_pond, newdata = data_test)



#----- 3.2.2. Comparer les valeurs prédites et réelles ---

# Numérique

result_rf_pond <- data.frame(date = data_test$date,
                             infl_reelle = data_test$infl_energie,
                             infl_predite = pred_rf_pond)

print(result_rf_pond)



# Graphique

ggplot(result_rf_pond, aes(x = date)) +
  geom_line(aes(y = infl_reelle, color = "Inflation réelle"), size = 1) +  # Courbe des données réelles
  geom_line(aes(y = infl_predite, color = "Prédictions RF"), size = 1, linetype = "dashed") +  # Courbe des prédictions
  scale_color_manual(values = c("Inflation réelle" = "blue", "Prédictions RF" = "red")) +
  labs(title = "Comparaison des valeurs réelles et prédites de l'inflation", 
       x = "Date", 
       y = "Inflation", 
       color = "Légende") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#----- 3.3. Évaluer le modèle ----------

# Calculer l'erreur quadratique moyenne

mse_rf_pond <- mean((result_rf_pond$infl_predite - result_rf_pond$infl_reelle)^2)
mse_rf_pond



# Calculer le RMSE

rmse_rf_pond <- sqrt(mse_rf_pond)
rmse_rf_pond










#----- 4. MÉTHODE 3 : RANDOM FOREST "ROLLING WINDOW vraies valeurs" ----------

#----- 4.1. Définir les paramètres -----

fenetre <- 155  # Taille de la fenêtre (2010-2022)

result_rf_roll_reel <- data.frame(date = NULL, infl_reelle = NULL, infl_predite = NULL)

data_train_roll <- data_train[(155 - fenetre + 1):155, ]



#----- 4.2. Réaliser la boucle ---

for (i in 1:12) {
  
  # Sélectionner le mois de test courant
  
  data_test_roll <- data_test[i, ]
  
  
  # Entraîner le modèle Random Forest sur la fenêtre
  
  set.seed(123)
  model_rf_roll <- randomForest(infl_energie ~ . - date, 
                                data = data_train_roll, 
                                importance = TRUE)
  
  
  # Prédire
  
  pred_rf_roll <- predict(model_rf_roll, newdata = data_test_roll)
  
  
  # Sauvegarder le résultat
  
  result_rf_roll_reel <- rbind(result_rf_roll_reel, 
                          data.frame(date = data_test_roll$date,
                                     infl_reelle = data_test_roll$infl_energie,
                                     infl_predite = pred_rf_roll,
                                     stringsAsFactors = FALSE))
  
  
  # Construire une nouvelle observation à ajouter à l'entraînement
  
  new_row <- data_test_roll # On ajoute la vraie valeur d'inflation
  
  
  # Ajouter la nouvelle observation dans la fenêtre d'entraînement
  
  data_train_roll <- rbind(data_train_roll, new_row)
  
  
  # Retirer la vieille ligne
  
  if (nrow(data_train_roll) > fenetre) {
    data_train_roll <- data_train_roll[(nrow(data_train_roll) - fenetre + 1):nrow(data_train_roll), ]
  }
}



#----- 4.3. Comparer les valeurs prédites et réelles -----

# Numérique

print(result_rf_roll_reel)


# Graphique

ggplot(result_rf_roll_reel, aes(x = date)) +
  geom_line(aes(y = infl_reelle, color = "Inflation réelle"), size = 1) +  # Courbe des données réelles
  geom_line(aes(y = infl_predite, color = "Prédictions RF"), size = 1, linetype = "dashed") +  # Courbe des prédictions
  scale_color_manual(values = c("Inflation réelle" = "blue", "Prédictions RF" = "red")) +
  labs(title = "Comparaison des valeurs réelles et prédites de l'inflation", 
       x = "Date", 
       y = "Inflation", 
       color = "Légende") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#----- 4.4. Évaluer le modèle ----------

# Calculer l'erreur quadratique moyenne

mse_rf_roll <- mean((result_rf_roll_reel$infl_predite - result_rf_roll_reel$infl_reelle)^2)
mse_rf_roll


# Calculer le RMSE

rmse_rf_roll <- sqrt(mse_rf_roll)
rmse_rf_roll










#----- 5. MÉTHODE 4 : RANDOM FOREST "ROLLING WINDOW valeurs prédites" ----------

#----- 5.1. Définir les paramètres -----

fenetre <- 130  # Taille de la fenêtre (2010-2022)

result_rf_roll_pred <- data.frame(date = NULL, infl_reelle = NULL, infl_predite = NULL)

data_train_roll <- data_train[(155 - fenetre + 1):155, ]



#----- 5.2. Réaliser la boucle ---

for (i in 1:12) {
  
  # Sélectionner le mois de test courant
  
  data_test_roll <- data_test[i, ]
  
  
  # Entraîner le modèle Random Forest sur la fenêtre
  
  set.seed(123)
  model_rf_roll <- randomForest(infl_energie ~ . - date, 
                                data = data_train_roll, 
                                importance = TRUE)
  
  
  # Prédire
  
  pred_rf_roll <- predict(model_rf_roll, newdata = data_test_roll)
  
  
  # Sauvegarder le résultat
  
  result_rf_roll_pred <- rbind(result_rf_roll_pred, 
                               data.frame(date = data_test_roll$date,
                                          infl_reelle = data_test_roll$infl_energie,
                                          infl_predite = pred_rf_roll,
                                          stringsAsFactors = FALSE))
  
  
  # Construire une nouvelle observation à ajouter à l'entraînement
  
  new_row <- data_test_roll
  new_row$infl_energie <- pred_rf_roll # On ajoute la valeur prédite (remplacer la colonne infl_energie par la prédite)
  
  
  # Ajouter la nouvelle observation dans la fenêtre d'entraînement
  
  data_train_roll <- rbind(data_train_roll, new_row)
  
  
  # Retirer la vieille ligne
  
  if (nrow(data_train_roll) > fenetre) {
    data_train_roll <- data_train_roll[(nrow(data_train_roll) - fenetre + 1):nrow(data_train_roll), ]
  }
}



#----- 5.3. Comparer les valeurs prédites et réelles -----

# Numérique

print(result_rf_roll_pred)


# Graphique

ggplot(result_rf_roll_pred, aes(x = date)) +
  geom_line(aes(y = infl_reelle, color = "Inflation réelle"), size = 1) +  # Courbe des données réelles
  geom_line(aes(y = infl_predite, color = "Prédictions RF"), size = 1, linetype = "dashed") +  # Courbe des prédictions
  scale_color_manual(values = c("Inflation réelle" = "blue", "Prédictions RF" = "red")) +
  labs(title = "Comparaison des valeurs réelles et prédites de l'inflation", 
       x = "Date", 
       y = "Inflation", 
       color = "Légende") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#----- 5.4. Évaluer le modèle ----------

# Calculer l'erreur quadratique moyenne

mse_rf_roll <- mean((result_rf_roll_pred$infl_predite - result_rf_roll_pred$infl_reelle)^2)
mse_rf_roll


# Calculer le RMSE

rmse_rf_roll <- sqrt(mse_rf_roll)
rmse_rf_roll
