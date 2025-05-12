#----- 1. ÉTAPES PRÉLIMINAIRES ----------

#----- 1.1. Charger les packages -----

library(readxl)
library(randomForest)
library(zoo)
library(ggplot2)
library(leaps)
library(corrplot)
library(forecast)
library(tseries)
library(psych)
library(patchwork)





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










#----- 2. STATISTIQUES DESCRIPTIVES ----------

#----- 2.1. Déterminer les statistiques univariées -----

#----- 2.1.1. Numériques ---

stat_num <- function(data) {
  num_vars <- names(data)[sapply(data, is.numeric)]
  
  stats_list <- lapply(num_vars, function(var) {
    x <- data[[var]]
    qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
    iqr <- qnt[2] - qnt[1]
    outliers <- sum(x < (qnt[1] - 1.5 * iqr) | x > (qnt[2] + 1.5 * iqr), na.rm = TRUE)
    
    descr <- psych::describe(x)
    
    return(data.frame(
      variable = var,
      moyenne = descr$mean,
      mediane = descr$median,
      ecart_type = descr$sd,
      min = descr$min,
      q25 = qnt[1],
      q75 = qnt[2],
      max = descr$max,
      skewness = descr$skew,
      kurtosis = descr$kurtosis,
      nb_na = sum(is.na(x)),
      nb_outliers = outliers
    ))
  })
  
  stats_df <- do.call(rbind, stats_list)
  rownames(stats_df) <- NULL
  return(stats_df)
}


stat_num(data_infl)
stat_num(data_train)
stat_num(data_test)



#----- 2.1.2. Graphiques ---

stat_graph <- function(data, dataset_name = "Dataset") {
  
  num_vars <- names(data)[sapply(data, is.numeric)]
  
  par(mfrow = c(2, 2))
  
  # Afficher tous les histogrammes
  for (var in num_vars) {
    hist(data[[var]], 
         main = paste0("Histogramme de ", var, " (", dataset_name, ")"), 
         xlab = var, 
         col = "lightblue", 
         border = "black")
  }
  
  
  # Afficher le barplot
  
  barplot(table(data$saison),
          main = paste0("Barplot de saison (", dataset_name, ")"),
          col = "lightgreen",
          border = "black",
          xlab = "Saison",
          ylab = "Fréquence")
  
  par(mfrow = c(1, 1))
  par(mfrow = c(2, 2))
  
  
  # Afficher tous les boxplots
  
  for (var in num_vars) {
    boxplot(data[[var]], 
            main = paste0("Boxplot de ", var, " (", dataset_name, ")"), 
            ylab = var, 
            col = "skyblue", 
            border = "black")
  }
  
  par(mfrow = c(1, 1))
  
}



stat_graph(data_infl, "data_infl")
stat_graph(data_train, "data_train")
stat_graph(data_test, "data_test")





#----- 2.2. Statistiques bivariées -----

#----- 2.2.1. Quantitatif - Quantitatif ---

# Calculer la matrice

corr_matrix <- data_infl[ , !(names(data_infl) %in% c("date", "saison"))] |> 
  cor(, use = "complete.obs")


# Représenter

corrplot(corr_matrix, 
         method = "circle",
         type = "upper",
         col = colorRampPalette(c("red", "white", "blue"))(200),
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.8)



#----- 2.2.2. Quantitatif - Qualitatif ---

plot_violin_horizontal <- function(data, dataset_name = "Dataset") {
  
  # Sélection des variables numériques
  
  num_vars <- names(data)[sapply(data, is.numeric)]
  
  
  # Boucle sur chaque variable quantitative
  
  for (var in num_vars) {
    p <- ggplot(data, aes(x = .data[[var]], y = saison)) +
      geom_violin(fill = "lightblue", color = NA, alpha = 0.6, trim = FALSE) +
      geom_boxplot(width = 0.1, fill = "darkblue", color = "black", alpha = 0.5, outlier.shape = NA) +
      geom_jitter(height = 0.1, color = "black", alpha = 0.3, size = 1) +
      labs(
        title = paste0("Distribution de ", var, " par saison (", dataset_name, ")"),
        x = var,
        y = "Saison"
      ) +
      theme_minimal()
    
    print(p)
  }
}










#----- 3. MÉTHODE 1 : RANDOM FOREST "CLASSIQUE" ----------

#----- 3.1. Entraîner le modèle -----

#----- 3.1.1. Entraîner ---

set.seed(123)

model_rf_class <- randomForest(infl_energie ~ . - date, 
                               data = data_train, 
                               importance = TRUE) # connaître l'importance de chaque variable

# changer nodesize ne change pas grand chose dans les résultats


#----- 3.1.2. Visualiser l'importance des variables ---

# Numérique

importance(model_rf_class)


# Graphiques

varImpPlot(model_rf_class)





#----- 3.2. Prédire avec le modèle -----

#----- 3.2.1. Prédire sur les données de test (2023) ---

pred_rf_class <- predict(model_rf_class, newdata = data_test)



#----- 3.2.2. Comparer les valeurs prédites et réelles ---

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





#----- 3.3. Évaluer le modèle ----------

# Calculer l'erreur quadratique moyenne

mse_rf_class <- mean((result_rf_class$infl_predite - result_rf_class$infl_reelle)^2)
mse_rf_class


# Calculer le RMSE

rmse_rf_class <- sqrt(mse_rf_class)
rmse_rf_class










#----- 4. MÉTHODE 2 : RANDOM FOREST "PONDÉRÉ" ----------

#----- 4.1. Entraîner le modèle -----

#----- 4.1.1. Définir les poids ---

poids <- exp(-(as.numeric(data_train$date) - max(as.numeric(data_train$date))) / 365)  # Exponentiellement dégressif



#----- 4.1.2. Entraîner ---

set.seed(123)

model_rf_pond <- randomForest(infl_energie ~ . - date, 
                              data = data_train,
                              weights = poids,
                              importance = TRUE)



#----- 4.1.3. Visualiser l'importance des variables ---

# Numérique

importance(model_rf_pond)


# Graphiques

varImpPlot(model_rf_pond)





#----- 4.2. Prédire avec le modèle -----

#----- 4.2.1. Prédire sur les données de test (2023) ---

pred_rf_pond <- predict(model_rf_pond, newdata = data_test)



#----- 4.2.2. Comparer les valeurs prédites et réelles ---

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





#----- 4.3. Évaluer le modèle ----------

# Calculer l'erreur quadratique moyenne

mse_rf_pond <- mean((result_rf_pond$infl_predite - result_rf_pond$infl_reelle)^2)
mse_rf_pond



# Calculer le RMSE

rmse_rf_pond <- sqrt(mse_rf_pond)
rmse_rf_pond










#----- 5. MÉTHODE 3 : RANDOM FOREST "ROLLING WINDOW vraies valeurs" ----------

#----- 5.1. Définir les paramètres -----

fenetre <- 155  # Taille de la fenêtre (2010-2022)

result_rf_roll_reel <- data.frame(date = NULL, infl_reelle = NULL, infl_predite = NULL)

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



#----- 5.3. Comparer les valeurs prédites et réelles -----

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





#----- 5.4. Évaluer le modèle ----------

# Calculer l'erreur quadratique moyenne

mse_rf_roll <- mean((result_rf_roll_reel$infl_predite - result_rf_roll_reel$infl_reelle)^2)
mse_rf_roll


# Calculer le RMSE

rmse_rf_roll <- sqrt(mse_rf_roll)
rmse_rf_roll










#----- 6. MÉTHODE 4 : RANDOM FOREST "ROLLING WINDOW valeurs prédites" ----------

#----- 6.1. Définir les paramètres -----

fenetre <- 130  # Taille de la fenêtre (2010-2022)

result_rf_roll_pred <- data.frame(date = NULL, infl_reelle = NULL, infl_predite = NULL)

data_train_roll <- data_train[(155 - fenetre + 1):155, ]



#----- 6.2. Réaliser la boucle ---

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



#----- 6.3. Comparer les valeurs prédites et réelles -----

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





#----- 6.4. Évaluer le modèle ----------

# Calculer l'erreur quadratique moyenne

mse_rf_roll <- mean((result_rf_roll_pred$infl_predite - result_rf_roll_pred$infl_reelle)^2)
mse_rf_roll


# Calculer le RMSE

rmse_rf_roll <- sqrt(mse_rf_roll)
rmse_rf_roll










#----- 7. MÉTHODE 5 : Régression linéaire multiple  ----------

#----- 7.1. Sélectionner les variables ---

# Appliquer la méthode best subset

model_bestsub <- regsubsets(infl_energie ~ . - date, data = data_train, nvmax = ncol(data_train) - 1)

summary_best <- summary(model_bestsub)


# Afficher les R² ajustés

summary_best$adjr2


# Sélectionner les variables d'après le R² ajusté

nb_variables_opt <- which.max(summary_best$adjr2)

variables_selectionnees <- names(coef(model_bestsub, nb_variables_opt))[-1]  # enlever l'intercept
variables_selectionnees


# Remplacer "saisonete", "saisonhiver" et "saisonprintemps" par "saison"

variables_selectionnees <- variables_selectionnees[!variables_selectionnees %in% c("saisonete", "saisonhiver", "saisonprintemps")]

variables_selectionnees <- unique(c(variables_selectionnees, "saison"))



#----- 7.2. Entraîner le modèle ---

# Définir la formule

formule <- as.formula(paste("infl_energie ~", paste(variables_selectionnees, collapse = " + ")))


# Entraîner le modèle linéaire

model_lm <- lm(formule, data = data_train)


# Résumé

summary(model_lm)





#----- 7.3. Prédire avec le modèle -----

pred_lm <- predict(model_lm, newdata = data_test)





#----- 7.4. Comparer les valeurs prédites et réelles ---

# Numérique

result_lm <- data.frame(
  date = data_test$date,
  infl_reelle = data_test$infl_energie,
  infl_predite = pred_lm
)

print(result_lm)


# Graphique

ggplot(result_lm, aes(x = date)) +
  geom_line(aes(y = infl_reelle, color = "Inflation réelle"), size = 1) +
  geom_line(aes(y = infl_predite, color = "Prédictions LM"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Inflation réelle" = "blue", "Prédictions LM" = "red")) +
  labs(title = "Comparaison des valeurs réelles et prédites (modèle linéaire)", 
       x = "Date", y = "Inflation", color = "Légende") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#----- 7.5. Évaluer le modèle ----------

# Calculer l'erreur quadratique moyenne

mse_lm <- mean((result_lm$infl_predite - result_lm$infl_reelle)^2)
mse_lm


# Calculer le RMSE

rmse_lm <- sqrt(mse_lm)
rmse_lm















#----- 8. MÉTHODE 6 : FORECAST  ----------

#----- 7.1. Convertir les données en série temporelle -----

data_train_ts <- ts(data_train$infl_energie, start = c(2010, 01), frequency = 12)





#----- 8.2. Retirer les outliers -----

# Recherche et correction 1 : 5 outliers

ts_outliers_1 <- tso(data_train_ts, maxit.iloop = 20)
ts_outliers_1

plot(ts_outliers_1)

data_train_ts_corr_1 <- ts_outliers_1$yadj


# Recherche et correction 2 : 4 outliers

ts_outliers_2 <- tso(data_train_ts_corr_1, maxit.iloop = 20)
ts_outliers_2

plot(ts_outliers_2)

data_train_ts_corr_2 <- ts_outliers_2$yadj

# Recherche et correction 3 : 2 outliers

ts_outliers_3 <- tso(data_train_ts_corr_2, maxit.iloop = 20)
ts_outliers_3

plot(ts_outliers_3)

data_train_ts_corr_3 <- ts_outliers_3$yadj


# Recherche 4 : 0 outlier

tso(data_train_ts_corr_3, maxit.iloop = 20)



#----- 8.3. Vérifier la stationnarité -----

# Test Dickey-Fuller

adf.test(data_train_ts_corr_3)  # p-value < 0.01 -> on rejette H0, la série est stationnaire


# Test KPSS

kpss.test(data_train_ts_corr_3) # p-value > 0.1 -> on accepte H0, la série est stationnaire





#----- 8.4. Entraîner le modèle ARIMA -----


# Entraîner le modèle ARIMA

model_arima <- auto.arima(data_train_ts_corr_3)


# Résumé du modèle ARIMA

summary(model_arima)





#----- 8.5. Prédire avec le modèle ----------

#----- 8.5.1. Prédire sur les données de test (2023) -----

# Prédictions pour les 12 prochains mois avec ARIMA

forecast_arima <- forecast(model_arima, h = 12)



#----- 8.5.2. Comparer les valeurs prédites et réelles ----------

# Comparer les valeurs réelles et les prévisions

result_forecast <- data.frame(
  date = data_test$date,
  infl_reelle = data_test$infl_energie,
  infl_predite = forecast_arima$mean
)


# Affichage des résultats

print(result_forecast)





#----- 8.6. Visualiser les résultats ----------


ggplot(result_forecast, aes(x = date)) +
  geom_line(aes(y = infl_reelle, color = "Inflation réelle"), size = 1) +
  geom_line(aes(y = infl_predite, color = "Prédictions ARIMA"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Inflation réelle" = "blue", 
                                "Prédictions ARIMA" = "red")) +
  labs(title = "Comparaison des valeurs réelles et prédites de l'inflation", 
       x = "Date", 
       y = "Inflation", 
       color = "Légende") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#----- 8.7. Évaluer les modèles ----------

# Calculer l'erreur quadratique moyenne

mse_arima <- mean((result_forecast$infl_predite - result_forecast$infl_reelle)^2)
mse_arima


# Calculer le RMSE

rmse_arima <- sqrt(mse_arima)
rmse_arima




