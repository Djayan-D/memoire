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
library(dtw)
library(dplyr)
library(tsoutliers)




#----- 1.2. Charger les données -----

data <- read_excel("data/donnees.xlsx")

noms <- c("date",
          "ipc_energie",
          "c_charbon",
          "c_brent",
          "c_gaz",
          "c_uranium",
          "ipi",
          "cons_elec",
          "prod_elec_nuc",
          "prod_elec_fos",
          "prod_elec_enr",
          "tx_dolleuro",
          "ipc_transport",
          "co2",
          "temperature",
          "saison")

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

data_train <- data_infl[1:156,] # [2010-2022]
data_test <- data_infl[157:168,] # 2023










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
  cor(method = "spearman", use = "complete.obs")


# Représenter

corrplot(corr_matrix, 
         method = "circle",
         type = "upper",
         col = colorRampPalette(c("blue", "white", "red"))(200),
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

#----- FONCTION 1 : Tracer le graphique inflation réelle vs prédite -----

plot_result <- function(resultats, nom_methode = deparse(substitute(resultats))) {
  
  ggplot(resultats, aes(x = date)) +
    geom_line(aes(y = infl_reelle, color = "Inflation réelle"), size = 1) +
    geom_line(aes(y = infl_predite, color = "Inflation prédite"), 
              size = 1, linetype = "dashed") +
    scale_color_manual(values = c("Inflation réelle" = "blue", "Inflation prédite" = "red")) +
    labs(title = "Comparaison des valeurs réelles et prédites de l'inflation",
         subtitle = paste("Méthode :", nom_methode),
         x = "Date", 
         y = "Inflation", 
         color = "Légende") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#----- FIN FONCTION 1 -----

plot_result(result_rf_class, "Random Forest Classique")




#----- 3.3. Évaluer le modèle -----

# Calculer l'erreur quadratique moyenne

mse_rf_class <- mean((result_rf_class$infl_predite - result_rf_class$infl_reelle)^2)
mse_rf_class


# Calculer le RMSE

rmse_rf_class <- sqrt(mse_rf_class)
rmse_rf_class



#----- 3.4. Voir la variabilité des prédicitons -----

# Paramètres

nb_simulation <- 100
liste_pred_rf_class <- list()


# Simuler et prédire

for (i in 1:nb_simulation) {
  set.seed(100 + i)
  model <- randomForest(infl_energie ~ . - date, data = data_train)
  pred <- predict(model, newdata = data_test)
  
  liste_pred_rf_class[[i]] <- data.frame(
    date = data_test$date,
    pred = pred,
    iteration = paste0("RF_", i)
  )
}


# Fusionner dans un seul data frame

pred_rf_class_multi <- bind_rows(liste_pred_rf_class)


# Représenter les simulation, la seed 123 et les valeurs réelles sur une seul graphique

#----- FONCTION 2 : Voir la variabilité des prédictions -----

plot_result_var <- function(diminutif_methode, nom_methode = diminutif_methode) {
  
  # Construire les noms pour récupérer les dataframes
  nom_multi <- paste0("pred_", diminutif_methode, "_multi")
  nom_result <- paste0("result_", diminutif_methode)
  
  # Récupérer les dataframes depuis l'environnement global
  df_multi <- get(nom_multi, envir = .GlobalEnv)
  df_result <- get(nom_result, envir = .GlobalEnv)
  
  # Construire le graphique
  ggplot() +
    # Prévisions multiples
    geom_line(data = df_multi, aes(x = date, y = pred, group = iteration, color = "Prévisions multiples (seeds aléatoires)"), 
              alpha = 0.3, size = 0.8) +
    
    # Prévision principale
    geom_line(data = df_result, aes(x = date, y = infl_predite, color = "Prévision principale (seed = 123)"), 
              size = 1.2) +
    
    # Série réelle
    geom_line(data = df_result, aes(x = date, y = infl_reelle, color = "Inflation réelle"), 
              size = 1.2) +
    
    scale_color_manual(values = c(
      "Prévisions multiples (seeds aléatoires)" = "#ffb8b8",
      "Prévision principale (seed = 123)" = "red",
      "Inflation réelle" = "blue"
    )) +
    
    labs(
      title = "Variabilité des prévisions",
      subtitle = paste("Méthode :", nom_methode, 
                       "\nNombre de séries supplémentaires simulées :", nrow(df_multi)/12 ),
      x = "Date", y = "Inflation prédite",
      color = "Légende"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#----- FIN FONCTION 2 -----

plot_result_var("rf_class", "Random Forest Classique")











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

plot_result(result_rf_pond, "Random Forest Pondéré")





#----- 4.3. Évaluer le modèle ----------

# Calculer l'erreur quadratique moyenne

mse_rf_pond <- mean((result_rf_pond$infl_predite - result_rf_pond$infl_reelle)^2)
mse_rf_pond



# Calculer le RMSE

rmse_rf_pond <- sqrt(mse_rf_pond)
rmse_rf_pond



#----- 4.4. Voir la variabilité des prédictions -----

# Paramètres

nb_simulation <- 100
liste_pred_rf_pond <- list()


# Simuler et prédire

for (i in 1:nb_simulation) {
  set.seed(100 + i)
  model <- randomForest(infl_energie ~ . - date, 
                        data = data_train,
                        weights = poids,
                        importance = TRUE)
  
  pred <- predict(model, newdata = data_test)
  
  liste_pred_rf_pond[[i]] <- data.frame(
    date = data_test$date,
    pred = pred,
    iteration = paste0("RF_pond_", i)
  )
}


# Fusionner dans un seul data frame

pred_rf_pond_multi <- bind_rows(liste_pred_rf_pond)


# Représenter les simulation, la seed 123 et les valeurs réelles sur une seul graphique

plot_result_var("rf_pond", "Random Forest Pondéré")










#----- 5. MÉTHODE 3 : RANDOM FOREST "ROLLING WINDOW vraies valeurs" ----------

#----- 5.1. Définir les paramètres -----

fenetre <- 156  # Taille de la fenêtre (2010-2022)

result_rf_roll_reel <- data.frame(date = NULL, infl_reelle = NULL, infl_predite = NULL)

data_train_roll <- data_train[(156 - fenetre + 1):156, ]



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
  
  pred_rf_roll_reel <- predict(model_rf_roll, newdata = data_test_roll)
  
  
  # Sauvegarder le résultat
  
  result_rf_roll_reel <- rbind(result_rf_roll_reel, 
                          data.frame(date = data_test_roll$date,
                                     infl_reelle = data_test_roll$infl_energie,
                                     infl_predite = pred_rf_roll_reel,
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

plot_result(result_rf_roll_reel, "Random Forest Rolling Window avec valeurs réelles")





#----- 5.4. Évaluer le modèle ----------

# Calculer l'erreur quadratique moyenne

mse_rf_roll_reel <- mean((result_rf_roll_reel$infl_predite - result_rf_roll_reel$infl_reelle)^2)
mse_rf_roll_reel


# Calculer le RMSE

rmse_rf_roll_reel <- sqrt(mse_rf_roll_reel)
rmse_rf_roll_reel





#----- 5.5. Voir la variabilité des prédictions -----

# Paramètres

nb_simulation <- 100
liste_pred_rf_roll_reel <- list()


# Boucle sur les simulations

for (i in 1:nb_simulation) {
  
  set.seed(100 + i)
  
  # Réinitialiser les données d'entraînement
  data_train_roll_sim <- data_train[(156 - fenetre + 1):156, ]
  
  # Pour stocker les prédictions de cette simulation
  pred_sim <- data.frame()
  
  # Rolling sur les 12 mois
  for (j in 1:12) {
    
    data_test_roll <- data_test[j, , drop = FALSE]
    
    # Modèle
    model_rf_roll_sim <- randomForest(infl_energie ~ . - date, 
                                      data = data_train_roll_sim)
    
    # Prédiction
    pred_j <- predict(model_rf_roll_sim, newdata = data_test_roll)
    
    # Stocker la prédiction
    pred_sim <- rbind(pred_sim, data.frame(
      date = data_test_roll$date,
      pred = pred_j,
      iteration = paste0("RF_roll_", i)
    ))
    
    # Mise à jour de la fenêtre
    data_train_roll_sim <- rbind(data_train_roll_sim, data_test_roll)
    
    if (nrow(data_train_roll_sim) > fenetre) {
      data_train_roll_sim <- tail(data_train_roll_sim, fenetre)
    }
    
    # Progression
    print(sprintf("%.2f %%", 100 * (i - 1 + j / 12) / nb_simulation))
  }
  
  # Ajouter les 12 prédictions de cette simulation à la liste
  liste_pred_rf_roll_reel[[i]] <- pred_sim
}


# Fusionner dans un seul data frame

pred_rf_roll_reel_multi <- bind_rows(liste_pred_rf_roll_reel)


# Représenter les simulation, la seed 123 et les valeurs réelles sur une seul graphique

plot_result_var("rf_roll_reel", "Random Forest Rolling Window avec valeurs réelles")




























#----- 6. MÉTHODE 4 : RANDOM FOREST "ROLLING WINDOW valeurs prédites" ----------

#----- 6.1. Définir les paramètres -----

fenetre <- 100  # Taille de la fenêtre (2010-2022)

result_rf_roll_pred <- data.frame(date = NULL, infl_reelle = NULL, infl_predite = NULL)

data_train_roll <- data_train[(156 - fenetre + 1):156, ]



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
  
  pred_rf_roll_pred <- predict(model_rf_roll, newdata = data_test_roll)
  
  
  # Sauvegarder le résultat
  
  result_rf_roll_pred <- rbind(result_rf_roll_pred, 
                               data.frame(date = data_test_roll$date,
                                          infl_reelle = data_test_roll$infl_energie,
                                          infl_predite = pred_rf_roll_pred,
                                          stringsAsFactors = FALSE))
  
  
  # Construire une nouvelle observation à ajouter à l'entraînement
  
  new_row <- data_test_roll
  new_row$infl_energie <- pred_rf_roll_pred # On ajoute la valeur prédite (remplacer la colonne infl_energie par la prédite)
  
  
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

plot_result(result_rf_roll_pred, "Random Forest Rolling Window avec valeurs prédites")





#----- 6.4. Évaluer le modèle ----------

# Calculer l'erreur quadratique moyenne

mse_rf_roll_pred <- mean((result_rf_roll_pred$infl_predite - result_rf_roll_pred$infl_reelle)^2)
mse_rf_roll_pred


# Calculer le RMSE

rmse_rf_roll_pred <- sqrt(mse_rf_roll_pred)
rmse_rf_roll_pred





#----- 6.4. Voir la variabilité des prédicitons -----

# Paramètres

nb_simulation <- 100
liste_pred_rf_roll_pred <- list()


# Boucle sur les simulations

for (i in 1:nb_simulation) {
  
  set.seed(100 + i)
  
  # Réinitialiser les données d'entraînement
  data_train_roll_sim <- data_train[(156 - fenetre + 1):156, ]
  
  # Pour stocker les prédictions de cette simulation
  pred_sim <- data.frame()
  
  # Rolling sur les 12 mois
  for (j in 1:12) {
    
    data_test_roll <- data_test[j, , drop = FALSE]
    
    # Modèle
    model_rf_roll_sim <- randomForest(infl_energie ~ . - date, 
                                      data = data_train_roll_sim)
    
    # Prédiction
    pred_j <- predict(model_rf_roll_sim, newdata = data_test_roll)
    
    # Stocker la prédiction
    pred_sim <- rbind(pred_sim, data.frame(
      date = data_test_roll$date,
      pred = pred_j,
      iteration = paste0("RF_roll_", i)
    ))
    
    # Mise à jour de la fenêtre AVEC la valeur PRÉDITE
    new_row <- data_test_roll
    new_row$infl_energie <- pred_j
    
    data_train_roll_sim <- rbind(data_train_roll_sim, new_row)
    
    if (nrow(data_train_roll_sim) > fenetre) {
      data_train_roll_sim <- tail(data_train_roll_sim, fenetre)
    }
    
    # Progression
    print(sprintf("%.2f %%", 100 * (i - 1 + j / 12) / nb_simulation))
  }
  
  # Ajouter les 12 prédictions de cette simulation à la liste
  liste_pred_rf_roll_pred[[i]] <- pred_sim
}

# Fusionner dans un seul data frame

pred_rf_roll_pred_multi <- bind_rows(liste_pred_rf_roll_pred)

# Représenter les simulations, la seed 123 et les valeurs réelles sur un seul graphique

plot_result_var("rf_roll_pred", "Random Forest Rolling Window avec valeurs prédites")











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

pred_conf_80 <- predict(model_lm, newdata = data_test, interval = "confidence", level = 0.80)
pred_conf_90 <- predict(model_lm, newdata = data_test, interval = "confidence", level = 0.90)
pred_conf_95 <- predict(model_lm, newdata = data_test, interval = "confidence", level = 0.95)



#----- 7.4. Comparer les valeurs prédites et réelles ---

# Numérique

result_lm <- data.frame(
  date = data_test$date,
  infl_reelle = data_test$infl_energie,
  infl_predite = pred_conf_80[,1],
  low_80 = pred_conf_80[,2],
  up_80 = pred_conf_80[,3],
  low_90 = pred_conf_90[,2],
  up_90 = pred_conf_90[,3],
  low_95 = pred_conf_95[,2],
  up_95 = pred_conf_95[,3]
)

print(result_lm)



# Graphique

plot_result(result_lm, "Régréssion Linéaire Multiple")





#----- 7.5. Évaluer le modèle ----------

# Calculer l'erreur quadratique moyenne

mse_lm <- mean((result_lm$infl_predite - result_lm$infl_reelle)^2)
mse_lm


# Calculer le RMSE

rmse_lm <- sqrt(mse_lm)
rmse_lm





#----- 7.6. Voir la variabilité des prédicitons -----

ggplot(result_lm, aes(x = date)) +
  # Intervalle de confiance à 95 %
  geom_ribbon(aes(ymin = low_95, ymax = up_95, fill = "IC 95 %"), alpha = 0.2) +
  
  # Intervalle de confiance à 90 %
  geom_ribbon(aes(ymin = low_90, ymax = up_90, fill = "IC 90 %"), alpha = 0.4) +
  
  # Intervalle de confiance à 80 %
  geom_ribbon(aes(ymin = low_80, ymax = up_80, fill = "IC 80 %"), alpha = 0.6) +
  
  # Ligne de prévision
  geom_line(aes(y = infl_predite, color = "Prévision LM"), size = 1.2) +
  
  # Ligne réelle
  geom_line(aes(y = infl_reelle, color = "Inflation réelle"), size = 1.2) +
  
  # Couleurs manuelles
  scale_color_manual(values = c(
    "Prévision LM" = "red",
    "Inflation réelle" = "blue"
  )) +
  
  # Couleurs des IC
  scale_fill_manual(values = c(
    "IC 95 %" = "#ffb8b8",
    "IC 90 %" = "#ffb8b8",
    "IC 80 %" = "#ffb8b8"
  )) +
  
  labs(
    title = "Prévision de l'inflation énergétique avec une Régression Linéaire Multiple",
    subtitle = "Avec intervalles de confiance à 80 %, 90 % et 95 %",
    x = "Date", y = "Inflation (%)",
    color = "Courbes",
    fill = "Intervalle de confiance"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))










#----- 8. MÉTHODE 6 : FORECAST  ----------

#----- 8.1. Convertir les données en série temporelle -----

data_train_ts <- ts(data_train$infl_energie, start = c(2010, 02), frequency = 12)





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

forecast_arima <- forecast(model_arima, h = 12, level = c(80, 90, 95))



#----- 8.5.2. Comparer les valeurs prédites et réelles ----------

# Comparer les valeurs réelles et les prévisions

result_forecast <- data.frame(
  date = data_test$date,
  infl_reelle = data_test$infl_energie,
  infl_predite = forecast_arima$mean,
  low_80 = forecast_arima$lower[,1],
  up_80 = forecast_arima$upper[,1],
  low_90 = forecast_arima$lower[,2],
  up_90 = forecast_arima$upper[,2],
  low_95 = forecast_arima$lower[,3],
  up_95 = forecast_arima$upper[,3]
)


# Affichage des résultats

print(result_forecast)





#----- 8.6. Visualiser les résultats ----------

plot_result(result_forecast, "ARIMA")





#----- 8.7. Évaluer les modèles ----------

# Calculer l'erreur quadratique moyenne

mse_arima <- mean((result_forecast$infl_predite - result_forecast$infl_reelle)^2)
mse_arima


# Calculer le RMSE

rmse_arima <- sqrt(mse_arima)
rmse_arima



#----- 8.8. Voir la variabilité des prédicitons -----

ggplot(result_forecast, aes(x = date)) +
  # Intervalle de confiance à 95 %
  geom_ribbon(aes(ymin = low_95, ymax = up_95, fill = "IC 95 %"), alpha = 0.2) +
  
  # Intervalle de confiance à 90 %
  geom_ribbon(aes(ymin = low_90, ymax = up_90, fill = "IC 90 %"), alpha = 0.4) +
  
  # Intervalle de confiance à 80 %
  geom_ribbon(aes(ymin = low_80, ymax = up_80, fill = "IC 80 %"), alpha = 0.6) +
  
  # Ligne de prévision
  geom_line(aes(y = infl_predite, color = "Prévision ARIMA"), size = 1.2) +
  
  # Ligne réelle
  geom_line(aes(y = infl_reelle, color = "Inflation réelle"), size = 1.2) +
  
  # Couleurs manuelles
  scale_color_manual(values = c(
    "Prévision ARIMA" = "red",
    "Inflation réelle" = "blue"
  )) +
  
  # Couleurs des IC
  scale_fill_manual(values = c(
    "IC 95 %" = "#ffb8b8",
    "IC 90 %" = "#ffb8b8",
    "IC 80 %" = "#ffb8b8"
  )) +
  
  labs(
    title = "Prévision de l'inflation énergétique avec ARIMA",
    subtitle = "Avec intervalles de confiance à 80 %, 90 % et 95 %",
    x = "Date", y = "Inflation (%)",
    color = "Courbes",
    fill = "Intervalle de confiance"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))










#----- 9. COMPARER LES RÉSULTATS  ----------

#----- 9.1. Comparer directement -----

#----- 9.1.1. Comparer les valeurs brutes ---

# Numérique

results <- data.frame(
  date = data_test$date,
  infl_reelle = data_test$infl_energie,
  infl_predite_rf_class = pred_rf_class,
  infl_predite_rf_pond = pred_rf_pond,
  infl_predite_rw_reel = result_rf_roll_reel$infl_predite,
  infl_predite_rw_pred = result_rf_roll_pred$infl_predite,
  infl_predite_lm = result_lm$infl_predite,
  infl_predite_arima = forecast_arima$mean
)

results


# Graphique

ggplot(results, aes(x = date)) +
  geom_line(aes(y = infl_reelle, color = "Inflation réelle"), size = 1) +
  geom_line(aes(y = infl_predite_rf_class, color = "Prédictions RF Classique"), size = 1, linetype = "dashed") +
  geom_line(aes(y = infl_predite_rf_pond, color = "Prédictions RF Pondéré"), size = 1, linetype = "dashed") +
  geom_line(aes(y = infl_predite_rw_reel, color = "Prédictions RF RW réel"), size = 1, linetype = "dashed") +
  geom_line(aes(y = infl_predite_rw_pred, color = "Prédictions RF RW pred"), size = 1, linetype = "dashed") +
  geom_line(aes(y = infl_predite_lm, color = "Prédictions LM"), size = 1, linetype = "dashed") +
  geom_line(aes(y = infl_predite_arima, color = "Prédictions ARIMA"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Inflation réelle" = "blue",
                                "Prédictions RF Classique" = "green",
                                "Prédictions RF Pondéré" = "darkgreen",
                                "Prédictions RF RW réel" = "red",
                                "Prédictions RF RW pred" = "darkred",
                                "Prédictions LM" = "purple",
                                "Prédictions ARIMA" = "pink")) +
  labs(title = "Comparaison des valeurs réelles et prédites de l'inflation", 
       x = "Date", 
       y = "Inflation", 
       color = "Légende") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#----- 9.1.2. Comparer avec le RMSE ---

rmse <- data.frame(
  rf_class = rmse_rf_class,
  rf_pond = rmse_rf_pond,
  rw_reel = rmse_rf_roll_reel,
  rw_pred = rmse_rf_roll_pred,
  lm = rmse_lm,
  arima = rmse_arima
)

rmse



#----- 9.1.3. Tester avec Diebold Mariano ---

# Liste de toutes les erreurs des modèles

erreurs_models <- list(
  rf_class = results$infl_reelle - results$infl_predite_rf_class,
  rf_pond = results$infl_reelle - results$infl_predite_rf_pond,
  rw_reel = results$infl_reelle - results$infl_predite_rw_reel,
  rw_pred = results$infl_reelle - results$infl_predite_rw_pred,
  lm = results$infl_reelle - results$infl_predite_lm,
  arima = results$infl_reelle - results$infl_predite_arima
)


# Comparaison des paires de modèles

comparaison <- combn(names(erreurs_models), 2, simplify = FALSE)


# Test de Diebold-Mariano entre chaque paire de modèles

test_dw <- lapply(comparaison, function(models) {
  model1 <- models[1]
  model2 <- models[2]
  test_dm <- dm.test(erreurs_models[[model1]], erreurs_models[[model2]], alternative = "two.sided")
  return(c(model1 = model1, model2 = model2, DM_statistic = test_dm$statistic, p_value = test_dm$p.value))
})


# Résultats sous forme de data frame

results_test_dw <- do.call(rbind, test_dw)
results_test_dw <- as.data.frame(results_test_dw)


# Afficher les résultats

print(results_test_dw)



#----- 9.1.3. Tester avec DTW ---

# Liste des séries prédites

series_modeles <- list(
  rf_class = results$infl_predite_rf_class,
  rf_pond = results$infl_predite_rf_pond,
  rw_reel = results$infl_predite_rw_reel,
  rw_pred = results$infl_predite_rw_pred,
  lm = results$infl_predite_lm,
  arima = results$infl_predite_arima
)


# Référence : la série réelle

series_reelle <- results$infl_reelle


# Comparer les DTW distances entre la série réelle et chaque prédiction

dtw_distances <- lapply(names(series_modeles), function(model_name) {
  distance <- dtw(series_reelle, series_modeles[[model_name]], distance.only = TRUE)$distance
  return(c(model = model_name, dtw_distance = distance))
})


# Convertir en data frame

dtw_distances_df <- do.call(rbind, dtw_distances)
dtw_distances_df <- as.data.frame(dtw_distances_df)


# Convertir la colonne distance en numérique

dtw_distances_df$dtw_distance <- as.numeric(dtw_distances_df$dtw_distance)


# Afficher

print(dtw_distances_df)





#----- 9.2. Comparer après normalisation -----

#----- 9.2.1. Normaliser les séries ---

normaliser <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

results_norm <- results
results_norm$infl_reelle <- normaliser(results$infl_reelle)
results_norm$infl_predite_rf_class <- normaliser(results$infl_predite_rf_class)
results_norm$infl_predite_rf_pond <- normaliser(results$infl_predite_rf_pond)
results_norm$infl_predite_rw_reel <- normaliser(results$infl_predite_rw_reel)
results_norm$infl_predite_rw_pred <- normaliser(results$infl_predite_rw_pred)
results_norm$infl_predite_lm <- normaliser(results$infl_predite_lm)
results_norm$infl_predite_arima <- normaliser(results$infl_predite_arima)



#----- 9.2.2. Représenter après la normalisation ---

ggplot(results_norm, aes(x = date)) +
  geom_line(aes(y = infl_reelle, color = "Inflation réelle"), size = 1) +
  geom_line(aes(y = infl_predite_rf_class, color = "Prédictions RF Classique"), size = 1, linetype = "dashed") +
  geom_line(aes(y = infl_predite_rf_pond, color = "Prédictions RF Pondéré"), size = 1, linetype = "dashed") +
  geom_line(aes(y = infl_predite_rw_reel, color = "Prédictions RF RW réel"), size = 1, linetype = "dashed") +
  geom_line(aes(y = infl_predite_rw_pred, color = "Prédictions RF RW pred"), size = 1, linetype = "dashed") +
  geom_line(aes(y = infl_predite_lm, color = "Prédictions LM"), size = 1, linetype = "dashed") +
  geom_line(aes(y = infl_predite_arima, color = "Prédictions ARIMA"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Inflation réelle" = "blue",
                                "Prédictions RF Classique" = "green",
                                "Prédictions RF Pondéré" = "darkgreen",
                                "Prédictions RF RW réel" = "red",
                                "Prédictions RF RW pred" = "darkred",
                                "Prédictions LM" = "purple",
                                "Prédictions ARIMA" = "pink")) +
  labs(title = "Comparaison des valeurs réelles et prédites de l'inflation", 
       x = "Date", 
       y = "Inflation", 
       color = "Légende") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#----- 9.2.3. Évaluer les corrélations ---

correlations <- data.frame(
  modèle = c("RF class", "RF pondérée", "RW réel", "RW prédit", "Régression linéaire", "ARIMA"),
  correlation_spearman = c(
    cor(results$infl_reelle, results$infl_predite_rf_class, method = "spearman", use = "complete.obs"),
    cor(results$infl_reelle, results$infl_predite_rf_pond, method = "spearman", use = "complete.obs"),
    cor(results$infl_reelle, results$infl_predite_rw_reel, method = "spearman", use = "complete.obs"),
    cor(results$infl_reelle, results$infl_predite_rw_pred, method = "spearman", use = "complete.obs"),
    cor(results$infl_reelle, results$infl_predite_lm, method = "spearman", use = "complete.obs"),
    cor(results$infl_reelle, results$infl_predite_arima, method = "spearman", use = "complete.obs")
  )
)

print(correlations)



#----- 9.2.4. Tester avec DTW ---

# Normaliser la série réelle

series_reelle_norm <- normaliser(series_reelle)


# Normaliser toutes les séries prédites

series_models_norm <- lapply(series_modeles, normaliser)


# Calculer les distances DTW normalisées

dtw_distances_norm <- lapply(names(series_models_norm), function(model_name) {
  distance <- dtw(series_reelle_norm, series_models_norm[[model_name]], distance.only = TRUE)$distance
  return(c(model = model_name, dtw_distance = distance))
})


# Convertir en data frame

dtw_distances_norm_df <- do.call(rbind, dtw_distances_norm)
dtw_distances_norm_df <- as.data.frame(dtw_distances_norm_df)


# Convertir la colonne distance en numérique

dtw_distances_norm_df$dtw_distance <- as.numeric(dtw_distances_norm_df$dtw_distance)


# Affichage

print(dtw_distances_norm_df)
