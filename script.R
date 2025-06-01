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
          "tx_eurodoll",
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

data_infl <- cbind(
  date = data$date[-1],
  infl_energie = diff(log(data$ipc_energie))*100,
  data[-nrow(data), !(names(data) %in% c("date", "ipc_energie"))]
)

## Strcture :
## 1ère colonne : Date
## 2ème colonne : Inflation correspondant à la Date
## 3ème-16ème colonne : Variables du mois d'avant
##
## Objectif : 
## Permettre sur une même ligne l'inflation à prévoir et les données (valeurs 
## des variables) utilisées pour cela
##
## Exemple :
## Je suis à la ligne 1
##   - La date est "Février 2010"
##   - infl_energie → inflation de février 2010 (= entre fin janvier 2010 et fin 
##     février 2010 avec l'IPC-énergie)
##   - c_charbon est le cours du charbon en janvier 2010
##   - c_brent est le cours du pétrole en janvier 2010
##   ...
##
## Remarque :
## Il n'y a pas janvier 2010 car sinon cela supposerait d'avoir les valeurs des
## variables en décembre 2009. De la même façon, même si on ne le voit pas, les
## valeurs des variables en décembre 2023 ne sont pas utilisées et ont été 
## retirées) car sinon cela reviendrait à prédire janvier 2024.
##
## Résumé :
## À la date t, c'est l'inflation à t mais les variables à t-1





#----- 1.5. Séparer train (2010-2022) et test (2023)

data_train <- data_infl[1:155,] # [2010-2022]
data_test <- data_infl[156:167,] # 2023





#----- 1.6 Visualiser les séries

plot_all_vars <- function(data, title_prefix = "Évolution de") {

  vars <- names(data)[-c(1,16)]
  
  for (var in vars) {
    p <- ggplot(data, aes(x = date, y = .data[[var]])) +
      geom_line(color = "#0072B2", size = 1.2) +
      labs(title = paste(title_prefix, var),
           x = "Date",
           y = var) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank()
      )
    
    print(p)
  }
}


# Appliquer aux trois datasets

plot_all_vars(data_infl, title_prefix = "Inflation -")
plot_all_vars(data_train, title_prefix = "Train -")
plot_all_vars(data_test, title_prefix = "Test -")










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

plot_violin_horizontal(data_infl, "data_infl")
plot_violin_horizontal(data_train, "data_train")
plot_violin_horizontal(data_test, "data_test")








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

#----- FONCTION 2 : Calculer les statistiques de qualité du modèle -----

stats_methode <- function(diminutif_methode, prefixe = "result") {
  result_methode <- get(paste0(prefixe, "_", diminutif_methode))
  
  y_reel <- result_methode$infl_reelle
  y_pred <- result_methode$infl_predite
  
  # RMSE
  rmse <- sqrt(mean((y_pred - y_reel)^2))
  
  # MAE
  mae <- mean(abs(y_pred - y_reel))
  
  # Correlation (Spearman)
  correlation <- cor(y_pred, y_reel, method = "spearman")
  
  # DTW
  dtw <- dtw(y_pred, y_reel)$distance
  
  # R²
  ss_res <- sum((y_reel - y_pred)^2)
  ss_tot <- sum((y_reel - mean(y_reel))^2)
  r_squared <- 1 - (ss_res / ss_tot)
  
  # Biais
  biais <- mean(y_pred - y_reel)
  
  # sMAPE (Symmetric Mean Absolute Percentage Error)
  smape <- mean(2 * abs(y_pred - y_reel) / (abs(y_reel) + abs(y_pred))) * 100
  
  # MAS (Mean Absolute Scaled Error)
  naive_pred_annee <- data_infl$infl_energie[155] # valeur de Décembre 2022 (dernière valeur avant prédiction)
  naive_mae_annee <- mean(abs(naive_pred_annee - y_reel))
  mase_annee <- mae / naive_mae_annee
  
  naive_pred_mois <- data_infl$infl_energie[155:166] # valeurs de Décembre 2022 à Novembre 2023, décalage de 1 car prend la valeur précédente
  naive_mae_mois <- mean(abs(naive_pred_mois - y_reel))
  mase_mois <- mae / naive_mae_mois
  
  return(c(RMSE = rmse,
           MAE = mae,
           Corr = correlation,
           DTW = dtw,
           R2 = r_squared,
           Biais = biais,
           sMAPE = smape,
           MASE_annee = mase_annee,
           MASE_mois = mase_mois))
}

#----- FIN FONCTION 2 -----

stats_rf_class <- stats_methode("rf_class")
stats_rf_class





#----- 3.4. Voir la variabilité des prédicitons -----

# Paramètres

nb_simulation <- 1000
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
  
  # Progression
  print(sprintf("%.2f %%", 100 * (i - 1 + i / 12) / nb_simulation))
  
}


# Fusionner dans un seul data frame

pred_rf_class_multi <- bind_rows(liste_pred_rf_class)


# Représenter les simulation, la seed 123 et les valeurs réelles sur une seul graphique

#----- FONCTION 3 : Voir la variabilité des prédictions -----

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
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
}

#----- FIN FONCTION 3 -----

plot_result_var("rf_class", "Random Forest Classique")











#----- 4. MÉTHODE 2 : RANDOM FOREST "PONDÉRÉ" ----------

#----- 4.1. Entraîner le modèle -----

#----- 4.1.1. Définir les poids ---

poids <- exp(-(as.numeric(data_train$date) - max(as.numeric(data_train$date))))  # Exponentiellement dégressif



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

stats_rf_pond <- stats_methode("rf_pond")
stats_rf_pond





#----- 4.4. Voir la variabilité des prédictions -----

# Paramètres

nb_simulation <- 1000
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
  
  # Progression
  print(sprintf("%.2f %%", 100 * (i - 1 + i / 12) / nb_simulation))
}


# Fusionner dans un seul data frame

pred_rf_pond_multi <- bind_rows(liste_pred_rf_pond)


# Représenter les simulation, la seed 123 et les valeurs réelles sur une seul graphique

plot_result_var("rf_pond", "Random Forest Pondéré")










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

stats_rf_roll_reel <- stats_methode("rf_roll_reel")
stats_rf_roll_reel





#----- 5.5. Voir la variabilité des prédictions -----

# Paramètres

nb_simulation <- 1000
liste_pred_rf_roll_reel <- list()


# Boucle sur les simulations

for (i in 1:nb_simulation) {
  
  set.seed(100 + i)
  
  # Réinitialiser les données d'entraînement
  data_train_roll_sim <- data_train[(155 - fenetre + 1):155, ]
  
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

stats_rf_roll_pred <- stats_methode("rf_roll_pred")
stats_rf_roll_pred





#----- 6.4. Voir la variabilité des prédicitons -----

# Paramètres

nb_simulation <- 1000
liste_pred_rf_roll_pred <- list()


# Boucle sur les simulations

for (i in 1:nb_simulation) {
  
  set.seed(100 + i)
  
  # Réinitialiser les données d'entraînement
  data_train_roll_sim <- data_train[(155 - fenetre + 1):155, ]
  
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

stats_lm <- stats_methode("lm")
stats_lm





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

# Recherche et correction 1 : 4 outliers

ts_outliers_1 <- tso(data_train_ts, maxit.iloop = 20)
ts_outliers_1

plot(ts_outliers_1)

data_train_ts_corr_1 <- ts_outliers_1$yadj


# Recherche et correction 2 : 2 outliers

ts_outliers_2 <- tso(data_train_ts_corr_1, maxit.iloop = 20)
ts_outliers_2

plot(ts_outliers_2)

data_train_ts_corr_2 <- ts_outliers_2$yadj

# Recherche et correction 3 : 2 outliers

ts_outliers_3 <- tso(data_train_ts_corr_2, maxit.iloop = 20)
ts_outliers_3

plot(ts_outliers_3)

data_train_ts_corr_3 <- ts_outliers_3$yadj


# Recherche 4 : 1 outlier

ts_outliers_4 <- tso(data_train_ts_corr_3, maxit.iloop = 20)
ts_outliers_4

plot(ts_outliers_4)

data_train_ts_corr_4 <- ts_outliers_4$yadj


# Recherche 5 : 0 outlier

tso(data_train_ts_corr_4, maxit.iloop = 20)





#----- 8.3. Vérifier la stationnarité -----

# Test Dickey-Fuller

adf.test(data_train_ts_corr_4)  # p-value < 0.01 -> on rejette H0, la série est stationnaire


# Test KPSS

kpss.test(data_train_ts_corr_4) # p-value > 0.1 -> on accepte H0, la série est stationnaire





#----- 8.4. Entraîner le modèle ARIMA -----


# Entraîner le modèle ARIMA

model_arima <- auto.arima(data_train_ts_corr_4, stepwise = FALSE, approximation = FALSE)


# Résumé du modèle ARIMA

summary(model_arima)





#----- 8.5. Prédire avec le modèle ----------

#----- 8.5.1. Prédire sur les données de test (2023) -----

# Prédictions pour les 12 prochains mois avec ARIMA

forecast_arima <- forecast(model_arima, h = 12, level = c(80, 90, 95))



#----- 8.5.2. Comparer les valeurs prédites et réelles ----------

# Comparer les valeurs réelles et les prévisions

result_arima <- data.frame(
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

print(result_arima)





#----- 8.6. Visualiser les résultats ----------

plot_result(result_arima, "ARIMA")





#----- 8.7. Évaluer les modèles ----------

stats_arima <- stats_methode("arima")
stats_arima



#----- 8.8. Voir la variabilité des prédicitons -----

ggplot(result_arima, aes(x = date)) +
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
  geom_line(aes(y = infl_reelle, color = "Inflation réelle"), size = 1.2) +
  geom_line(aes(y = infl_predite_rf_class, color = "Prédictions RF Classique"), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = infl_predite_rf_pond, color = "Prédictions RF Pondéré"), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = infl_predite_rw_reel, color = "Prédictions RF RW réel"), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = infl_predite_rw_pred, color = "Prédictions RF RW pred"), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = infl_predite_lm, color = "Prédictions LM"), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = infl_predite_arima, color = "Prédictions ARIMA"), size = 1.2, linetype = "dashed") +
  scale_color_manual(values = c("Inflation réelle" = "blue",
                                "Prédictions RF Classique" = "#92ff92",
                                "Prédictions RF Pondéré" = "#23d923",
                                "Prédictions RF RW réel" = "#ffa6a6",
                                "Prédictions RF RW pred" = "#ff0909",
                                "Prédictions LM" = "#ffbdfb",
                                "Prédictions ARIMA" = "#f915eb")) +
  labs(title = "Comparaison des valeurs réelles et prédites de l'inflation", 
       x = "Date", 
       y = "Inflation", 
       color = "Légende") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#----- 9.1.2. Comparer avec les statistiques ---

stats_df <- rbind(
  rf_class = stats_rf_class,
  rf_pond = stats_rf_pond,
  rf_roll_pred = stats_rf_roll_pred,
  rf_roll_reel = stats_rf_roll_reel,
  lm = stats_lm,
  arima = stats_arima
)

stats_df



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





#----- 9.2. Comparer après normalisation -----

#----- 9.2.1. Normaliser les séries ---

normaliser <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}


# Créer des df de la bonne forme pour utiliser stats_methode et un df complet (results_norm)

methodes <- c("rf_class", "rf_pond", "rw_reel", "rw_pred", "lm", "arima")
results_norm <- results
results_norm$infl_reelle <- normaliser(results$infl_reelle)

for (m in methodes) {
  # Créer chaque dataframe results_norm_m avec infl_reelle et infl_predite normalisés
  temp <- results[, c("infl_reelle", paste0("infl_predite_", m))]
  temp$infl_reelle <- normaliser(temp$infl_reelle)
  temp[[paste0("infl_predite_", m)]] <- normaliser(temp[[paste0("infl_predite_", m)]])
  # Renommer la colonne infl_predite spécifique en "infl_predite" pour faciliter le traitement
  colnames(temp)[2] <- "infl_predite"
  assign(paste0("results_norm_", m), temp)
  
  # Ajouter dans results_norm la colonne normalisée infl_predite correspondante
  results_norm[[paste0("infl_predite_", m)]] <- temp$infl_predite
}







#----- 9.2.2. Représenter après la normalisation ---

ggplot(results_norm, aes(x = date)) +
  geom_line(aes(y = infl_reelle, color = "Inflation réelle"), size = 1.2) +
  geom_line(aes(y = infl_predite_rf_class, color = "Prédictions RF Classique"), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = infl_predite_rf_pond, color = "Prédictions RF Pondéré"), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = infl_predite_rw_reel, color = "Prédictions RF RW réel"), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = infl_predite_rw_pred, color = "Prédictions RF RW pred"), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = infl_predite_lm, color = "Prédictions LM"), size = 1.2, linetype = "dashed") +
  geom_line(aes(y = infl_predite_arima, color = "Prédictions ARIMA"), size = 1.2, linetype = "dashed") +
  scale_color_manual(values = c("Inflation réelle" = "blue",
                                "Prédictions RF Classique" = "#92ff92",
                                "Prédictions RF Pondéré" = "#23d923",
                                "Prédictions RF RW réel" = "#ffa6a6",
                                "Prédictions RF RW pred" = "#ff0909",
                                "Prédictions LM" = "#ffbdfb",
                                "Prédictions ARIMA" = "#f915eb")) +
  labs(title = "Comparaison des valeurs réelles et prédites de l'inflation après normalisation", 
       x = "Date", 
       y = "Inflation", 
       color = "Légende") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






#----- 9.2.3. Comparer avec les statistiques ---

stats_df_norm <- rbind(
  rf_class = stats_methode("rf_class", "results_norm"),
  rf_pond = stats_methode("rf_pond", "results_norm"),
  rf_roll_pred = stats_methode("rw_reel", "results_norm"),
  rf_roll_reel = stats_methode("rw_pred", "results_norm"),
  lm = stats_methode("lm", "results_norm"),
  arima = stats_methode("arima", "results_norm")
)

stats_df_norm






