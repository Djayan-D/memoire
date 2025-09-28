# Analyse et prévision de l’inflation énergétique en France (2010-2023)

**[Lien vers le mémoire complet](https://drive.google.com/file/d/1ZZ2wFh3iIVA3aviSY4xsYMFaEMLiHrfA/view?usp=sharing)**  
**[Lien vers la note de synthèse](https://drive.google.com/file/d/1gDLtkRr-KxcC9YNp5GDBLk72TRFEWAAC/view?usp=sharing)**  
**[Lien vers le tableau de bord Power BI](https://app.powerbi.com/view?r=eyJrIjoiMDUzNGZkZDUtMjIzYi00NmNjLTkyMTAtMjJmZGRjOTkzZTcxIiwidCI6IjcwYzYxNjVjLWFlNDktNDg0My1hODhlLTZiOTY5OWViYTBkMCJ9)**

---
## Contexte et objectifs
Ce projet s’inscrit dans le cadre d’un mémoire de **Master 1 Économétrie et Statistique** (parcours économétrie appliquée). L’objectif principal est d’**identifier les déterminants de l’inflation énergétique en France entre 2010 et 2023** et d’évaluer la capacité prédictive d’un modèle **Random Forest** par rapport à des approches traditionnelles (régression linéaire, ARIMA).

L’inflation énergétique, en tant que composante volatile et déterminante de l’inflation globale, a connu des fluctuations majeures ces dernières années, notamment sous l’effet de crises géopolitiques (guerre en Ukraine), de transitions énergétiques et de chocs d’offre/demande post-COVID. Ce travail propose une **analyse empirique approfondie** des variables explicatives et une **modélisation prédictive** pour anticiper l’évolution de cette inflation.

---
## Données
### Sources
Les données proviennent de sources institutionnelles :
- **INSEE** (IPC, IPI, température)
- **Eurostat** (prix des quotas CO₂, taux de change)
- **IEA** (production/consommation d’énergie)
- **RTE** (production électrique par source)
- **Trading Economics** (cours du charbon, Brent, gaz naturel, uranium)

### Variables clés
| Catégorie                | Variable                          | Description                                                                 |
|--------------------------|-----------------------------------|-----------------------------------------------------------------------------|
| **Variable cible**       | Inflation énergétique             | Taux de variation mensuel de l’IPCénergie (log-différence).               |
| **Prix des ressources**  | Charbon, Brent, Gaz naturel, Uranium | Cours mensuels en \$/tonne, €/baril, €/MWh, €/lb.                          |
| **Offre/Demande**        | IPI, Consommation électrique, Production (nucléaire/fossile/renouvelable) | Indicateurs de l’activité économique et du mix énergétique.              |
| **Macroéconomie/Climat** | Taux de change €/$, IPC Transport, Quotas CO₂, Température, Saisonnalité | Facteurs externes influençant la demande et les coûts.                     |

### Période et fréquence
- **Période** : Janvier 2010 – Décembre 2023
- **Fréquence** : Mensuelle
- **Échantillons** :
  - **Train** : 2010–2022 (prédiction de l’inflation à *t* avec les variables à *t-1*)
  - **Test** : 2023 (évaluation ex-post des performances du modèle)

---
## Méthodologie
### 1. Analyse descriptive
- **Statistiques univariées** : Distribution, asymétrie, kurtosis, valeurs extrêmes (ex. : pic du gaz naturel à 236 €/MWh en 2022).
- **Analyse bivariée** :
  - Corrélations de Spearman entre variables quantitatives (ex. : lien fort entre prix du gaz et du Brent, ρ=0.82).
  - Effets saisonniers (ex. : consommation électrique 50 % plus élevée en hiver).

### 2. Modélisation
Trois modèles sont comparés :
1. **Random Forest** :
   - Avantages : Robustesse aux non-linéarités, interactions complexes, valeurs extrêmes.
   - Paramètres : 500 arbres, sélection aléatoire de 5 variables par nœud.
   - Métriques : RMSE, MAE, sMAPE.
2. **Régression linéaire multiple** : Modèle de référence pour évaluer les gains du Random Forest.
3. **ARIMA** : Modèle temporel classique pour séries chronologiques.

### 3. Prévision
Deux approches :
- **Prévision directe** : Estimation de l’inflation pour 2023 à partir des données 2010–2022.
- **Rolling Window** : Réestimation mensuelle du modèle avec une fenêtre glissante pour capturer les dynamiques temporelles.

---
### Visualisations
Exemples de graphiques générés :
- Évolution de l’inflation énergétique vs. prix du gaz (2010–2023).
- Importance des variables dans le Random Forest.
- Comparaison des prédictions vs. valeurs réelles (2023).

> 📊 **Accéder au tableau de bord interactif** : [Lien Power BI](https://app.powerbi.com/view?r=eyJrIjoiMDUzNGZkZDUtMjIzYi00NmNjLTkyMTAtMjJmZGRjOTkzZTcxIiwidCI6IjcwYzYxNjVjLWFlNDktNDg0My1hODhlLTZiOTY5OWViYTBkMCJ9)

---
## Limites et perspectives
### Limites
- **Données manquantes** : Certaines variables pertinentes (ex. : production hydroélectrique) n’étaient pas disponibles en fréquence mensuelle.
- **Horizon temporel** : La prévision à court terme (1 mois) est robuste, mais l’extrapolation à moyen terme nécessiterait des ajustements.
- **Interprétabilité** : Le Random Forest est moins intuitif qu’un modèle linéaire pour l’analyse causale.

### Perspectives
- **Améliorer les données** : Intégrer des variables géopolitiques (ex. : indices de tensions commerciales).
- **Hybrider les modèles** : Combiner Random Forest et ARIMA pour capturer à la fois les non-linéarités et les dynamiques temporelles.
- **Étendre l’analyse** : Appliquer la méthodologie à d’autres pays européens pour une comparaison régionale.

