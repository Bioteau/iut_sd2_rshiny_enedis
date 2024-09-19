#chemin d'accés
setwd("L:\\BUT\\SD\\Promo 2023\\cbouteaud\\SD 2\\Prog stat\\dataset")
#importation des deux datasets
neufs<-read.csv("dpe-v2-logements-neufs.csv",header=TRUE,dec=",", sep=",")
existants<-read.csv("dpe-v2-logements-existants.csv",header=TRUE,dec=",", sep=",")
#affiche la dimension
dim(neufs)
dim(existants)
#ajout d'une colonne avec ancien ou neuf selon la base
existants$Logement="ancien"
neufs$Logement="neufs"
#Fusionner les sdeux datasets
neufs$Année_construction <- 2024
noms_colonnes_neufs <- colnames(neufs)
noms_colonnes_existants <- colnames(existants)
noms_colonnes <- intersect (noms_colonnes_existants, noms_colonnes_neufs)
neufs_1 <- neufs[,noms_colonnes]
existants_1 <- existants[,noms_colonnes]
df <- rbind(neufs_1, existants_1)
#Ajout d'une colonne avec seulement l'année d'une autre colonne
df$Date_réception_DPE <- as.Date(df$Date_réception_DPE, format="%Y-%m-%d")
df$Année_réception_DPE <- format(df$Date_réception_DPE,"%Y")
#Création colonne verif

df$Coût_chauffage <- as.numeric(df$Coût_chauffage)
df$Coût_éclairage <- as.numeric(df$Coût_éclairage)
df$Coût_ECS <- as.numeric(df$Coût_ECS)
df$Coût_refroidissement <- as.numeric(df$Coût_refroidissement)
df$Coût_auxiliaires <- as.numeric(df$Coût_auxiliaires)
df$Coût_total <- df$Coût_chauffage+df$Coût_éclairage+df$Coût_ECS+df$Coût_refroidissement+df$Coût_auxiliaires
df$Coût_total_5_usages <- as.numeric(df$Coût_total_5_usages)
df$Verif <- ifelse(df$Coût_total_5_usages == df$Coût_total, "Vrai", "Faux")
#Création colonne Cout chauffage qyu est le pourcentage du total
df$Coût_chauffage_pourcentage <- (df$Coût_chauffage / df$Coût_total_5_usages) * 100
#Création colonne Periode construction pra rapport a un tableau
df$Periode_construction <- ifelse(df$Année_construction < 1960, "Avant 1960",
                                  ifelse(df$Année_construction < 1970, "1961-1970",
                                         ifelse(df$Année_construction < 1980, "1971-1980",
                                                ifelse(df$Année_construction < 1990, "1981-1990",
                                                       ifelse(df$Année_construction < 2000, "1991-2000",
                                                              ifelse(df$Année_construction <= 2010, "2001-2010",
                                                                     ifelse(df$Année_construction > 2010, "Après 2010", NA)))))))
#Statistiques générales
df$Etiquette_DPE <- as.character(df$Etiquette_DPE)
compte_notes <- table(df$Etiquette_DPE)
print(compte_notes)
# Comptage du nombre de DPE par année
compte_dpe_par_annee <- table(df$Année_réception_DPE)
print(compte_dpe_par_annee)
#Surface habitable moyenne logement
df$Surface_habitable_logement <- as.numeric(df$Surface_habitable_logement)
# Calculer la moyenne en excluant les valeurs NA
moyenne_surface <- mean(df$Surface_habitable_logement, na.rm = TRUE)
print(moyenne_surface)
mean(df$Coût_chauffage)
quartiles <- quantile(df$Coût_ECS, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
print(quartiles)
deciles <- quantile(df$Coût_ECS, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
print(deciles)
correlation <- cor(df$Surface_habitable_logement, df$Coût_chauffage, use = "complete.obs")
print(correlation)

# Installer le package corrplot s'il n'est pas déjà installé
install.packages("corrplot")

# Charger le package corrplot
library(corrplot)
# Créer un DataFrame avec les variables spécifiques
df_corr <- df[, c("Coût_total_5_usages", "Coût_chauffage", "Coût_éclairage", "Coût_ECS", "Coût_refroidissement", "Coût_auxiliaires", "Surface_habitable_logement", "Emission_GES_5_usages")]

# Vérifier les premières lignes du DataFrame pour s'assurer que les variables sont correctes
head(df_corr)
# Calculer la matrice de corrélation
matrice_corr <- cor(df_corr, use = "complete.obs")
print(matrice_corr)
# Convertir les colonnes en numérique si nécessaire
df_corr$Coût_total_5_usages <- as.numeric(df_corr$Coût_total_5_usages)
df_corr$Coût_chauffage <- as.numeric(df_corr$Coût_chauffage)
df_corr$Coût_éclairage <- as.numeric(df_corr$Coût_éclairage)
df_corr$Coût_ECS <- as.numeric(df_corr$Coût_ECS)
df_corr$Coût_refroidissement <- as.numeric(df_corr$Coût_refroidissement)
df_corr$Coût_auxiliaires <- as.numeric(df_corr$Coût_auxiliaires)
df_corr$Surface_habitable_logement <- as.numeric(df_corr$Surface_habitable_logement)
df_corr$Emission_GES_5_usages <- as.numeric(df_corr$Emission_GES_5_usages)


#Filtre
#creation data frame avec seulement les lignes qui sont des appartements
dfappart <- df[df$Type_bâtiment == "appartement", ]
View(dfappart)
#création data frame dont les dpe sont D,E,F,G
dfDPE <- df[df$Etiquette_DPE %in% c("D","E","F","G"), ]
View(dfDPE)
#création date frame logemetns ancien avant 1948
df1948 <- df[df$Année_construction < "1948", ]
df1948 <- df1948[!is.na(df1948[, 1]), ]
View(df1948)
#création data fram surface habitale>surface habitable moyenne
surfacemoyenne <- mean(df$Surface_habitable_logement,na.rm =TRUE)
dfsurface <- df[df$Surface_habitable_logement>surfacemoyenne, ]
View(dfsurface)
# Convertir la colonne Conso_5_usages.m._é_finale en numérique
df$Conso_5_usages.m._é_finale <- as.numeric(df$Conso_5_usages.m._é_finale)
#création data frame avec filtre sur colonne
df_tri <- df[order(-df$Conso_5_usages.m._é_finale), ]
View(df_tri)
#aggrégation
coût_moyen_chauffage <- aggregate(Coût_chauffage ~ Etiquette_DPE, data = df, FUN = mean)
