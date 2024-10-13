setwd("C:\\Users\\Clovis\\OneDrive\\Bureau\\SD\\Projet R\\Dataset")
existants <- read.csv("existants_01.csv", sep = ",", header = TRUE)
neufs <- read.csv("neufs_01.csv", sep = ",", header = TRUE)
library(tidyr)
#Fusionner les sdeux datasets

neuf <- colnames(neufs)
existant <- colnames(existants)
noms_colonnes <- intersect (existant,neuf)
neufs_1 <- neufs[,noms_colonnes]
existants_1 <- existants[,noms_colonnes]
neufs_1$Type <- "Neuf"
existants_1$Type <- "Existant"


df <- rbind(neufs_1, existants_1)
df <- df %>%
  separate(X_geopoint, into = c("Latitude", "Longitude"), sep = ",")

write.csv(df, "C:\\Users\\Clovis\\OneDrive\\Bureau\\SD\\Projet R\\Dataset\\DATA.csv", row.names = FALSE)


