setwd("C:\\Users\\Clovis\\OneDrive\\Bureau\\SD\\Projet R\\Dataset")
data <- read.csv("adresses-01.csv", sep = ";", header = TRUE)
library(httr)
library(jsonlite)
library(dplyr)

# Préparation des codes postaux
cp <- paste0("0", unique(data$code_postal))
years <- c(2021, 2022, 2023, 2024)
base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-neufs/lines"

# Initialisation d'une DataFrame vide
df <- data.frame()

# Boucle à travers les codes postaux et les années
for (i in 1:length(cp)) {
  for (j in 1:length(years)) {
    params <- list(
      page = 1,
      size = 10000,
      select = "N°DPE,Code_postal_(BAN),Etiquette_DPE,Date_réception_DPE,_geopoint,Type_bâtiment,Conso_é_finale_installation_ECS,Conso_5_usages_é_finale,Conso_ECS_dépensier_é_finale,Conso_éclairage_é_finale,Conso_auxiliaires_é_finale,Conso_refroidissement_é_finale,Conso_chauffage_é_finale,Conso_chauffage_dépensier_é_finale",
      q = cp[i],
      q_fields = "Code_postal_(BAN)",
      qs = paste("Date_réception_DPE:[", years[j], "-01-01 TO ", years[j], "-12-31]", sep = "")
    )
    
    # Encodage des paramètres
    url_encoded <- modify_url(base_url, query = params)
    cat("Récupération des données pour le code postal:", cp[i], "et l'année:", years[j], "\n")
    
    # Effectuer la requête
    response <- GET(url_encoded)
    
    # Vérification du statut de la réponse
    if (status_code(response) == 200) {
      # Convertir le contenu de la réponse en texte lisible par R (format JSON)
      content <- fromJSON(rawToChar(response$content), flatten = FALSE)
      
      # Associer les données récoltées
      df <- bind_rows(df, content$results)
    } else {
      cat("Erreur lors de la récupération des données:", status_code(response), "\n")
    }
  }
}

# Afficher les données récupérées
View(df)

# Exporter la DataFrame en fichier CSV
write.csv(df, "C:\\Users\\Clovis\\OneDrive\\Bureau\\SD\\Projet R\\Dataset\\neufs_01.csv", row.names = FALSE)
