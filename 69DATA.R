
# Importer les bibliothèques nécessaires
library(httr)
library(jsonlite)
library(dplyr)

# Charger les codes postaux depuis le fichier CSV
adresses_69 <- read.csv("C:/Users/bastf/Downloads/adresses-69.csv", sep = ';', stringsAsFactors = FALSE)

# Extraire les codes postaux uniques
codes_postaux <- unique(adresses_69$code_postal)

# URL de base pour les logements existants et neufs
base_url_existants <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-existants/lines"
base_url_neufs <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-neufs/lines"

# Initialiser deux dataframes vides pour stocker les résultats
df_existants <- data.frame()
df_neufs <- data.frame()

### Partie 1 : Récupérer les logements existants

for (code_postal in codes_postaux) {
  # Paramètres pour logements existants
  params_existants <- list(
    page = 1,
    size = 10000,
    select = "N°DPE,Code_postal_(BAN),Etiquette_DPE,Date_réception_DPE,Coût_total_5_usages,Type_bâtiment,Surface_habitable_logement,Surface_habitable_immeuble,Coût_chauffage,Coût_éclairage,Coût_ECS,Coût_refroidissement,Coût_auxiliaires,Conso_5_usages_par_m²_é_primaire,Conso_5_usages_é_finale",
    q = as.character(code_postal),
    q_fields = "Code_postal_(BAN)",
    qs = "Date_réception_DPE:[2021-01-01 TO 2024-12-31]"
  )
  
  # Encodage de l'URL pour logements existants
  url_encoded_existants <- modify_url(base_url_existants, query = params_existants)
  
  # Effectuer la requête GET pour les logements existants
  response_existants <- GET(url_encoded_existants)
  
  if (status_code(response_existants) == 200) {
    content_existants <- fromJSON(rawToChar(response_existants$content), flatten = TRUE)
    
    if (!is.null(content_existants$result)) {
      df_temp <- as.data.frame(content_existants$result)
      df_existants <- bind_rows(df_existants, df_temp)
    }
  }
}

# Exporter les logements existants vers un fichier CSV
write.csv(df_existants, "C:/Users/bastf/Downloads/existants-69.csv", row.names = FALSE)

### Partie 2 : Récupérer les logements neufs

for (code_postal in codes_postaux) {
  # Paramètres pour logements neufs
  params_neufs <- list(
    page = 1,
    size = 10000,
    select = "N°DPE,Code_postal_(BAN),Etiquette_DPE,Date_réception_DPE,Coût_total_5_usages,Type_bâtiment,Surface_habitable_logement,Surface_habitable_immeuble,Coût_chauffage,Coût_éclairage,Coût_ECS,Coût_refroidissement,Coût_auxiliaires,Conso_5_usages_par_m²_é_primaire,Conso_5_usages_é_finale",
    q = as.character(code_postal),
    q_fields = "Code_postal_(BAN)",
    qs = "Date_réception_DPE:[2021-01-01 TO 2024-12-31]"
  )
  
  # Encodage de l'URL pour logements neufs
  url_encoded_neufs <- modify_url(base_url_neufs, query = params_neufs)
  
  # Effectuer la requête GET pour les logements neufs
  response_neufs <- GET(url_encoded_neufs)
  
  if (status_code(response_neufs) == 200) {
    content_neufs <- fromJSON(rawToChar(response_neufs$content), flatten = TRUE)
    
    if (!is.null(content_neufs$result)) {
      df_temp <- as.data.frame(content_neufs$result)
      df_neufs <- bind_rows(df_neufs, df_temp)
    }
  }
}

# Exporter les logements neufs vers un fichier CSV
write.csv(df_neufs, "C:/Users/bastf/Downloads/neufs-69.csv", row.names = FALSE)
