NEUF = read.csv("L:/BUT/SD/Promo 2023/bfontaine/RSHINY/DATA/dpe-v2-logements-neufs.csv", sep = ',', dec = '.')
EXIST = read.csv("L:/BUT/SD/Promo 2023/bfontaine/RSHINY/DATA/dpe-v2-logements-existants.csv", sep = ',', dec = '.')
dim(NEUF)
dim(EXIST)
NEUF$Logement = 'neuf'
NEUF$Période_construction="2019-2024"
EXIST$Logement = 'ancien'
COL_COMMUNES <- intersect(colnames(EXIST), colnames(NEUF))
FUSION <- rbind(EXIST[ ,COL_COMMUNES],NEUF[ ,COL_COMMUNES])
FUSION$Date_réception_DPE_Année = substr(FUSION$Date_réception_DPE,start = 1, stop = 4)
FUSION$Verif_coût <- ifelse(FUSION$Coût_total_5_usages == (FUSION$Coût_chauffage + FUSION$Coût_éclairage + 
                                                             FUSION$Coût_ECS + FUSION$Coût_refroidissement + 
                                                             FUSION$Coût_auxiliaires), 'OUI', 'NON')
FUSION$Coût_chauffage_prc = FUSION$Coût_chauffage/(FUSION$Coût_chauffage + FUSION$Coût_éclairage + FUSION$Coût_ECS + FUSION$Coût_refroidissement + FUSION$Coût_auxiliaires)
by(FUSION,FUSION$Etiquette_DPE,nrow)
by(FUSION,FUSION$Date_réception_DPE_Année,nrow)
table(FUSION$Type_installation_chauffage)/nrow(FUSION)
mean(FUSION$Surface_habitable_logement,na.rm = TRUE)
mean(FUSION$Coût_chauffage,na.rm = TRUE)
quantile(FUSION$Coût_ECS,c(0.25, 0.5, 0.75),na.rm = TRUE)
cor(FUSION$Surface_habitable_logement,FUSION$Coût_chauffage,method = 'pearson', use = "complete.obs")
variables = c(FUSION$Coût_total_5_usages,FUSION$Coût_chauffage,FUSION$Coût_éclairage,FUSION$Coût_ECS,FUSION$Coût_refroidissement, FUSION$Coût_auxiliaires, FUSION$Surface_habitable_logement , FUSION$Emission_GES_5_usages)
plot(variables,col="#77B5FE")
install.packages('dplyr')
library('dplyr')
filter(FUSION,FUSION$Type_bâtiment=='appartement')
filter(FUSION,FUSION$Etiquette_DPE %in% c('D','E','F','G'))
filter(FUSION,FUSION$Période_construction == 'avant 1948')
filter(FUSION,FUSION$Surface_habitable_logement > mean(FUSION$Surface_habitable_logement, na.rm = TRUE))
d=arrange(FUSION,desc(FUSION$Conso_5_usages.m._é_finale))
adresse = read.csv("L:/BUT/SD/Promo 2023/bfontaine/RSHINY/DATA/adresses-69.csv",sep = ';', dec = '.')
df <- FUSION %>%
  left_join(adresse, by = c("Identifiant__BAN = id"))