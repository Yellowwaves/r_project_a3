
library(dplyr)

df <- read.csv("stat_acc_V3.csv", header = TRUE, sep = ";")

unique_descr <- unique(df$descr_dispo_secu)
print(unique_descr)


# Changement des valeurs dans la colonne 'grav'
desc_grav <- c("Indemne", "Tué", "Blessé hospitalisé", "Blessé léger")
desc_grav_new <- c("1", "4", "3", "2")
df$descr_grav <- factor(df$descr_grav, levels = desc_grav, labels = desc_grav_new)

# Changement des valeurs dans la colonne 'agglo'

descr_agglo <- c("Hors agglomération", "En agglomération")
descr_agglo_new <- c("1", "2")
df$descr_athmo <- as.numeric(factor(df$descr_athmo))

# Changement des valeurs dans la colonne 'athmo'

# descr_athmo <- c("Brouillard – fumée","Neige – grêle","Pluie forte",
#                 "Normale","Autre","Temps éblouissant"
#                 ,"Pluie légère","Temps couvert","Vent fort – tempête")
df$descr_athmo <- as.numeric(factor(df$descr_athmo))

# Changement des valeurs dans la colonne 'descr_lum'

descr_lum <- c("Plein jour","Crépuscule ou aube","Nuit sans éclairage public",
               "Nuit avec éclairage public non allumé","Nuit avec éclairage public allumé")
descr_lum_new <- c("1", "2", "3", "4", "5")
df$descr_lum <- factor(df$descr_lum, levels = descr_lum, labels = descr_lum_new)

# Changement des valeurs dans la colonne 'descr_etat_surf' [1] "Verglacée"          "Enneigée"           "Mouillée" "Normale"            "Autre"              "Corps gras – huile" "Boue"               "Flaques"            "Inondée"

descr_etat_surf <- c("Verglacée", "Enneigée", "Mouillée", "Normale", "Autre", "Corps gras – huile", "Boue", "Flaques", "Inondée")
descr_etat_surf_new <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
df$descr_etat_surf <- factor(df$descr_etat_surf, levels = descr_etat_surf, labels = descr_etat_surf_new)

# Changement des valeurs dans la colonne 'intersection'

# descr_intersection <- c("Hors intersection", "Intersection en X", "Intersection en T", "Intersection en Y", "Intersection à plus de 4 branches", "Giratoire", "Place", "Passage à niveau", "Autre intersection")
# descr_intersection_new <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
# df$descr_intersection <- factor(df$descr_intersection, levels = descr_intersection, labels = descr_intersection_new)

# Changement des valeurs dans la colonne 'descr_dispo_secu' 

#  descr_dispo_secu <- c("Utilisation d'une ceinture de sérucité", ""
#  descr_dispo_secu_new <- c("1", "2", "3", "4", "5")
#  df$descr_dispo_secu <- factor(df$descr_dispo_secu, levels = descr_dispo_secu, labels = descr_dispo_secu_new)

# Définition des nouvelles catégories
nouvelles_categories <- c(
  "cyclo", "moto légère", "moto lourde", "VT", "VU", "PL", "autres"
)

# Remplacement des valeurs dans la colonne 'descr_cat_veh'
df <- df %>% mutate(descr_cat_veh = case_when(
  descr_cat_veh == "PL seul > 7,5T" ~ "PL",
  descr_cat_veh == "VU seul 1,5T <= PTAC <= 3,5T avec ou sans remorque " ~ "VU",
  descr_cat_veh == "VL seul" ~ "VT",
  descr_cat_veh == "Autocar" ~ "autres",
  descr_cat_veh == "PL > 3,5T + remorque" ~ "PL",
  descr_cat_veh == "Cyclomoteur <50cm3" ~ "cyclo",
  descr_cat_veh == "Motocyclette > 125 cm3" ~ "moto lourde",
  descr_cat_veh == "Tracteur routier + semi-remorque" ~ "autres",
  descr_cat_veh == "Tracteur agricole" ~ "autres",
  descr_cat_veh == "PL seul 3,5T <PTCA <= 7,5T" ~ "PL",
  descr_cat_veh == "Autobus" ~ "autres",
  descr_cat_veh == "Scooter > 50 cm3 et <= 125 cm3" ~ "moto légère",
  descr_cat_veh == "Train" ~ "autres",
  descr_cat_veh == "Scooter > 125 cm3" ~ "moto lourde",
  descr_cat_veh == "Scooter < 50 cm3" ~ "cyclo",
  descr_cat_veh == "Voiturette (Quadricycle à moteur carrossé) (anciennement \"voiturette ou tricycle à moteur\")" ~ "VT",
  descr_cat_veh == "Autre véhicule" ~ "autres",
  descr_cat_veh == "Bicyclette" ~ "autres",
  descr_cat_veh == "Motocyclette > 50 cm3 et <= 125 cm3" ~ "moto légère",
  descr_cat_veh == "Engin spécial" ~ "autres",
  descr_cat_veh == "Quad lourd > 50 cm3 (Quadricycle à moteur non carrossé)" ~ "moto lourde",
  descr_cat_veh == "Tramway" ~ "autres",
  descr_cat_veh == "Tracteur routier seul" ~ "autres",
  descr_cat_veh == "Quad léger <= 50 cm3 (Quadricycle à moteur non carrossé)" ~ "cyclo",
  TRUE ~ NA_character_
))

# Affichage des résultats
print(tail(df))