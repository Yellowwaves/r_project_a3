library(dplyr)
library(ggplot2)


df <- read.csv("stat_acc_V3.csv", header = TRUE, sep = ";")

unique_descr <- unique(df$id_code_insee)
print(unique_descr)


# Changement des valeurs dans la colonne 'grav'

desc_grav <- c("Indemne","Blessé léger", "Blessé hospitalisé","Tué")
desc_grav_new <- c("1", "2", "3", "4")
df$descr_grav <- factor(df$descr_grav, levels = desc_grav, labels = desc_grav_new)

# Changement des valeurs dans la colonne 'agglo'

descr_agglo <- c("Hors agglomération", "En agglomération")
descr_agglo_new <- c("1", "2")
df$descr_athmo <- factor(df$descr_athmo, levels = descr_agglo, labels = descr_agglo_new)

# Changement des valeurs dans la colonne 'athmo'
descr_athmo <- c("Brouillard – fumée","Neige – grêle","Pluie forte",
                "Normale","Autre","Temps éblouissant"
                ,"Pluie légère","Temps couvert","Vent fort – tempête")
df$descr_athmo <- factor(df$descr_athmo, levels = descr_athmo, labels = c("1","2","3","4","5","6","7","8","9"))

# Changement des valeurs dans la colonne 'descr_lum'

descr_lum <- c("Plein jour","Crépuscule ou aube","Nuit sans éclairage public",
               "Nuit avec éclairage public non allumé","Nuit avec éclairage public allumé")
descr_lum_new <- c("1", "2", "3", "4", "5")
df$descr_lum <- factor(df$descr_lum, levels = descr_lum, labels = descr_lum_new)

# Changement des valeurs dans la colonne 'descr_etat_surf'

df$descr_etat_surf <- as.numeric(factor(df$descr_etat_surf))
# Changement des valeurs dans la colonne 'intersection'

df$description_intersection <- as.numeric(factor(df$description_intersection))

# Changement des valeurs dans la colonne 'descr_dispo_secu' 

df$descr_dispo_secu <- as.numeric(factor(df$descr_dispo_secu))

# Changement des valeurs dans la colonne 'descr_cat_veh'

df$descr_cat_veh <- as.numeric(factor(df$descr_cat_veh))

# Changement des valeurs dans la colonne 'descr_motif_traj'

df$descr_motif_traj <- as.numeric(factor(df$descr_motif_traj))

# Changement des valeurs dans la colonne 'descr_type_col'

df$descr_type_col <- as.numeric(factor(df$descr_type_col))


#
# Affichage des graphiques
#
# Partie 2
#

# Plot en fonction des conditions atmosphériques
ggplot(df, aes(x = descr_athmo)) +
  geom_bar() +
  xlab("Conditions atmosphériques") +
  ylab("Nombre d'accidents") +
  ggtitle("Nombre d'accidents en fonction des conditions atmosphériques")

# Plot en fonction de la gravité
ggplot(df, aes(x = descr_grav)) +
  geom_bar() +
  xlab("Gravité") +
  ylab("Nombre d'accidents") +
  ggtitle("Nombre d'accidents en fonction de la gravité")

# Plot en fonction des 10 villes les plus accidentées
# Compter le nombre d'accidents par ville
accidents_par_ville <- df %>%
  count(ville, name = "nombre_accidents")

# Trier les villes par ordre décroissant du nombre d'accidents
top_10_villes <- accidents_par_ville %>%
  arrange(desc(nombre_accidents)) %>%
  head(10)

# Créer un diagramme en barres pour les 10 villes avec le plus d'accidents
ggplot(top_10_villes, aes(x = reorder(ville, -nombre_accidents), y = nombre_accidents)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Ville") +
  ylab("Nombre d'accidents") +
  ggtitle("Top 10 des villes avec le plus d'accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))