#Script de création des couches "communes", "pat_com" et "pat_aura-112025"

#install.packages(c("sf", "dplyr"))
library(sf)
library(dplyr)
library(tidyr)

#Fixation du dossier
setwd("C:/Users/oprima/Desktop/dossier") #Fixer le chemin source


#################### Étape 1 : Chargement des données ####################

########## Chargement des couches si shape ##########
# com_AE_d <- st_read("nom_de_la_couche.shp")        # Admin Express détaillé communes
# com_AE_PE <- st_read("nom_de_la_couche.shp")       # Admin Express COG PE communes
# PAT_source <- st_read("nom_de_la_couche.shp")      # Couche source des PAT de la DRAAF (en Admin Express détaillé)
# RPG_parcelles <- st_read("nom_de_la_couche.shp")   # RPG parcelles
# RPG_Bio <- st_read("nom_de_la_couche.shp")         # RPG Bio
# cantines <- st_read("nom_de_la_couche.csv")        # registe_cantines

########## Chargement des couches si gpkg ##########
#st_layers("nom_du_fichier.gpkg")   # pour connaître les noms des couches dans un géopackage

#nom <- st_read(nom_du_gpkg.gpkg", layer = "nom_de_la_couche")

com_AE_d <- st_read("ADE_4-0_GPKG_LAMB93_FXX-ED2026-01-19.gpkg", layer = "commune")                     # Admin Express détaillé communes
com_AE_PE <- st_read("ADE-COG-CARTO-PE_4-0_GPKG_LAMB93_FXX-ED2025-01-01.gpkg", layer = "commune")       # Admin Express COG PE communes
PAT_source <- st_read("pat_112025_etudiant.gpkg", layer = "pat_112025")                                 # Couche source des PAT de la DRAAF (en Admin Express détaillé)
RPG_parcelles <- st_read("RPG_Parcelles.gpkg", layer = "RPG_Parcelles")                                 # RPG parcelles
RPG_Bio <- st_read("RPG_BIO.gpkg", layer = "RPG_BIO")                                                   # RPG Bio
cantines <- st_read("registre_cantines.csv")                                                            # registe_cantines

# Réparation des géométries du RPG
RPG_parcelles <- st_make_valid(RPG_parcelles)
RPG_Bio       <- st_make_valid(RPG_Bio)

com_AE_PE <- com_AE_PE %>%
  mutate(
    nom_officiel = as.character(nom_officiel),
    code_insee = as.character(code_insee),
    population = as.integer(population),
    code_insee_du_departement = as.character(code_insee_du_departement),
    code_reg = as.character(code_insee_de_la_region),
    superficie_ha = as.numeric(st_area(geometrie)) / 10000 # calcul surface en hectares
    )

# Calcul population AuRA
pop_aura <- com_AE_PE %>% st_drop_geometry() %>% filter(code_reg=="84") %>% summarise(pop_aura = sum(population, na.rm = TRUE)) %>% pull(pop_aura)

#################### Étape 2 : Création de la table "pat_com" ####################
resultat_pat_com <- st_join(com_AE_d %>% select(code_insee),   # on ne récupère que code_insee
                    PAT_source %>% select(code_pat),   # on ne récupère que code_pat
                    join = st_within,
                    left = FALSE)

export_pat_com <- resultat_pat_com %>%
  st_drop_geometry() %>%                        # supprime la géométrie
  mutate(
    code_insee = as.character(code_insee),      # force en caractère
    code_pat   = as.character(code_pat)         # force en caractère
  )

write.csv(export_pat_com, "pat_com.csv")


#################### Étape 3 : Création des indices à l'échelle communales ####################

# Ajouter le code INSEE aux parcelles agricoles
# Fonction : code_insee par plus grand chevauchement ---
attribuer_code_insee <- function(couche, communes) {
  
  # Ajout d'un identifiant unique avant l'intersection
  couche <- couche %>% mutate(id_temp = row_number())
  
  # Calcul des intersections avec leur aire
  intersections <- st_intersection(couche, communes %>% select(code_insee)) %>%
    mutate(aire_intersection = as.numeric(st_area(.)))
  
  # Pour chaque entité, on garde uniquement la commune avec la plus grande intersection/cheuvauchement
  resultat <- intersections %>%
    st_drop_geometry() %>%
    group_by(id_temp) %>%                               # identifie chaque entité source par son id
    slice_max(aire_intersection, n = 1) %>%             # garde le plus grand chevauchement
    ungroup() %>%
    select(-id_temp) %>%
    mutate(code_insee = as.character(code_insee))
  
  return(resultat)
}

# Application aux deux couches RPG
RPG_parcelles_com <- attribuer_code_insee(RPG_parcelles, com_AE_d)
RPG_Bio_com       <- attribuer_code_insee(RPG_Bio,       com_AE_d)

# Somme de surf_parc par code_insee
surf_parcelles <- RPG_parcelles_com %>%
  group_by(code_insee) %>%
  summarise(rpg_ha = sum(surf_parc, na.rm = TRUE))

surf_bio <- RPG_Bio_com %>%
  group_by(code_insee) %>%
  summarise(bio_ha = sum(surf_parc, na.rm = TRUE))

# Jointure des résultats dans com_AE_PE
com_AE_PE <- com_AE_PE %>%
  mutate(
    code_insee = as.character(code_insee)) %>%
  left_join(surf_parcelles, by = "code_insee") %>%   # ajout surf agricole totale
  left_join(surf_bio,       by = "code_insee") %>%   # ajout surf bio
  mutate(
    rpg_ha = replace_na(rpg_ha, 0),      # communes sans parcelles = 0
    bio_ha  = replace_na(bio_ha,  0),      # communes sans bio = 0
  ) %>% 
  mutate(
    part_bio = bio_ha/rpg_ha*100,      #Calcul de la part de SAU bio sur la SAU total de la commune
    part_sau = rpg_ha/superficie_ha*100      #Calcul de la part de SAU sur la superficie total de la commune
  )

# Ajout du nombre de restaurants scolaires
nb_cantines <- cantines %>%
  group_by(city_insee_code) %>%
  summarise(nb_cantines = n())

com_AE_PE <- com_AE_PE %>%
  left_join(nb_cantines, by = c("code_insee" = "city_insee_code")) %>%
  mutate(nb_cantines = replace_na(nb_cantines, 0)) %>%
  mutate(
    nom_officiel = as.character(nom_officiel),
    code_insee = as.character(code_insee),
    population = as.integer(population),
    code_insee_du_departement = as.character(code_insee_du_departement),
    rpg_ha = rpg_ha,
    bio_ha = bio_ha,
    part_sau,
    part_bio,
    nb_cantines = nb_cantines
  )

# Calcul SAU AuRA
sau_aura <- com_AE_PE %>% st_drop_geometry() %>% summarise(sau_aura = sum(rpg_ha, na.rm = TRUE)) %>% pull(sau_aura)
bio_aura <- com_AE_PE %>% st_drop_geometry() %>% summarise(bio_aura = sum(bio_ha, na.rm = TRUE)) %>% pull(bio_aura)

# Export du résultat en gpkg
st_write(com_AE_PE %>% filter(code_reg == "84"),
         "communes.gpkg", layer = "communes", delete_layer = TRUE)

#################### Étape 4 : Création des indices à l'échelle des PAT ####################

# Chargement des données
pat_com <- read.csv("pat_com.csv", colClasses = c(code_insee = "character", code_pat = "character")) %>% select(-X)
communes <- st_read("communes.gpkg", layer = "communes")

# Ajout des géométries des communes à pat_com
pat_com <- pat_com %>%
  mutate(code_insee = as.character(code_insee)) %>%
  left_join(communes %>% st_drop_geometry() %>% select(code_insee, population, rpg_ha, bio_ha, nb_cantines), by = "code_insee")

# Somme des attributs par code_pat
pat_indices <- pat_com %>%
  group_by(code_pat) %>%
  summarise(
    population   = sum(population,   na.rm = TRUE),
    rpg_ha       = sum(rpg_ha,       na.rm = TRUE),
    bio_ha       = sum(bio_ha,       na.rm = TRUE),
    nb_cantines  = sum(nb_cantines,  na.rm = TRUE)
  )

# Fusion des géométries par code_pat
pat_geo <- communes %>%
  left_join(pat_com %>% st_drop_geometry() %>% select(code_insee, code_pat), by = "code_insee") %>%
  filter(!is.na(code_pat)) %>%
  group_by(code_pat) %>%
  summarise(geometry = st_union(geom))

# Jointure des indices sur les géométries fusionnées
pat_aura <- pat_geo %>%
  left_join(pat_indices, by = "code_pat")

# Calcul des attributs
pat_aura <- pat_aura %>%
  mutate(
    code_pat = as.double(code_pat),
    nb_cantines = as.integer(nb_cantines),
    rpg_ha = as.integer(rpg_ha),
    bio_ha = as.integer(bio_ha),
    part_bio = bio_ha/rpg_ha*100,
    part_sau_pat = rpg_ha/sau_aura*100,
    part_bio_pat = bio_ha/bio_aura*100,
    bio_aura = bio_aura/sau_aura*100,
    part_pop = population/pop_aura*100
  )

# Jointure des attributs de PAT_source à pat_aura
pat_aura_112025 <- pat_aura %>%
  left_join(PAT_source %>% st_drop_geometry(), by = "code_pat") %>%
  mutate(
    code_pat = as.double(code_pat),
    pop_hab = as.integer(pop_hab),
    nom_du_pat = as.character(nom_du_pat),
    territoire = as.character(territoire),
    echelle = as.character(echelle),
    niveau = as.integer(niveau),
    annee_labe = as.integer(annee_labe),
    mail_coord = as.character(mail_coord),
    nb_cantines = as.integer(nb_cantines),
    rpg_ha = as.integer(rpg_ha),
    bio_ha = as.integer(bio_ha),
    part_bio = as.double(part_bio),
    part_sau_pat = as.double(part_sau_pat),
    part_bio_pat = as.double(part_bio_pat),
    bio_aura = as.double(bio_aura),
    part_pop = as.double(part_pop)
  )

# Export en gpkg
st_write(pat_aura_112025, "pat_aura_112025.gpkg", layer = "pat_aura_112025", delete_layer = TRUE)