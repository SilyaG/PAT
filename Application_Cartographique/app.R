###########################################PROJET CAROT########################################################
##APPEL AUX LIBRAIRIES NECÉSSAIRES À LA CRÉATION DE LA CARTE INTERACTIVE##
library(shiny)
library(leaflet)
library(sf)
library(readr)
library(stringi)

# Chargement des couches et reprojection en 4326
couche_pat_4326 <- st_read("./data/pat_aura_112025.gpkg")%>%
  st_transform(couche_pat, crs = 4326)
commune_aura <- st_read("./data/communes.gpkg")%>%
  st_transform(admin_express_com, crs = 4326)
couche_cls_4326 <- st_read("./data/cls_aura.gpkg")%>% 
  st_transform(couche_cls, crs = 4326)
dep_aura_4326 <- st_read("./data/departement_aura.gpkg")%>% 
  st_transform(dep_aura, crs = 4326)
pat_com <- read_csv("./data/pat_com.csv")
 


###########################################Partie UI#############################################################
ui <- fluidPage(
  
#Appel des éléments nécessaires à la stylisation/mise en page (DSFR)
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/@gouvfr/dsfr@1.12.1/dist/dsfr.min.css"
    ),
    tags$script(
      src = "https://cdn.jsdelivr.net/npm/@gouvfr/dsfr@1.12.1/dist/dsfr.module.min.js",
      type = "module"
    ),
#Création d'un style pour le menu de sélection des couches 
    tags$style(HTML("
    .menu-couches {
      background-color:#f6f6f6;
      padding: 10px;
      border-radius: 8px;
      height: 90hv;
      overflow-y:auto;
    }
    .menu-couches h4 {
      font-weight: 600;
      margin-top: 15px;
    }
    "))
  ),
  
#Création des élements structurants/qui aparaissent sur la page (en-tête, début du contenu principal, 
#pied de page, logo...) en utilisant les classes du Design System de l’État (DSFR)
tags$header(
    class = "fr-header",
    tags$div(
      class = "fr-header__body",
      tags$div(
        class = "fr-container",
        tags$div(
          class = "fr-header__body-row",
          tags$div(
            class = "fr-header__brand",
            tags$a(
              class = "fr-header__brand-link",
              href = "#",
              tags$p(class = "fr-logo","République\nFrançaise")
            )
          ),
          tags$div(
            class = "fr-header__service",
            tags$p(class = "fr-header__service-title","Cartographie des Projets Alimentaires Territoriaux"),
            tags$p(class = "fr-header__service-tagline","Région Auvergne-Rhône-Alpes")
          )
        )
      )
    )
  ),
  
  tags$main(
    class = "fr-container-fluid",
    br(),
    
#Placement filtre et barre de recherche
    tags$div(
      style = "display:flex; gap:20px; align-items:flex-end; margin:0px 0 20px 0; padding:0;", 
      #Conteneur principal : aligne les filtres (gauche) et la barre de recherche (droite) et les placent sur la page
      
# Bloc filtres à gauche
      tags$div(
        style = "display:flex; gap:20px; margin:0; padding:0;", 
        #Permet d'afficher les deux menus de sélection côte à côte
        
        tags$div(
          style = "width:250px; margin:0; padding:0;", 
          
          tags$select( #Menu déroulant pour filtrer selon le niveau de labellisation
            id = "filtre_niveau",  
            class = "fr-select",   
            style = "color:black; margin:0;",
            
            tags$option( #Option par défaut 
              "Sélectionner un niveau de labellisation",
              value = "",
              selected = TRUE,
              disabled = TRUE
            ), #Options dans le menu déroulant 
            tags$option(value = "Tous", "Tous les niveaux"),
            tags$option(value = "1", "Niveau 1"),
            tags$option(value = "2", "Niveau 2")
          )
        ),
        
        tags$div(
          style = "width:250px; margin:0; padding:0;", 

          tags$select( #Menu déroulant pour filtrer selon l’échelle territoriale
            id = "filtre_niveau_terri", 
            class = "fr-select",       
            style = "color:black; margin:0;", #Personnalisation visuelle sans marge
            
            tags$option( #Option par défaut 
              "Sélectionner l'échelle du territoire",
              value = "",
              selected = TRUE,
              disabled = TRUE
            ),#Options dans le menu déroulant 
            tags$option(value = "Tous", "Toutes les échelles"),
            tags$option(value = "PAT interterritorial (PAiT)", "Interterritorial (PAiT)"),
            tags$option(value = "PAT d'échelle intercommunale", "Intercommunale"),
            tags$option(value = "PAT d'échelle départementale", "Départementale")
          )
        )
      ),
      
# Barre de recherche à droite
      tags$div(
        class = "fr-search-bar", #Style officiel de la barre de recherche selon le DSFR
        role = "search",         #Zone où l'on tape l'objet recherché
        style = "width:250px; margin-left:auto; margin-top:0; margin-bottom:0; padding:0;", #Taille et placement sans marges inutiles
        
        tags$label(   
          class = "fr-label",
          `for` = "nom_du_pat",
        ),
        
        tags$input( #Champ dans lequel l'utilisateur saisit sa recherche
          class = "fr-input",
          id = "nom_du_pat",   #Identifiant du PAT (utilisé côté server)
          type = "search",
          placeholder = "Rechercher une Commune ou un PAT",
          `aria-describedby` = "search_input_messages",
          style = "margin:0;"
        ),
        
        tags$div( #Message d'erreur / aide à la compréhension
          class = "fr-messages-group",
          id = "search_input_messages",
          `aria-live` = "polite",
          style = "margin:0;"
        ),
        
#Bouton rechercher (loupe)
        actionButton( #Création de la réactivité du bouton (lié au côté server)
          inputId = "search_button",
          label = "Rechercher",
          class = "fr-btn",
          style = "margin:0;"
        )
        
        #Bouton reinitialiser le filtre sur le zoom 
      )
    ),
    
#Création des colonnes pour ajouter le menu de sélection des couches à gauche de la carte 
#Menu de sélection des couches (lateral gauche)
    fluidRow(
      column(
        width = 2,
        div(class = "menu-couches",
            h4("Fond cartographique"),
            radioButtons( # limite la sélection à un seul plan géographique (un par un)
              "fond",
              label = NULL,
              choices = c(
                "Plan IGN" = "ign",
                "Registre Parcellaire Graphique" = "rpg",
                "OpenStreetMap" = "osm"
              ),
              selected = "ign" #La couche IGN est présente à l'ouverture de la carte
            ),
            
            hr(),# ligne de séparation 
            
            h4("Couches"),
            
            #"Checkbox" Permet de sélectionner plusieurs couches en même temps 
            checkboxInput("pat_layer", "Projet Alimentaire Territoriaux", TRUE),#La couche PAT est présente à l'ouverture de la carte
            checkboxInput("cls_layer", "Contrat Locaux de Santé", FALSE),
            checkboxInput("dep_layer", "Départements", FALSE),
            checkboxInput("com_layer", "Communes",FALSE),
            
            hr(),# ligne de séparation 
            
            conditionalPanel( #oblige la sélection de la couche commune pour afficher le sélecteur lié aux indicateurs
              condition = "input.com_layer == true",
              
              h4("Indicateurs communaux"),
              
              radioButtons( # limite la sélection à un seul indicateur (un par un)
                "indicateur",
                label = NULL,
                choices = c(
                  "Aucun" = "none",
                  "Population" = "pop",
                  "surface agricole utile (ha)" = "sau",
                  "surface agricole utile bio" = "bio"
                ),
                selected = "none"
              )
            )
        )
      ),
      
#Intégration de la carte dans la colonne de droite 
      column(
        width = 9,
        leafletOutput("map", height = "90vh")
      )
    )
  ),

#Création du pied de page officiel (DSFR)
  tags$footer(
    class = "fr-footer",
    tags$div(
      class = "fr-container",
      tags$p("© République Française - Tous droits réservés")
    )
  )
)
  
  
###########################################Partie SERVER###########################################################
server <- function(input, output, session) {
    
    output$map <- renderLeaflet({#affichage de la carte et paramétrages de la BBOX
      
      bbox <- st_bbox(commune_aura)
      
      xmin <- unname(bbox["xmin"])
      ymin <- unname(bbox["ymin"])
      xmax <- unname(bbox["xmax"])
      ymax <- unname(bbox["ymax"])
      
#Préparation des indicateurs (SAU, SAU BIO, Population)
#Recherche du centroïdes des communes
      communes_centroid <- st_centroid(commune_aura)
      
#Calcul de la part en % de la SAU bio par communes 
      part_bio <- communes_centroid$part_bio
      
#Sécurisation (évite la division par 0 et les valeurs NA)
      part_bio[is.na(part_bio) | is.infinite(part_bio)] <- 0

#Création des valeurs permettant la création des cercles proportionnels
#Création des cercles proportionnels du nombre d'habitants par communes 
      pop_com <- communes_centroid$population
      rayon_brut_pop <- sqrt(pop_com)
      rayon_pop <- scales::rescale(rayon_brut_pop, to = c(1, 50))
      
#Création des cercles proportionnels du nombre d'hectares de SAU par communes 
      sau_com <- communes_centroid$rpg_ha_sum
      rayon_brut_sau <- sqrt(sau_com)
      rayon_sau <- scales::rescale(rayon_brut_sau, to = c(1, 30))
      
#Création des cercles proportionnels du nombre d'hectares de SAU BIO par communes 
      saubio_com <- communes_centroid$bio_ha_sum
      rayon_brut_saubio <- sqrt(saubio_com)
      rayon_saubio <- scales::rescale(rayon_brut_saubio, to = c(1, 30))
      
##Palettes de couleur des couches
#Palette PAT 
      pal_pat <- colorFactor(
        palette = c("#fbe769", "#E4794A"),
        domain = couche_pat_4326$niveau
      )
#Palette % SAU bio (la couleur des cercles proportionnels)
      pal_bio <- colorNumeric(
        palette = c("#bcd9a3","#306600"),
        domain = part_bio,
        na.color = "transparent"
      )
      
#Limitation du dézoom maximal de la carte 
      leaflet(
        options = leafletOptions(
          minZoom = 6,
          maxZoom = 15
        )
      ) %>%
        addProviderTiles("OpenStreetMap", group = "OSM") %>%
        fitBounds(xmin, ymin, xmax, ymax) %>%
        setMaxBounds(xmin, ymin, xmax, ymax) %>%
        

#Ajout/Appel des couches à la carte 
#Plan IGN
        addWMSTiles(
          baseUrl = "https://data.geopf.fr/wms-r/wms",
          layers  = "GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2",
          options = WMSTileOptions(
            format = "image/png",
            transparent = FALSE,
            version = "1.3.0"
          ),
          group = "Plan IGN"
        ) %>%
        
#RPG
        addWMSTiles(
          baseUrl = "https://data.geopf.fr/wms-r/wms",
          layers  = "LANDUSE.AGRICULTURE2024",
          options = WMSTileOptions(
            format = "image/png",
            transparent = FALSE,
            version = "1.3.0"
          ),
          group = "Registre Parcellaire Graphique"
        ) %>%

#Départements
        addPolygons(
          data = dep_aura_4326,
          color = "#7b7b7b",
          weight = 2,
          fillColor = NA,
          fillOpacity = 0,
          popup = ~paste(nom_officiel, sep = "<br/>"),
          group = "Departement"
        ) %>%
        
#Communes AURA
        addPolygons(
          data = commune_aura,
          color = "#929292",
          weight = 1,
          fillColor = NA,
          fillOpacity = 0,
          popup = ~paste(nom_officiel, sep = "<br/>"),
          group = "Communes",
          label="nom_officiel"
        ) %>%
        

#CLS 
        addPolygons(
          data = couche_cls_4326,
          color = "#869ECE",
          weight = 2,
          fillOpacity = 0.7,
          popup = ~paste(Nom_CLS, sep= "<br/>"),
          group = "Contrats locaux de santé"
        ) %>%
        
#PAT 
        addPolygons(
          data = couche_pat_4326,
          color = ~pal_pat(niveau),
          fillColor = ~pal_pat(niveau),
          weight = 3,
          fillOpacity = 0.35,
          popup = ~paste(nom_du_pat,niveau,pop_hab, sep= "<br/>"),
          group = "Projet Alimentaire Territoriaux"
        )%>%
        
#Cercle population
        addCircleMarkers(
          data = communes_centroid,
          radius = rayon_pop,
          fillColor = "#CE614A",
          color = "#ffffff",
          weight = 1,
          fillOpacity = 0.7,
          popup = ~paste(
            "<strong>", nom_officiel, "</strong><br/>",
            "Population :", population
          ),
          group = "Population communale"
        ) %>%
        
#Cercle SAU
        addCircleMarkers(
          data = communes_centroid,
          radius = rayon_sau,
          fillColor = "#CE614A",
          color = "#ffffff",
          weight = 1,
          fillOpacity = 0.7,
          popup = ~paste(
            "<strong>", nom_officiel, "</strong><br/>",
            "SAU (ha) :", rpg_ha_sum
          ),
          group = "SAU"
        ) %>%
        
#Cercle SAU BIO
        addCircleMarkers(
          data = communes_centroid,
          radius = rayon_saubio,
          fillColor = ~pal_bio(part_bio),
          color = "#ffffff",
          weight = 1,
          fillOpacity = 1,
          popup = ~paste(
            "<strong>", nom_officiel, "</strong><br/>",
            "SAU Bio (ha) :", bio_ha_sum/2,"<br/>",
            "Part de la SAU Bio (%) :", part_bio
          ),
          group = "SAU bio"
        )
    })
    
#Paramétrages du sélecteur de couches 
#Fonds de plans 
    observe({
      proxy <- leafletProxy("map")
      
#Cache les fonds de plan à l'ouverture de la carte 
      proxy %>% hideGroup("Plan IGN")
      proxy %>% hideGroup("Registre Parcellaire Graphique")
      proxy %>% hideGroup("OSM")
      
#Pour afficher la couche choisi par utilisateur 
      if (input$fond == "ign"){ #Si l'utilisateur choisi IGN, affiche la couche IGN 
        proxy %>% showGroup("Plan IGN")
      }
      
      if (input$fond == "rpg"){ #Si l'utilisateur choisi RPG, affiche la couche RPG 
        proxy %>% showGroup("Registre Parcellaire Graphique")
      }
      
      if (input$fond == "osm"){#Si l'utilisateur choisi OSM, affiche la couche OSM
        proxy %>% showGroup("OSM")
      }
    })  
    
#Polygones
    observe({
      proxy <- leafletProxy("map")
      
      if (input$pat_layer){
        proxy %>% showGroup("Projet Alimentaire Territoriaux")
      } else {
        proxy %>%  hideGroup("Projet Alimentaire Territoriaux")
      }
      if (input$cls_layer){
        proxy %>% showGroup("Contrats locaux de santé")
      } else {
        proxy %>%  hideGroup("Contrats locaux de santé")
      }
      if (input$com_layer){
        proxy %>% showGroup("Communes")
      } else {
        proxy %>%  hideGroup("Communes")
      }
      if(input$dep_layer){
        proxy %>% showGroup("Departement")
      }else{
        proxy %>%  hideGroup("Departement")
      }
    })
    
#Indicateurs 
    observe({
      proxy <- leafletProxy("map")
      
      proxy %>% hideGroup("Population communale")
      proxy %>% hideGroup("SAU")
      proxy %>% hideGroup("SAU bio")
      
      if (input$indicateur == "pop"){
        proxy %>% showGroup("Population communale")
      }
      if (input$indicateur == "sau"){
        proxy %>% showGroup("SAU")
      }
      if (input$indicateur == "bio"){
        proxy %>%  showGroup("SAU bio")
      }
    })
    
    
#Paramétrages de l'action déclenchée par le bouton recherche 
    observeEvent(input$search_button, {
      req(input$nom_du_pat)
      
#Normalise la recherche (évite la sensibilité à la casse notamment)
      recherche <- tolower(trimws(input$nom_du_pat))
      
#Recherche exacte du nom du PAT
      selection_pat <- couche_pat_4326[
        tolower(trimws(couche_pat_4326$nom_du_pat)) == recherche, #Recherche la valeur exacte dans le champ mais désensibilise aussi
      ]
      
      if (nrow(selection_pat) > 0) { #Si le résultat > 0, zoom sur le résultat 
        
#Zoom animé sur l'emprise du PAT trouvé
        bb <- st_bbox(selection_pat)
        leafletProxy("map") %>% #Zoom en prenant la BBOX du PAT
          flyToBounds(
            lng1 = unname(bb["xmin"]),
            lat1 = unname(bb["ymin"]),
            lng2 = unname(bb["xmax"]),
            lat2 = unname(bb["ymax"])
          ) 
        
        return()
      }
      
#Recherche exacte du nom de la commune 
      selection_com <- commune_aura[
        tolower(trimws(commune_aura$nom_officiel)) == recherche, #recherche le nom exact mais désensibilise aussi
      ]
      
      if (nrow(selection_com) == 0) { #Si aucune réponse trouvée, affiche un message d'erreur 
        showNotification("PAT ou commune non trouvé", type = "warning")
        return()
      }
      
      bb <- st_bbox(selection_com) #prend en compte les limites du polygones pour le zoom (centroide impossible car multipolygones)
      
      leafletProxy("map") %>%
        flyToBounds(  #Zoom en prenant la BBOX de la commune 
          lng1 = unname(bb["xmin"]),
          lat1 = unname(bb["ymin"]),
          lng2 = unname(bb["xmax"]),
          lat2 = unname(bb["ymax"])
        )})
    
    
#Paramétrages des filtres (combinés)
    observe({
      proxy <- leafletProxy("map")
      
#Obligatoire de recréer la palette dans cet observe pour que elle soit effective
      pal_pat <- colorFactor(
        palette = c("#fbe769", "#E4794A"),
        domain = couche_pat_4326$niveau
      )
      
#On enlève la couche PAT existante (évite superposition)
      proxy %>% clearGroup("Projet Alimentaire Territoriaux")
      
#On duplique la couche des PAT pour filtrer soit à l'échelle soit au niveau sans modifier la couche initiale
      pat_filtre <- couche_pat_4326
      
#Filtre niveau : vérifie si l'utilisateur a choisi un niveau et affiche les PAT résultats 
      if (!is.null(input$filtre_niveau) && input$filtre_niveau != "Tous") {
        pat_filtre <- pat_filtre[pat_filtre$niveau == input$filtre_niveau, ] #combinaison des filtres
      }
      
#Filtre échelle : vérifie si l'utilisateur a choisi une échelle et affiche les PAT résultats 
      if (!is.null(input$filtre_niveau_terri) && input$filtre_niveau_terri != "Tous") {
        pat_filtre <- pat_filtre[pat_filtre$echelle == input$filtre_niveau_terri, ]#combinaison des filtres
      }
      
#Réaffichage uniquement de la sélection 
      proxy %>% addPolygons(
        data = pat_filtre,
        color = ~pal_pat(niveau),
        fillColor = ~pal_pat(niveau),
        weight = 3,
        fillOpacity = 0.35,
        popup = ~paste(nom_du_pat,niveau,pop_hab, sep= "<br/>"),
        group = "Projet Alimentaire Territoriaux"
      )
  })
}
    

#################################LANCEMENT DE L'APPLICATION#########################################################
shinyApp(ui, server)
