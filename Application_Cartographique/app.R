library(shiny)
library(leaflet)
library(sf)
library(readr)

#Chargement des couches 
couche_pat <- st_read("./data/pat_aura_112025.gpkg")
admin_express_com<- st_read("./data/communes.gpkg")
couche_cls<- st_read("./data/cls_aura.gpkg")
dep_aura <- st_read("./data/departement_aura.gpkg")

#Chargement de la table pat_com
pat_com<- read_csv("./data/pat_com.csv")

#Reprojection
couche_pat_4326 <- st_transform(couche_pat, crs = 4326)
commune_aura<- st_transform(admin_express_com, crs = 4326)
couche_cls_4326<- st_transform(couche_cls, crs = 4326)
dep_aura_4326 <- st_transform(dep_aura, crs = 4326)




#ui
ui <- fluidPage(
  
  # Inclusion DSFR depuis CDN
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/@gouvfr/dsfr@1.12.1/dist/dsfr.min.css"
    ),
    tags$script(
      src = "https://cdn.jsdelivr.net/npm/@gouvfr/dsfr@1.12.1/dist/dsfr.module.min.js",
      type = "module"
    ),
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
  
  # Header officiel
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
  
  # Filtres au-dessus de la carte
  tags$div(
    style = "display: flex; gap: 20px; margin: 50px 0 20px 0;",  # flex pour côte à côte
  style = "display: flex; gap: 20px; margin: 50px 0 20px 0;",  # flex pour côte à côte
  # Premier filtre
  tags$div(
    style = "width: 250px;",
    tags$select(
      id = "filtre_niveau",
      class = "fr-select",
      style = "background-color: #d3d3d3; color: black;",  # fond gris clair et texte noir tout le temps
      tags$option("Sélectionner un niveau de labellisation", value = "", selected = TRUE, disabled = TRUE),
      tags$option(value = "Tous", "Tous les niveaux"),
      tags$option(value = "1", "Niveau 1"),
      tags$option(value = "2", "Niveau 2")
    )
  ),
  # Deuxième filtre
  tags$div(
    style = "width: 250px;",
    tags$select(
      id = "filtre_niveau_terri",
      class = "fr-select",
      style = "background-color: #d3d3d3; color: black;",  # fond gris clair et texte noir tout le temps
      tags$option("Sélectionner l'échelle du territoire", value = "", selected = TRUE, disabled = TRUE),
      tags$option(value = "Tous", "Toutes les échelles"),
      tags$option(value = "PAT interterritorial (PAiT)", "Interterritorial (PAiT)"),
      tags$option(value = "PAT d'échelle intercommunale", "Intercommunale"),
      tags$option(value = "PAT d'échelle départementale", "Départementale")
    )
  )
),

# Création de colone pour la carte et le menu de couche 

#menu lateral gauche 
fluidRow(
  
  column(
    width = 2,
    
    div(class = "menu-couches",
        
        h4("Fond cartographique"),
        
        radioButtons(
          "fond",
          label = NULL,
          choices = c(
            "Plan IGN" = "ign",
            "Registre Parcellaire Graphique" = "rpg",
            "OpenStreetMap" = "osm"
          ),
          selected = "ign"
        ),
        
        hr(),
        
        h4("Couches"),
        
        checkboxInput("pat_layer", "Projet Alimentaire Territoriaux", TRUE),
        checkboxInput("cls_layer", "Contrat Locaux de Santé", FALSE),
        checkboxInput("dep_layer", "Départements", FALSE),
        checkboxInput("com_layer", "Communes",FALSE),
        
        hr(),
        
        conditionalPanel(
          condition = "input.com_layer == true",
          
          h4("Indicateurs communaux"),
          
          radioButtons(
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

#Intégration de la carte 
column(
  width = 9,
  leafletOutput("map", height = "90vh")
   )
  )
 )

  # Footer officiel
  tags$footer(
    class = "fr-footer",
    tags$div(
      class = "fr-container",
      tags$p("© République Française - Tous droits réservés")
  )
)


#Carte
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    bbox <- st_bbox(commune_aura) #bbox caler sur commune aura
    
    xmin <- unname(bbox["xmin"])
    ymin <- unname(bbox["ymin"])
    xmax <- unname(bbox["xmax"])
    ymax <- unname(bbox["ymax"])
    
    #Cercle proportionnel (création des rayons des cercles)
    # Centroïdes des communes
    communes_centroid <- st_centroid(commune_aura)
    
    # Part de SAU bio
    part_bio <- communes_centroid$bio_ha_sum/2 / communes_centroid$rpg_ha_sum
    
    # Sécurisation (évite division par 0 et NA)
    part_bio[is.na(part_bio) | is.infinite(part_bio)] <- 0
    
    #Population
    pop_com <- communes_centroid$population
    rayon_brut_pop <- sqrt(pop_com)
    rayon_pop <- scales::rescale(rayon_brut_pop, to = c(1, 50))
    
    #SAU
    sau_com <- communes_centroid$rpg_ha_sum
    rayon_brut_sau <- sqrt(sau_com)
    rayon_sau <- scales::rescale(rayon_brut_sau, to = c(1, 30))
    
    #SAU_bio
    saubio_com <- communes_centroid$bio_ha_sum
    rayon_brut_saubio <- sqrt(saubio_com)
    rayon_saubio <- scales::rescale(rayon_brut_saubio, to = c(1, 30))
    
    ##Palettes de couleur
    #Palette PAT
    pal_pat <- colorFactor(
      palette = c("#fbe769", "#E4794A"),
      domain = couche_pat_4326$niveau
    )
    
    #Palette % SAU bio
    pal_bio <- colorNumeric(
      palette = c("#bcd9a3","#306600"),
      domain = part_bio,
      na.color = "transparent"
    )
    
    #Limitation du dézoom
    leaflet(
      options = leafletOptions(
        minZoom = 6,
        maxZoom = 15
      )
    ) %>%
      addProviderTiles("OpenStreetMap", group = "OSM") %>%
      fitBounds(xmin, ymin, xmax, ymax) %>%
      setMaxBounds(xmin, ymin, xmax, ymax) %>%
      
      # Plan IGN
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
      
      # RPG landuse
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
      
      # Departement AURA
      addPolygons(
        data = dep_aura_4326,
        color = "#7b7b7b",
        weight = 2,
        fillColor = NA,
        fillOpacity = 0,
        popup = ~paste(nom_officiel, sep = "<br/>"),
        group = "Departement"
      ) %>%
      
      # Commune AURA
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

      
      # Cercle population
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
      
      # Cercle SAU
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
      
      # Cercle SAU bio
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
      ) %>%
      
      # couche CLS
      addPolygons(
        data = couche_cls_4326,
        color = "#869ECE",
        weight = 2,
        fillOpacity = 0.7,
        popup = ~paste(Nom_CLS, sep= "<br/>"),
        group = "Contrats locaux de santé"
      ) %>%
      
      # couche Pat
      addPolygons(
        data = couche_pat_4326,
        color = ~pal_pat(niveau),
        fillColor = ~pal_pat(niveau),
        weight = 3,
        fillOpacity = 0.35,
        popup = ~paste(nom_du_pat,niveau,pop_hab, sep= "<br/>"),
        group = "Projet Alimentaire Territoriaux"
      ) 
  })
  
#Affichage des couches en fonction du choix utilisateur dans le menu de selection
observe({
  proxy <- leafletProxy("map")
  
  #pour cacher les fonds de plan
  
  proxy %>% hideGroup("Plan IGN")
  proxy %>% hideGroup("Registre Parcellaire Graphique")
  proxy %>% hideGroup("OSM")
  
  #Pour afficher la couche choisi par utilisateur 
  if (input$fond == "ign"){
    proxy %>% showGroup("Plan IGN")
  }
  
  if (input$fond == "rpg"){
    proxy %>% showGroup("Registre Parcellaire Graphique")
  }
  
  if (input$fond == "osm"){
    proxy %>% showGroup("OSM")
  }
})  

#Affichage des couches de polygones (PAT; CLS; Communes ; Departement)

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

#Affichage des couches d'indicateurs 

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

  
  # ---- Filtrage dynamique PAT via liste déroulante (niveau)----
  # Filtrage combiné PAT (niveau + échelle)
  observe({
    proxy <- leafletProxy("map")
    
    pal_pat <- colorFactor(
      palette = c("#fbe769", "#E4794A"),
      domain = couche_pat_4326$niveau
    )
    
    # On enlève la couche PAT existante
    proxy %>% clearGroup("Projet Alimentaire Territoriaux")
    
    # On commence avec toute la couche
    pat_filtre <- couche_pat_4326
    
    # Filtre niveau
    if (!is.null(input$filtre_niveau) && input$filtre_niveau != "Tous") {
      pat_filtre <- pat_filtre[pat_filtre$niveau == input$filtre_niveau, ]
    }
    
    # Filtre échelle
    if (!is.null(input$filtre_niveau_terri) && input$filtre_niveau_terri != "Tous") {
      pat_filtre <- pat_filtre[pat_filtre$echelle == input$filtre_niveau_terri, ]
    }
    
    # Réaffichage
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

#
shinyApp(ui, server)