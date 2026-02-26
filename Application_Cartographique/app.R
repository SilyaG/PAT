library(shiny)
library(leaflet)
library(sf)
library(readr)
library(leaflet.extras2)


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
    )
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
  # Carte pleine largeur
  leafletOutput("map", height = "90vh"),
  
  # Footer officiel
  tags$footer(
    class = "fr-footer",
    tags$div(
      class = "fr-container",
      tags$p("© République Française - Tous droits réservés")
    )
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
    
    #Limitation du dézoom
    leaflet(
      options = leafletOptions(
        minZoom = 8,
        maxZoom = 11
      )
    ) %>%
      addProviderTiles("OpenStreetMap", group = "OSM") %>%
      fitBounds(xmin, ymin, xmax, ymax) %>%
      setMaxBounds(xmin, ymin, xmax, ymax) %>%
      
    
      
      
      # RGA landuse
      addWMSTiles(
        baseUrl = "https://data.geopf.fr/wms-r/wms",
        layers  = "LANDUSE.AGRICULTURE2024",
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          version = "1.3.0"
        ),
        group = "Registre Parcellaire Graphique"
      ) %>%
      
      # Departement AURA
      addPolygons(
        data = dep_aura_4326,
        color = "black",
        weight = 1,
        fillColor = NA,
        fillOpacity = 0,
        popup = ~paste(nom_officiel, sep = "<br/>"),
        group = "Departement"
      ) %>%
      
      
      
      
      # Commune AURA
      addPolygons(
        data = commune_aura,
        color = "black",
        weight = 1,
        fillColor = NA,
        fillOpacity = 0,
        popup = ~paste(nom_officiel, sep = "<br/>"),
        group = "Communes",
        label="nom_officiel"
      ) %>%

      
      # couche CLS
      addPolygons(
        data = couche_cls_4326,
        color = "white",
        weight = 2,
        fillOpacity = 0.4,
        popup = ~paste(Nom_CLS, sep= "<br/>"),
        group = "Contrats locaux de santé"
      ) %>%
      
      # couche Pat
      addPolygons(
        data = couche_pat_4326,
        color = "orange",
        weight = 2,
        fillOpacity = 0.4,
        popup = ~paste(nom_du_pat,niveau,pop_hab, sep= "<br/>"),
        group = "Projet Alimentaire Territoriaux"
      ) %>%
      
      # Menu couche
      addLayersControl(
        overlayGroups = c("Projet Alimentaire Territoriaux","Contrats locaux de santé","Communes","Departement","Registre Parcellaire Graphique"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  # ---- Filtrage dynamique PAT via liste déroulante (niveau)----
  # Filtrage combiné PAT (niveau + échelle)
  observe({
    proxy <- leafletProxy("map")
    
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
      color = "orange",
      weight = 2,
      fillOpacity = 0.4,
      popup = ~paste(nom_du_pat, niveau, pop_hab, sep = "<br/>"),
      group = "Projet Alimentaire Territoriaux"
    )
  })
}

#
shinyApp(ui, server)







