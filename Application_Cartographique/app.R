library(shiny)
library(leaflet)
library(sf)
library(readr)
library(stringi)

# Chargement des couches 
couche_pat <- st_read("./data/pat_aura_112025.gpkg")
admin_express_com <- st_read("./data/communes.gpkg")
couche_cls <- st_read("./data/cls_aura.gpkg")
dep_aura <- st_read("./data/departement_aura.gpkg")

# Chargement de la table pat_com
pat_com <- read_csv("./data/pat_com.csv")

# Reprojection
couche_pat_4326 <- st_transform(couche_pat, crs = 4326)
commune_aura <- st_transform(admin_express_com, crs = 4326)
couche_cls_4326 <- st_transform(couche_cls, crs = 4326)
dep_aura_4326 <- st_transform(dep_aura, crs = 4326)

# UI
ui <- fluidPage(
  
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
              tags$p(
                class = "fr-logo",
                "République\nFrançaise"
              )
            )
          ),
          tags$div(
            class = "fr-header__service",
            tags$p(
              class = "fr-header__service-title",
              "Cartographie des Projets Alimentaires Territoriaux"
            ),
            tags$p(
              class = "fr-header__service-tagline",
              "Région Auvergne-Rhône-Alpes"
            )
          )
        )
      )
    )
  ),
  
  tags$main(
    class = "fr-container-fluid",
    br(),
    
    
    #Barre de recherche 
    tags$div(
      class = "fr-search-bar", #Stle barre de recherche
      role = "search",  #Zone ou l'on tape l'objet recherche
      style = "max-width: 300px; margin-left: auto;", #Taille et placement
      
      tags$label(   
        class = "fr-label",
        `for` = "nom_du_pat",
      ),
      
      tags$input(
        class = "fr-input",
        id = "nom_du_pat",   #identifiant
        type = "search",
        placeholder = "Rechercher une Commune ou un PAT",
        `aria-describedby` = "search_input_messages"
      ),
      
      tags$div(
        class = "fr-messages-group",
        id = "search_input_messages",
        `aria-live` = "polite"
      ),
      
      #Bouton rechercher
      actionButton(
        inputId = "search_button",
        label = "Rechercher",
        class = "fr-btn"
      )
    ),
    
    br(),
    leafletOutput("map", height = "800px")
  ),
  
  tags$footer(
    class = "fr-footer",
    tags$div(
      class = "fr-container",
      tags$p("© République Française - Tous droits réservés")
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    bbox <- st_bbox(commune_aura)
    
    xmin <- unname(bbox["xmin"])
    ymin <- unname(bbox["ymin"])
    xmax <- unname(bbox["xmax"])
    ymax <- unname(bbox["ymax"])
    
    leaflet(
      options = leafletOptions(
        minZoom = 8,
        maxZoom = 11
      )
    ) %>%
      addProviderTiles("OpenStreetMap", group = "OSM") %>%
      fitBounds(xmin, ymin, xmax, ymax) %>%
      setMaxBounds(xmin, ymin, xmax, ymax) %>%
      
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
      
      addPolygons(
        data = dep_aura_4326,
        color = "black",
        weight = 1,
        fillColor = NA,
        fillOpacity = 0,
        popup = ~paste(nom_officiel, sep = "<br/>"),
        group = "Departement"
      ) %>%
      
      addPolygons(
        data = commune_aura,
        color = "black",
        weight = 1,
        fillColor = NA,
        fillOpacity = 0,
        popup = ~paste(nom_officiel, sep = "<br/>"),
        group = "Communes"
      ) %>%
      
      addPolygons(
        data = couche_cls_4326,
        color = "white",
        weight = 2,
        fillOpacity = 0.4,
        popup = ~paste(Nom_CLS, sep= "<br/>"),
        group = "Contrats locaux de santé"
      ) %>%
      
      addPolygons(
        data = couche_pat_4326,
        color = "orange",
        weight = 2,
        fillOpacity = 0.4,
        popup = ~paste(nom_du_pat, niveau, pop_hab, sep = "<br/>"),
        group = "Projet Alimentaire Territoriaux"
      ) %>%
      
      addLayersControl(
        overlayGroups = c("Projet Alimentaire Territoriaux", "Contrats locaux de santé", "Communes", "Departement", "Registre Parcellaire Graphique"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # Fonction pour barre de recherche PAT ou Commune
  observeEvent(input$search_button, {
    req(input$nom_du_pat)
    
    # Caractère particulier fonctionne (a conserver ?)
    recherche <- tolower(trimws(input$nom_du_pat))
    
    # ) Recherche exacte du nom du PAT
    selection_pat <- couche_pat_4326[
      tolower(trimws(couche_pat_4326$nom_du_pat)) == recherche, #Recherche la valeur exacte dans le champ
    ]
    
    if (nrow(selection_pat) > 0) {
      
      
      # Zoom animé sur l'emprise du PAT trouvé
      bb <- st_bbox(selection_pat)
      
      leafletProxy("map") %>% #prend les limites du polygones Pat
        flyToBounds(
          lng1 = unname(bb["xmin"]),
          lat1 = unname(bb["ymin"]),
          lng2 = unname(bb["xmax"]),
          lat2 = unname(bb["ymax"])
        ) 
      
      return()
    }
    
    # Recherche exacte du nom de la commune 
    selection_com <- commune_aura[
      tolower(trimws(commune_aura$nom_officiel)) == recherche, #recherche le nom exact
    ]
    
    if (nrow(selection_com) == 0) {
      showNotification("PAT ou commune non trouvé", type = "warning")
      return()
    }
    
    bb <- st_bbox(selection_com) #prend en compte les limites du polygones pour le zoom (centroide impossible car multipolygones)
    
    leafletProxy("map") %>%
      flyToBounds(  #zoom suivant les extremités du polygones
        lng1 = unname(bb["xmin"]),
        lat1 = unname(bb["ymin"]),
        lng2 = unname(bb["xmax"]),
        lat2 = unname(bb["ymax"])
      ) 
  })
}

shinyApp(ui, server)


