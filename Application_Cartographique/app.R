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
          # Logo République Française
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
          # Titre de service
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
  
  # Contenu principal
  tags$main(
    style = "padding: 0;",
    leafletOutput("map", height = "85vh", width = "100%")
  ),
  
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
    
    #Palettes de couleur
    #Palette PAT
    pal_pat <- colorFactor(
      palette = c("#fbe769", "#E4794A"),
      domain = couche_pat_4326$niveau
    )
    
    # Centroïdes des communes
    communes_centroid <- st_centroid(commune_aura)
    
    # Variable utilisée pour les communes
    pop_com <- communes_centroid$population
    
    rayon_brut <- sqrt(pop_com)
    
    # Rayon proportionnel (mise à l'échelle)
    rayon <- scales::rescale(rayon_brut, to = c(1, 50))
    
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
          transparent = TRUE,
          version = "1.3.0"
        ),
        group = "Plan IGN"
      ) %>%
      
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
        group = "Communes"
      ) %>%
      
      # Cercle population
      addCircleMarkers(
        data = communes_centroid,
        radius = rayon,
        fillColor = "#CE614A",
        color = "#ffffff",
        weight = 1,
        fillOpacity = 0.7,
        popup = ~paste(
          "<strong>", nom_officiel, "</strong><br/>",
          "Valeur :", population
        ),
        group = "Indicateur communal"
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
      ) %>%
      
      # Menu couche
      addLayersControl(
        overlayGroups = c("Projet Alimentaire Territoriaux","Contrats locaux de santé","Communes","Departement","Registre Parcellaire Graphique","Plan IGN","Indicateur communal"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}

#
shinyApp(ui, server)