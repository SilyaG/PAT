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
    class = "fr-container",
    br(),
    leafletOutput("map", height = "800px")
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
        group = "Communes"
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
}

#
shinyApp(ui, server)







