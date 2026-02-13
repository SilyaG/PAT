library(shiny)
library(leaflet)
library(sf)

#Chargement des couches 
couche_pat <- st_read("./data/pat_112025_etudiant.gpkg")
admin_express_com<- st_read("./data/communes.gpkg")


#Reprojection
couche_pat_4326 <- st_transform(couche_pat, crs = 4326)
commune_aura<- st_transform(admin_express_com, crs = 4326)

#ui
ui <- fluidPage(
  titlePanel("Test Shiny PAT"), #Titre
  mainPanel(
    leafletOutput("map", height = "800px") #Taille de la carte
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
        layers  = "LANDUSE.AGRICULTURE2023",
        options = WMSTileOptions(
          format = "image/png",
          transparent = TRUE,
          version = "1.3.0"
        ),
        group = "RPG"
      ) %>%
      
      # couche Pat
      addPolygons(
        data = couche_pat_4326,
        color = "orange",
        weight = 2,
        fillOpacity = 0.4,
        popup = ~paste(nom_du_pat,niveau,pop_hab, sep= "<br/>"),
        group = "PAT"
      ) %>%
      
      
      # Commune AURA
      addPolygons(
        data = commune_aura,
        color = "black",
        weight = 1,
        fillColor = NA,
        fillOpacity = 0,
        popup = ~paste(nom_officiel, sep = "<br/>"),
        group = "COM"
      ) %>%
      
      
      # Menu couche
      addLayersControl(
        baseGroups = c("OSM"),
        overlayGroups = c("RPG", "PAT","COM"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}

#
shinyApp(ui, server)





