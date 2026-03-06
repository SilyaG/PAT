########################################### PROJET CAROT ########################################################
## APPEL AUX LIBRAIRIES NÉCESSAIRES À LA CRÉATION DE LA CARTE INTERACTIVE ##
library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(readr)
library(stringi)
library(jsonlite)
library(scales)
library(stringr)

# remotes::install_github("trafficonese/leaflet.extras")

# Petit helper sécurisé
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

# Chargement des couches et reprojection en 4326
couche_pat_4326 <- st_read("./data/pat_aura_112025.gpkg", quiet = TRUE) %>%
  st_transform(4326)

commune_aura <- st_read("./data/communes.gpkg", quiet = TRUE) %>%
  st_transform(4326)

couche_cls_4326 <- st_read("./data/cls_aura.gpkg", quiet = TRUE) %>%
  st_transform(4326)

dep_aura_4326 <- st_read("./data/departement_aura.gpkg", quiet = TRUE) %>%
  st_transform(4326)

reg_aura_4326 <- st_read("./data/region.gpkg", quiet = TRUE) %>%
  st_transform(4326)

pat_com <- read_csv("./data/pat_com.csv", show_col_types = FALSE)

# Normalisation robuste des codes INSEE
commune_aura$code_insee_chr <- str_pad(as.character(commune_aura$code_insee), width = 5, side = "left", pad = "0")
pat_com$code_insee_chr      <- str_pad(as.character(pat_com$code_insee),      width = 5, side = "left", pad = "0")

# Autocomplétion : listes séparées (Communes vs PAT)
autocomplete_communes <- sort(unique(na.omit(commune_aura$nom_officiel)))
autocomplete_pats     <- sort(unique(na.omit(couche_pat_4326$nom_du_pat)))

# Palette PAT
pal_pat <- colorFactor(
  palette = c("#5576c0", "#ff732c", "#1f8d49"),
  domain  = couche_pat_4326$echelle
)

########################################### Partie UI #############################################################
ui <- fluidPage(
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/@gouvfr/dsfr@1.12.1/dist/dsfr.min.css"
    ),
    tags$script(
      src = "https://cdn.jsdelivr.net/npm/@gouvfr/dsfr@1.12.1/dist/dsfr.module.min.js",
      type = "module"
    ),
    tags$link(
      href = "https://cdn.jsdelivr.net/npm/remixicon@3.5.0/fonts/remixicon.css",
      rel = "stylesheet"
    ),
    
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "app_styles.css"
    ),
    
    tags$script(HTML(sprintf(
      "window.AUTOCOMPLETE_COMMUNES = %s;\nwindow.AUTOCOMPLETE_PATS = %s;",
      jsonlite::toJSON(autocomplete_communes, auto_unbox = TRUE),
      jsonlite::toJSON(autocomplete_pats, auto_unbox = TRUE)
    ))),
    
    tags$script(src = "autocomplete.js"),
    tags$script(src = "legende.js"),
    tags$script(src = "liste_pat.js"),
    tags$script(src = "hachure_pat.js"),
    tags$script(src = "tutoriel.js"),
    tags$script(src = "popup_pat.js")
  ),
  
  includeHTML("www/intro_overlay.html"),
  includeHTML("www/header.html"),
  
  tags$main(
    class = "fr-container-fluid",
    br(),
    
    tags$div(
      style = "display:flex; gap:20px; align-items:flex-end; margin:0px 0 20px 0; padding:0;",
      
      tags$div(
        style = "display:flex; gap:20px; margin:0; padding:0;",
        
        tags$div(
          style = "width:250px; margin:0; padding:0;",
          tags$select(
            id = "filtre_niveau",
            class = "fr-select",
            style = "color:black; margin:0;",
            tags$option(
              "Sélectionner un niveau de labellisation",
              value = "",
              selected = TRUE,
              disabled = TRUE
            ),
            tags$option(value = "Tous", "Tous les niveaux"),
            tags$option(value = "1", "Niveau 1"),
            tags$option(value = "2", "Niveau 2")
          )
        ),
        
        tags$div(
          style = "width:250px; margin:0; padding:0;",
          tags$select(
            id = "filtre_niveau_terri",
            class = "fr-select",
            style = "color:black; margin:0;",
            tags$option(
              "Sélectionner l'échelle du territoire",
              value = "",
              selected = TRUE,
              disabled = TRUE
            ),
            tags$option(value = "Tous", "Toutes les échelles"),
            tags$option(value = "PAT interterritorial (PAiT)", "Interterritorial (PAiT)"),
            tags$option(value = "PAT d'échelle intercommunale", "Intercommunale"),
            tags$option(value = "PAT d'échelle départementale", "Départementale")
          )
        )
      ),
      
      tags$div(
        style = "display:flex; align-items:flex-end; gap:8px; margin-left:auto;",
        
        tags$button(
          id = "info_tutorial",
          class = "fr-btn fr-btn--secondary",
          type = "button",
          tags$i(class = "ri-question-line", style = "margin-right:6px;"),
          "Tutoriel"
        ),
        
        tags$div(
          class = "fr-search-bar",
          role = "search",
          style = "width:250px; margin:0;",
          
          tags$label(
            class = "fr-label",
            `for` = "nom_du_pat"
          ),
          
          tags$div(
            id = "autocomplete_panel",
            class = "autocomplete-panel",
            tags$div(
              class = "autocomplete-card",
              tags$div(class = "autocomplete-title", "Communes"),
              tags$ul(id = "suggest_communes", class = "autocomplete-list"),
              tags$div(class = "autocomplete-sep"),
              tags$div(class = "autocomplete-title", "PAT"),
              tags$ul(id = "suggest_pats", class = "autocomplete-list")
            )
          ),
          
          tags$input(
            class = "fr-input",
            id = "nom_du_pat",
            type = "search",
            placeholder = "Rechercher une Commune ou un PAT",
            `aria-describedby` = "search_input_messages"
          ),
          
          tags$div(
            class = "fr-messages-group",
            id = "search_input_messages",
            `aria-live` = "polite"
          ),
          
          actionButton(
            inputId = "search_button",
            label = "Rechercher",
            class = "fr-btn"
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 2,
        div(
          class = "menu-couches",
          h4("Fond cartographique"),
          radioButtons(
            "fond",
            label = NULL,
            choices = c(
              "Plan IGN" = "ign",
              "Registre Parcellaire Graphique" = "rpg",
              "OpenStreetMap" = "osm",
              "Limites administratives" = "fond_admin"
            ),
            selected = "ign"
          ),
          
          hr(),
          
          h4("Couches"),
          checkboxInput("pat_layer", "Projet Alimentaire Territoriaux", TRUE),
          checkboxInput("cls_layer", "Contrat Locaux de Santé", FALSE),
          checkboxInput("dep_layer", "Départements", FALSE),
          checkboxInput("com_layer", "Communes", FALSE),
          
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
      
      column(
        width = 8,
        div(
          style = "position: relative;",
          leafletOutput("map", height = "80vh"),
          includeHTML("www/popup_pat.html")
        )
      ),
      
      column(
        width = 2,
        div(
          id = "right_sidebar",
          class = "right-panel visible-sidebar",
          uiOutput("pat_sidemenu")
        )
      )
    ),
    
    includeHTML("www/footer.html"),
    includeHTML("www/tutorial_modal.html")
  )
)

########################################### Partie SERVER ###########################################################
server <- function(input, output, session) {
  
  # PAT actif = nom du PAT sélectionné
  pat_actif <- reactiveVal(NULL)
  
  # Pour éviter le reset juste après un clic PAT
  clic_sur_pat <- reactiveVal(FALSE)
  
  expand_bbox <- function(bbox, factor = 1.25) {
    xmid <- (bbox["xmin"] + bbox["xmax"]) / 2
    ymid <- (bbox["ymin"] + bbox["ymax"]) / 2
    dx <- (bbox["xmax"] - bbox["xmin"]) * factor / 2
    dy <- (bbox["ymax"] - bbox["ymin"]) * factor / 2
    
    bbox2 <- bbox
    bbox2["xmin"] <- xmid - dx
    bbox2["xmax"] <- xmid + dx
    bbox2["ymin"] <- ymid - dy
    bbox2["ymax"] <- ymid + dy
    bbox2
  }
  
  bbox_init <- sf::st_bbox(commune_aura)
  bbox_init <- expand_bbox(bbox_init, factor = 1.1)
  
  # Pré-calcul centroids + rayons indicateurs
  communes_centroid <- st_centroid(commune_aura)
  communes_centroid$code_insee_chr <- str_pad(as.character(communes_centroid$code_insee), width = 5, side = "left", pad = "0")
  
  communes_centroid$part_bio[is.na(communes_centroid$part_bio) | is.infinite(communes_centroid$part_bio)] <- 0
  communes_centroid$rayon_pop <- scales::rescale(sqrt(communes_centroid$population), to = c(1, 50))
  communes_centroid$rayon_sau <- scales::rescale(sqrt(communes_centroid$rpg_ha_sum), to = c(1, 30))
  communes_centroid$rayon_bio <- scales::rescale(sqrt(communes_centroid$bio_ha_sum), to = c(1, 30))
  
  pal_bio <- colorNumeric(
    palette = c("#bcd9a3", "#306600"),
    domain = communes_centroid$part_bio,
    na.color = "transparent"
  )
  
  # Reset carte si changement de filtres
  observeEvent(
    list(input$filtre_niveau, input$filtre_niveau_terri),
    {
      pat_actif(NULL)
      clic_sur_pat(FALSE)
      session$sendCustomMessage("hide_pat_popup", list())
      
      leafletProxy("map") %>%
        clearPopups() %>%
        flyToBounds(
          lng1 = unname(bbox_init["xmin"]),
          lat1 = unname(bbox_init["ymin"]),
          lng2 = unname(bbox_init["xmax"]),
          lat2 = unname(bbox_init["ymax"])
        )
    },
    ignoreInit = TRUE
  )
  
  # Popup PAT custom
  show_popup_pat <- function(pat_row) {
    contacts <- unlist(strsplit(as.character(pat_row$contacts[1] %||% ""), ";"))
    contacts <- trimws(contacts[contacts != ""])
    
    session$sendCustomMessage("show_pat_popup", list(
      nom        = as.character(pat_row$nom_du_pat[1]),
      niveau     = as.character(pat_row$niveau[1]),
      annee      = as.character(pat_row$annee[1]     %||% ""),
      population = as.character(pat_row$pop_hab[1] %||% ""),
      sau        = as.character(pat_row$rpg_ha_sum_sum[1]        %||% ""),
      bio        = as.character(pat_row$bio_ha_sum_sum[1]        %||% ""),
      partbio    = as.character(pat_row$part_bio[1]        %||% ""),
      contacts   = as.list(contacts)
    ))
  }
  
  # Filtres PAT
  pat_filtre <- reactive({
    pat <- couche_pat_4326
    
    if (!is.null(input$filtre_niveau) &&
        input$filtre_niveau != "Tous" &&
        input$filtre_niveau != "") {
      pat <- pat[pat$niveau == input$filtre_niveau, ]
    }
    
    if (!is.null(input$filtre_niveau_terri) &&
        input$filtre_niveau_terri != "Tous" &&
        input$filtre_niveau_terri != "") {
      pat <- pat[pat$echelle == input$filtre_niveau_terri, ]
    }
    
    pat
  })
  
  # Communes du PAT actif via code_pat -> code_insee
  communes_dans_pat_actif <- reactive({
    req(pat_actif())
    
    codes_pat <- unique(na.omit(couche_pat_4326$code_pat[couche_pat_4326$nom_du_pat == pat_actif()]))
    if (length(codes_pat) == 0) return(commune_aura[0, ])
    
    insee <- unique(na.omit(pat_com$code_insee_chr[pat_com$code_pat %in% codes_pat]))
    if (length(insee) == 0) return(commune_aura[0, ])
    
    commune_aura[commune_aura$code_insee_chr %in% insee, ]
  })
  
  select_pat <- function(pat_row, depuis_clic_carte = FALSE) {
    req(nrow(pat_row) > 0)
    
    pat_actif(pat_row$nom_du_pat[1])
    clic_sur_pat(depuis_clic_carte)
    
    # option utile du code 2 : afficher automatiquement les communes du PAT actif
    updateCheckboxInput(session, "com_layer", value = TRUE)
    
    bb <- st_bbox(pat_row)
    
    leafletProxy("map") %>%
      clearPopups() %>%
      flyToBounds(
        lng1 = unname(bb["xmin"]),
        lat1 = unname(bb["ymin"]),
        lng2 = unname(bb["xmax"]),
        lat2 = unname(bb["ymax"])
      )
    
    show_popup_pat(pat_row)
  }
  
  deselect_pat <- function() {
    pat_actif(NULL)
    clic_sur_pat(FALSE)
    session$sendCustomMessage("hide_pat_popup", list())
    
    leafletProxy("map") %>%
      clearPopups() %>%
      flyToBounds(
        lng1 = unname(bbox_init["xmin"]),
        lat1 = unname(bbox_init["ymin"]),
        lng2 = unname(bbox_init["xmax"]),
        lat2 = unname(bbox_init["ymax"])
      )
  }
  
  # Liste des PAT visibles
  pat_visibles_dans_vue <- reactive({
    if (isFALSE(input$pat_layer)) return(couche_pat_4326[0, ])
    
    pat <- pat_filtre()
    
    if (!is.null(pat_actif())) {
      pat <- pat[pat$nom_du_pat == pat_actif(), ]
    }
    
    b <- input$map_bounds
    if (is.null(b)) return(pat)
    
    bbox_view <- sf::st_bbox(
      c(xmin = b$west, ymin = b$south, xmax = b$east, ymax = b$north),
      crs = 4326
    )
    poly_view <- sf::st_as_sfc(bbox_view)
    
    idx <- sf::st_intersects(pat, poly_view, sparse = FALSE)
    pat[idx[, 1], ]
  })
  
  output$pat_sidemenu <- renderUI({
    pat <- pat_visibles_dans_vue()
    pats <- sort(unique(na.omit(pat$nom_du_pat)))
    
    if (length(pats) == 0) {
      return(tags$p("Aucun PAT visible dans la vue (ou couche PAT masquée)."))
    }
    
    tags$nav(
      class = "fr-sidemenu",
      role = "navigation",
      `aria-labelledby` = "sidemenu-title",
      tags$div(
        class = "fr-sidemenu__inner",
        
        tags$button(
          class = "fr-sidemenu__btn",
          type = "button",
          `aria-expanded` = "true",
          `aria-controls` = "sidemenu-collapse-pat",
          "PAT(s) visibles"
        ),
        
        tags$div(
          id = "sidemenu-collapse-pat",
          class = "fr-collapse fr-collapse--expanded",
          
          tags$p(
            class = "fr-sidemenu__title",
            id = "sidemenu-title",
            style = "color:#000091; font-weight:bold; font-size:18px;",
            paste0("PAT visibles : ", length(pats))
          ),
          
          tags$ul(
            class = "fr-sidemenu__list",
            lapply(pats, function(nm) {
              tags$li(
                class = "fr-sidemenu__item",
                tags$a(
                  href = "#",
                  class = "fr-sidemenu__link pat-link",
                  `data-pat` = nm,
                  nm
                )
              )
            })
          )
        )
      )
    )
  })
  
  # Carte initiale
  output$map <- renderLeaflet({
    xmin <- unname(bbox_init["xmin"])
    ymin <- unname(bbox_init["ymin"])
    xmax <- unname(bbox_init["xmax"])
    ymax <- unname(bbox_init["ymax"])
    
    leaflet(
      options = leafletOptions(
        minZoom = 6,
        maxZoom = 15
      )
    ) %>%
      addProviderTiles("OpenStreetMap", group = "OSM") %>%
      
      # Fond neutre toujours visible (base pour les limites admin)
      addProviderTiles(
        "CartoDB.PositronNoLabels",
        group = "fond_neutre_fixe",
        options = providerTileOptions(opacity = 1)
      ) %>%
      
      fitBounds(xmin, ymin, xmax, ymax) %>%
      setMaxBounds(xmin, ymin, xmax, ymax) %>%
      
      addScaleBar(
        position = "bottomleft",
        options = scaleBarOptions(
          metric = TRUE,
          imperial = FALSE,
          updateWhenIdle = TRUE,
          maxWidth = 150
        )
      ) %>%
      
      addControl(
        html = "<a id='legend_toggle' href='#' title='Afficher la légende'><i class='ri-list-check-2'></i></a>",
        position = "topleft"
      ) %>%
      
      addFullscreenControl(position = "topright") %>%
      htmlwidgets::onRender("
        function(el, x) {
          this.zoomControl.setPosition('topright');
        }
      ") %>%
      
      # Fonds
      addWMSTiles(
        baseUrl = "https://data.geopf.fr/wms-r/wms",
        layers  = "GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2",
        options = WMSTileOptions(format = 'image/png', transparent = FALSE, version = '1.3.0'),
        group = "Plan IGN"
      ) %>%
      addWMSTiles(
        baseUrl = "https://data.geopf.fr/wms-r/wms",
        layers  = "LANDUSE.AGRICULTURE2024",
        options = WMSTileOptions(format = 'image/png', transparent = FALSE, version = '1.3.0'),
        group = "Registre Parcellaire Graphique"
      ) %>%
      
      # Départements
      addPolygons(
        data = dep_aura_4326,
        color = "#7b7b7b",
        weight = 2,
        fillColor = NA,
        fillOpacity = 0,
        group = "Departement"
      ) %>%
      
      # Limites administratives
      addPolygons(
        data        = reg_aura_4326,
        color       = "#161616",
        weight      = 3,
        fillColor   = NA,
        fillOpacity = 0,
        options     = pathOptions(interactive = FALSE),  # ← clics désactivés
        group       = "Limites administratives"
      ) %>%
      
      # Départements
      addPolygons(
        data        = dep_aura_4326,
        color       = "#7b7b7b",
        weight      = 1.5,
        fillColor   = NA,
        fillOpacity = 0,
        options     = pathOptions(interactive = FALSE),  # ← clics désactivés
        group       = "Limites administratives"
      ) %>%
      
      # Communes placeholder : gérées dynamiquement ensuite
      addPolygons(
        data = commune_aura[0, ],
        color = "#929292",
        weight = 1,
        fillColor = NA,
        fillOpacity = 0,
        group = "Communes"
      ) %>%
      
      # CLS
      addPolygons(
        data = couche_cls_4326,
        color = "#6a6af4",
        weight = 2,
        fillOpacity = 0.5,
        popup = ~paste(Nom_CLS, sep = "<br/>"),
        group = "Contrats locaux de santé"
      )
  })
  
  # Gestion fonds de plan
  observe({
    proxy <- leafletProxy("map")
    
    proxy %>% hideGroup("Plan IGN")
    proxy %>% hideGroup("Registre Parcellaire Graphique")
    proxy %>% hideGroup("OSM")
    proxy %>% hideGroup("Limites administratives")
    proxy %>% hideGroup("fond_neutre_fixe")  # ← ajout
    
    if (input$fond == "ign"){
      proxy %>% showGroup("Plan IGN")
    }
    if (input$fond == "rpg") {
      proxy %>% showGroup("Registre Parcellaire Graphique")
    }
    if (input$fond == "osm") {
      proxy %>% showGroup("OSM")
    }
    if (input$fond == "fond_admin"){
      proxy %>% showGroup("fond_neutre_fixe")   # ← fond de base
      proxy %>% showGroup("Limites administratives")  # ← WMS par-dessus
    }
  })
  
  # Affichage couches simples
  observe({
    proxy <- leafletProxy("map")
    
    if (isTRUE(input$pat_layer)) {
      proxy %>% showGroup("Projet Alimentaire Territoriaux")
    } else {
      proxy %>% hideGroup("Projet Alimentaire Territoriaux")
    }
    
    if (isTRUE(input$cls_layer)) {
      proxy %>% showGroup("Contrats locaux de santé")
    } else {
      proxy %>% hideGroup("Contrats locaux de santé")
    }
    
    if (isTRUE(input$com_layer)) {
      proxy %>% showGroup("Communes")
    } else {
      proxy %>% hideGroup("Communes")
    }
    
    if (isTRUE(input$dep_layer)) {
      proxy %>% showGroup("Departement")
    } else {
      proxy %>% hideGroup("Departement")
    }
  })
  
  # Affichage dynamique des communes : uniquement celles du PAT actif
  observe({
    proxy <- leafletProxy("map")
    proxy %>% clearGroup("Communes")
    
    if (isFALSE(input$com_layer)) return()
    if (is.null(pat_actif())) return()
    
    communes_pat <- communes_dans_pat_actif()
    if (is.null(communes_pat) || nrow(communes_pat) == 0) return()
    
    proxy %>% addPolygons(
      data = communes_pat,
      color = "#929292",
      weight = 1,
      fillColor = NA,
      fillOpacity = 0,
      group = "Communes",
    )
  })
  
  # Indicateurs dynamiques : uniquement sur les communes du PAT actif
  observe({
    proxy <- leafletProxy("map")
    
    proxy %>%
      clearGroup("Population communale") %>%
      clearGroup("SAU") %>%
      clearGroup("SAU bio")
    
    if (!isTRUE(input$com_layer)) return()
    if (is.null(input$indicateur) || input$indicateur == "none") return()
    if (is.null(pat_actif())) return()
    
    communes_pat <- communes_dans_pat_actif()
    if (is.null(communes_pat) || nrow(communes_pat) == 0) return()
    
    centroid_pat <- communes_centroid[
      communes_centroid$code_insee_chr %in% communes_pat$code_insee_chr,
    ]
    
    if (nrow(centroid_pat) == 0) return()
    
    if (input$indicateur == "pop") {
      proxy %>% addCircleMarkers(
        data = centroid_pat,
        radius = ~rayon_pop,
        fillColor = "#CE614A",
        color = "#ffffff",
        weight = 1,
        fillOpacity = 0.7,
        popup = ~paste(
          "<strong>", nom_officiel, "</strong><br/>",
          "Population :", population
        ),
        group = "Population communale"
      )
    }
    
    if (input$indicateur == "sau") {
      proxy %>% addCircleMarkers(
        data = centroid_pat,
        radius = ~rayon_sau,
        fillColor = "#CE614A",
        color = "#ffffff",
        weight = 1,
        fillOpacity = 0.7,
        popup = ~paste(
          "<strong>", nom_officiel, "</strong><br/>",
          "SAU (ha) :", rpg_ha_sum
        ),
        group = "SAU"
      )
    }
    
    if (input$indicateur == "bio") {
      proxy %>% addCircleMarkers(
        data = centroid_pat,
        radius = ~rayon_bio,
        fillColor = ~pal_bio(part_bio),
        color = "#ffffff",
        weight = 1,
        fillOpacity = 1,
        popup = ~paste(
          "<strong>", nom_officiel, "</strong><br/>",
          "SAU Bio (ha) :", bio_ha_sum / 2, "<br/>",
          "Part de la SAU Bio (%) :", part_bio
        ),
        group = "SAU bio"
      )
    }
  })
  
  # Légende
  observe({
    sections <- list()
    
    if (isTRUE(input$pat_layer)) {
      sections <- append(sections, list("
        <div class='leg-cat'>Projets Alimentaires Territoriaux</div>

        <div class='leg-item' style='font-size:11px;color:#555;font-style:italic;margin-bottom:6px;'>
          Hachuré = Niveau 1 &nbsp;|&nbsp; Aplat = Niveau 2
        </div>

        <div class='leg-item' style='margin-top:4px;'>
          <svg width='14' height='14' style='flex-shrink:0;'>
            <defs><pattern id='lh1' patternUnits='userSpaceOnUse' width='4' height='4' patternTransform='rotate(45)'>
              <line x1='0' y1='0' x2='0' y2='4' stroke='#5576c0' stroke-width='1.5'/></pattern></defs>
            <rect width='14' height='14' fill='url(#lh1)' stroke='#5576c0' stroke-width='1.5' rx='2'/>
          </svg>&nbsp;PAiT — Niveau 1
        </div>
        <div class='leg-item'>
          <svg width='14' height='14' style='flex-shrink:0;'>
            <defs><pattern id='lh2' patternUnits='userSpaceOnUse' width='4' height='4' patternTransform='rotate(45)'>
              <line x1='0' y1='0' x2='0' y2='4' stroke='#ff732c' stroke-width='1.5'/></pattern></defs>
            <rect width='14' height='14' fill='url(#lh2)' stroke='#ff732c' stroke-width='1.5' rx='2'/>
          </svg>&nbsp;Intercommunal — Niveau 1
        </div>
        <div class='leg-item' style='margin-bottom:6px;'>
          <svg width='14' height='14' style='flex-shrink:0;'>
            <defs><pattern id='lh3' patternUnits='userSpaceOnUse' width='4' height='4' patternTransform='rotate(45)'>
              <line x1='0' y1='0' x2='0' y2='4' stroke='#1f8d49' stroke-width='1.5'/></pattern></defs>
            <rect width='14' height='14' fill='url(#lh3)' stroke='#1f8d49' stroke-width='1.5' rx='2'/>
          </svg>&nbsp;Départemental — Niveau 1
        </div>

        <div class='leg-item' style='margin-top:4px;'>
          <span class='swatch' style='background:#5576c0;opacity:0.35;border:2px solid #5576c0;'></span>&nbsp;PAiT — Niveau 2
        </div>
        <div class='leg-item'>
          <span class='swatch' style='background:#ff732c;opacity:0.35;border:2px solid #ff732c;'></span>&nbsp;Intercommunal — Niveau 2
        </div>
        <div class='leg-item'>
          <span class='swatch' style='background:#1f8d49;opacity:0.35;border:2px solid #1f8d49;'></span>&nbsp;Départemental — Niveau 2
        </div>
      "))
    }
    
    if (isTRUE(input$cls_layer)) {
      sections <- append(sections, list("
        <div class='leg-cat'>Contrats Locaux de Santé</div>
        <div class='leg-item'><span class='swatch' style='background:#869ECE;'></span>CLS</div>
      "))
    }
    
    if (isTRUE(input$dep_layer) && input$fond != "fond_admin") {
      sections <- append(sections, list("
        <div class='leg-cat'>Départements</div>
        <div class='leg-item'><span class='swatch-line' style='background:#7b7b7b;'></span>Limites départementales</div>
      "))
    }
    
    if (isTRUE(input$com_layer)) {
      sections <- append(sections, list("
        <div class='leg-cat'>Communes</div>
        <div class='leg-item'><span class='swatch-line' style='background:#929292;'></span>Limites communales</div>
      "))
      
      if (!is.null(input$indicateur) && input$indicateur != "none") {
        if (input$indicateur == "pop") {
          sections <- append(sections, list("
            <div class='leg-cat'>Population communale</div>
            <div class='leg-item'><span class='swatch swatch-circle' style='background:#CE614A;width:8px;height:8px;'></span>Faible</div>
            <div class='leg-item'><span class='swatch swatch-circle' style='background:#CE614A;width:14px;height:14px;'></span>Moyenne</div>
            <div class='leg-item'><span class='swatch swatch-circle' style='background:#CE614A;width:20px;height:20px;'></span>Forte</div>
          "))
        }
        
        if (input$indicateur == "sau") {
          sections <- append(sections, list("
            <div class='leg-cat'>Surface Agricole Utile (ha)</div>
            <div class='leg-item'><span class='swatch swatch-circle' style='background:#CE614A;width:8px;height:8px;'></span>&lt; 500 ha</div>
            <div class='leg-item'><span class='swatch swatch-circle' style='background:#CE614A;width:14px;height:14px;'></span>500–2000 ha</div>
            <div class='leg-item'><span class='swatch swatch-circle' style='background:#CE614A;width:20px;height:20px;'></span>&gt; 2000 ha</div>
          "))
        }
        
        if (input$indicateur == "bio") {
          sections <- append(sections, list("
            <div class='leg-cat'>Part de SAU Bio (%)</div>
            <div class='leg-item'><span class='swatch-grad'></span>
            <span style='font-size:11px;color:#666;'>0% → 100%</span></div>
          "))
        }
      }
    }
    
    if (!is.null(input$fond) && input$fond == "fond_admin") {
      sections <- append(sections, list("
        <div class='leg-cat'>Limites administratives</div>
        <div class='leg-item'><span class='swatch-line' style='background:black; height:2px;'></span>Limites régionales</div>
        <div class='leg-item'><span class='swatch-line' style='background:#7b7b7b; height:2px;'></span>Limites départementales</div>
      "))
    }
    
    content <- if (length(sections) == 0) {
      "<div style='font-size:12px;color:#888;font-style:italic;'>Aucune couche active.</div>"
    } else {
      paste(sections, collapse = "<div class='leg-sep'></div>")
    }
    
    html <- paste0("<div id='map_legend'>", content, "</div>")
    
    leafletProxy("map") %>%
      removeControl("legend_control") %>%
      addControl(
        html = html,
        position = "topleft",
        layerId = "legend_control"
      )
  })
  
  # Recherche PAT / commune
  observeEvent(input$search_button, {
    req(input$nom_du_pat)
    recherche <- tolower(trimws(input$nom_du_pat))
    
    selection_pat <- couche_pat_4326[
      tolower(trimws(couche_pat_4326$nom_du_pat)) == recherche, ]
    
    if (nrow(selection_pat) > 0) {
      select_pat(selection_pat[1, ])
      return()
    }
    
    selection_com <- commune_aura[
      tolower(trimws(commune_aura$nom_officiel)) == recherche, ]
    
    if (nrow(selection_com) == 0) return()
    
    bb <- st_bbox(selection_com)
    leafletProxy("map") %>%
      flyToBounds(
        lng1 = unname(bb["xmin"]),
        lat1 = unname(bb["ymin"]),
        lng2 = unname(bb["xmax"]),
        lat2 = unname(bb["ymax"])
      )
  })
  
  # Affichage PAT filtrés
  observe({
    proxy <- leafletProxy("map")
    proxy %>% clearGroup("Projet Alimentaire Territoriaux")
    proxy %>% clearPopups()
    
    pat_affiche <- pat_filtre()
    
    if (!is.null(pat_actif())) {
      pat_affiche <- pat_affiche[pat_affiche$nom_du_pat == pat_actif(), ]
    }
    
    if (nrow(pat_affiche) == 0) return()
    
    pat_niv2 <- pat_affiche[!is.na(pat_affiche$niveau) & pat_affiche$niveau == "2", ]
    if (nrow(pat_niv2) > 0) {
      proxy %>% addPolygons(
        data        = pat_niv2,
        layerId     = ~nom_du_pat,
        color       = ~pal_pat(echelle),
        fillColor   = ~pal_pat(echelle),
        weight      = 3,
        fillOpacity = 0.35,
        group       = "Projet Alimentaire Territoriaux"
      )
    }
    
    pat_niv1 <- pat_affiche[!is.na(pat_affiche$niveau) & pat_affiche$niveau == "1", ]
    if (nrow(pat_niv1) > 0) {
      proxy %>% addPolygons(
        data        = pat_niv1,
        layerId     = ~nom_du_pat,
        color       = ~pal_pat(echelle),
        fillColor   = ~pal_pat(echelle),
        weight      = 3,
        fillOpacity = 0.05,
        group       = "Projet Alimentaire Territoriaux"
      )
      
      couleurs_map <- setNames(
        as.list(pal_pat(pat_niv1$echelle)),
        pat_niv1$nom_du_pat
      )
      session$sendCustomMessage("apply_hatch", couleurs_map)
    }
  })
  
  # Warning si filtres sans résultat
  observeEvent(
    list(input$filtre_niveau, input$filtre_niveau_terri),
    {
      pat <- pat_filtre()
      if (nrow(pat) == 0 &&
          !is.null(input$filtre_niveau) && input$filtre_niveau != "" &&
          !is.null(input$filtre_niveau_terri) && input$filtre_niveau_terri != "") {
        showNotification(
          "Aucun PAT correspondant aux filtres sélectionnés.",
          type = "warning", duration = 5, id = "warning_pat"
        )
      }
    },
    ignoreInit = TRUE
  )
  
  # Hachures au démarrage
  observeEvent(input$map_initialized, {
    pat_affiche <- pat_filtre()
    pat_niv1 <- pat_affiche[!is.na(pat_affiche$niveau) & pat_affiche$niveau == "1", ]
    if (nrow(pat_niv1) == 0) return()
    
    couleurs_map <- setNames(
      as.list(pal_pat(pat_niv1$echelle)),
      pat_niv1$nom_du_pat
    )
    session$sendCustomMessage("apply_hatch", couleurs_map)
  })
  
  # Clic sur PAT
  observeEvent(input$map_shape_click, {
    if (!isTRUE(input$pat_layer)) return()
    
    click <- input$map_shape_click
    req(click)
    
    point <- st_sfc(st_point(c(click$lng, click$lat)), crs = 4326)
    
    # IMPORTANT : si un PAT est déjà actif, on ne teste le clic
    # que sur ce PAT visible, pas sur tous les PAT filtrés
    pat_sf <- pat_filtre()
    if (!is.null(pat_actif())) {
      pat_sf <- pat_sf[pat_sf$nom_du_pat == pat_actif(), ]
    }
    
    intersect <- st_intersects(pat_sf, point, sparse = FALSE)
    pat_click <- pat_sf[unlist(intersect), ]
    
    # Si un PAT est actif et qu'on clique hors de lui => reset direct
    if (nrow(pat_click) == 0) {
      if (!is.null(pat_actif())) {
        deselect_pat()
      }
      return()
    }
    
    # Cas 1 : un seul PAT
    if (nrow(pat_click) == 1) {
      select_pat(pat_click[1, ], depuis_clic_carte = TRUE)
      return()
    }
    
    # Cas 2 : plusieurs PAT superposés (seulement quand aucun PAT n'est actif)
    clic_sur_pat(TRUE)
    
    liens <- paste0(
      vapply(
        seq_len(nrow(pat_click)),
        function(i) {
          code_json <- jsonlite::toJSON(as.character(pat_click$code_pat[i]), auto_unbox = TRUE)
          libelle   <- htmltools::htmlEscape(as.character(pat_click$nom_du_pat[i]))
          
          paste0(
            "<li><a href='#' onclick='Shiny.setInputValue(\"pat_selectionne_code\", ",
            code_json,
            ", {priority:\"event\"}); return false;'>",
            libelle,
            "</a></li>"
          )
        },
        character(1)
      ),
      collapse = ""
    )
    leafletProxy("map") %>%
      clearPopups() %>%
      addPopups(
        lng = click$lng,
        lat = click$lat,
        popup = paste0("<strong>Plusieurs PAT à cet endroit :</strong><br/><ul>", liens, "</ul>")
      )
  })
  
  # Clic sur PAT depuis liste / popup multi-PAT
  observeEvent(input$pat_selectionne, {
    req(input$pat_selectionne)
    
    pat <- pat_filtre()
    pat_row <- pat[pat$nom_du_pat == input$pat_selectionne, ]
    if (nrow(pat_row) == 0) return()
    
    select_pat(pat_row, depuis_clic_carte = FALSE)
  })
  
  # Clic sur Pop-up choix du pat pour le zoom
  
  observeEvent(input$pat_selectionne_code, {
    req(input$pat_selectionne_code)
    
    pat <- pat_filtre()
    pat_row <- pat[as.character(pat$code_pat) == as.character(input$pat_selectionne_code), ]
    if (nrow(pat_row) == 0) return()
    
    select_pat(pat_row, depuis_clic_carte = FALSE)
  })
  
  # Reset si clic ailleurs
  observeEvent(input$map_click, {
    if (clic_sur_pat()) {
      clic_sur_pat(FALSE)
      return()
    }
    
    if (!is.null(pat_actif())) {
      deselect_pat()
    }
  })
}

################################# LANCEMENT DE L'APPLICATION #########################################################
shinyApp(ui, server)