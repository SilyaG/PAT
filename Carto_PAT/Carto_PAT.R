########################################### PROJET CAROT ###########################################
######### Copyright (C) [2026] [Gréaume Paul, Guerboub Silya, Jouve Charlotte,Prima Oliver #########
####################################Université Lumière Lyon 2]######################################
#########Distribué sous licence CeCILL-2.1 — voir le fichier LICENSE pour les détails.##############


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

# Enlever la commentarisation pour installation de leaflet.extras
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

reg_aura_4326 <- st_read("./data/region_aura.gpkg", quiet = TRUE) %>%
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
# Définition de l'interface utilisateur (UI) de l'application Shiny
# Structure générale : fluidPage contenant tous les éléments visuels
ui <- fluidPage(
  # En-tête (head) : charger les styles CSS et scripts JavaScript externes
  tags$head(
    # Design System de la République Française (DSFR) - CSS et JS
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/@gouvfr/dsfr@1.12.1/dist/dsfr.min.css"
    ),
    tags$script(
      src = "https://cdn.jsdelivr.net/npm/@gouvfr/dsfr@1.12.1/dist/dsfr.module.min.js",
      type = "module"
    ),
    # Icônes Remix (boutons, logos, etc.)
    tags$link(
      href = "https://cdn.jsdelivr.net/npm/remixicon@3.5.0/fonts/remixicon.css",
      rel = "stylesheet"
    ),
    # Styles personnalisés spécifiques à la cartographie
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "style_carto_PAT.css"
    ),
    
    # Variables JavaScript globales : listes d'autocomplétion des communes et PAT
    tags$script(HTML(sprintf(
      "window.AUTOCOMPLETE_COMMUNES = %s;\nwindow.AUTOCOMPLETE_PATS = %s;",
      jsonlite::toJSON(autocomplete_communes, auto_unbox = TRUE),
      jsonlite::toJSON(autocomplete_pats, auto_unbox = TRUE)
    ))),
    
    # Scripts JavaScript personnalisés pour l'interactivité
    tags$script(src = "barre_recherche_autocomplete.js"),     # Autocomplétion barre de recherche
    tags$script(src = "legende_interactive.js"),              # Gestion interactive de la légende
    tags$script(src = "liste_pat_interactive.js"),            # Clic sur PAT dans le menu latéral
    tags$script(src = "symbologie_hachure_pat.js"),           # Hachures SVG pour PAT
    tags$script(src = "tutoriel_demarrage.js"),               # Modal tutoriel au démarrage
    tags$script(src = "popup_pat.js"),                        # Popup personnalisée PAT
    tags$script(src = "popup_communes.js"),                   # Popup communes (si besoin)
    tags$script(src = "bouton_reset.js"),                     # RéAction bouton reset
    tags$script(src = "warning_indicateurs_communaux.js")     # Avertissements indicateurs
  ),
  
  # Éléments HTML statiques inclus depuis des fichiers externes
  includeHTML("www/apparence_page_introductive.html"),    # Page d'introduction/splash screen
  includeHTML("www/apparence_header_aura.html"),          # En-tête avec logo AURA
  
  # Contenu principal de l'application
  tags$main(
    class = "fr-container-fluid",
    br(),
    
    # BARRE DE FILTRES : Section supérieure avec contrôles de filtrage
    tags$div(
      style = "display:flex; gap:20px; align-items:flex-end; margin:0px 0 20px 0; padding:0;",
      
      # Groupe des filtres + bouton reset
      tags$div(
        style = "display:flex; gap:20px; margin:0; padding:0;",
        
        # Filtre 1 : Niveau de labellisation (Niveau 1 ou 2)
        tags$div(
          style = "width:250px; margin:0; padding:0; position:relative; display:flex; align-items:center;",
          tags$select(
            id = "filtre_niveau",
            class = "fr-select",
            style = "color:black; margin:0; flex:1;",
            tags$option(
              "Sélectionner un niveau de labellisation",
              value = "",
              selected = TRUE,
              disabled = TRUE
            ),
            tags$option(value = "Tous", "Tous les niveaux"),
            tags$option(value = "1", "Niveau 1"),
            tags$option(value = "2", "Niveau 2")
          ),
          
          #Button info pour en savoir plus sur le niveau des PAT 
          tags$button(
            id = "btn_info_niveau",
            type = "button",
            `aria-controls` = "modal-info-niveau",
            `data-fr-opened` = "false",
            title = "En savoir plus sur les niveaux",
            style = paste(
              "background:none; border:none; padding:0 0 0 6px; cursor:pointer;",
              "color:#000091; display:flex; align-items:center; flex-shrink:0;"
            ),
            tags$i(class = "ri-information-line", style = "font-size:16px;")
          )
        ),
        
        # Filtre 2 : Échelle du territoire (PAiT, intercommunale, départementale)
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
        ),
        
        # Bouton réinitialiser : remet les filtres à zéro
        tags$button(
          id = "reset_button",
          class = "fr-btn fr-btn--secondary",
          type = "button",
          tags$i(class = "ri-restart-line", style = "margin-right:6px;"),
          "Réinitialiser"
        )
      ),
      # Zone pour afficher les avertissements (pas de résultats avec les filtres actuels)
      uiOutput("warning_pat_ui"),
      tags$div(
        style = "display:flex; align-items:flex-end; gap:8px; margin-left:auto;"),
      
      # BOUTONS ET BARRE DE RECHERCHE (alignés à droite)
      tags$div(
        style = "display:flex; align-items:flex-end; gap:8px; margin-left:auto;",
        
        # Bouton Tutoriel : affiche le guide d'utilisation
        tags$button(
          id = "info_tutorial",
          class = "fr-btn fr-btn--secondary",
          type = "button",
          tags$i(class = "ri-question-line", style = "margin-right:6px;"),
          "Tutoriel"
        ),
        
        # BARRE DE RECHERCHE : Permet de chercher un PAT ou une commune
        tags$div(
          class = "fr-search-bar",
          role = "search",
          style = "width:250px; margin:0;",
          
          tags$label(
            class = "fr-label",
            `for` = "nom_du_pat"
          ),
          
          # Panneau d'autocomplétion : affiche les suggestions (communes et PAT)
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
          
          # Champ de saisie pour la recherche
          tags$input(
            class = "fr-input",
            id = "nom_du_pat",
            type = "search",
            placeholder = "Rechercher une Commune ou un PAT",
            `aria-describedby` = "search_input_messages"
          ),
          
          # Zone pour les messages (erreurs, confirmations, etc.)
          tags$div(
            class = "fr-messages-group",
            id = "search_input_messages",
            `aria-live` = "polite"
          ),
          
          # Bouton rechercher : lance la recherche et zoom/sélection
          actionButton(
            inputId = "search_button",
            label = "Rechercher",
            class = "fr-btn"
          )
        )
      )
    ),
    
    
    # LAYOUT PRINCIPAL : 3 colonnes (gauche: menu | centre: carte | droite: sidebar)
    fluidRow(
      # COLONNE GAUCHE : Menu des fonds et couches (20% de la largeur)
      column(
        width = 2,
        div(
          class = "menu-couches",
          
          # Section 1 : Sélection du fond cartographique
          h4("Fond cartographique"),
          radioButtons(
            "fond",
            label = NULL,
            choiceNames = list(
              "Plan IGN",
              tags$span(
                "Registre Parcellaire Graphique",
                tags$br(),
                tags$a(
                  href = "https://www.cartoexplorer.com/imgs/legendes/ign/legende_registre_parcellaire_graphique.pdf",
                  target = "_blank",
                  style = "font-size: 8px; color: #888; text-decoration: none; margin-left: 2px;",
                  "Légende RPG "                )
              ),
              "OpenStreetMap",
              "Limites administratives"
            ),
            choiceValues = list("ign", "rpg", "osm", "fond_admin"),
            selected = "ign"
          ),
          hr(), # Séparateur
          
          # Section 2 : Affichage/masquage des couches de données
          h4("Couches"),
          checkboxInput("pat_layer", "Projet Alimentaire Territoriaux", TRUE), # PAT visibles par défaut
          checkboxInput("cls_layer", "Contrat Locaux de Santé", FALSE),        # CLS masqués par défaut
          checkboxInput("dep_layer", "Départements", FALSE),                   # Départements masqués par défaut
          uiOutput("com_layer_ui"),     # Communes et indicateurs (affichés si PAT sélectionné)
          
        )
      ),
      
      # COLONNE CENTRALE : Carte Leaflet interactive (65% de la largeur)
      column(
        width = 8,
        div(
          style = "position: relative;",
          
          # Carte Leaflet occupant 80% de la hauteur de la fenêtre
          leafletOutput("map", height = "80vh"),
          
          # Bouton pour afficher/masquer la légende (superposé sur la carte)
          tags$button(
            id = "legend_toggle",
            title = "Afficher / masquer la légende",
            style = paste(
              "position:absolute; top:10px; left:10px; z-index:1001;",
              "width:36px; height:36px;"
            ),
            tags$i(class = "ri-list-check-2")
          ),
          # Popup personnalisée des PAT (HTML/CSS externos)
          includeHTML("www/apparence_popup_pat.html")
        )
      ),
      
      # COLONNE DROITE : Panneau latéral avec liste des PAT visibles (15% de la largeur)
      column(
        width = 2,
        div(
          id = "right_sidebar",
          class = "right-panel visible-sidebar",
          uiOutput("pat_sidemenu") # Menu dépliable des PAT visibles dans la vue
        )
      )
    ),
    
    # Éléments HTML statiques en bas de page
    includeHTML("www/apparence_footer_aura.html"),  # Pied de page avec crédits/infos AURA
    includeHTML("www/apparence_tutoriel.html"),     # Modal tutoriel (affichage au démarrage)
    
    # Modale d'information sur les niveaux
    tags$dialog(
      id = "modal-info-niveau",
      class = "fr-modal",
      role = "dialog",
      `aria-labelledby` = "modal-info-niveau-title",
      tags$div(
        class = "fr-container fr-container--fluid fr-container-md",
        tags$div(
          class = "fr-grid-row fr-grid-row--center",
          tags$div(
            class = "fr-col-12 fr-col-md-8 fr-col-lg-6",
            tags$div(
              class = "fr-modal__body",
              tags$div(
                class = "fr-modal__header",
                tags$button(
                  class = "fr-btn--close fr-btn",
                  type = "button",
                  `aria-controls` = "modal-info-niveau",
                  "Fermer"
                )
              ),
              tags$div(
                class = "fr-modal__content",
                tags$h1(
                  id = "modal-info-niveau-title",
                  class = "fr-modal__title",
                  "Niveaux de labellisation PAT"
                ),
                tags$p(
                  style = "color:#555; font-size:13px; margin-bottom:20px;",
                  "Les PAT sont reconnus par le Ministère de l'Agriculture selon deux niveaux :"
                ),
                tags$div(
                  style = "border-left:3px solid #000091; padding:10px 14px; margin-bottom:12px; background:#f8f8f8;",
                  tags$p(
                    style = "font-weight:700; color:#000091; margin:0 0 4px 0; font-size:13px;",
                    "Niveau 1 — Projet en émergence"
                  ),
                  tags$p(
                    style = "margin:0; font-size:12px; color:#444; line-height:1.5;",
                    "Démarche initiée : diagnostic, gouvernance et plan d'action en cours de structuration."
                  )
                ),
                tags$div(
                  style = "border-left:3px solid #000091; padding:10px 14px; background:#f8f8f8;",
                  tags$p(
                    style = "font-weight:700; color:#000091; margin:0 0 4px 0; font-size:13px;",
                    "Niveau 2 — Projet opérationnel"
                  ),
                  tags$p(
                    style = "margin:0; font-size:12px; color:#444; line-height:1.5;",
                    "Gouvernance consolidée, diagnostic validé et plan d'actions pleinement mis en œuvre."
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

# Fin de fluidPage(ui)

########################################### Partie SERVER ###########################################################
server <- function(input, output, session) {
  
  # PAT actif = nom du PAT sélectionné
  pat_actif <- reactiveVal(NULL)
  
  # Pour éviter le reset juste après un clic PAT
  clic_sur_pat <- reactiveVal(FALSE)
  
  # Pour delai message d'erreur pas de Pat visible suivant les filtres
  warning_visible <- reactiveVal(FALSE)
  
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
  communes_centroid$rayon_sau <- scales::rescale(sqrt(communes_centroid$rpg_ha), to = c(1, 30))
  communes_centroid$rayon_bio <- scales::rescale(sqrt(communes_centroid$bio_ha), to = c(1, 30))
  
  
  # Reset carte si changement de filtres
  observeEvent(
    list(input$filtre_niveau, input$filtre_niveau_terri),
    {
      pat_actif(NULL)
      clic_sur_pat(FALSE)
      session$sendCustomMessage("hide_pat_popup", list())
      
      leafletProxy("map") %>%
        clearPopups() %>%
        fitBounds(
          lng1 = unname(bbox_init["xmin"]),
          lat1 = unname(bbox_init["ymin"]),
          lng2 = unname(bbox_init["xmax"]),
          lat2 = unname(bbox_init["ymax"]),
          options = list(animate = TRUE, duration = 0.8, easeLinearity = 0.1)
        )
    },
    ignoreInit = TRUE
  )
  
  # Filtre réactif des PAT basé sur les sélections utilisateur
  # Applique les filtres de niveau (1/2) et d'échelle (interterritorial/intercommunal/départemental)
  pat_filtre <- reactive({
    pat <- couche_pat_4326
    
    # Filtre par niveau de labellisation (1 ou 2)
    if (!is.null(input$filtre_niveau) &&
        input$filtre_niveau != "Tous" &&
        input$filtre_niveau != "") {
      pat <- pat[pat$niveau == input$filtre_niveau, ]
    }
    
    # Filtre par échelle du territoire (PAiT, intercommunale, départementale)
    if (!is.null(input$filtre_niveau_terri) &&
        input$filtre_niveau_terri != "Tous" &&
        input$filtre_niveau_terri != "") {
      pat <- pat[pat$echelle == input$filtre_niveau_terri, ]
    }
    
    pat
  })
  
  # Gestion du clic sur le bouton "Réinitialiser" - remet les filtres à zéro et recentre la carte 
  observeEvent(input$reset_button, {
    session$sendCustomMessage("reset_filtres", list())
    
    pat_actif(NULL)
    clic_sur_pat(FALSE)
    session$sendCustomMessage("hide_pat_popup", list())
    
    leafletProxy("map") %>%
      clearPopups() %>%
      fitBounds(
        lng1 = unname(bbox_init["xmin"]),
        lat1 = unname(bbox_init["ymin"]),
        lng2 = unname(bbox_init["xmax"]),
        lat2 = unname(bbox_init["ymax"]),
        options = list(animate = TRUE, duration = 0.8, easeLinearity = 0.1)
      )
  })
  
  # Affiche une popup personnalisée contenant les informations détaillées du PAT sélectionné
  # Envoie les données au client via sendCustomMessage pour le rendu HTML personnalisé
  show_popup_pat <- function(pat_row) {
    
    # Récupère et nettoie la liste des contacts (séparés par des points-virgules)
    contacts <- unlist(strsplit(as.character(pat_row$mail_coord[1] %||% ""), ";"))
    contacts <- trimws(contacts[contacts != ""])
    
    # Envoie les données du PAT au JavaScript pour affichage dans la popup personnalisée
    session$sendCustomMessage("show_pat_popup", list(
      nom            = as.character(pat_row$nom_du_pat[1]),
      niveau         = as.character(pat_row$niveau[1]),
      annee          = as.character(pat_row$annee[1]     %||% ""),
      population     = as.character(pat_row$pop_hab[1] %||% ""),
      pct_population = as.character(pat_row$part_pop[1]   %||% ""),
      sau            = as.character(pat_row$rpg_ha[1]        %||% ""),
      pct_sau        = as.character(pat_row$part_sau_pat[1]   %||% ""),
      bio            = as.character(pat_row$bio_ha[1]        %||% ""),
      pct_sau_bio    = as.character(pat_row$part_bio_pat[1]   %||% ""),
      partbio        = as.character(pat_row$part_bio[1]        %||% ""),
      bio_aura       = as.character(pat_row$bio_aura[1]   %||% ""),
      restau         = as.character(pat_row$nb_cantines[1]   %||% ""),
      contacts       = as.list(contacts)
    ))
  }
  
  # Récupère la liste des communes appartenant au PAT actuellement sélectionné
  # Utilise le lien code_pat -> code_insee pour retrouver les communes
  communes_dans_pat_actif <- reactive({
    req(pat_actif())
    
    # Récupère les codes PAT associés au PAT actif
    codes_pat <- unique(na.omit(couche_pat_4326$code_pat[couche_pat_4326$nom_du_pat == pat_actif()]))
    if (length(codes_pat) == 0) return(commune_aura[0, ])
    
    # Retrouve les codes INSEE associés à ces codes PAT
    insee <- unique(na.omit(pat_com$code_insee_chr[pat_com$code_pat %in% codes_pat]))
    if (length(insee) == 0) return(commune_aura[0, ])
    
    # Retourne les géométries des communes correspondantes
    commune_aura[commune_aura$code_insee_chr %in% insee, ]
  })
  
  # Fonction pour sélectionner un PAT : met à jour l'état, zoome et affiche la popup
  select_pat <- function(pat_row, depuis_clic_carte = FALSE) {
    req(nrow(pat_row) > 0)
    
    # Définit le PAT actif et marque si la sélection vient d'un clic sur la carte
    pat_actif(pat_row$nom_du_pat[1])
    clic_sur_pat(depuis_clic_carte)
    
    # Affiche un toast pour indiquer le chargement des indicateurs
    session$sendCustomMessage("show_toast_indicateurs", list())
    
    # Récupère la bounding box du PAT et zoome dessus
    bb <- st_bbox(pat_row)
    
    leafletProxy("map") %>%
      clearPopups() %>%
      fitBounds(
        lng1 = unname(bb["xmin"]),
        lat1 = unname(bb["ymin"]),
        lng2 = unname(bb["xmax"]),
        lat2 = unname(bb["ymax"]),
        options = list(animate = TRUE, duration = 0.8, easeLinearity = 0.1)
      )
    
    # Affiche la popup personnalisée avec les informations du PAT
    show_popup_pat(pat_row)
  }
  
  # Fonction pour désélectionner le PAT actif et revenir à la vue initiale
  deselect_pat <- function() {
    pat_actif(NULL)
    clic_sur_pat(FALSE)
    session$sendCustomMessage("hide_pat_popup", list())
    
    # Recentre la carte sur la bounding box initiale
    leafletProxy("map") %>%
      clearPopups() %>%
      fitBounds(
        lng1 = unname(bbox_init["xmin"]),
        lat1 = unname(bbox_init["ymin"]),
        lng2 = unname(bbox_init["xmax"]),
        lat2 = unname(bbox_init["ymax"]),
        options = list(animate = TRUE, duration = 0.8, easeLinearity = 0.1)
      )
  }
  
  # Détermine la liste des PAT visibles dans la vue actuelle de la carte
  # Filtre par couche active, filtres appliqués, et zone visible (bounding box)
  pat_visibles_dans_vue <- reactive({
    
    # Si la couche PAT est masquée, retourne un objet vide
    if (isFALSE(input$pat_layer)) return(couche_pat_4326[0, ])
    
    # Applique les filtres sélectionnés par l'utilisateu
    pat <- pat_filtre()
    
    # Si un PAT est actif, affiche uniquement ce PAT
    if (!is.null(pat_actif())) {
      pat <- pat[pat$nom_du_pat == pat_actif(), ]
    }
    
    # Récupère la bounding box de la vue actuelle
    b <- input$map_bounds
    if (is.null(b)) return(pat)
    
    # Crée un polygone de la zone visible et filtre les PAT qui l'intersectent
    bbox_view <- sf::st_bbox(
      c(xmin = b$west, ymin = b$south, xmax = b$east, ymax = b$north),
      crs = 4326
    )
    poly_view <- sf::st_as_sfc(bbox_view)
    
    idx <- sf::st_intersects(pat, poly_view, sparse = FALSE)
    pat[idx[, 1], ]
  })
  
  # Rendu du menu latéral droit : affiche la liste des PAT visibles dans la vue courante
  output$pat_sidemenu <- renderUI({
    
    # Récupère les PAT visibles et extrait les noms uniques
    pat <- pat_visibles_dans_vue()
    pats <- sort(unique(na.omit(pat$nom_du_pat)))
    
    # Affiche un message si aucun PAT n'est visible
    if (length(pats) == 0) {
      return(tags$p("Aucun PAT visible dans la vue (ou couche PAT masquée)."))
    }
    
    # Construit le menu latéral collapsible avec la liste des PAT
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
            paste0("PAT visible(s) : ", length(pats))
          ),
          
          # Liste cliquable des PAT avec attribut data-pat pour le JavaScript
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
  
  # Rendu de la carte Leaflet initiale avec tous les fonds et couches de base
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
      # Fond OpenStreetMap standard
      addProviderTiles("OpenStreetMap", group = "OSM") %>%
      
      # Fond neutre toujours visible (base pour les limites admin)
      addProviderTiles(
        "CartoDB.PositronNoLabels",
        group = "fond_neutre_fixe",
        options = providerTileOptions(opacity = 1)
      ) %>%
      
      # Fit aux limites et restriction du zoom
      fitBounds(xmin, ymin, xmax, ymax) %>%
      
      # Barre d'échelle en bas à gauche
      addScaleBar(
        position = "bottomleft",
        options = scaleBarOptions(
          metric = TRUE,
          imperial = FALSE,
          updateWhenIdle = TRUE,
          maxWidth = 150
        )
      ) %>%
      
      # Bouton plein écran
      addFullscreenControl(position = "topright") %>%
      
      # Repositionne les contrôles de zoom en haut à droite
      htmlwidgets::onRender("
        function(el, x) {
          this.zoomControl.setPosition('topright');
        }
      ") %>%
      
      # Plan IGN (WMS)
      addWMSTiles(
        baseUrl = "https://data.geopf.fr/wms-r/wms",
        layers  = "GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2",
        options = WMSTileOptions(format = 'image/png', transparent = FALSE, version = '1.3.0'),
        group = "Plan IGN"
      ) %>%
      
      # Registre Parcellaire Graphique (WMS)
      addWMSTiles(
        baseUrl = "https://data.geopf.fr/wms-r/wms",
        layers  = "LANDUSE.AGRICULTURE2024",
        options = WMSTileOptions(format = 'image/png', transparent = FALSE, version = '1.3.0'),
        group = "Registre Parcellaire Graphique"
      ) %>%
      
      # Département (limites grises fines)
      addPolygons(
        data = dep_aura_4326,
        color = "#7b7b7b",
        weight = 2,
        fillColor = NA,
        fillOpacity = 0,
        group = "Departement"
      ) %>%
      
      # Région (limites épaisses, non interactive)
      addPolygons(
        data        = reg_aura_4326,
        color       = "#161616",
        weight      = 3,
        fillColor   = NA,
        fillOpacity = 0,
        options     = pathOptions(interactive = FALSE),  # ← clics désactivés
        group       = "Limites administratives"
      ) %>%
      
      # Département (limites contours, non interactive)
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
      
      # Contrats Locaux de Santé (CLS)
      addPolygons(
        data = couche_cls_4326,
        color = "#6a6af4",
        weight = 2,
        fillOpacity = 0.5,
        popup = ~paste0(
          "<div style='font-family:Marianne,Arial,sans-serif; font-size:11px; padding:4px;'>",
          "<div style='font-weight:bold; color:#000091; font-size:12px; margin-bottom:4px;'>",
          Nom_CLS
        ),
        group = "Contrats locaux de santé"
      )
  })
  
  
  # Gestion dynamique de la sélection des fonds cartographiques
  # Cache tous les fonds et affiche uniquement celui sélectionné par l'utilisateur
  observe({
    proxy <- leafletProxy("map")
    
    # Cache tous les fonds
    proxy %>% hideGroup("Plan IGN")
    proxy %>% hideGroup("Registre Parcellaire Graphique")
    proxy %>% hideGroup("OSM")
    proxy %>% hideGroup("Limites administratives")
    proxy %>% hideGroup("fond_neutre_fixe")  # ← ajout
    
    # Affiche le fond sélectionné
    if (input$fond == "ign"){
      proxy %>% showGroup("Plan IGN")
    }
    if (input$fond == "rpg") {
      proxy %>% showGroup("Registre Parcellaire Graphique")
    }
    if (input$fond == "osm") {
      proxy %>% showGroup("OSM")
    }
    
    # Pour le fond administratif, affiche le fond neutre + les limites administratives superposées
    if (input$fond == "fond_admin"){
      proxy %>% showGroup("fond_neutre_fixe")   # ← fond de base
      proxy %>% showGroup("Limites administratives")  # ← WMS par-dessus
    }
  })
  
  # Gestion de l'affichage des couches simples (PAT, CLS, Communes, Départements)
  # Affiche ou masque ces couches selon les checkboxes de l'utilisateur
  observe({
    proxy <- leafletProxy("map")
    
    # Couche PAT
    if (isTRUE(input$pat_layer)) {
      proxy %>% showGroup("Projet Alimentaire Territoriaux")
    } else {
      proxy %>% hideGroup("Projet Alimentaire Territoriaux")
      session$sendCustomMessage("hide_pat_popup", list())
      pat_actif(NULL)
      clic_sur_pat(FALSE)
    }
    
    # Couche Contrats Locaux de Santé
    if (isTRUE(input$cls_layer)) {
      proxy %>% showGroup("Contrats locaux de santé")
    } else {
      proxy %>% hideGroup("Contrats locaux de santé")
    }
    
    # Couche Communes
    if (isTRUE(input$com_layer)) {
      proxy %>% showGroup("Communes")
    } else {
      proxy %>% hideGroup("Communes")
    }
    
    # Couche Départements
    if (isTRUE(input$dep_layer)) {
      proxy %>% showGroup("Departement")
    } else {
      proxy %>% hideGroup("Departement")
    }
  })
  
  
  # Affichage dynamique des limites communales : uniquement celles du PAT actif
  # Les communes ne s'affichent que si un PAT est sélectionné et la couche est activée
  observe({
    proxy <- leafletProxy("map")
    proxy %>% clearGroup("Communes")
    
    # Retour si la couche est masquée ou aucun PAT actif
    if (isFALSE(input$com_layer)) return()
    if (is.null(pat_actif())) return()
    
    # Récupère les communes du PAT actif
    communes_pat <- communes_dans_pat_actif()
    if (is.null(communes_pat) || nrow(communes_pat) == 0) return()
    
    # Affiche les géométries des communes avec des limites grises
    proxy %>% addPolygons(
      data = communes_pat,
      color = "#929292",
      weight = 1,
      fillColor = NA,
      fillOpacity = 0,
      group = "Communes",
    )
  })
  
  
  # Affichage dynamique des indicateurs : population, SAU, SAU bio
  # Les indicateurs s'affichent uniquement sur les communes du PAT actif
  observe({
    proxy <- leafletProxy("map")
    
    # Nettoie les groupes d'indicateurs précédents
    proxy %>%
      clearGroup("Population communale") %>%
      clearGroup("SAU") %>%
      clearGroup("SAU bio")
    
    # Retour si pas de couche commune visible, pas d'indicateur sélectionné, ou pas de PAT actif
    if (!isTRUE(input$com_layer)) return()
    if (is.null(input$indicateur) || input$indicateur == "none") return()
    if (is.null(pat_actif())) return()
    
    # Récupère les communes du PAT actif
    communes_pat <- communes_dans_pat_actif()
    if (is.null(communes_pat) || nrow(communes_pat) == 0) return()
    
    # Filtre les centroïdes des communes du PAT actif pour les indicateurs
    centroid_pat <- communes_centroid[
      communes_centroid$code_insee_chr %in% communes_pat$code_insee_chr,
    ]
    
    if (nrow(centroid_pat) == 0) return()
    
    # Recalcule les rayons des cercles localement (proportionnels aux valeurs du PAT)
    centroid_pat$rayon_pop_local <- scales::rescale(sqrt(centroid_pat$population), to = c(3, 30))
    centroid_pat$rayon_sau_local <- scales::rescale(sqrt(centroid_pat$rpg), to = c(3, 30))
    
    #les valeurs des centroids s'appliquent uniquement si la valeur est au dessus de 0 
    bio_vals_nonzero <- centroid_pat$bio[centroid_pat$bio > 0]
    bio_range <- if (length(bio_vals_nonzero) > 0) range(sqrt(bio_vals_nonzero)) else c(0, 1)
    
    centroid_pat$rayon_bio_local <- ifelse(
      centroid_pat$bio > 0,
      scales::rescale(sqrt(centroid_pat$bio), from = bio_range, to = c(3, 30)),
      0
    )
    
    # Palette BIO — définie ici sans dépendre de communes_sau
    pal_bio_local <- colorQuantile(
      palette  = c("#e6feda", "#99c221", "#68a532", "#447049"),
      domain   = centroid_pat$part_bio[centroid_pat$part_bio > 0],
      n        = min(4, length(unique(centroid_pat$part_bio[centroid_pat$part_bio > 0]))),
      na.color = "transparent"
    )
    
    # Indicateur Population : cercles rouges proportionnels au nombre d'habitants
    if (input$indicateur == "pop") {
      proxy %>% addCircleMarkers(
        data = centroid_pat,
        radius = ~rayon_pop_local,
        fillColor = "#CE614A", color = "#ffffff", weight = 1, fillOpacity = 1,
        popup = ~paste0(
          "<div style='font-family:Marianne,Arial,sans-serif; min-width:180px;'>",
          "<div style='font-weight:700; color:#000091; font-size:13px; padding-bottom:6px;",
          " margin-bottom:8px; border-bottom:1px solid #000091;'>", nom_officiel, "</div>",
          "<div style='display:flex; align-items:center; gap:8px; font-size:13px; color:#333;'>",
          "<span style='font-size:18px;'>&#128101;</span>",
          "<div><div style='font-weight:600;'>", formatC(population, format="d", big.mark=" "), " habitants</div>",
          "<div style='font-size:11px; color:#666;'>Population communale</div></div>",
          "</div></div>"
        ),
        popupOptions = popupOptions(autoPan = TRUE,   autoPanPadding = c(20, 20)),
        group = "Population communale"
      )
    }
    
    # Indicateur SAU : polygones colorés selon le % de SAU communale
    if (input$indicateur == "sau") {
      communes_sau <- commune_aura[
        commune_aura$code_insee_chr %in% communes_pat$code_insee_chr, ]
      
      # Palette SAU définie ici, après communes_sau
      pal_sau_local <- colorQuantile(
        palette  = c("#fef6e3", "#fde39c", "#efcb3a", "#c8aa39"),
        domain   = communes_sau$part_sau,
        n        = min(4, length(unique(na.omit(communes_sau$part_sau)))),
        na.color = "transparent"
      )
      
      # APRÈS — couleur dynamique
      communes_sau$couleur_barre <- pal_sau_local(communes_sau$part_sau)
      
      proxy %>% addPolygons(
        data        = communes_sau,
        fillColor   = ~pal_sau_local(part_sau),
        fillOpacity = 1,
        color       = "#929292",
        weight      = 1,
        popup = ~paste0(
          "<div style='font-family:Marianne,Arial,sans-serif; min-width:200px;'>",
          "<div style='font-weight:700; color:#000091; font-size:13px; padding-bottom:6px;",
          " margin-bottom:8px; border-bottom:1px solid #000091;'>", nom_officiel, "</div>",
          "<div style='display:flex; align-items:center; gap:8px; font-size:13px;",
          " color:#333; margin-bottom:8px;'>",
          "<span style='font-size:18px;'>&#127807;</span>",
          "<div>",
          "<div style='font-size:11px; color:#666;'>Part de la Surface Agricole Utile dans la commune</div>",
          "</div></div>",
          "<div style='background:#e5e5e5; border-radius:3px; height:10px; width:100%;",
          " border:1px solid #ccc;'>",                                    # ← bordure sur le fond gris
          "<div style='width:", pmin(round(part_sau), 100), "%; background:", couleur_barre, ";",
          " height:100%; border-radius:3px; min-width:2px;",
          " outline:1px solid rgba(0,0,0,0.15);'>",                       # ← contour subtil sur la barre
          "</div>",
          "</div>",
          "<div style='font-size:12px; font-weight:600; color:#666666; margin-top:4px;'>",
          round(part_sau, 1), " %</div>",
          "</div></div>"
        ),
        popupOptions = popupOptions(autoPan = TRUE, autoPanPadding = c(20, 20)),
        group = "SAU"
      )
    }
    
    if (input$indicateur == "bio") {
      centroid_pat$couleur_barre <- pal_bio_local(centroid_pat$part_bio)
      
      # Indicateur SAU Bio : cercles colorés selon le % de SAU en bio
      proxy %>% addCircleMarkers(
        data = centroid_pat,
        radius = ~rayon_bio_local,
        fillColor = ~pal_bio_local(part_bio),
        color = "#ffffff", weight = 1, fillOpacity = 1,
        popup = ~paste0(
          "<div style='font-family:Marianne,Arial,sans-serif; min-width:200px;'>",
          "<div style='font-weight:700; color:#000091; font-size:13px; padding-bottom:6px;",
          " margin-bottom:8px; border-bottom:1px solid #000091;'>", nom_officiel, "</div>",
          "<div style='display:flex; align-items:center; gap:8px; font-size:13px;",
          " color:#333; margin-bottom:8px;'>",
          "<span style='font-size:18px;'>&#127807;</span>",
          "<div><div style='font-weight:600;'>",
          formatC(round(bio_ha), format="d", big.mark=" "),
          ifelse(round(bio_ha) == 1, " hectare", " hectares"), "</div>",
          "<div style='font-size:11px; color:#666;'>Part de la Surface Agricole Utile Bio dans la commune</div></div>",
          "</div>",
          "<div style='background:#e5e5e5; border-radius:3px; height:10px; width:100%;",
          " border:1px solid #ccc;'>",                               # ← bordure sur le fond gris
          "<div style='width:", pmin(round(part_bio), 100), "%; background:", couleur_barre, ";",
          " height:100%; border-radius:3px; min-width:2px;",
          " outline:1px solid rgba(0,0,0,0.15);'>",                  # ← contour subtil sur la barre
          "</div>",
          "</div>",
          "<div style='font-size:12px; font-weight:600; color:#666666; margin-top:4px;'>",
          round(part_bio, 1), " %</div>",
          "</div></div>"
        ),
        popupOptions = popupOptions(autoPan = TRUE, autoPanPadding = c(20, 20)),
        group = "SAU bio"
      )
    }
  })
  
# Gestion dynamique de la légende : met à jour le contenu selon les couches actives et les filtres
# La légende affiche les symboles correspondant uniquement aux couches visibles et aux PAT filtrés
  observe({
    sections <- list()
    
    # Section : Projets Alimentaires Territoriaux
    if (isTRUE(input$pat_layer)) {
      
      # Récupère les PAT affichés (filtrés et visibles)
      pat_legende <- pat_filtre()
      if (!is.null(pat_actif())) {
        pat_legende <- pat_legende[pat_legende$nom_du_pat == pat_actif(), ]
      }
      
      # Détermine quels styles sont présents dans les PAT affichés
      niveaux_affiches  <- unique(pat_legende$niveau)
      echelles_affichees <- unique(pat_legende$echelle)
      
      show_niv1 <- "1" %in% niveaux_affiches
      show_niv2 <- "2" %in% niveaux_affiches
      
      show_pait  <- "PAT interterritorial (PAiT)"    %in% echelles_affichees
      show_inter <- "PAT d'échelle intercommunale"   %in% echelles_affichees
      show_dep   <- "PAT d'échelle départementale"   %in% echelles_affichees
      
      # Sous-titre hachuré/aplat uniquement si les deux niveaux sont présents
      sous_titre <- if (show_niv1 && show_niv2) "
    <div class='leg-item' style='font-size:11px;color:#555;font-style:italic;margin-bottom:6px;'>
      Hachuré = Niveau 1 &nbsp;|&nbsp; Aplat = Niveau 2
    </div>" else ""
      
      # Entrées niveau 1 (hachurées)
      niv1_html <- ""
      if (show_niv1) {
        if (show_inter) niv1_html <- paste0(niv1_html, "
  <div class='leg-item' style='margin-top:4px;'>
    <svg width='14' height='14' style='flex-shrink:0;'>
      <defs>
        <pattern id='lh1' patternUnits='userSpaceOnUse' width='4' height='4' patternTransform='rotate(45)'>
          <line x1='0' y1='0' x2='0' y2='4' style='stroke:#ff732c; stroke-width:1.5;'/>
        </pattern>
      </defs>
      <rect width='14' height='14' fill='url(#lh1)' stroke='#ff732c' stroke-width='1.5' rx='2'/>
    </svg>&nbsp;Intercommunal — Niveau 1
  </div>")
        
        if (show_pait) niv1_html <- paste0(niv1_html, "
  <div class='leg-item'>
    <svg width='14' height='14' style='flex-shrink:0;'>
      <defs>
        <pattern id='lh2' patternUnits='userSpaceOnUse' width='4' height='4' patternTransform='rotate(45)'>
          <line x1='0' y1='0' x2='0' y2='4' style='stroke:#1f8d49; stroke-width:1.5;'/>
        </pattern>
      </defs>
      <rect width='14' height='14' fill='url(#lh2)' stroke='#1f8d49' stroke-width='1.5' rx='2'/>
    </svg>&nbsp;PAiT — Niveau 1
  </div>")
        
        if (show_dep) niv1_html <- paste0(niv1_html, "
  <div class='leg-item' style='margin-bottom:6px;'>
    <svg width='14' height='14' style='flex-shrink:0;'>
      <defs>
        <pattern id='lh3' patternUnits='userSpaceOnUse' width='4' height='4' patternTransform='rotate(45)'>
          <line x1='0' y1='0' x2='0' y2='4' style='stroke:#5576c0; stroke-width:1.5;'/>
        </pattern>
      </defs>
      <rect width='14' height='14' fill='url(#lh3)' stroke='#5576c0' stroke-width='1.5' rx='2'/>
    </svg>&nbsp;Départemental — Niveau 1
  </div>")
      }
      
      # Entrées niveau 2 (aplat)
      niv2_html <- ""
      if (show_niv2) {
        if (show_inter) niv2_html <- paste0(niv2_html, "
    <div class='leg-item' style='margin-top:4px;'>
      <span class='swatch' style='background:#ff732c;opacity:0.35;border:2px solid #ff732c;'></span>&nbsp;Intercommunal — Niveau 2
    </div>")
        if (show_pait) niv2_html <- paste0(niv2_html, "
    <div class='leg-item'>
      <span class='swatch' style='background:#1f8d49;opacity:0.35;border:2px solid #1f8d49;'></span>&nbsp;PAiT — Niveau 2
    </div>")
        if (show_dep) niv2_html <- paste0(niv2_html, "
    <div class='leg-item'>
      <span class='swatch' style='background:#5576c0;opacity:0.35;border:2px solid #5576c0;'></span>&nbsp;Départemental — Niveau 2
    </div>")
      }
      
      # Ajoute la section PAT à la légende
      sections <- append(sections, list(paste0(
        "<div class='leg-cat'>Projets Alimentaires Territoriaux</div>",
        sous_titre,
        niv1_html,
        niv2_html
      )))
    }
    
    # Section : Contrats Locaux de Santé
    if (isTRUE(input$cls_layer)) {
      sections <- append(sections, list("
        <div class='leg-cat'>Contrats Locaux de Santé</div>
        <div class='leg-item'><span class='swatch' style='background:#869ECE;'></span>CLS</div>
      "))
    }
    
    # Section : Départements (affichée seulement si le fond administratif n'est pas sélectionné)
    if (isTRUE(input$dep_layer) && input$fond != "fond_admin") {
      sections <- append(sections, list("
        <div class='leg-cat'>Départements</div>
        <div class='leg-item'><span class='swatch-line' style='background:#7b7b7b;'></span>Limites départementales</div>
      "))
    }
    
    # Section : Communes et indicateurs (affichée quand communes sont visibles et un PAT est actif)
    if (isTRUE(input$com_layer) && !is.null(pat_actif())) {
      
      indicateur_html <- ""
      
      # Construit le HTML des indicateurs selon celui sélectionné
      if (!is.null(input$indicateur) && input$indicateur != "none") {
        
        # Calcul des valeurs réelles du PAT actif pour la légende
        communes_pat <- communes_dans_pat_actif()
        centroid_pat_leg <- communes_centroid[
          communes_centroid$code_insee_chr %in% communes_pat$code_insee_chr, ]
        
        # Indicateur Population : légende avec cercles de différentes tailles
        if (input$indicateur == "pop" && nrow(centroid_pat_leg) > 0) {
          vals <- sort(unique(na.omit(centroid_pat_leg$population)))
          v_min <- format(round(min(vals)), big.mark = " ")
          v_med <- format(round(sqrt(min(vals) * max(vals))), big.mark = " ")  # ← moyenne géométrique
          v_max <- format(round(max(vals)), big.mark = " ")
          
          # Valeur intermédiaire = moyenne géométrique pour rescale cohérent
          rayons <- scales::rescale(sqrt(c(min(vals), sqrt(min(vals) * max(vals)), max(vals))), to = c(3, 30))
          r_min <- round(rayons[1])
          r_med <- round(rayons[2])
          r_max <- round(rayons[3])
          
          indicateur_html <- paste0("
<div class='leg-sep-inner'></div>
<div class='leg-cat'>Indicateurs</div>
<div class='leg-item'>Population communale</div>
<div style='display:flex; flex-direction:column; align-items:flex-start; gap:6px; padding-left:2px; margin-top:4px;'>
  <div style='display:flex; align-items:center; gap:8px;'>
    <div style='width:60px; display:flex; justify-content:center; align-items:center; flex-shrink:0;'>
      <span style='background:#CE614A; border-radius:50%; width:", r_min*2, "px; height:", r_min*2, "px; display:block;'></span>
    </div>
    <span style='font-size:12px;'>", v_min, " hab.</span>
  </div>
  <div style='display:flex; align-items:center; gap:8px;'>
    <div style='width:60px; display:flex; justify-content:center; align-items:center; flex-shrink:0;'>
      <span style='background:#CE614A; border-radius:50%; width:", r_med*2, "px; height:", r_med*2, "px; display:block;'></span>
    </div>
    <span style='font-size:12px;'>", v_med, " hab.</span>
  </div>
  <div style='display:flex; align-items:center; gap:8px;'>
    <div style='width:60px; display:flex; justify-content:center; align-items:center; flex-shrink:0;'>
      <span style='background:#CE614A; border-radius:50%; width:", r_max*2, "px; height:", r_max*2, "px; display:block;'></span>
    </div>
    <span style='font-size:12px;'>", v_max, " hab.</span>
  </div>
</div>
")
        }
        
        # Indicateur SAU : légende avec gradient de couleurs
        if (input$indicateur == "sau" && nrow(centroid_pat_leg) > 0) {
          communes_pat_leg <- commune_aura[
            commune_aura$code_insee_chr %in% communes_pat$code_insee_chr, ]
          vals <- sort(na.omit(communes_pat_leg$part_sau))
          
          # Seuils quantiles adaptés au PAT
          breaks_sau <- unique(round(quantile(vals, probs = c(0, 0.25, 0.5, 0.75, 1)), 1))
          couleurs_sau <- c("#fef6e3", "#fde39c", "#efcb3a", "#c8aa39")
          
          classes_html <- paste0(vapply(seq_len(length(breaks_sau) - 1), function(i) {
            paste0(
              "<div style='display:flex; align-items:center; gap:6px; margin-bottom:4px;'>",
              "<span style='width:14px; height:14px; background:", couleurs_sau[i],
              "; border:1px solid #ccc; flex-shrink:0; display:inline-block;'></span>",
              "<span style='font-size:12px;'>", breaks_sau[i], " - ", breaks_sau[i+1], " %</span>",
              "</div>"
            )
          }, character(1)), collapse = "")
          
          indicateur_html <- paste0("
      <div class='leg-sep-inner'></div>
      <div class='leg-cat'>Indicateurs</div>
      <div class='leg-item' style='font-weight:600; font-size:11px; margin-top:4px; margin-bottom:6px;'>
        Part de SAU (%)
      </div>", classes_html)
        }
        
        # Indicateur Bio : légende avec cercles de tailles et gradient de couleurs
        if (input$indicateur == "bio" && nrow(centroid_pat_leg) > 0) {
          
          vals_bio_nonzero <- sort(unique(na.omit(centroid_pat_leg$bio[centroid_pat_leg$bio > 0])))
          if (length(vals_bio_nonzero) == 0) return()
          
          v_bio_min <- min(vals_bio_nonzero)
          v_bio_max <- max(vals_bio_nonzero)
          v_bio_med <- sqrt(v_bio_min * v_bio_max)
          
          b_min <- format(round(v_bio_min), big.mark = " ")
          b_med <- format(round(v_bio_med), big.mark = " ")
          b_max <- format(round(v_bio_max), big.mark = " ")
          
          bio_range_leg <- range(sqrt(vals_bio_nonzero))
          rayons_bio <- scales::rescale(
            sqrt(c(v_bio_min, v_bio_med, v_bio_max)),
            from = bio_range_leg,
            to   = c(3, 30)
          )
          #Réglage de la taille des cercles dans la légende en prenant la moyenne géométrique 
          r_min <- round(rayons_bio[1])
          r_med <- round(rayons_bio[2])
          r_max <- round(rayons_bio[3])
          
          vals_part <- sort(na.omit(centroid_pat_leg$part_bio[centroid_pat_leg$bio > 0]))
          breaks_bio <- unique(round(quantile(vals_part, probs = c(0, 0.25, 0.5, 0.75, 1)), 1))
          couleurs_bio <- c("#e6feda", "#99c221", "#68a532", "#447049")
          
          classes_bio_html <- paste0(vapply(seq_len(length(breaks_bio) - 1), function(i) {
            paste0(
              "<div style='display:flex; align-items:center; gap:6px; margin-bottom:4px;'>",
              "<span style='width:14px; height:14px; border-radius:50%; background:", couleurs_bio[i],
              "; flex-shrink:0; display:inline-block;'></span>",
              "<span style='font-size:12px;'>", breaks_bio[i], " - ", breaks_bio[i+1], " %</span>",
              "</div>"
            )
          }, character(1)), collapse = "")
          
          #
          indicateur_html <- paste0("
<div class='leg-sep-inner'></div>
<div class='leg-cat'>Indicateurs</div>
<div class='leg-item' style='font-weight:600; font-size:11px; margin-top:4px;'>Surface agricole bio (ha)</div>
<div class='leg-item' style='font-size:11px;color:#555;font-style:italic;margin-bottom:4px;'>Taille du cercle proportionnel</div>
<div style='display:flex; flex-direction:column; align-items:flex-start; gap:6px; padding-left:2px; margin-top:4px;'>
  <div style='display:flex; align-items:center; gap:8px;'>
    <div style='width:60px; display:flex; justify-content:center; align-items:center; flex-shrink:0;'>
      <span style='background:transparent; border:1.5px solid #161616; border-radius:50%; width:", r_min*2, "px; height:", r_min*2, "px; display:block;'></span>
    </div>
    <span style='font-size:12px;'>", b_min, " ha</span>
  </div>
  <div style='display:flex; align-items:center; gap:8px;'>
    <div style='width:60px; display:flex; justify-content:center; align-items:center; flex-shrink:0;'>
      <span style='background:transparent; border:1.5px solid #161616; border-radius:50%; width:", r_med*2, "px; height:", r_med*2, "px; display:block;'></span>
    </div>
    <span style='font-size:12px;'>", b_med, " ha</span>
  </div>
  <div style='display:flex; align-items:center; gap:8px;'>
    <div style='width:60px; display:flex; justify-content:center; align-items:center; flex-shrink:0;'>
      <span style='background:transparent; border:1.5px solid #161616; border-radius:50%; width:", r_max*2, "px; height:", r_max*2, "px; display:block;'></span>
    </div>
    <span style='font-size:12px;'>", b_max, " ha</span>
  </div>
</div>
<div class='leg-item' style='font-weight:600; font-size:11px; margin-top:10px;'>Part de SAU bio (%)</div>
<div class='leg-item' style='font-size:11px;color:#555;font-style:italic;margin-bottom:6px;'>Couleur du cercle</div>",
                                    classes_bio_html
          )
        }
      }
      
      # Ajoute la section Communes et indicateurs
      sections <- append(sections, list(paste0("
    <div class='leg-cat'>Communes</div>
    <div class='leg-item'><span class='swatch-line' style='background:#929292;'></span>Limites communales</div>",
                                               indicateur_html
      )))
    }
    
    # Section : Limites administratives (affichée quand le fond administratif est sélectionné)
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
    
    # Ajoute ou met à jour le contrôle de légende sur la carte
    leafletProxy("map") %>%
      removeControl("legend_control") %>%
      addControl(
        html = html,
        position = "topleft",
        layerId = "legend_control"
      )
  })
  
  # Gestion du UI pour le rendu conditionnel des indicateurs communaux
  # Les indicateurs ne s'affichent que lorsqu'un PAT est sélectionné
  output$com_layer_ui <- renderUI({
    if (is.null(pat_actif())) return(NULL)
    
    tagList(
      # Checkbox pour afficher/masquer les communes
      checkboxInput("com_layer", "Communes", TRUE),
      hr(),
      # Section conditionnelle : affiche les options d'indicateurs que si les communes sont visibles
      conditionalPanel(
        condition = "input.com_layer == true",
        h4("Indicateurs communaux"),
        radioButtons(
          "indicateur",
          label = NULL,
          choices = c(
            "Aucun"                       = "none",
            "Population"                  = "pop",
            "Surface agricole utile" = "sau",
            "Surface agricole utile bio"  = "bio"
          ),
          selected = "none"
        )
      )
    )
  })

# Gestion du formulaire de recherche des PAT et communes
# Permet à l'utilisateur de chercher un PAT ou une commune par son nom
  observeEvent(input$search_button, {
    req(input$nom_du_pat)
    pat_actif(NULL)
    clic_sur_pat(FALSE)
    session$sendCustomMessage("hide_pat_popup", list())
    recherche <- tolower(trimws(input$nom_du_pat))
    
    # Cherche d'abord si c'est un PAT
    selection_pat <- couche_pat_4326[
      tolower(trimws(couche_pat_4326$nom_du_pat)) == recherche, ]
    
    if (nrow(selection_pat) > 0) {
      select_pat(selection_pat[1, ])
      return()
    }
    
    # Sinon cherche une commune
    selection_com <- commune_aura[
      tolower(trimws(commune_aura$nom_officiel)) == recherche, ]
    
    if (nrow(selection_com) == 0) return()
    
    # Zoome sur la commune trouvée
    bb <- st_bbox(selection_com)
    leafletProxy("map") %>%
      fitBounds(
        lng1 = unname(bb["xmin"]),
        lat1 = unname(bb["ymin"]),
        lng2 = unname(bb["xmax"]),
        lat2 = unname(bb["ymax"]),
        options = list(animate = TRUE, duration = 0.8, easeLinearity = 0.1)
      )
  })
  
  # Affichage dynamique des PAT filtrés avec deux styles : hachuré pour Niveau 1, aplat pour Niveau 2
  # Les PAT sont triés par échelle (départemental → interterritorial → intercommunal)
  observe({
    proxy <- leafletProxy("map")
    proxy %>% clearGroup("Projet Alimentaire Territoriaux")
    proxy %>% clearPopups()
      
    # Récupère les PAT filtrés
    pat_affiche <- pat_filtre()
    
    # Si un PAT est actif, affiche uniquement ce PAT
    if (!is.null(pat_actif())) {
      pat_affiche <- pat_affiche[pat_affiche$nom_du_pat == pat_actif(), ]
    }
    
    if (nrow(pat_affiche) == 0) return()
    
    # Ordre d'affichage : départemental d'abord (fond), puis interterritorial, puis intercommunal (dessus)
    ordre_echelle <- c(
      "PAT d'échelle intercommunale",
      "PAT interterritorial (PAiT)",
      "PAT d'échelle départementale"
    )
    
    pat_affiche$echelle <- factor(pat_affiche$echelle, levels = ordre_echelle)
    pat_affiche <- pat_affiche[order(pat_affiche$echelle, na.last = TRUE), ]
    
    # --- Niveau 2 (aplat) ---
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
    
    # --- Niveau 1 (hachuré) ---
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
      
      # Prépare la map des couleurs pour appliquer les hachures via JavaScript
      couleurs_map <- setNames(
        as.list(pal_pat(pat_niv1$echelle)),
        pat_niv1$nom_du_pat
      )
      # Envoie les couleurs au client pour appliquer les patterns de hachures
      session$sendCustomMessage("apply_hatch", couleurs_map)
    }
  })
  
  # Warning si filtres sans résultat
  warning_visible <- reactiveVal(FALSE)
  
  # Warning si filtres sans résultat
  output$warning_pat_ui <- renderUI({
    pat <- pat_filtre()
    
    # Affiche l'alerte seulement si des filtres sont appliqués et qu'aucun PAT ne correspond
    if (nrow(pat) == 0 &&
        !is.null(input$filtre_niveau) && input$filtre_niveau != "" &&
        !is.null(input$filtre_niveau_terri) && input$filtre_niveau_terri != "") {
      
      tags$div(
        class = "fr-alert fr-alert--warning alert-inline-warning",
        tags$p(
          class = "fr-alert__title",
          "Aucun PAT correspondant aux filtres sélectionnés."
        )
      )
    } else {
      NULL
    }
  })
  
# Initialisation des hachures au démarrage pour les PAT Niveau 1
# Applique les patterns SVG via JavaScript une fois la carte chargée
  observeEvent(input$map_initialized, {
    pat_affiche <- pat_filtre()
    pat_niv1 <- pat_affiche[!is.na(pat_affiche$niveau) & pat_affiche$niveau == "1", ]
    if (nrow(pat_niv1) == 0) return()
    
    # Prépare la map nom_du_pat → couleur pour les hachures
    couleurs_map <- setNames(
      as.list(pal_pat(pat_niv1$echelle)),
      pat_niv1$nom_du_pat
    )
    # Envoie au JavaScript pour appliquer les patterns
    session$sendCustomMessage("apply_hatch", couleurs_map)
  })
  
  # Gestion des clics sur les PAT de la carte
  # Permet de sélectionner un PAT ou un groupe de PAT si plusieurs se chevauchent
  observeEvent(input$map_shape_click, {
    if (!isTRUE(input$pat_layer)) return()
    
    click <- input$map_shape_click
    req(click)
    
    # Crée un point avec les coordonnées du clic
    point <- st_sfc(st_point(c(click$lng, click$lat)), crs = 4326)
    
    # IMPORTANT : si un PAT est déjà actif, on ne teste le clic
    # que sur ce PAT visible, pas sur tous les PAT filtrés
    pat_sf <- pat_filtre()
    if (!is.null(pat_actif())) {
      pat_sf <- pat_sf[pat_sf$nom_du_pat == pat_actif(), ]
    }
    
    # Détermine quels PAT intersectent le point cliqué
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
      echelle   <- htmltools::htmlEscape(as.character(pat_click$echelle[i]))

      # Crée le lien HTML avec onmouseover/onmouseout pour le style au survol
      paste0(
        "<li style='margin-bottom:6px; list-style:none;'>",
        "<a href='#'",
        " style='display:block; padding:6px 10px; background:#f5f5fe;",
        " border-left:3px solid #000091; color:#000091 !important;",
        " text-decoration:none !important; font-size:12px;",
        " font-family:Marianne,Arial,sans-serif;'",
        " onmouseover=\"this.style.background='#e3e3fd'\"",
        " onmouseout=\"this.style.background='#f5f5fe'\"",
        " onclick='Shiny.setInputValue(\"pat_selectionne_code\", ",
        code_json,
        ", {priority:\"event\"}); return false;'>",
        "<span style='font-weight:600;'>", libelle, "</span>",
        "<br/><span style='font-size:11px; color:#666;'>", echelle, "</span>",
        "</a></li>"
      )
    },
    character(1)
  ),
  collapse = ""
)

    # Affiche le popup avec les options de sélection
    leafletProxy("map") %>%
      clearPopups() %>%
      addPopups(
        lng = click$lng,
        lat = click$lat,
        popup = paste0("
  <div style='font-family:Marianne,Arial,sans-serif; font-size:13px; min-width:200px;'>
    <div style='font-weight:700; color:#000091; font-size:13px;
      margin-bottom:8px; padding-bottom:6px; border-bottom:2px solid #000091;'>
      Quel PAT souhaitez-vous consulter ? 
    </div>
    <ul style='margin:0; padding:0;'>", liens, "</ul>
  </div>
")
      )
  })
  
  # Gestion du clic sur un PAT depuis le menu latéral ou la recherche
  observeEvent(input$pat_selectionne, {
    req(input$pat_selectionne)
    
    # Récupère le PAT correspondant au nom sélectionné
    pat <- pat_filtre()
    pat_row <- pat[pat$nom_du_pat == input$pat_selectionne, ]
    if (nrow(pat_row) == 0) return()
    
    select_pat(pat_row, depuis_clic_carte = FALSE)
  })
  
  
  # Gestion du clic sur un PAT depuis le popup multi-PAT (via code_pat)
  observeEvent(input$pat_selectionne_code, {
    req(input$pat_selectionne_code)
    
    # Récupère le PAT correspondant au code_pat
    pat <- pat_filtre()
    pat_row <- pat[as.character(pat$code_pat) == as.character(input$pat_selectionne_code), ]
    if (nrow(pat_row) == 0) return()
    
    select_pat(pat_row, depuis_clic_carte = FALSE)
  })
  
  # Gestion du clic ailleurs sur la carte : désélectionne le PAT actif
  observeEvent(input$map_click, {
    # Si un popup multi-PAT est affiché, on ignore le clic (pour ne pas le fermer immédiatement)
    if (clic_sur_pat()) {
      clic_sur_pat(FALSE)
      return()
    }
    
    # Sinon, désélectionne le PAT actif
    if (!is.null(pat_actif())) {
      deselect_pat()
    }
  })
}

################################# LANCEMENT DE L'APPLICATION #########################################################
# Lance l'application Shiny en reliant l'interface utilisateur (UI) au serveur (Server)
shinyApp(ui, server)
