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

#  Autocomplétion : liste des valeurs possibles (PAT + communes) 
autocomplete_choices <- sort(unique(na.omit(c(
  couche_pat_4326$nom_du_pat,
  commune_aura$nom_officiel
))))  #sort valeurs unqique, supprime valeurs manquantes, combine les 2 colonnes en 1 seule



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
    tags$link(
      href = "https://cdn.jsdelivr.net/npm/remixicon@3.5.0/fonts/remixicon.css",
      rel = "stylesheet"
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
    .leaflet-control-scale-line {
    text-align: center;  /* Centre le texte horizontalement */
    font-size: 14px;     /* Ajuste la taille du texte */
  }
  .tutorial-overlay {
    position: fixed;
    top: 0; left: 0;
    width: 100vw; height: 100vh;
    z-index: 9998;
    display: none;
  }
  .tutorial-highlight {
    position: absolute;
    background-color: transparent;
    border: 2px solid #000091;
    border-radius: 6px;
    box-shadow: 0 0 10px #6a6af4;
    z-index: 9999;
    pointer-events: none;
    display: none;
    transition: all 0.3s ease;
  }
  .tutorial-modal {
    position: fixed;
    z-index: 10000;
    background: white;
    border: 2px solid #e5e5e5;
    padding: 20px;
    border-radius: 8px;
    width: 300px;
    display: none;
    transform: none !important;
  }
    .tutorial-modal p {
    font-size: 14px; /* Augmente la taille des paragraphes */
    }
  
/*STyle page d'introduction*/
  .intro-overlay {
    position: fixed;
    top: 0; left: 0;
    width: 100vw; height: 100vh;
    background-color: rgba(0, 0, 0, 0.7); /* Assure que l'overlay est bien visible */
    z-index: 9998;
    display: flex;
    justify-content: center;
    align-items: center;
    color: white;
    font-size: 18px;
    border: 2px solid #e5e5e5;
    padding: 20px;
    border-radius: 8px;
  }

.intro-overlay .content {
  background-color: white;
  padding: 30px;
  border-radius: 10px;
  max-width: 600px;
  text-align: center;
  box-shadow: 0px 0px 20px rgba(0, 0, 0, 0.5);
  color: #333;
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
  width: 90%; /* Donne une largeur relative à la fenêtre */
  height: 60%; /* Donne une hauteur relative à la fenêtre */
}

  .intro-overlay p {
    color: #333;  /* Couleur du texte pour s'assurer qu'il soit lisible */
    font-size: 14px;  /* Taille de texte pour une meilleure lisibilité */
    line-height: 1.5; /* Espacement entre les lignes */
    margin-bottom: 20px;  /* Ajout d'une marge pour espacer les paragraphes */
    text-align: center; /* Alignement des paragraphes au centre */
  }

  .intro-overlay h2 {
    color: #333;
    font-size: 24px;
    margin-bottom: 20px;  /* Ajout d'un espacement pour aérer */
    text-align: center; /* Assurez-vous que le titre est centré */
  }
    .intro-overlay button {
    padding: 12px 24px;  /* Plus d'espace autour du texte */
    font-size: 12px;  /* Taille de police plus grande */

    }
    "))
  ),
# --- Autocomplétion (datalist) : relie la liste au champ DSFR sans modifier le champ existant ---
tags$script(HTML("
  document.addEventListener('DOMContentLoaded', function () {
    var input = document.getElementById('nom_du_pat'); // Récupère le champ input ayant l'id 'nom_du_pat'
    if (input) {
      input.setAttribute('list', 'autocomplete_pat_communes'); // Associe le champ au datalist ayant l'id 'autocomplete_pat_communes'
      input.setAttribute('autocomplete', 'off');
    }
  });
")),
#  Liste des suggestions pour l'autocomplétion 
tags$datalist(id = "autocomplete_pat_communes"),

#  Autocomplétion (startsWith + insensible aux accents) 
  tags$script(HTML(sprintf("
  document.addEventListener('DOMContentLoaded', function () {
    var input = document.getElementById('nom_du_pat');
    var dl = document.getElementById('autocomplete_pat_communes'); // Récupère le datalist vide créé dans l’UI
    if (!input || !dl) return;

    // Relie le datalist au champ existant
    input.setAttribute('list', 'autocomplete_pat_communes');
    input.setAttribute('autocomplete', 'off');

    var ALL = %s; // Injection des données R dans JavaScript
    
    // Fonction de normalisation :
    function norm(s) {
      return (s || '')
        .toString()
        .trim() // supprime espaces début/fin
        .toLowerCase() // ignore majuscules/minuscules
        .normalize('NFD') // sépare lettres + accents
        .replace(/[\\u0300-\\u036f]/g, '') // supprime accents
        
    }

    input.addEventListener('input', function () {
      var q = norm(input.value);

      if (q.length === 0) {
        dl.innerHTML = '';
        return;
      }

      var matches = ALL.filter(function(x){
        return norm(x).startsWith(q);
      }).slice(0, 30);

      dl.innerHTML = '';
      for (var i = 0; i < matches.length; i++) {
        var opt = document.createElement('option');
        opt.value = matches[i];
        dl.appendChild(opt);
      }
    });
  });
", jsonlite::toJSON(autocomplete_choices, auto_unbox = TRUE)))),

#Création de la page introductive
tags$div(
  id = "intro_overlay",
  class = "intro-overlay fr-overlay",  # Le style DSFR de l'overlay
  tags$div(
    class = "fr-container fr-container--fluid",  # Container fluide de DSFR
    tags$div(
      class = "fr-card fr-card--xl fr-p-5 fr-m-auto fr-text-center",  # Le container avec une carte DSFR, texte centré
      # Titre de l'introduction
      tags$h2(class = "fr-h2", "Bienvenue sur l'application des Projets Alimentaires Territoriaux"),
      
      # Texte d'introduction
      tags$p(class = "fr-text fr-mb-3", "Cette application permet d'explorer les Projets Alimentaires Territoriaux (PAT) et leurs indicateurs. Vous pouvez filtrer les PAT, rechercher une commune ou un PAT, et visualiser des informations détaillées."),
      tags$p(class = "fr-text fr-mb-3", "Cliquez sur le bouton ci-dessous pour commencer le tutoriel et apprendre à utiliser l'application."),
      
      # Boutons pour démarrer ou passer le tutoriel
      tags$div(
        style = "display: flex; gap: 20px; justify-content: center;",  # Alignement des boutons
        tags$button(
          class = "fr-btn fr-btn--primary",  # Bouton principal "Démarrer"
          id = "start_tutorial",
          "Démarrer le tutoriel"
        ),
        tags$button(
          class = "fr-btn fr-btn--secondary",  # Bouton secondaire "Passer"
          id = "skip_tutorial",
          "Passer le tutoriel"
        )
      )
    )
  )
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
    
    # --- Liste des suggestions pour l'autocomplétion ---
    tags$datalist(
      id = "autocomplete_pat_communes",
      lapply(autocomplete_choices, function(x) tags$option(value = x))
    ),
    
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
      
# Barre de recherche à droite + bouton info 
# Bloc recherche + bouton info
tags$div(
  style = "display:flex; align-items:flex-end; gap:8px; margin-left:auto;",
  
  # 🔹 Bouton information DSFR (à gauche)
  # Bouton information DSFR robuste (SVG natif)
  tags$button(
    id = "info_tutorial",
    class = "fr-btn--tooltip fr-btn",
    type = "button",
    title = "Lancer le tutoriel",
    `aria-label` = "Lancer le tutoriel"
  ),
  
  # 🔹 Barre de recherche DSFR
  tags$div(
    class = "fr-search-bar",
    role = "search",
    style = "width:250px; margin:0;",
    
    tags$label(
      class = "fr-label",
      `for` = "nom_du_pat"
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
        leafletOutput("map", height = "80vh")
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
  ),

#Création du tutotiel 
tags$div(id = "tutorial_overlay", class = "tutorial-overlay"),
tags$div(id = "tutorial_highlight", class = "tutorial-highlight"),
tags$div(id = "tutorial_modal", class = "tutorial-modal",
         tags$button(
           id = "tutorial_close",
           class = "fr-btn fr-btn--tertiary-no-outline",
           style = "position:absolute; top:10px; right:10px;",
           "✕"
         ),
         tags$h3(id="tutorial_title", "Titre"),
         tags$p(id="tutorial_text", "Texte de description"),
         tags$div(style="text-align:right; margin-top:10px;",
                  tags$button(class="fr-btn", id="tutorial_prev", "Précédent"),
                  tags$button(class="fr-btn", id="tutorial_next", "Suivant")
         )
),
tags$script(HTML("
                 document.addEventListener('DOMContentLoaded', function() {
                   // Lorsque l'utilisateur clique sur 'Démarrer le tutoriel'
      document.getElementById('start_tutorial').addEventListener('click', function() {
        // Masquer l'overlay d'introduction
        document.getElementById('intro_overlay').style.display = 'none';
        
        // Lancer le tutoriel
        startTutorial();
      });

      // Lorsque l'utilisateur clique sur 'Passer'
                   document.getElementById('skip_tutorial').addEventListener('click', function() {
                     // Masquer l'overlay d'introduction sans lancer le tutoriel
                     document.getElementById('intro_overlay').style.display = 'none';
                   });
                 });
                 
                 // Fonction pour démarrer le tutoriel
                 function startTutorial() {
                   var tutorialSteps = [
                     {el:'.menu-couches', title:'Menu des couches', text:'Sélectionnez le fond cartographique et les couches à afficher sur la carte.'},
                     {el:'#filtre_niveau', title:'Filtre niveau', text:'Utilisez ce filtre pour sélectionner le niveau des PAT.'},
                     {el:'#filtre_niveau_terri', title:'Filtre échelle', text:'Filtrez selon l’échelle territoriale.'},
                     {el:'#nom_du_pat', title:'Barre de recherche', text:'Recherchez un PAT ou une commune ici.', position:{ top: 130, left: 1600 }},
                     {el:'#map', title:'Carte', text:'La carte centrale affiche les PAT et indicateurs.', position:{ top: 750, left: 10 }},
                     {el:'#info_tutorial', title:'Relancer le tutoriel', text:'Vous pouvez relancer le tutoriel à tout moment en cliquant sur cette icône.', position:{ top: 130, left: 1600 }}
                   ];
                   
                   var currentStep = 0;
                                     
                function showStep(step){
                  var s = tutorialSteps[step];
                  var el = document.querySelector(s.el);
                  if(!el) return;
                
                  var rect = el.getBoundingClientRect();
                
                  var overlay = document.getElementById('tutorial_overlay');
                  overlay.style.display='block';
                
                  var hl = document.getElementById('tutorial_highlight');
                  hl.style.display='block';
                  hl.style.top = (rect.top - 5) + 'px';
                  hl.style.left = (rect.left - 5) + 'px';
                  hl.style.width = (rect.width + 10) + 'px';
                  hl.style.height = (rect.height + 10) + 'px';
                
                  var modal = document.getElementById('tutorial_modal');
                  modal.style.display='block';
                
                  if(s.position){
                    modal.style.top = s.position.top + 'px';
                    modal.style.left = s.position.left + 'px';
                  } else {
                    modal.style.top = (rect.bottom + 10) + 'px';
                    modal.style.left = rect.left + 'px';
                  }
                
                  document.getElementById('tutorial_title').innerText = s.title;
                  document.getElementById('tutorial_text').innerText = s.text;
                
                  var prevBtn = document.getElementById('tutorial_prev');
                  var nextBtn = document.getElementById('tutorial_next');
                
                  if(step === 0){
                    prevBtn.style.display = 'none';
                  } else {
                    prevBtn.style.display = 'inline-block';
                  }
                
                  if(step === tutorialSteps.length - 1){
                    nextBtn.innerText = 'Fin';
                  } else {
                    nextBtn.innerText = 'Suivant';
                  }
                }
                   document.getElementById('tutorial_next').addEventListener('click', function(){
                     if(currentStep < tutorialSteps.length-1){currentStep++; showStep(currentStep);}
                     else {
                       document.getElementById('tutorial_overlay').style.display='none';
                       document.getElementById('tutorial_highlight').style.display='none';
                       document.getElementById('tutorial_modal').style.display='none';
                     }
                   });
                   
                   document.getElementById('tutorial_prev').addEventListener('click', function(){
                     if(currentStep > 0){currentStep--; showStep(currentStep);}
                   });
                   
                   // Affiche le premier step
                   showStep(currentStep);
                 }
                 
                 function closeTutorial(){
  document.getElementById('tutorial_overlay').style.display='none';
  document.getElementById('tutorial_highlight').style.display='none';
  document.getElementById('tutorial_modal').style.display='none';
                 }
document.getElementById('tutorial_close').addEventListener('click', function(){
  closeTutorial();
})

var infoBtn = document.getElementById('info_tutorial');
if (infoBtn) {
  infoBtn.addEventListener('click', function () {
    startTutorial();
  });
};
                 "))
)#fermeture de l'UI
  
  
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
        
        addScaleBar(
          position = "bottomleft",
          options = scaleBarOptions(
            metric = TRUE,
            imperial = FALSE,
            updateWhenIdle = TRUE,
            maxWidth = 150  # Longueur
          )
        )%>%

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
