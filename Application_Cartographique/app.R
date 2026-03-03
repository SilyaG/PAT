###########################################PROJET CAROT########################################################
##APPEL AUX LIBRAIRIES NECÉSSAIRES À LA CRÉATION DE LA CARTE INTERACTIVE##
library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(readr)
library(stringi)
library(jsonlite)  
library(scales)   
#remotes::install_github("trafficonese/leaflet.extras")

# Chargement des couches et reprojection en 4326
# (Correction technique) : st_transform() doit recevoir directement le CRS cible (ex: 4326)
couche_pat_4326 <- st_read("./data/pat_aura_112025.gpkg") %>%
  st_transform(4326)

commune_aura <- st_read("./data/communes.gpkg") %>%
  st_transform(4326)

couche_cls_4326 <- st_read("./data/cls_aura.gpkg") %>% 
  st_transform(4326)

dep_aura_4326 <- st_read("./data/departement_aura.gpkg") %>% 
  st_transform(4326)

pat_com <- read_csv("./data/pat_com.csv")

#Autocomplétion : listes séparées (Communes vs PAT)
autocomplete_communes <- sort(unique(na.omit(commune_aura$nom_officiel))) #liste nom_officiel
autocomplete_pats     <- sort(unique(na.omit(couche_pat_4326$nom_du_pat))) #liste nom_du_pat


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
    tags$link(
      href = "https://cdn.jsdelivr.net/npm/@gouvfr/dsfr@latest/dist/dsfr.min.css",
      rel = "stylesheet"
    ),
    
#Création des divers styles nécessaires à la mise en page 
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
    /* Autocomplete panel : place le menu sous la barre de recherche */
    .fr-search-bar { position: relative; }

    /* design du menu déroulant barre de recherche */
    .autocomplete-panel{
      display:none;
      position:absolute;
      left:0;
      right:0;
      top: calc(100% + 4px);
      z-index: 9999;
    }

    /* style visuel du panneau déroulant */
    .autocomplete-card{
      background:#fff;
      border:1px solid #e5e5e5;
      border-radius:8px;
      box-shadow: 0 6px 18px rgba(0,0,0,0.12);
      padding:8px;
    }

    /* Design du titre */
    .autocomplete-title{
      font-size:12px;
      font-weight:600;
      margin:2px 0 6px 0;
      color:#000091;;
    }
    /*ligne de séparation */
    .autocomplete-sep{
      height:1px;
      background:#eee;
      margin:8px 0;
    }
    .autocomplete-list{
      list-style:none;
      padding:0;
      margin:0;
      max-height:140px;
      overflow:auto;
    }
    .autocomplete-list li{
      font-size:12px;
      padding:6px 8px;
      border-radius:6px;
      cursor:pointer;
      line-height:1.2;
      color:#161616;
      user-select:none;
    }
    .autocomplete-list li:hover{
      background:#f6f6f6;
    }
    .autocomplete-empty{
      cursor:default;
      opacity:0.7;
    }
    
    
// CHARLOTTE ET OLIVIER 
// Style des boutons zoom/dezoom/plein écran
    .leaflet-control-zoom.leaflet-bar {
      border: none !important;
      box-shadow: none !important;
      background: none !important;
    }
    
    .leaflet-control-zoom a {
      background-color: #ffffff !important; /* Couleur de fond souhaitée */
      border: 1px solid #e5e5e5 !important; /* Bordure légère type DSFR */
      color: #000091 !important; /* Bleu France */
      background-image: none !important; 
      text-indent: 0 !important;
      display: flex !important;
      align-items: center !important;
      justify-content: center !important;
      height: 36px !important;
      width: 36px !important;
      font-size: 24px !important; /* Taille du + et - */
      font-weight: normal !important;
      text-decoration: none !important;
    }

    .leaflet-control-fullscreen a {
      border-radius: 4px !important;
      border: 1px solid #e5e5e5 !important;
      box-shadow: none !important;
      height: 36px !important;
      width: 36px !important;
      justify-content: center !important;
      display: flex !important;
      align-items: center !important;
      padding: 0 !important; 
      line-height: 1 !important; 
      background-size: 30px 58px !important;
      background-repeat: no-repeat !important;
    }
    
    .leaflet-control-fullscreen a:hover,
    .leaflet-control-zoom a:hover{
      background-color: #eeeeee !important;
      color: #000091 !important;
    }
// FIN DU BLOC DE STYLISATION DES ZOOM/DEZOOM/PLEIN ECRAN
    
    
    ")),


#Script autocomplétion : panneau (Communes/PAT) + clic => lance la recherche
    tags$script(HTML(sprintf("
      document.addEventListener('DOMContentLoaded', function () {

        var input  = document.getElementById('nom_du_pat'); // (2e) Entrer de recherche
        var panel  = document.getElementById('autocomplete_panel'); // (2e) Recupere les éléments du panneau déroulant
        var ulCom  = document.getElementById('suggest_communes'); // (2e) Liste des communes issues du champs communes 
        var ulPat  = document.getElementById('suggest_pats'); // (2e) Liste des pat issus du champ nom_du_pat
        var btn    = document.getElementById('search_button'); // (2e) Bouton rechercher

        if (!input || !panel || !ulCom || !ulPat || !btn) return;

        var COMMUNES = %s;  // (2e) Variable table communes 
        var PATS     = %s;  // (2e) Variable table PAT

        // Fonction de normalisation 
        function norm(s) {
          return (s || '')
            .trim() // supprime les espaces
            .toLowerCase() //Ne prend pas en compte les Majuscules et minuscule
            .normalize('NFD')  // Sépare lettre et accents 
            .replace(/[\\u0300-\\u036f]/g, '')   //supprime accents
            .replace(/[’']/g, '')               // supprime apostrophes
            .replace(/[-‐-‒–—―]/g, '')           //supprime tous les tirets
            .replace(/\\s+/g, '');              // supprime tous les espaces
        }

        function clearList(ul) { ul.innerHTML = ''; }

        function addItem(ul, label, clickable) {
          var li = document.createElement('li');
          li.textContent = label;

          if (clickable) {
            li.addEventListener('click', function () {

              input.value = label;

              // informe Shiny que la valeur a changé
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('nom_du_pat', label, { priority: 'event' });
              }

              panel.style.display = 'none';

              // Lance la recherche après que Shiny ait bien reçu la valeur
              setTimeout(function () {
                btn.click();
              }, 50);

            });
          } else {
            li.classList.add('autocomplete-empty');
          }

          ul.appendChild(li);
        }

// fonction de mise a jour suivant entrée 
        function updatePanel() {
          var q = norm(input.value);  // Récupere la saisie de l'utilisateur

          if (q.length === 0) {
            panel.style.display = 'none';
            clearList(ulCom);
            clearList(ulPat);
            return;
          }

  // Filtrage saisie commune commence par X
          var comMatches = COMMUNES.filter(function(x){ return norm(x).startsWith(q); }).slice(0, 20);

// Filtrage saisie PAT contient x
          var patMatches = PATS.filter(function(x){ return norm(x).includes(q); }).slice(0, 20);

// Affichage des nouvelles listes
          clearList(ulCom);
          clearList(ulPat);

          if (comMatches.length === 0) addItem(ulCom, 'Aucun résultat', false);
          for (var i = 0; i < comMatches.length; i++) addItem(ulCom, comMatches[i], true);

          if (patMatches.length === 0) addItem(ulPat, 'Aucun résultat', false);
          for (var j = 0; j < patMatches.length; j++) addItem(ulPat, patMatches[j], true);

          panel.style.display = 'block';
        }

        input.addEventListener('input', updatePanel);

// Entrée = recherche 
        input.addEventListener('keydown', function(e){
          if (e.key === 'Enter') {
            e.preventDefault();
            btn.click();
            panel.style.display = 'none';
          }
          if (e.key === 'Escape') {
            panel.style.display = 'none';
          }
        });
// clic ailleurs => masque 
        document.addEventListener('click', function(e){
          if (e.target === input || panel.contains(e.target)) return;
          panel.style.display = 'none';
        });

      });
    ",
                             jsonlite::toJSON(autocomplete_communes, auto_unbox = TRUE),
                             jsonlite::toJSON(autocomplete_pats, auto_unbox = TRUE)
    )))
  ), # FIN tags$head
  

#Création de la page introductive (1ère page du tutoriel)
  tags$div(
    id = "intro_overlay",
    class = "intro-overlay fr-overlay",  # Le style DSFR de l'overlay
    tags$div(
      class = "fr-container fr-container--fluid",  # Container fluide de DSFR
      tags$div(
        class = "fr-card fr-card--xl fr-p-5 fr-m-auto fr-text-center",  # Le container avec une carte DSFR, texte centré
        tags$h2(class = "fr-h2", "Bienvenue sur l'application des Projets Alimentaires Territoriaux"),
        tags$p(class = "fr-text fr-mb-3", "Cette application permet d'explorer les Projets Alimentaires Territoriaux (PAT) et leurs indicateurs. Vous pouvez filtrer les PAT, rechercher une commune ou un PAT, et visualiser des informations détaillées."),
        tags$p(class = "fr-text fr-mb-3", "Cliquez sur le bouton ci-dessous pour commencer le tutoriel et apprendre à utiliser l'application."),
        tags$div(
          style = "display: flex; gap: 20px; justify-content: center;",
          tags$button(class = "fr-btn fr-btn--primary", id = "start_tutorial", "Démarrer le tutoriel"),
          tags$button(class = "fr-btn fr-btn--secondary", id = "skip_tutorial", "Passer le tutoriel")
        )
      )
    )
  ),
  
  
#En-tête DSFR
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
# Bloc filtres à gauche
      tags$div(
        style = "display:flex; gap:20px; margin:0; padding:0;",
        
        tags$div(
          style = "width:250px; margin:0; padding:0;",
          
          tags$select( #Menu déroulant pour filtrer selon le niveau de labellisation
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
          
          tags$select( #Menu déroulant pour filtrer selon l’échelle territoriale
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
      

#Barre de recherche à droite + bouton info
#Ajout du panneau de suggestions (Communes et PAT)
      tags$div(
        style = "display:flex; align-items:flex-end; gap:8px; margin-left:auto;",
        
# Bouton information DSFR
        tags$button(
          id = "info_tutorial",
          class = "fr-btn--tooltip fr-btn",
          type = "button",
          title = "Lancer le tutoriel",
          `aria-label` = "Lancer le tutoriel"
        ),
        
# Barre de recherche DSFR + panneau suggestions 
        tags$div(
          class = "fr-search-bar",
          role = "search",
          style = "width:250px; margin:0;",
          
          tags$label(
            class = "fr-label",
            `for` = "nom_du_pat"
          ),
          
#Panneau de suggestions (Communes et PAT)
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
    
#Création des colonnes : menu couches + carte
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
      
      column(
        width = 9,
        leafletOutput("map", height = "80vh")
      )
    )
  ),
  
  
#Pied de page DSFR
  tags$footer(
    class = "fr-footer",
    tags$div(
      class = "fr-container",
      tags$p("© République Française - Tous droits réservés")
    )
  ),
  
#Création du tutoriel
  ###########################################
  tags$div(id = "tutorial_overlay", class = "tutorial-overlay"),
  tags$div(id = "tutorial_highlight", class = "tutorial-highlight"),
  tags$div(
    id = "tutorial_modal", class = "tutorial-modal",
    tags$button(
      id = "tutorial_close",
      class = "fr-btn fr-btn--tertiary-no-outline", #bouton "?"
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
  
#Script tutoriel
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
) # fermeture UI



###########################################Partie SERVER###########################################################
server <- function(input, output, session) {
  
  
  ## début Silya##
  
#Permet de réafficher tous les PAT suite à un clic
  pat_actif <- reactiveVal(NULL)
  
#pour afficher uniquement le PAT sur lequel on as cliqué 
  clic_sur_pat <- reactiveVal(FALSE)
  
#pour que les popup des PAT filtrer ne s'affiche plus au clic quand ils ne sont pas afficher
  pat_filtre <- reactive({
    pat <- couche_pat_4326
    
    #en fonction du filtre niveau
    if(!is.null(input$filtre_niveau)&&
        input$filtre_niveau != "Tous" &&
        input$filtre_niveau !=""){
      pat <- pat[pat$niveau == input$filtre_niveau,]
    }
    
    #en fonction du filtre échelle
    if(!is.null(input$filtre_niveau_terri) &&
       input$filtre_niveau_terri != "Tous" &&
       input$filtre_niveau_terri !=""){
      pat <- pat[pat$echelle == input$filtre_niveau_terri,]
    }
  
    return(pat)
  })
  
#Creation d'un fonction permettant de créer le Pop-up de la couche PAT (A AMELIORER) 
  popup_pat <- function(pat){
    paste0(
      "<strong>",pat$nom_du_pat,"</strong><br/>",
      "Niveau :", pat$niveau, "<br/>",
      "Population :",pat$pop_hab, "<br/>",
      "<hr>",
      "<strong>Indicateurs :</strong><br/>",
      "Indicateur 1 : ...<br/>"
    )
  }
  
  zoom_pat <- function(pat){
    req(input$pat_layer)
    req(nrow(pat) > 0)
    pat_actif(pat$nom_du_pat)
    
    bb <- st_bbox(pat)
    
    centre <- st_centroid(pat)
    coords <- st_coordinates(centre)
    
    leafletProxy("map") %>% 
      flyToBounds(
        lng1 = unname(bb["xmin"]),
        lat1 = unname(bb["ymin"]),
        lng2 = unname(bb["xmax"]),
        lat2 = unname(bb["ymax"])
      ) %>% 
      clearPopups() %>% 
      addPopups(
        lng = coords[1],
        lat = coords[2],
        popup = popup_pat(pat)
      )
  }
  
  
  ### fin silya ###
  
  
  
#L'affichage de la carte en elle-même paramétrages de la BBOX 
  output$map <- renderLeaflet({
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
      
##aJOUT PLEIN ECRAN/ZOOM/DEZOOM
      addFullscreenControl(position = "topright")%>%
      addTiles() %>%
      htmlwidgets::onRender("
    function(el, x) {
      this.zoomControl.setPosition('topright');
    }
  ")%>%
      
      
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
  
# Paramétrages du sélecteur de couches
  #Fonds de plans 
  observe({
    proxy <- leafletProxy("map")
    
    #Cache les fonds de plan à l'ouverture de la carte 
    proxy %>% hideGroup("Plan IGN")
    proxy %>% hideGroup("Registre Parcellaire Graphique")
    proxy %>% hideGroup("OSM")
    
    #Pour ensuite afficher la couche choisi par utilisateur 
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
  
#Polygones 
  observe({
    proxy <- leafletProxy("map")
    
    if (input$pat_layer){
      proxy %>% showGroup("Projet Alimentaire Territoriaux")
    } else {
      proxy %>% hideGroup("Projet Alimentaire Territoriaux")
    }
    
    if (input$cls_layer){
      proxy %>% showGroup("Contrats locaux de santé")
    } else {
      proxy %>% hideGroup("Contrats locaux de santé")
    }
    
    if (input$com_layer){
      proxy %>% showGroup("Communes")
    } else {
      proxy %>% hideGroup("Communes")
    }
    
    if(input$dep_layer){
      proxy %>% showGroup("Departement")
    } else {
      proxy %>% hideGroup("Departement")
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
      proxy %>% showGroup("SAU bio")
    }
  })
  
# Paramétrages de l'action déclenchée par le bouton recherche
# l'autocomplétion déclenche le bouton via btn.click()
  observeEvent(input$search_button, {
    req(input$nom_du_pat)
    
#Normalise la recherche (évite la sensibilité à la casse notamment) 
    recherche <- tolower(trimws(input$nom_du_pat))
    
#Recherche exacte du nom du PAT 
    selection_pat <- couche_pat_4326[
      tolower(trimws(couche_pat_4326$nom_du_pat)) == recherche,
    ]
    
    if (nrow(selection_pat) > 0) {
#Zoom animé sur l'emprise du PAT trouvé
      bb <- st_bbox(selection_pat)
      leafletProxy("map") %>%
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
      tolower(trimws(commune_aura$nom_officiel)) == recherche,
    ]
    
    if (nrow(selection_com) == 0) {
#Si aucune réponse trouvée, affiche un message d'erreur
      showNotification("PAT ou commune non trouvé", type = "warning")
      return()
    }
    
#prend en compte les limites du polygones pour le zoom (centroide impossible car multipolygones)
    bb <- st_bbox(selection_com)
    
    leafletProxy("map") %>%
      flyToBounds(
        lng1 = unname(bb["xmin"]),
        lat1 = unname(bb["ymin"]),
        lng2 = unname(bb["xmax"]),
        lat2 = unname(bb["ymax"])
      )
  })
  
  
# Paramétrages des filtres (combinés) 
  observe({
    
    proxy <- leafletProxy("map")
    
    #on vide la couche 
    proxy %>% clearGroup("Projet Alimentaire Territoriaux")
    
    pat_affiche <- pat_filtre()
    
    #Si un PAT est cliqué on affiche uniquement celui la 
    if(!is.null(pat_actif())){
      pat_affiche <- pat_affiche[
        pat_affiche$nom_du_pat == pat_actif(),
      ]
    }
  
     #Si aucun PAT après filtre on affiche rien 
     if(nrow(pat_affiche)==0)return()
    
#Obligatoire de recréer la palette dans cet observe pour que elle soit effective 
    pal_pat <- colorFactor(
      palette = c("#fbe769", "#E4794A"),
      domain = couche_pat_4326$niveau
    )
    
  #evite l'affichage des popup quand la couche n'est pas coché
    proxy %>% clearGroup("Projet Alimentaire Territoriaux")
    proxy %>% clearPopups()
    
#Réaffichage uniquement de la sélection
    proxy %>% addPolygons(
      data = pat_affiche,
      layerId  = ~nom_du_pat,
      color = ~pal_pat(niveau),
      fillColor = ~pal_pat(niveau),
      weight = 3,
      fillOpacity = 0.35,
      group = "Projet Alimentaire Territoriaux"
    )
  })
  
  
# Interception des clics sur la couche PAT pour l'affichage de pop-up 
  observeEvent(input$map_shape_click, {
    
    if(!input$pat_layer) return()
    
    clic_sur_pat(TRUE)
    
    click <- input$map_shape_click
    req(click)
    
    #point cliqué 
    point <- st_sfc(
      st_point(c(click$lng, click$lat)),
      crs = 4326
    )
    
#Recherche des PAT qui s'intersectent 
    pat_sf <- pat_filtre()
    intersect <- st_intersects(pat_sf, point, sparse = FALSE)
    pat_click <- pat_sf[unlist(intersect),]
    
#Cas 1 : aucun PAT présent à l'endroit du clic 
    if (nrow(pat_click)== 0) return()
    
#Cas 2 : un seul PAT present à l'endroit du clic
    if (nrow(pat_click)==1){
      zoom_pat(pat_click[1,])
      return()
    }
      
#Cas 3 : Plusieurs PAT à l'endroit du clic 
      liens <- paste0(
        "<li><a href='#' onclick=\"Shiny.setInputValue('pat_selectionne','",
        pat_click$nom_du_pat,
        "', {priority:'event'})\">",
        pat_click$nom_du_pat,
        "</a></li>",
        collapse = ""
      )
      contenu <- paste0(
        "<strong>Plusieurs PAT à cet endroit :</strong><br/>",
        "<ul>", liens, "</ul>"
      )
      
      leafletProxy("map") %>%
        clearPopups() %>%
        addPopups(
          lng = click$lng,
          lat = click$lat,
          popup = contenu
        )
  })
  
#Gerer le clic sur un PAT de la liste 
  observeEvent(input$pat_selectionne, {
    
    clic_sur_pat(TRUE)
    
    req(input$pat_selectionne)
    
    pat_select <- pat_filtre()[
      pat_filtre()$nom_du_pat == input$pat_selectionne,
    ]
    
    if (nrow(pat_select)== 0) return()
    
    zoom_pat(pat_select)
  })
  
#Reset si on clic ailleurs sur la carte (tous les PAT se réaffiche) 
  observeEvent(input$map_click, {
    
#si on clique sur le polygone d'un PAT on ignore le reset 
    if (clic_sur_pat()){
      clic_sur_pat(FALSE)
      return()
    }
    
#sinon on reset 
    if(!is.null(pat_actif())){
      pat_actif(NULL)
      leafletProxy("map") %>%
        clearPopups()
    }
  })
}


#################################LANCEMENT DE L'APPLICATION#########################################################
shinyApp(ui, server)