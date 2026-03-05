// Chacun des fichier JS correspond à une fonction bien défini compréhensible par le nom du fichier 
// Ici il s'agit du fichier concernant le tutoriel d'explication de l'application

// ================ Tutoriel de lancement ==============


document.addEventListener('DOMContentLoaded', function() {

  // Bouton demarrer le tutoriel
    document.getElementById('start_tutorial').addEventListener('click', function() {
      // cache l'écran d'introduction
      document.getElementById('intro_overlay').style.display = 'none';
      // lance le tuto
      startTutorial();
    });
  
    // Bouton passer le tutoriel
    document.getElementById('skip_tutorial').addEventListener('click', function() {
      // cache l'écran d'intro sans lancer le tuto
      document.getElementById('intro_overlay').style.display = 'none';
    });
  
    // bouton de fermeture du tutoriel (croix)
    document.getElementById('tutorial_close').addEventListener('click', function() {
      // ferme completement le tuto
      closeTutorial();
    });
  
    // bouton info permettant de relancer le tuto à tout moment
    var infoBtn = document.getElementById('info_tutorial');
    if (infoBtn) {
      infoBtn.addEventListener('click', function() {
        startTutorial();
      });
    }
  });
  
  // Fonction pour fermer le tuto
  function closeTutorial() {
    // cache le fond sombre
    document.getElementById('tutorial_overlay').style.display = 'none';
    // cache les cadre de surbrillance
    document.getElementById('tutorial_highlight').style.display = 'none';
    // cache la fenetre explicative
    document.getElementById('tutorial_modal').style.display = 'none';
  }
  
  // Fonction principale qui lance le tuto
  function startTutorial() {

    // Tableau contenant toutes les étapes du tuto : 
    // - élément ciblé 
    // -titre affiché
    // - texte explicatif 
    var tutorialSteps = [
      {el:'.menu-couches',        title:'Menu des couches',                   text:'Sélectionnez le fond cartographique et les couches à afficher sur la carte.'},
      {el:'#filtre_niveau',       title:'Filtre des niveaux de labellisation', text:'Utilisez ce filtre pour sélectionner le niveau de labellisation des PAT.'},
      {el:'#filtre_niveau_terri', title:'Filtre des échelles territoriales',   text:'Vous pouvez aussi utiliser ce filtre pour sélectionner l\'échelle territoriale des PAT.'},
      {el:'#map',                 title:'Carte',                              text:'La carte centrale affiche les couches et le fond de carte souhaités.', position:{ top: 700, left: 10 }},
      {el:'#legend_toggle',       title:'Afficher/Masquer la légende',        text:'Cliquez sur ce bouton pour afficher ou masquer la légende des couches visibles sur la carte.'},
      {el:'#map_legend',          title:'Légende',                            text:'La légende s\'actualise en fonction des couches présentes sur la carte.'},
      {el:'#nom_du_pat',          title:'Barre de recherche',                 text:'Vous pouvez rechercher un PAT ou une commune ici.'},
      {el:'#right_sidebar',       title:'Liste des PAT visibles',             text:'Cette liste affiche uniquement les PAT visibles dans la vue actuelle de la carte. Cliquez sur un PAT pour zoomer dessus.'},
      {el:'#info_tutorial',       title:'Relancer le tutoriel',               text:'Vous pouvez relancer le tutoriel à tout moment en cliquant sur cette icône.'}
    ];
  
    var currentStep = 0;
  
    // Fonction affichant les étapes du tuto
    function showStep(step) {
      var s  = tutorialSteps[step]; // recupère les infos de l'étape
      var el = document.querySelector(s.el); // selectionne l'élément ciblé dans la page
      if (!el) return; 
  
      // Recupère la position et la taille de l'élément
      var rect = el.getBoundingClientRect();
  
      // Affiche le fond sombre qui bloque tout la page
      document.getElementById('tutorial_overlay').style.display = 'block';
  
      // Mise en évidence des éléments ciblés
      var hl = document.getElementById('tutorial_highlight');
      hl.style.display = 'block';
      hl.style.top    = (rect.top    - 5)  + 'px';
      hl.style.left   = (rect.left   - 5)  + 'px';
      hl.style.width  = (rect.width  + 10) + 'px';
      hl.style.height = (rect.height + 10) + 'px';
  
      var modal = document.getElementById('tutorial_modal');
      modal.style.display = 'block';
  
      // Cacul la position du modal
      var top, left;

      // Si une position personnalisée est défini on n'utilise pas la position relative
      if (s.position) {
        top  = s.position.top;
        left = s.position.left;
      } else {

        // Position relative aux éléments de la page
        top  = rect.bottom + 15;
        left = rect.left;

        //Empêche que la fenêtre dépasse de l'écran
        if (left + modal.offsetWidth  > window.innerWidth)  left = window.innerWidth  - modal.offsetWidth  - 20;
        if (top  + modal.offsetHeight > window.innerHeight) top  = rect.top - modal.offsetHeight - 15;
        if (left < 10) left = 10;
        if (top  < 10) top  = 10;
      }
      // applique la position calculé
      modal.style.top  = top  + 'px';
      modal.style.left = left + 'px';
  
      // insère le titre et le texte de l'étape
      document.getElementById('tutorial_title').innerText = s.title;
      document.getElementById('tutorial_text').innerText  = s.text;

      // Bouton de navigation
      var prevBtn = document.getElementById('tutorial_prev');
      var nextBtn = document.getElementById('tutorial_next');
  
      // cache le bouton précédents si on est à la première étape
      prevBtn.style.display = (step === 0) ? 'none' : 'inline-block';
      // remplace le bouton suivant en bouton fin à la dernière étape
      nextBtn.innerText     = (step === tutorialSteps.length - 1) ? 'Fin' : 'Suivant';
  
      // onclick remplace le listener précédent : pas d'accumulation au relancement
      nextBtn.onclick = function() {
        if (currentStep < tutorialSteps.length - 1) {
          currentStep++;
          showStep(currentStep);
        } else {
          closeTutorial();
        }
      };
  
      // Bouton précédent
      prevBtn.onclick = function() {
        if (currentStep > 0) {
          currentStep--;
          showStep(currentStep);
        }
      };
    }

  //Lance l'affichage à la première étape 
    showStep(currentStep);
  }