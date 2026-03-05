document.addEventListener('DOMContentLoaded', function() {

    document.getElementById('start_tutorial').addEventListener('click', function() {
      document.getElementById('intro_overlay').style.display = 'none';
      startTutorial();
    });
  
    document.getElementById('skip_tutorial').addEventListener('click', function() {
      document.getElementById('intro_overlay').style.display = 'none';
    });
  
    document.getElementById('tutorial_close').addEventListener('click', function() {
      closeTutorial();
    });
  
    var infoBtn = document.getElementById('info_tutorial');
    if (infoBtn) {
      infoBtn.addEventListener('click', function() {
        startTutorial();
      });
    }
  });
  
  function closeTutorial() {
    document.getElementById('tutorial_overlay').style.display = 'none';
    document.getElementById('tutorial_highlight').style.display = 'none';
    document.getElementById('tutorial_modal').style.display = 'none';
  }
  
  function startTutorial() {
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
  
    function showStep(step) {
      var s  = tutorialSteps[step];
      var el = document.querySelector(s.el);
      if (!el) return;
  
      var rect = el.getBoundingClientRect();
  
      document.getElementById('tutorial_overlay').style.display = 'block';
  
      var hl = document.getElementById('tutorial_highlight');
      hl.style.display = 'block';
      hl.style.top    = (rect.top    - 5)  + 'px';
      hl.style.left   = (rect.left   - 5)  + 'px';
      hl.style.width  = (rect.width  + 10) + 'px';
      hl.style.height = (rect.height + 10) + 'px';
  
      var modal = document.getElementById('tutorial_modal');
      modal.style.display = 'block';
  
      var top, left;
      if (s.position) {
        top  = s.position.top;
        left = s.position.left;
      } else {
        top  = rect.bottom + 15;
        left = rect.left;
        if (left + modal.offsetWidth  > window.innerWidth)  left = window.innerWidth  - modal.offsetWidth  - 20;
        if (top  + modal.offsetHeight > window.innerHeight) top  = rect.top - modal.offsetHeight - 15;
        if (left < 10) left = 10;
        if (top  < 10) top  = 10;
      }
      modal.style.top  = top  + 'px';
      modal.style.left = left + 'px';
  
      document.getElementById('tutorial_title').innerText = s.title;
      document.getElementById('tutorial_text').innerText  = s.text;
  
      var prevBtn = document.getElementById('tutorial_prev');
      var nextBtn = document.getElementById('tutorial_next');
  
      prevBtn.style.display = (step === 0) ? 'none' : 'inline-block';
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
  
      prevBtn.onclick = function() {
        if (currentStep > 0) {
          currentStep--;
          showStep(currentStep);
        }
      };
    }
  
    showStep(currentStep);
  }