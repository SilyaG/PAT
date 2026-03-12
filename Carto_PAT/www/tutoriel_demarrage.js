// Copyright (C) [2026] [Gréaume Paul, Guerboub Silya, Jouve Charlotte, Prima Oliver / Université Lumière Lyon 2] 
// Distribué sous licence CeCILL-2.1 — voir le fichier LICENSE pour les détails.


// Chacun des fichier JS correspond à une fonction bien défini compréhensible par le nom du fichier 
// Ici il s'agit du fichier concernant le tutoriel d'explication de l'application

// ================ Tutoriel de lancement ==============


document.addEventListener('DOMContentLoaded', function() {

  // Bouton de démarrage du tutoriel afficher sur l'écran d'acceuil
  document.getElementById('start_tutorial').addEventListener('click', function() {
    document.getElementById('intro_overlay').style.display = 'none';
    startTutorial(); // Lancer la séquence
  });

  // Bouton "passer" sur l'écran d'accueil
  document.getElementById('skip_tutorial').addEventListener('click', function() {
    document.getElementById('intro_overlay').style.display = 'none';
  });

  // Croix de fermeture quand le tutoriel est déja en cours
  document.getElementById('tutorial_close').addEventListener('click', function() {
    closeTutorial();
  });

  var infoBtn = document.getElementById('info_tutorial');
  if (infoBtn) {
    // Icône d'info pour relancer le tuto
    infoBtn.addEventListener('click', function() {
      startTutorial();
    });
  }
});

function closeTutorial() {
  //Masque tous les éléments de l'overlay/tutorial
  document.getElementById('tutorial_overlay').style.display = 'none';
  document.getElementById('tutorial_highlight').style.display = 'none';
  document.getElementById('tutorial_modal').style.display = 'none';
}

function startTutorial() {

  var tutorialSteps = [
    {el:'.menu-couches',        title:'Menu des couches',  text:'Sélectionnez le fond cartographique et les couches à afficher sur la carte.'},
    {el: '.menu-couches',title: 'Indicateurs communaux',
      html: `
        <p style="font-size:13px; color:#333; line-height:1.5; margin-bottom:8px;">
          Après avoir <strong>cliqué sur un PAT</strong>, une nouvelle couche 
          <strong>« Communes »</strong> apparaît dans la section <strong>« Couches »</strong> du panneau de sélection.
        </p>
        <p style="font-size:13px; color:#333; line-height:1.5;">
          En l'activant, vous pourrez visualiser des <strong>indicateurs communaux</strong> 
          directement sur la carte.
        </p>
        <p style="font-size:11px; color:#666; margin-top:10px; font-style:italic;">
          Cliquez sur un PAT → cochez "Communes" → choisissez un indicateur.
        </p>
      `},
    {el:'#filtre_niveau',       title:'Filtre des niveaux de labellisation',text:'Utilisez ce filtre pour sélectionner le niveau de labellisation des PAT.'},
    {el:'#filtre_niveau_terri', title:'Filtre des échelles territoriales', text:'Utilisez ce filtre pour sélectionner l\'échelle territoriale des PAT. Les filtres "niveau" et "échelle" sont cumulables.'},
    {el:'#reset_button',        title:'Réinitialiser la carte', text:'Ce bouton remet la carte dans son état initial : les filtres sont effacés, le zoom revient sur la région entière et tout PAT sélectionné est désélectionné.'},
    {el:'#legend_toggle',       title:'Afficher/Masquer la légende', text:'Cliquez sur ce bouton pour afficher ou masquer la légende des couches visibles sur la carte.'},
    {el:'#map_legend',          title:'Légende', text:'La légende s\'actualise automatiquement en fonction des couches visibles sur la carte.'},
    {el:'#map',                 title:'Carte',   text:'La carte centrale affiche les couches et le fond cartographique sélectionnés. Cliquez sur un PAT pour le sélectionner et cliquez ailleurs pour revenir à la vue initiale.', placement:'center'},
    {el:'#nom_du_pat',          title:'Barre de recherche', text:'Vous pouvez rechercher un PAT ou une commune ici.'},
    {el:'#right_sidebar',       title:'Liste des PAT visibles', text:'Cette liste affiche les PAT visibles dans la vue actuelle de la carte. Cliquez sur un PAT pour zoomer dessus.', placement:'left'},
    {el:'#info_tutorial',       title:'Relancer le tutoriel', text:'Vous pouvez relancer ce tutoriel à tout moment en cliquant sur cette icône.'}
     ];
     
  var currentStep = 0;

  function showStep(step) {
    var s  = tutorialSteps[step];
    var el = document.querySelector(s.el);

    console.log("Étape", step, "- sélecteur:", s.el, "- élément trouvé:", el);

    // Si l'élément n'existe pas, on saute l'étape
    if (!el) {
      if (step < tutorialSteps.length - 1) {
        currentStep++;
        showStep(currentStep);
      } else {
        closeTutorial();
      }
      return;
    }

    // Scroll vers l'élément puis délai pour stabilisation
    el.scrollIntoView({ block: 'nearest', inline: 'nearest' });

    setTimeout(function() {
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

      var isMobile = window.innerWidth <= 768;
      var margin = 12;
      var vw = window.innerWidth;
      var vh = window.innerHeight;
      var modalW = modal.offsetWidth;
      var modalH = modal.offsetHeight;

      if (isMobile) {
        modal.style.top  = '';
        modal.style.left = '';
      } else {
        var top, left;
      
        if (s.placement === 'left') {
          // Placement à gauche de l'élément, centré verticalement
          left = rect.left - modalW - margin;
          top  = rect.top + (rect.height / 2) - (modalH / 2);
          // Si ça dépasse à gauche → placer à droite
          if (left < margin) left = rect.right + margin;
          // Sécurités haut/bas
          if (top < margin)                top = margin;
          if (top + modalH > vh - margin)  top = vh - modalH - margin;
      
        } else if (s.placement === 'center') {
          // Centré dans la fenêtre (pas par rapport à l'élément)
          left = (vw / 2) - (modalW / 2);
          top  = (vh / 2) - (modalH / 2);
        
        } else {
          // Placement par défaut : en dessous
          top  = rect.bottom + margin;
          left = rect.left;
          if (left + modalW > vw - margin) left = vw - modalW - margin;
          if (top + modalH > vh - margin)  top  = rect.top - modalH - margin;
          if (top < margin)                top  = Math.max(margin, (vh - modalH) / 2);
          if (left < margin)               left = margin;
        }
      
        modal.style.top  = top  + 'px';
        modal.style.left = left + 'px';
      }

      document.getElementById('tutorial_title').innerText   = s.title;
      var textEl = document.getElementById('tutorial_text');
      if (s.html) {
        textEl.innerHTML = s.html;
      } else {
        textEl.innerText = s.text;
      }
      var prevBtn = document.getElementById('tutorial_prev');
      var nextBtn = document.getElementById('tutorial_next');

      prevBtn.style.display = (step === 0) ? 'none' : 'inline-block';
      nextBtn.innerText     = (step === tutorialSteps.length - 1) ? 'Fin' : 'Suivant';

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

    }, 50); // délai de 50ms pour laisser le scroll se stabiliser
  }

  showStep(currentStep);
}


