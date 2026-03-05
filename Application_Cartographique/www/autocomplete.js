document.addEventListener('DOMContentLoaded', function () {

    var input  = document.getElementById('nom_du_pat'); // (2e) Entrer de recherche
    var panel  = document.getElementById('autocomplete_panel'); // (2e) Recupere les éléments du panneau déroulant
    var ulCom  = document.getElementById('suggest_communes'); // (2e) Liste des communes issues du champs communes 
    var ulPat  = document.getElementById('suggest_pats'); // (2e) Liste des pat issus du champ nom_du_pat
    var btn    = document.getElementById('search_button'); // (2e) Bouton rechercher

    if (!input || !panel || !ulCom || !ulPat || !btn) return;

    var COMMUNES = window.AUTOCOMPLETE_COMMUNES || [];  // (2e) Variable table communes 
    var PATS     = window.AUTOCOMPLETE_PATS || [];  // (2e) Variable table PAT

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