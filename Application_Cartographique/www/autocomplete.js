// Chacun des fichier JS correspond à une fonction bien défini compréhensible par le nom du fichier 
// Ici il s'agit de la fonction d'autocompletion de la barre de recherche 

document.addEventListener('DOMContentLoaded', function () {

// Recupération des éléments HTML nécessaire à la fonction
    var input  = document.getElementById('nom_du_pat'); //Entrer de recherche
    var panel  = document.getElementById('autocomplete_panel'); //Recupere les éléments du panneau déroulant
    var ulCom  = document.getElementById('suggest_communes'); //Liste des communes issues du champs communes 
    var ulPat  = document.getElementById('suggest_pats'); //Liste des pat issus du champ nom_du_pat
    var btn    = document.getElementById('search_button'); //Bouton rechercher

// securité : si un élément est manquant le script s'arrete
    if (!input || !panel || !ulCom || !ulPat || !btn) return;

// données utile pour l'autocompletion
    var COMMUNES = window.AUTOCOMPLETE_COMMUNES || [];  //Variable table communes 
    var PATS     = window.AUTOCOMPLETE_PATS || [];  //Variable table PAT

    // Fonction de normalisation du texte, serte à comparer le texte sans être sensible à la casse
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

// vide le contenu d'une liste HTML
    function clearList(ul) { ul.innerHTML = ''; }

// Ajout d'un élément dans la liste de suggestion
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

          // ferme le paneau d'autocompletion
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

// fonction de mise a jour du panneau d'aucompletion 
    function updatePanel() {
      var q = norm(input.value);  // Récupere la saisie de l'utilisateur

      // si le champs est vide le panneau est masqué 
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

// Affichage des resultats des filtres
      clearList(ulCom);
      clearList(ulPat);

      // si aucune commune trouvé
      if (comMatches.length === 0) addItem(ulCom, 'Aucun résultat', false);

      // ajout des communes trouvées
      for (var i = 0; i < comMatches.length; i++) addItem(ulCom, comMatches[i], true);

      // si aucun PAT est trouvé
      if (patMatches.length === 0) addItem(ulPat, 'Aucun résultat', false);
      for (var j = 0; j < patMatches.length; j++) addItem(ulPat, patMatches[j], true);

      // affiche le panneau d'autocompletion
      panel.style.display = 'block';
    }

    // mise à jour des suggestion à chaque frappe
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