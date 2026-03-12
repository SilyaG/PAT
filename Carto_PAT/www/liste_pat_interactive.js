// Copyright (C) [2026] [Gréaume Paul, Guerboub Silya, Jouve Charlotte, Prima Oliver / Université Lumière Lyon 2] 
// Distribué sous licence CeCILL-2.1 — voir le fichier LICENSE pour les détails.

// Chacun des fichier JS correspond à une fonction bien défini compréhensible par le nom du fichier 
// Ici il s'agit du pannel de droite comprenant la liste des PAT 

// ===== Clic sur PAT dans la sidebar =====

// Ajout d'un écouteur d'élément permettant de capter les cliques et mettre à jour l'affichage en fonction
document.addEventListener('click', function(e){

  //Vérifie que l'élément cliqué existe et possède la classe "pat-link"
  if(e.target && e.target.classList.contains('pat-link')){

    // Empeche le comportement par defaut des liens
    e.preventDefault();

    // Recupère la valeur de l'attribut
    var pat = e.target.getAttribute('data-pat');
    if(window.Shiny){

      // Envoie la valeur du PAT au serveur shiny 
      // "pat_selectionne" devient un input coté serveur
      //  priority: 'event' force Shiny à traiter l'évenement en priorité
      Shiny.setInputValue('pat_selectionne', pat, {priority: 'event'});
    }
  }
});