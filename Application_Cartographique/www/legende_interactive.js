// Copyright (C) [2026] [Gréaume Paul, Guerboub Silya, Jouve Charlotte, Prima Oliver / Université Lumière Lyon 2] 
// Distribué sous licence CeCILL-2.1 — voir le fichier LICENSE pour les détails.


// Chacun des fichier JS correspond à une fonction bien défini compréhensible par le nom du fichier 
// ici il s'agit du fichier correspondant à la légende

// TOGGLE LÉGENDE 

// gestion du bouton d'affichage / masquage de la légende
document.addEventListener('click', function(e){

  // Vérifie si l'utilisateur à cliqué sur le bouton légende ou sur un élément à l'intérieur 
  var btn = e.target.closest('#legend_toggle');

  // si le clic ne concerne pas le bouton on sort de la boucle
  if(!btn) return;
  e.preventDefault();
  e.stopPropagation(); 

  // recupère l'élement HTML contenant la légende 
  var legend = document.getElementById('map_legend');

  // sécurité : si la légende n'existe pas on sort de la boucle
  if(!legend) return;

// Detection de l'état actuel de la légende
  // getComputedStyle permet de récupérer le style réel appliqué même si le display est défini dans le CSS
  var displayed = window.getComputedStyle(legend).display;
  
// Afficher ou masquer la légende
  if(displayed === 'none'){
    legend.style.display = 'block';
  } else {
    legend.style.display = 'none';
  }
});

// Mise à jour dynamique de la légende depuis Shiny
// Ce handler est appelé depuis Shiny avec SendCustomMessage()
// Il reçoit du code HTML contenant la légende à afficher
Shiny.addCustomMessageHandler('update_legende', function(html) {

  // Récupère les éléments de légende
  var legend = document.getElementById('map_legend');

  // Si l'élement existe alors on remplace le contenu
  if(legend) legend.innerHTML = html;
});


