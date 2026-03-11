// Copyright (C) [2026] [Gréaume Paul, Guerboub Silya, Jouve Charlotte, Prima Oliver / Université Lumière Lyon 2] 
// Distribué sous licence CeCILL-2.1 — voir le fichier LICENSE pour les détails.


// Chacun des fichier JS correspond à une fonction bien défini compréhensible par le nom du fichier
// Ici il s'agit de la fonction qui reset la carte et les filtres aux valeurs par défaut

// Ce script écoute le clic sur le bouton de reset et envoie un message à Shiny pour déclencher la réinitialisation des filtres et de la carte.
document.addEventListener('DOMContentLoaded', function() {
  var btn = document.getElementById("reset_button"); // Bouton de reset
  if (btn) {
    btn.addEventListener("click", function() {
      Shiny.setInputValue("reset_button", Math.random(), {priority: "event"});
    });
  }
});

// Ce message handler reçoit le message de Shiny pour réinitialiser les filtres et la carte
Shiny.addCustomMessageHandler("reset_filtres", function(msg) { 
  // Réinitialiser le filtre de niveau
  var filtreNiveau = document.getElementById("filtre_niveau"); // Filtre de niveau
  if (filtreNiveau) {
    filtreNiveau.value = ""; // Vider la valeur du filtre
    filtreNiveau.dispatchEvent(new Event("change")); // Déclencher l'événement change pour mettre à jour l'interface
  }

  // Réinitialiser le filtre de niveau territorial
  var filtreNiveauTerri = document.getElementById("filtre_niveau_terri");
  if (filtreNiveauTerri) {
    filtreNiveauTerri.value = ""; // Vider la valeur du filtre
    filtreNiveauTerri.dispatchEvent(new Event("change")); // Déclencher l'événement change pour mettre à jour l'interface
  }

  // Réinitialiser le champ de recherche
  var searchInput = document.getElementById("nom_du_pat");
  if (searchInput) {
    searchInput.value = ""; // Vider le champ de recherche
  }
});