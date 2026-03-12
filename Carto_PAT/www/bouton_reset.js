// Copyright (C) [2026] [Gréaume Paul, Guerboub Silya, Jouve Charlotte, Prima Oliver / Université Lumière Lyon 2] 
// Distribué sous licence CeCILL-2.1 — voir le fichier LICENSE pour les détails.


// Chacun des fichiers JS correspond à une fonction bien défini compréhensible par le nom du fichier
// Ici il s'agit de la fonction qui reset la carte et les filtres aux valeurs par défaut

// Ce script écoute le clic sur le bouton reset et envoie un message à Shiny pour déclencher la reinitialisation des filtres et de la carte 

document.addEventListener('DOMContentLoaded', function() {
  var btn = document.getElementById("reset_button");
  if (btn) {
    btn.addEventListener("click", function() {
      Shiny.setInputValue("reset_button", Math.random(), {priority: "event"});
    });
  }
});

// Ce message handler reçoiy le message shiny pour reinitialiser les filtres et lz carte 
Shiny.addCustomMessageHandler("reset_filtres", function(msg) {

  // ── Filtres niveaux ──────────────────────────────────────────────────
  var filtreNiveau = document.getElementById("filtre_niveau"); // Filtres de niveau
  if (filtreNiveau) {
    filtreNiveau.value = ""; // Vider la valeur du filtre
    filtreNiveau.dispatchEvent(new Event("change")); // Déclencher l'évenement met à jour l'interface
  }

// Reinitialiser le filtre de niveau territorial
  var filtreNiveauTerri = document.getElementById("filtre_niveau_terri"); 
  if (filtreNiveauTerri) {
    filtreNiveauTerri.value = ""; // Vider la valeur de filtre
    filtreNiveauTerri.dispatchEvent(new Event("change")); // Déclencher l'évenement met à jour l'interface
  }

  // ── Barre de recherche ───────────────────────────────────────────────
  var searchInput = document.getElementById("nom_du_pat");
  if (searchInput) searchInput.value = "";

  // ── Fond cartographique → Plan IGN ───────────────────────────────────
  // Les radioButtons Shiny ont leurs inputs dans #fond div avec value="ign"
  var fondRadios = document.querySelectorAll("input[type='radio'][name='fond']");
  fondRadios.forEach(function(radio) {
    if (radio.value === "ign") {
      radio.checked = true;
      radio.dispatchEvent(new Event("change", { bubbles: true }));
    } else {
      radio.checked = false;
    }
  });

  // ── Checkbox PAT → coché ─────────────────────────────────────────────
  var patLayer = document.getElementById("pat_layer");
  if (patLayer && !patLayer.checked) {
    patLayer.checked = true;
    patLayer.dispatchEvent(new Event("change", { bubbles: true }));
  }

  // ── Checkbox CLS → décoché ───────────────────────────────────────────
  var clsLayer = document.getElementById("cls_layer");
  if (clsLayer && clsLayer.checked) {
    clsLayer.checked = false;
    clsLayer.dispatchEvent(new Event("change", { bubbles: true }));
  }

  // ── Checkbox Départements → décoché ─────────────────────────────────
  var depLayer = document.getElementById("dep_layer");
  if (depLayer && depLayer.checked) {
    depLayer.checked = false;
    depLayer.dispatchEvent(new Event("change", { bubbles: true }));
  }

});