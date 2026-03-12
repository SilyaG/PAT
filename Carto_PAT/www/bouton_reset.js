document.addEventListener('DOMContentLoaded', function() {
  var btn = document.getElementById("reset_button");
  if (btn) {
    btn.addEventListener("click", function() {
      Shiny.setInputValue("reset_button", Math.random(), {priority: "event"});
    });
  }
});

Shiny.addCustomMessageHandler("reset_filtres", function(msg) {

  // ── Filtres niveaux ──────────────────────────────────────────────────
  var filtreNiveau = document.getElementById("filtre_niveau");
  if (filtreNiveau) {
    filtreNiveau.value = "";
    filtreNiveau.dispatchEvent(new Event("change"));
  }

  var filtreNiveauTerri = document.getElementById("filtre_niveau_terri");
  if (filtreNiveauTerri) {
    filtreNiveauTerri.value = "";
    filtreNiveauTerri.dispatchEvent(new Event("change"));
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