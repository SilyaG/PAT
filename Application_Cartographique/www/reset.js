document.addEventListener('DOMContentLoaded', function() {
  var btn = document.getElementById("reset_button");
  if (btn) {
    btn.addEventListener("click", function() {
      Shiny.setInputValue("reset_button", Math.random(), {priority: "event"});
    });
  }
});

Shiny.addCustomMessageHandler("reset_filtres", function(msg) {
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

  var searchInput = document.getElementById("nom_du_pat");
  if (searchInput) {
    searchInput.value = "";
  }
});