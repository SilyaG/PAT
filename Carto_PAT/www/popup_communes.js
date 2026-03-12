// Copyright (C) [2026] [Gréaume Paul, Guerboub Silya, Jouve Charlotte, Prima Oliver / Université Lumière Lyon 2] 
// Distribué sous licence CeCILL-2.1 — voir le fichier LICENSE pour les détails.



// Attend que le DOM soit entièrement chargé avant d'éxécuter le code
document.addEventListener('DOMContentLoaded', function() {
  // Vérifie toutes les 500 ms si l'élément de la carte (avec l'ID 'map' ) existe et est initialisé par Leaflet
    var checkMap = setInterval(function() {
      var mapEl = document.getElementById('map'); // Récupère l'élément HTML de la carte
      if (mapEl && mapEl._leaflet_id) { // Si l'élément existe et a un ID Leaflet (indiquant qu'il est prêt)
        var map = window.HTMLWidgets.getInstance(mapEl).getMap(); // Récupere l'instance de la carte Leaflet via HTMLWidgets
        clearInterval(checkMap); // Arrête la vérification périodique une fois la carte trouvée
  
        // Repositionne le popup ouvert sur le centroïde au moment du zoom
        map.on('zoomend moveend', function() { 
          map.eachLayer(function(layer) { // Parcours toutes les couches de la carte
            if (layer._popup && layer._popup.isOpen()) { // Si la couche a une méthode getLatlng 
              if (layer.getLatLng) {
                layer._popup.setLatLng(layer.getLatLng()); // Positionne le pop-up
              } else if (layer.getBounds) {
                layer._popup.setLatLng(layer.getBounds().getCenter());
              }
            }
          });
        });
  
        // Au clic, ancre sur le centroïde
        map.on('layeradd', function(e) {
          var layer = e.layer; // Récupère la couche ajoutée
          if (layer.getLatLng && layer._popup) { 
            layer.on('click', function() {
              layer._popup.setLatLng(layer.getLatLng());
            });
          }
          if (!layer.getLatLng && layer.getBounds && layer._popup) {
            layer.on('click', function() {
              layer._popup.setLatLng(layer.getBounds().getCenter());
            });
          }
        });
      }
    }, 500); // Intervalle de 500ms
  });