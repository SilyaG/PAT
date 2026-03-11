// Copyright (C) [2026] [Gréaume Paul, Guerboub Silya, Jouve Charlotte, Prima Oliver / Université Lumière Lyon 2] 
// Distribué sous licence CeCILL-2.1 — voir le fichier LICENSE pour les détails.


// Attend que le DOM soit entièrement chargé avant d'exécuter le code
document.addEventListener('DOMContentLoaded', function() {
    // Vérifie toutes les 500 ms si l'élément de la carte (avec l'ID 'map') existe et est initialisé par Leaflet
    var checkMap = setInterval(function() {
      var mapEl = document.getElementById('map');  // Récupère l'élément HTML de la carte
      if (mapEl && mapEl._leaflet_id) {  // Si l'élément existe et a un ID Leaflet (indiquant qu'il est prêt)
        var map = window.HTMLWidgets.getInstance(mapEl).getMap();  // Récupère l'instance de la carte Leaflet via HTMLWidgets
        clearInterval(checkMap);  // Arrête la vérification périodique une fois la carte trouvée
  
        // Événement déclenché à la fin d'un zoom ou d'un déplacement de la carte
        // Repositionne les popups ouverts sur le centroïde (centre) de leur couche associée
        map.on('zoomend moveend', function() {
          map.eachLayer(function(layer) {  // Parcourt toutes les couches de la carte
            if (layer._popup && layer._popup.isOpen()) {  // Si la couche a un popup ouvert
              if (layer.getLatLng) {  // Si la couche a une méthode getLatLng (ex. : marqueur)
                layer._popup.setLatLng(layer.getLatLng());  // Positionne le popup sur les coordonnées du marqueur
              } else if (layer.getBounds) {  // Sinon, si la couche a des limites (ex. : polygone)
                layer._popup.setLatLng(layer.getBounds().getCenter());  // Positionne le popup sur le centre des limites
              }
            }
          });
        });
  
        // Événement déclenché lorsqu'une nouvelle couche est ajoutée à la carte
        // Ajoute un gestionnaire de clic pour ancrer le popup sur le centroïde de la couche
        map.on('layeradd', function(e) {
          var layer = e.layer;  // Récupère la couche ajoutée
          if (layer.getLatLng && layer._popup) {  // Si la couche a getLatLng et un popup (ex. : marqueur)
            layer.on('click', function() {  // Au clic sur la couche
              layer._popup.setLatLng(layer.getLatLng());  // Ancre le popup sur les coordonnées du marqueur
            });
          }
          if (!layer.getLatLng && layer.getBounds && layer._popup) {  // Si la couche n'a pas getLatLng mais getBounds et un popup (ex. : polygone)
            layer.on('click', function() {  // Au clic sur la couche
              layer._popup.setLatLng(layer.getBounds().getCenter());  // Ancre le popup sur le centre des limites
            });
          }
        });
      }
    }, 500);  // Intervalle de 500 ms
  });