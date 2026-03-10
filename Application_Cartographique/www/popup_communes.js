document.addEventListener('DOMContentLoaded', function() {
    var checkMap = setInterval(function() {
      var mapEl = document.getElementById('map');
      if (mapEl && mapEl._leaflet_id) {
        var map = window.HTMLWidgets.getInstance(mapEl).getMap();
        clearInterval(checkMap);
  
        // Repositionne le popup ouvert sur le centroïde au moment du zoom
        map.on('zoomend moveend', function() {
          map.eachLayer(function(layer) {
            if (layer._popup && layer._popup.isOpen()) {
              if (layer.getLatLng) {
                layer._popup.setLatLng(layer.getLatLng());
              } else if (layer.getBounds) {
                layer._popup.setLatLng(layer.getBounds().getCenter());
              }
            }
          });
        });
  
        // Au clic, ancre sur le centroïde
        map.on('layeradd', function(e) {
          var layer = e.layer;
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
    }, 500);
  });