// ===== Handler hachurage SVG pour PAT niveau 1 =====
Shiny.addCustomMessageHandler('apply_hatch', function(data) {

  // Stocke les données pour pouvoir réappliquer après zoom
  window._lastHatchData = data;
  var expectedCount = Object.keys(data).length;

  function applyPatterns(dataToApply) {
    var widget = HTMLWidgets.find('#map');
    if (!widget || !widget.getMap()) return 0;
    var leafletMap = widget.getMap();
    var applied = 0;

    leafletMap.eachLayer(function(layer) {
      var lid = layer.options && layer.options.layerId;
      if (!lid || !dataToApply[lid]) return;
      var color = dataToApply[lid];
      if (!layer._path) return;
      var svgEl = layer._path.ownerSVGElement;
      if (!svgEl) return;

      var patId = 'hatch-' + lid.replace(/[^a-zA-Z0-9]/g, '_') + '_' + Math.random().toString(36).substr(2, 6);

      var defs = svgEl.querySelector('defs');
      if (!defs) {
        defs = document.createElementNS('http://www.w3.org/2000/svg', 'defs');
        svgEl.insertBefore(defs, svgEl.firstChild);
      }

      var pat = document.createElementNS('http://www.w3.org/2000/svg', 'pattern');
      pat.setAttribute('id', patId);
      pat.setAttribute('patternUnits', 'userSpaceOnUse');
      pat.setAttribute('width', '8');
      pat.setAttribute('height', '8');
      pat.setAttribute('patternTransform', 'rotate(45)');

      var line = document.createElementNS('http://www.w3.org/2000/svg', 'line');
      line.setAttribute('x1', '0'); line.setAttribute('y1', '0');
      line.setAttribute('x2', '0'); line.setAttribute('y2', '8');
      line.setAttribute('stroke', color);
      line.setAttribute('stroke-width', '8');
      line.setAttribute('stroke-opacity', '0.5');
      pat.appendChild(line);
      defs.appendChild(pat);

      layer._path.setAttribute('fill', 'url(#' + patId + ')');
      layer._path.setAttribute('fill-opacity', '1');
      applied++;
    });

    return applied;
  }

  // Boucle initiale
  var attempts = 0;
  var maxAttempts = 30;

  function tryLoop() {
    attempts++;
    var n = applyPatterns(data);
    if (n >= expectedCount) return;
    if (attempts >= maxAttempts) return;
    setTimeout(tryLoop, 200);
  }

  tryLoop();

  // Réapplique après chaque fin de zoom (flyToBounds réinitialise les _path SVG)
  var widget = HTMLWidgets.find('#map');
  if (widget && widget.getMap()) {
    var leafletMap = widget.getMap();

    // Supprime l'ancien listener pour éviter les doublons
    if (window._hatchZoomListener) {
      leafletMap.off('zoomend', window._hatchZoomListener);
    }

    window._hatchZoomListener = function() {
      // Courte attente pour que Leaflet finisse de re-rendre les paths SVG
      setTimeout(function() {
        if (window._lastHatchData) {
          applyPatterns(window._lastHatchData);
        }
      }, 100);
    };

    leafletMap.on('zoomend', window._hatchZoomListener);
  }
});