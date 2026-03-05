// Chacun des fichier JS correspond à une fonction bien défini compréhensible par le nom du fichier 
// Ici la fonction permet d'appliqué des hachures à la symbologie de la couche PAT 


// ===== Handler hachurage SVG pour PAT niveau 1 =====
// ce Handler est appelé depui Shiny avec SendCustomMessage()
// il reçoit un objet "data" contenant : {LayerID : couleur}

Shiny.addCustomMessageHandler('apply_hatch', function(data) {

  // Stocke les données pour pouvoir réappliquer après zoom
  window._lastHatchData = data;

  // nombre d'éléments attendu à styliser
  var expectedCount = Object.keys(data).length;

// Fonction qui applique les patterns SVG
  function applyPatterns(dataToApply) {

    // Recupère le widget leaflet de Shiny
    var widget = HTMLWidgets.find('#map');

    // si la carte n'existe pas encore la boucle s'arrete
    if (!widget || !widget.getMap()) return 0;

    var leafletMap = widget.getMap();
    
    var applied = 0; //compte les polygonnes à modifiés

    // Parcour toutes les couches de la carte 
    leafletMap.eachLayer(function(layer) {

      // recupère l'identifiant de la couche
      var lid = layer.options && layer.options.layerId;

      // ignore si pas d'id ou pas de correspondance dans les données
      if (!lid || !dataToApply[lid]) return;

      // couleur associé au PAT
      var color = dataToApply[lid];

      // vérifie que la couche possède un path SVG
      if (!layer._path) return;

      // Récupère l'élément SVG
      var svgEl = layer._path.ownerSVGElement;
      if (!svgEl) return;

      // Création d'un identifiant unique pour le pattern
      var patId = 'hatch-' + lid.replace(/[^a-zA-Z0-9]/g, '_') + '_' + Math.random().toString(36).substr(2, 6);

      // définition des patterns dans defs
      var defs = svgEl.querySelector('defs');
      if (!defs) {
        defs = document.createElementNS('http://www.w3.org/2000/svg', 'defs');
        svgEl.insertBefore(defs, svgEl.firstChild);
      }

      // Création du pattern de hachurage
      var pat = document.createElementNS('http://www.w3.org/2000/svg', 'pattern');
      pat.setAttribute('id', patId);
      pat.setAttribute('patternUnits', 'userSpaceOnUse');
      pat.setAttribute('width', '8');
      pat.setAttribute('height', '8');
      pat.setAttribute('patternTransform', 'rotate(45)'); //rotation pour mettre les hachures en diagonale

      // Création de la ligne de hachure
      var line = document.createElementNS('http://www.w3.org/2000/svg', 'line');
      line.setAttribute('x1', '0'); line.setAttribute('y1', '0');
      line.setAttribute('x2', '0'); line.setAttribute('y2', '8');
      line.setAttribute('stroke', color);
      line.setAttribute('stroke-width', '8');
      line.setAttribute('stroke-opacity', '0.5');
      pat.appendChild(line); // ajoute la ligne dans le pattern
      defs.appendChild(pat); // ajoute le patten dans le bloc defs

      // application du pattern sur les polygone
      layer._path.setAttribute('fill', 'url(#' + patId + ')');
      layer._path.setAttribute('fill-opacity', '1'); //opacité de remplissage
      applied++;
    });

    return applied;
  }

  // Boucle initiale pour attendre le rendu de couche
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

    // déclenche la boucle à chaque zoom
    leafletMap.on('zoomend', window._hatchZoomListener);
  }
});