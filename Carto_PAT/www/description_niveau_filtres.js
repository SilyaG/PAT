// ============================================================
// Tooltip DSFR sur les options du filtre niveau de labellisation
// ============================================================

document.addEventListener('DOMContentLoaded', function () {

  // Création du tooltip
  var tooltip = document.createElement('div');
  tooltip.id = 'tooltip_niveau';
  tooltip.style.cssText = `
    display: none;
    position: fixed;
    z-index: 9999;
    background: #1e1e1e;
    color: #fff;
    font-family: Marianne, Arial, sans-serif;
    font-size: 12px;
    line-height: 1.5;
    padding: 8px 12px;
    border-radius: 4px;
    max-width: 320px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.3);
    pointer-events: none;
  `;
  document.body.appendChild(tooltip);

  var descriptions = {
    "1": "Niveau 1 — Engagement structuré : le PAT dispose d'un diagnostic territorial et d'un plan d'action validé par le Ministère de l'Agriculture.",
    "2": "Niveau 2 — Reconnaissance avancée : le PAT dispose d'une gouvernance consolidée, d'un suivi-évaluation et d'actions opérationnelles engagées."
  };

  var select = document.getElementById('filtre_niveau');
  if (!select) return;

  select.addEventListener('mousemove', function (e) {
    // Lecture de l'option survolée via les coordonnées
    var val = select.value;

    // Détection via pointeur sur les options (approximation hover)
    var opts = select.options;
    for (var i = 0; i < opts.length; i++) {
      if (opts[i].value === "1" || opts[i].value === "2") {
        // On lit la valeur highlighted via focus
      }
    }
  });

  // Meilleure approche : écoute sur le focus clavier + mouseenter sur le select ouvert
  select.addEventListener('change', function () {
    tooltip.style.display = 'none';
  });

  // Injection d'un listener sur chaque option via un MutationObserver
  // car les <option> ne supportent pas nativement mouseenter
  // → on triche en lisant la valeur au mousemove sur le select

  select.addEventListener('mousemove', function (e) {
    var rect  = select.getBoundingClientRect();
    var optH  = rect.height / select.size || 24;
    var relY  = e.clientY - rect.top;
    var idx   = Math.floor(relY / optH);
    var opt   = select.options[idx];

    if (opt && descriptions[opt.value]) {
      tooltip.innerHTML =
        '<span style="font-weight:700; display:block; margin-bottom:4px; color:#8eb8f7;">' +
        opt.text + '</span>' +
        descriptions[opt.value];
      tooltip.style.display = 'block';
      tooltip.style.left = (e.clientX + 16) + 'px';
      tooltip.style.top  = (e.clientY - 10) + 'px';
    } else {
      tooltip.style.display = 'none';
    }
  });

  select.addEventListener('mouseleave', function () {
    tooltip.style.display = 'none';
  });
});