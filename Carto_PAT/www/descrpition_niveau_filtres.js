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
  
    function attachTooltip(selectId) {
      var select = document.getElementById(selectId);
      if (!select) return;
  
      // On remplace le <select> par un bloc custom pour pouvoir hover les options
      var wrapper = document.createElement('div');
      wrapper.style.cssText = 'position:relative; width:100%;';
      select.parentNode.insertBefore(wrapper, select);
      wrapper.appendChild(select);
  
      // Panneau custom visible au focus/click
      var panel = document.createElement('div');
      panel.style.cssText = `
        display: none;
        position: absolute;
        top: 100%;
        left: 0;
        width: 100%;
        background: #fff;
        border: 1px solid #6a6af4;
        border-radius: 4px;
        z-index: 10000;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
        font-family: Marianne, Arial, sans-serif;
        font-size: 14px;
      `;
      wrapper.appendChild(panel);
  
      function buildPanel() {
        panel.innerHTML = '';
        Array.from(select.options).forEach(function(opt) {
          if (opt.disabled) return;
          var item = document.createElement('div');
          item.style.cssText = `
            padding: 8px 12px;
            cursor: pointer;
            color: #1e1e1e;
            border-left: 3px solid transparent;
            transition: background 0.1s;
          `;
          item.textContent = opt.text;
          item.dataset.value = opt.value;
  
          item.addEventListener('mouseenter', function(e) {
            item.style.background = '#f0f0fb';
            item.style.borderLeft = '3px solid #6a6af4';
  
            if (descriptions[opt.value]) {
              tooltip.innerHTML =
                '<span style="font-weight:700; display:block; margin-bottom:4px; color:#8eb8f7;">' +
                opt.text + '</span>' +
                descriptions[opt.value];
              tooltip.style.display = 'block';
            }
          });
  
          item.addEventListener('mousemove', function(e) {
            tooltip.style.left = (e.clientX + 16) + 'px';
            tooltip.style.top  = (e.clientY - 10) + 'px';
          });
  
          item.addEventListener('mouseleave', function() {
            item.style.background = '';
            item.style.borderLeft = '3px solid transparent';
            tooltip.style.display = 'none';
          });
  
          item.addEventListener('mousedown', function(e) {
            e.preventDefault();
            select.value = opt.value;
            select.dispatchEvent(new Event('change', { bubbles: true }));
            panel.style.display = 'none';
            tooltip.style.display = 'none';
          });
  
          panel.appendChild(item);
        });
      }
  
      select.addEventListener('mousedown', function(e) {
        e.preventDefault();
        buildPanel();
        panel.style.display = panel.style.display === 'none' ? 'block' : 'none';
      });
  
      document.addEventListener('mousedown', function(e) {
        if (!wrapper.contains(e.target)) {
          panel.style.display = 'none';
          tooltip.style.display = 'none';
        }
      });
    }
  
    attachTooltip('filtre_niveau');
  });