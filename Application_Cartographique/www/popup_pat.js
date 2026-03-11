// Copyright (C) [2026] [Gréaume Paul, Guerboub Silya, Jouve Charlotte, Prima Oliver / Université Lumière Lyon 2] 
// Distribué sous licence CeCILL-2.1 — voir le fichier LICENSE pour les détails.




// gestionnaire envoyé depuis Shiny/R :
// reçoit un objet `data` et remplit le popup avec les valeurs correspondantes
Shiny.addCustomMessageHandler('show_pat_popup', function(data) {

  // --- helpers pour ne pas reproduire les mêmes conversions plusieurs fois ---
  function fmtNum(val) {
    // convertit en nombre et formate en français, retourne chaîne vide si falsy
    return (Number(val).toLocaleString('fr-FR') || '');
  }
  function fmtPct(val) {
    // parseFloat → 0 par défaut → une décimale
    return (parseFloat(val) || 0).toFixed(1);
  }

  // ── Champs texte ──────────────────────────────────────────────────────────
  // on remplit les éléments DOM un à un ; les helpers permettent de simplifier
  document.getElementById('pat_popup_nom').innerText            = data.nom        || '';
  document.getElementById('pat_popup_niveau').innerText         = 'Niveau de labellisation : ' + (data.niveau || '');
  document.getElementById('pat_popup_annee').innerText          = 'Année de labellisation : '  + (data.annee  || '');
  document.getElementById('pat_popup_population').innerText     = fmtNum(data.population) + ' habitants';
  document.getElementById('pat_popup_pct_population').innerText = '(soit ' + fmtPct(data.pct_population) + ' % de la population régionale)';
  document.getElementById('pat_popup_sau').innerText            = fmtNum(data.sau) + ' ha de SAU*';
  document.getElementById('pat_popup_pct_sau').innerText        = '(soit ' + fmtPct(data.pct_sau) + ' % de la SAU régionale)';
  document.getElementById('pat_popup_bio').innerText            = fmtNum(data.bio) + ' ha de SAU Bio';
  document.getElementById('pat_popup_pct_sau_bio').innerText    = '(soit ' + fmtPct(data.pct_sau_bio) + ' % de la SAU Bio régionale)';
  document.getElementById('pat_popup_restau').innerText         = fmtNum(data.restau) + ' restaurants scolaires au 9 mars 2026';

  // ── Graphique comparatif PAT vs AuRA ─────────────────────────────────────
  // calculs de barres horizontales ; on vérifie que les valeurs sont bien numériques
  var partBio = parseFloat(String(data.partbio  || '0').replace(',', '.'));
  var bioAura = parseFloat(String(data.bio_aura || '0').replace(',', '.'));
  if (isNaN(partBio)) partBio = 0;
  if (isNaN(bioAura)) bioAura = 0;

  // borne max arrondie au multiple de 5 supérieur, minimum 10
  var maxVal = Math.ceil(Math.max(partBio, bioAura) * 1.3 / 5) * 5 || 10;

  // largeur en pourcentage des barres
  var wPat  = (partBio / maxVal * 100).toFixed(1);
  var wAura = (bioAura / maxVal * 100).toFixed(1);

  // couleur du PAT : vert si supérieur ou égal à AuRA, orange sinon
  var colPat = partBio >= bioAura ? '#1f8d49' : '#e05c0a';

  // composition du HTML du graphique (plutôt verbeux, mais unique)
  var chartHTML =
    '<div style="margin:10px 0 4px 0;">' +
      '<div style="font-size:11px; font-weight:600; color:#3a3a3a; margin-bottom:8px;">' +
        'Part de SAU bio : PAT vs AuRA' +
      '</div>' +

      // Ligne PAT
      '<div style="margin-bottom:6px;">' +
        '<div style="display:flex; justify-content:space-between; font-size:10px; color:#555; margin-bottom:2px;">' +
          '<span>Pourcentage de SAU Bio sur le PAT</span>' +
          '<span style="font-weight:600; color:' + colPat + ';">' + partBio.toFixed(1) + ' %</span>' +
        '</div>' +
        '<div style="background:#f0f0f0; border-radius:3px; height:14px; width:100%; position:relative;">' +
          '<div style="width:' + wPat + '%; background:' + colPat + '; height:100%; border-radius:3px; min-width:2px;"></div>' +
        '</div>' +
      '</div>' +

      // Ligne AuRA
      '<div style="margin-bottom:4px;">' +
        '<div style="display:flex; justify-content:space-between; font-size:10px; color:#555; margin-bottom:2px;">' +
          '<span>Pourcentage de SAU Bio sur la Région AuRA</span>' +
          '<span style="font-weight:600; color:#1f8d49;">' + bioAura.toFixed(1) + ' %</span>' +
        '</div>' +
        '<div style="background:#f0f0f0; border-radius:3px; height:14px; width:100%; position:relative;">' +
          '<div style="width:' + wAura + '%; background:#1f8d49; opacity:0.5; height:100%; border-radius:3px; min-width:2px;"></div>' +
        '</div>' +
      '</div>' +

      // Axe 0 → max
      '<div style="display:flex; justify-content:space-between; font-size:9px; color:#aaa; margin-top:2px;">' +
        '<span>0 %</span>' +
        '<span>' + maxVal + ' %</span>' +
      '</div>' +

    '</div>';

  document.getElementById('pat_popup_chart').innerHTML = chartHTML;

  // ── Contacts ──────────────────────────────────────────────────────────────
  var contactsEl = document.getElementById('pat_popup_contacts');
  contactsEl.innerHTML = '';
  if (data.contacts && data.contacts.length > 0) {
    data.contacts.forEach(function(email) {
      var a       = document.createElement('a');
      a.href      = 'mailto:' + email;
      a.innerText = email;
      contactsEl.appendChild(a);
    });
  }

  document.getElementById('pat_popup').style.display = 'block';
});

// ── Fonctions auxiliaires (réutilisées ailleurs) ──────────────────────────
function resetPopupPosition() {
  var popup      = document.getElementById('pat_popup');
  popup.style.left   = '';
  popup.style.top    = '';
  popup.style.right  = '';
  popup.style.bottom = '';
}

// fermeture déclenchée côté R
Shiny.addCustomMessageHandler('hide_pat_popup', function(data) {
  document.getElementById('pat_popup').style.display = 'none';
  resetPopupPosition();
});

// gestion du bouton de fermeture et du drag & drop du popup
document.addEventListener('DOMContentLoaded', function() {
  document.getElementById('pat_popup_close').addEventListener('click', function() {
    document.getElementById('pat_popup').style.display = 'none';
    resetPopupPosition();
  });

  var popup     = document.getElementById('pat_popup');
  var header    = document.getElementById('pat_popup_header');
  var container = popup.parentElement;

  var isDragging = false;
  var offsetX = 0, offsetY = 0;

  header.addEventListener('mousedown', function(e) {
    isDragging = true;
    var popupRect     = popup.getBoundingClientRect();
    var containerRect = container.getBoundingClientRect();
    popup.style.right  = 'auto';
    popup.style.bottom = 'auto';
    popup.style.left   = (popupRect.left - containerRect.left) + 'px';
    popup.style.top    = (popupRect.top  - containerRect.top)  + 'px';
    offsetX = e.clientX - popupRect.left;
    offsetY = e.clientY - popupRect.top;
    popup.style.cursor = 'grabbing';
    e.preventDefault();
  });

  document.addEventListener('mousemove', function(e) {
    if (!isDragging) return;
    var containerRect = container.getBoundingClientRect();
    var newLeft = e.clientX - offsetX - containerRect.left;
    var newTop  = e.clientY - offsetY - containerRect.top;
    newLeft = Math.max(0, Math.min(newLeft, containerRect.width  - popup.offsetWidth));
    newTop  = Math.max(0, Math.min(newTop,  containerRect.height - popup.offsetHeight));
    popup.style.left = newLeft + 'px';
    popup.style.top  = newTop  + 'px';
  });

  document.addEventListener('mouseup', function() {
    if (isDragging) {
      isDragging = false;
      popup.style.cursor = 'default';
    }
  });

});