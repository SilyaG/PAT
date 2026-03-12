// Copyright (C) [2026] [Gréaume Paul, Guerboub Silya, Jouve Charlotte, Prima Oliver / Université Lumière Lyon 2] 
// Distribué sous licence CeCILL-2.1 — voir le fichier LICENSE pour les détails.


// Recçoit l'objet data et remplis les popups avec les valeurs correspondante
Shiny.addCustomMessageHandler('show_pat_popup', function(data) {

  // On remplis les éléments DOM un à un
  document.getElementById('pat_popup_nom').innerText            = data.nom        || '';
  document.getElementById('pat_popup_niveau').innerText         = 'Niveau de labellisation : ' + (data.niveau || '');
  document.getElementById('pat_popup_annee').innerText          = 'Année de labellisation : '  + (data.annee  || '');
  document.getElementById('pat_popup_population').innerText     = (Number(data.population).toLocaleString('fr-FR') || '') + ' habitants';
  document.getElementById('pat_popup_pct_population').innerText = '(soit ' + (parseFloat(data.pct_population)||0).toFixed(1) + ' % de la population régionale)';
  document.getElementById('pat_popup_sau').innerText            = (Number(data.sau).toLocaleString('fr-FR') || '') + ' ha de SAU*';
  document.getElementById('pat_popup_pct_sau').innerText        = '(soit ' + (parseFloat(data.pct_sau)||0).toFixed(1)        + ' % de la SAU* régionale)';
  document.getElementById('pat_popup_bio').innerText            = (Number(data.bio).toLocaleString('fr-FR') || '') + ' ha de SAU* Bio';
  document.getElementById('pat_popup_pct_sau_bio').innerText    = '(soit ' + (parseFloat(data.pct_sau_bio)||0).toFixed(1)    + ' % de la SAU* Bio régionale)';
  document.getElementById('pat_popup_restau').innerText         = (Number(data.restau).toLocaleString('fr-FR') || '') + ' restaurants scolaires au 9 mars 2026';


  // Graphique comparatif PAT vs AURA
  // Calculs de barres horizontales ; on vérifie que les valeurs sont bien nulériques
  var partBio = parseFloat(String(data.partbio  || '0').replace(',', '.'));
  var bioAura = parseFloat(String(data.bio_aura || '0').replace(',', '.'));
  if (isNaN(partBio)) partBio = 0;
  if (isNaN(bioAura)) bioAura = 0;

  // Borne max arrondie aux multiples de 5
  var maxVal = Math.ceil(Math.max(partBio, bioAura) * 1.3 / 5) * 5 || 10;

  // Largeur en pourcentage des barres
  var wPat   = (partBio / maxVal * 100).toFixed(1);
  var wAura  = (bioAura / maxVal * 100).toFixed(1);

  // Couleur du PAT
  var colPat = partBio >= bioAura ? '#1f8d49' : '#e05c0a';

  // Composition du HTML
  var chartHTML =
    '<div style="margin:10px 0 4px 0;">' +
      '<div style="font-size:11px; font-weight:600; color:#3a3a3a; margin-bottom:8px;">Part de SAU* bio : PAT vs AuRA</div>' +
      '<div style="margin-bottom:6px;">' +
        '<div style="display:flex; justify-content:space-between; font-size:10px; color:#555; margin-bottom:2px;">' +
          '<span>Pourcentage de SAU* Bio sur le PAT</span>' +
          '<span style="font-weight:600; color:' + colPat + ';">' + partBio.toFixed(1) + ' %</span>' +
        '</div>' +
        '<div style="background:#f0f0f0; border-radius:3px; height:14px; width:100%; position:relative;">' +
          '<div style="width:' + wPat + '%; background:' + colPat + '; height:100%; border-radius:3px; min-width:2px;"></div>' +
        '</div>' +
      '</div>' +
      '<div style="margin-bottom:4px;">' +
        '<div style="display:flex; justify-content:space-between; font-size:10px; color:#555; margin-bottom:2px;">' +
          '<span>Pourcentage de SAU* Bio sur la Région AuRA</span>' +
          '<span style="font-weight:600; color:#1f8d49;">' + bioAura.toFixed(1) + ' %</span>' +
        '</div>' +
        '<div style="background:#f0f0f0; border-radius:3px; height:14px; width:100%; position:relative;">' +
          '<div style="width:' + wAura + '%; background:#1f8d49; opacity:0.5; height:100%; border-radius:3px; min-width:2px;"></div>' +
        '</div>' +
      '</div>' +
      '<div style="display:flex; justify-content:space-between; font-size:9px; color:#aaa; margin-top:2px;">' +
        '<span>0 %</span><span>' + maxVal + ' %</span>' +
      '</div>' +
    '</div>';

  document.getElementById('pat_popup_chart').innerHTML = chartHTML;
  
 // Contacts
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

// ── Reset position ────────────────────────────────────────────────────────
function resetPopupPosition() {
  var popup          = document.getElementById('pat_popup');
  popup.style.left   = '';
  popup.style.top    = '';
  popup.style.right  = '';
  popup.style.bottom = '';
}

// ── Helper : détecte si on est en plein écran ─────────────────────────────
function isFullscreen() {
  return !!(document.fullscreenElement || document.webkitFullscreenElement);
}

// ── Fermeture depuis R ────────────────────────────────────────────────────
Shiny.addCustomMessageHandler('hide_pat_popup', function(data) {
  document.getElementById('pat_popup').style.display = 'none';
  resetPopupPosition();
});

// ── Fermeture bouton + drag ───────────────────────────────────────────────
document.addEventListener('DOMContentLoaded', function() {

  var popup  = document.getElementById('pat_popup');
  var header = document.getElementById('pat_popup_header');

  // ── Fermeture ─────────────────────────────────────────────────────────
  document.getElementById('pat_popup_close').addEventListener('click', function() {
    popup.style.display = 'none';
    resetPopupPosition();
  });

  // ── Drag (désactivé en plein écran) ───────────────────────────────────
  var isDragging = false;
  var offsetX = 0, offsetY = 0;

  header.addEventListener('mousedown', function(e) {
    // Pas de drag en plein écran
    if (isFullscreen()) return;

    isDragging = true;
    var container     = popup.parentElement;
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
    var container     = popup.parentElement;
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

  // ── Plein écran : déplacer le popup dans/hors leaflet-container ────────
  function handleFullscreenChange() {
    var mapEl   = document.querySelector('.leaflet-container');
    var origine = mapEl ? mapEl.parentElement : null;
    if (!popup || !mapEl) return;

    if (isFullscreen()) {
      // Entrée → déplacer dans la carte + fixer la position
      resetPopupPosition();
      mapEl.appendChild(popup);
      // Position fixe en bas à droite dans la carte
      popup.style.right  = '10px';
      popup.style.bottom = '30px';
      popup.style.left   = 'auto';
      popup.style.top    = 'auto';
      header.style.cursor = 'default'; // curseur normal = pas de drag
    } else {
      // Sortie → remettre à sa place + restaurer le drag
      resetPopupPosition();
      if (origine) origine.appendChild(popup);
      header.style.cursor = 'grab';
    }
  }

  document.addEventListener('fullscreenchange',       handleFullscreenChange);
  document.addEventListener('webkitfullscreenchange', handleFullscreenChange);

});