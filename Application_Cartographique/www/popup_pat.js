Shiny.addCustomMessageHandler('show_pat_popup', function(data) {
  document.getElementById('pat_popup_nom').innerText        = data.nom        || '';
  document.getElementById('pat_popup_niveau').innerText     = 'Niveau de labellisation : ' + (data.niveau || '');
  document.getElementById('pat_popup_annee').innerText      = 'Année de labellisation : ' + (data.annee || '');
  document.getElementById('pat_popup_population').innerText = (Number(data.population).toLocaleString('fr-FR') || '') + ' habitants';
  document.getElementById('pat_popup_pct_population').innerText = '(soit ' + (Number(data.pct_population).toLocaleString('fr-FR') || '') + ' % de la population régionale)';
  document.getElementById('pat_popup_sau').innerText        = (Number(data.sau).toLocaleString('fr-FR') || '') + ' ha';
  document.getElementById('pat_popup_pct_sau').innerText = '(soit ' + (Number(data.pct_sau).toLocaleString('fr-FR') || '') + ' % de la SAU régionale)';
  document.getElementById('pat_popup_bio').innerText        = (Number(data.bio).toLocaleString('fr-FR') || '') + ' ha';
  document.getElementById('pat_popup_pct_sau_bio').innerText = '(soit ' + (Number(data.pct_sau_bio).toLocaleString('fr-FR') || '') + ' % de la SAU Bio régionale)';
  document.getElementById('pat_popup_partbio').innerText    = (data.partbio    || '') + ' %';
  document.getElementById('pat_popup_pct_bio_aura').innerText = '(' + (Number(data.bio_aura).toLocaleString('fr-FR') || '') + ' % de SAU Bio dans la région AuRA)';

  var contactsEl = document.getElementById('pat_popup_contacts');
  contactsEl.innerHTML = '';
  if (data.contacts && data.contacts.length > 0) {
    data.contacts.forEach(function(email) {
      var a = document.createElement('a');
      a.href = 'mailto:' + email;
      a.innerText = email;
      contactsEl.appendChild(a);
    });
  }
  document.getElementById('pat_popup').style.display = 'block';
});

function resetPopupPosition() {
  var popup = document.getElementById('pat_popup');
  popup.style.left   = '';
  popup.style.top    = '';
  popup.style.right  = '';
  popup.style.bottom = '';
}

// Fermeture depuis R
// Fermeture depuis R (désélection, filtres, clic hors PAT)
Shiny.addCustomMessageHandler('hide_pat_popup', function(data) {
  document.getElementById('pat_popup').style.display = 'none';
  resetPopupPosition();
});

// Fermeture via le bouton "Fermer ×"
document.addEventListener('DOMContentLoaded', function() {
  document.getElementById('pat_popup_close').addEventListener('click', function() {
    document.getElementById('pat_popup').style.display = 'none';
    resetPopupPosition();
  });

  var popup     = document.getElementById('pat_popup');
  var header    = document.getElementById('pat_popup_header');
  var container = popup.parentElement; // le div position:relative de la carte

  var isDragging = false;
  var offsetX = 0;
  var offsetY = 0;

  header.addEventListener('mousedown', function(e) {
    isDragging = true;
  
    var popupRect     = popup.getBoundingClientRect();
    var containerRect = container.getBoundingClientRect();
  
    // Force left/top en valeurs absolues dès le premier drag
    // pour neutraliser le positionnement bottom/right initial du CSS
    popup.style.right  = 'auto';
    popup.style.bottom = 'auto';
    popup.style.left   = (popupRect.left - containerRect.left) + 'px';
    popup.style.top    = (popupRect.top  - containerRect.top)  + 'px';
  
    // Offset du curseur par rapport au coin haut-gauche du popup
    offsetX = e.clientX - popupRect.left;
    offsetY = e.clientY - popupRect.top;
  
    popup.style.cursor = 'grabbing';
    e.preventDefault();
  });

  document.addEventListener('mousemove', function(e) {
    if (!isDragging) return;

    var containerRect = container.getBoundingClientRect();

    // Position souhaitée du popup (coin haut-gauche)
    var newLeft = e.clientX - offsetX - containerRect.left;
    var newTop  = e.clientY - offsetY - containerRect.top;

    // Limites pour rester dans le conteneur
    var maxLeft = containerRect.width  - popup.offsetWidth;
    var maxTop  = containerRect.height - popup.offsetHeight;

    newLeft = Math.max(0, Math.min(newLeft, maxLeft));
    newTop  = Math.max(0, Math.min(newTop,  maxTop));

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