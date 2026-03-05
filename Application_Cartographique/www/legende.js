// ===== TOGGLE LÉGENDE =====

document.addEventListener('click', function(e){
  var btn = e.target.closest('#legend_toggle');
  if(!btn) return;
  e.preventDefault();

  var legend = document.getElementById('map_legend');
  if(!legend) return;

  // getComputedStyle capte le display réel (même défini en CSS)
  var displayed = window.getComputedStyle(legend).display;
  
  if(displayed === 'none'){
    legend.style.display = 'block';
  } else {
    legend.style.display = 'none';
  }
});

// ===== Mise à jour dynamique de la légende =====
Shiny.addCustomMessageHandler('update_legende', function(html) {
var legend = document.getElementById('map_legend');
if(legend) legend.innerHTML = html;
});