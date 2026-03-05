// ===== Clic sur PAT dans la sidebar =====
  document.addEventListener('click', function(e){
    if(e.target && e.target.classList.contains('pat-link')){
      e.preventDefault();
      var pat = e.target.getAttribute('data-pat');
      if(window.Shiny){
        Shiny.setInputValue('pat_selectionne', pat, {priority: 'event'});
      }
    }
  });