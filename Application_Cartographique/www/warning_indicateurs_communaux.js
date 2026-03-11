// Copyright (C) [2026] [Gréaume Paul, Guerboub Silya, Jouve Charlotte, Prima Oliver / Université Lumière Lyon 2] 
// Distribué sous licence CeCILL-2.1 — voir le fichier LICENSE pour les détails.



// Ce script gère l’affichage d’une petite notification « toast »
// dans une application Shiny lorsqu’un message personnalisé est reçu.


Shiny.addCustomMessageHandler("show_toast_indicateurs", function(msg) {
    // éviter de créer deux fois le même toast si le handler est déclenché
    if (document.getElementById("toast-indicateurs")) return;

    // création de l’élément DOM
    var toast = document.createElement("div");
    toast.id = "toast-indicateurs";

    // contenu HTML et icône
    toast.innerHTML = "<i class='ri-bar-chart-line' style='margin-right:6px;'></i>Indicateurs communaux disponibles ↖";

    // styles en ligne : position fixe en bas à gauche, design
    toast.style.cssText = `
    position: fixed;
    top: 700px;        /* hauteur fixe ; pourrait être calculée selon la fenêtre */
    left: 15px;
    background: #000091;
    color: white;
    font-family: Marianne, Arial, sans-serif;
    font-size: 13px;
    padding: 10px 16px;
    border-radius: 4px;
    box-shadow: 0 4px 12px rgba(0,0,0,0.2);
    z-index: 9999;
    opacity: 0;                 /* départ invisible */
    transform: translateY(-10px);   /* ← inversion de la direction */
    transition: opacity 0.3s ease, transform 0.3s ease;
  `;
    document.body.appendChild(toast);

    // apparition animée en deux frames successives
    // (double requestAnimationFrame pour forcer le recalcul du style)
    requestAnimationFrame(function() {
      requestAnimationFrame(function() {
        toast.style.opacity = "1";
        toast.style.transform = "translateY(0)";
      });
    });

    // disparition automatique après 3 secondes
    setTimeout(function() {
      toast.style.opacity = "0";
      toast.style.transform = "translateY(-30px)";
      setTimeout(function() { toast.remove(); }, 400);
    }, 3000);
});