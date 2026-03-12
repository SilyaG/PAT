# Web-cartographie des Projets Alimentaires Territoriaux
### Région Auvergne-Rhône-Alpes

![Licence CeCILL-2.1](https://img.shields.io/badge/Licence-CeCILL--2.1-blue)
![Statut](https://img.shields.io/badge/Statut-Terminé-brightgreen)
![R Shiny](https://img.shields.io/badge/R-Shiny-276DC3?logo=r)
![Leaflet](https://img.shields.io/badge/Leaflet.js-199900?logo=leaflet)

---

## Présentation

Cette application de web-cartographie permet de visualiser et d'explorer les **Projets Alimentaires Territoriaux (PAT)** de la région Auvergne-Rhône-Alpes.

Elle a été développée dans le cadre d'une commande de la **DRAAF Auvergne-Rhône-Alpes** (Direction Régionale de l'Alimentation, de l'Agriculture et de la Forêt), par des étudiants de deuxième année des Masters **GéoNum** et **SENTINELLES** des Universités Lyon 2 et Lyon 3.

---

## Fonctionnalités

- Visualisation cartographique des PAT sur fond IGN, OpenStreetMap ou Registre Parcellaire Graphique
- Recherche par PAT ou commune
- Filtres par niveau de labellisation et/ou échelle territoriale
- Liste dynamique des PAT visibles dans la vue courante
- Pop-ups détaillés par entité
- Tutoriel interactif intégré

---

## Technologies utilisées

| Technologie | Rôle |
|---|---|
| R Shiny | Interface et serveur applicatif |
| Leaflet.js | Rendu cartographique interactif |
| JavaScript / HTML / CSS | Composants front-end personnalisés |

---

## Structure du projet
```
├── Carto_PAT/
│   ├── app.R                  # Point d'entrée de l'application Shiny
│   ├── data/                  # Données géographiques et script d'automatisation
│   └── www/                   # Fichiers front-end (JS, CSS, HTML)
└── Rapport_Application_Cartographique_PAT.pdf
```

---

## Installation et lancement
```r
# Installer les dépendances R nécessaires
install.packages(c("shiny", "leaflet", "sf", ...))

# Lancer l'application
shiny::runApp("Carto_PAT/")
```

---

## Documentation

| Ressource | Description |
|---|---|
| 📄 [Rapport méthodologique](./Rapport_Application_Cartographique_PAT.pdf) | Détail des choix techniques et cartographiques |
| 📂 [Données & script d'automatisation](./Carto_PAT/data) | Couches géographiques et script de génération |
| 🎨 [Fichiers front-end](./Carto_PAT/www) | Composants JS, CSS et HTML personnalisés |

---

## Équipe

**Développement cartographique** — M2 GéoNum, Université Lumière Lyon 2

| Nom | Profil |
|---|---|
| Paul **GRÉAUME** | [![GitHub](https://img.shields.io/badge/GitHub-181717?logo=github)](https://github.com/) |
| Silya **GUERBOUB** | [![GitHub](https://img.shields.io/badge/GitHub-181717?logo=github)](https://github.com/) |
| Charlotte **JOUVE** | [![GitHub](https://img.shields.io/badge/GitHub-181717?logo=github)](https://github.com/) |
| Olivier **PRIMA** | [![GitHub](https://img.shields.io/badge/GitHub-181717?logo=github)](https://github.com/) |

**Gestion de projet & rédaction** — M2 SENTINELLES, Université Jean Moulin Lyon 3

| Nom | Profil |
|---|---|
| Louise **DE SIMONE** | [![GitHub](https://img.shields.io/badge/GitHub-181717?logo=github)](https://github.com/) |
| Sabry **FANDI** | [![GitHub](https://img.shields.io/badge/GitHub-181717?logo=github)](https://github.com/) |

**Commanditaire** : DRAAF Auvergne-Rhône-Alpes

---

## Licence

Ce projet est distribué sous licence **CeCILL-2.1**, une licence libre compatible avec la GPL, régie par le droit français.  
→ [Consulter la licence complète](https://cecill.info/licences/Licence_CeCILL_V2.1-fr.txt)
