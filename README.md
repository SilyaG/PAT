# Web-cartographie des Projets Alimentaires Territoriaux
### Région Auvergne-Rhône-Alpes

![Licence CeCILL-2.1](https://img.shields.io/badge/Licence-CeCILL--2.1-blue)
![Statut](https://img.shields.io/badge/Statut-Terminé-brightgreen)

---

## Présentation

Cette application de web-cartographie permet de visualiser et d'explorer les **Projets Alimentaires Territoriaux (PAT)** de la région Auvergne-Rhône-Alpes.

Elle a été développée dans le cadre d'une commande de la **DRAAF Auvergne-Rhône-Alpes** (Direction Régionale de l'Alimentation, de l'Agriculture et de la Forêt), par les étudiants de deuxième année des Masters **GéoNum** et **SENTINELLES** de l'Université Lyon 2 / Lyon 3.

---

## Fonctionnalités principales 

- Visualisation cartographique des PAT sur fond IGN, OpenStreetMap ou Registre Parcellaire Graphique
- Recherche par PAT ou commune
- Filtres par niveau de labellisation et/ou échelle territoriale
- Liste dynamique des PAT visibles dans la vue courante
- Tutoriel interactif intégré
- Pop-up détaillés

---

## Technologies utilisées

| Technologie | Rôle |
|---|---|
| R Shiny | Interface et serveur applicatif |
| Leaflet.js | Rendu cartographique interactif |
| JavaScript / HTML / CSS | Composants front-end personnalisés |

---

## Équipe

Projet réalisé par les étudiants de M2 **GéoNum** (Université Lumière Lyon 2) : 
Paul **GRÉAUME**, Silya **GUERBOUB**, Charlotte **JOUVE**, Olivier **PRIMA** 
Projet réalisé par les étudiants de M2 **SENTINELLES** (Université Jean Moulin Lyon 3) : Louise **DE SIMONE**, Sabry **FANDI**
 
Commanditaire : **DRAAF Auvergne-Rhône-Alpes**

---
## Documentation

Ce projet est accompagné d'un rapport méthodologique détaillant les choix techniques et cartographiques réalisés.

📄 [Rapport_Application_Cartographique_PAT.pdf](./Rapport_Application_Cartographique_PAT.pdf)

Le script d'automatisation de la création des couches mobilisées dans l'application cartographique est disponible dans le dossier "data". 

📄 [Script d'automatisation](Carto_PAT/data/automatisation.R)


---
## Licence

Ce projet est distribué sous licence **CeCILL-2.1**, une licence libre compatible avec la GPL, régie par le droit français.  
→ [Consulter la licence complète](https://cecill.info/licences/Licence_CeCILL_V2.1-fr.txt)
