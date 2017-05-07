## Résultats de l'élection présidentielle française de 2017

Cartographies des votes exprimés et du taux d'abstention de l'élection présidentielle de 2017 pour chaque département de la France métropolitaine et d'outre-mer (exlusion des Territoires d'outre-mer).

### Source des données

Les limites administratives des départements français proviennent de la version 2.8 du _Global Administrative Areas_ ([GADM](http://www.gadm.org/version2)). Les résultats du vote français pour les deux tours de l'élection présidentielle de 2017 ont été téléchargés depuis la plateforme ouverte des données publiques françaises du Gouvernement Français ([data.gouv.fr](http://www.data.gouv.fr/fr/posts/les-donnees-des-elections/)).

### Informations pour les geeks

La carte a été réalisée sous le logiciel [R](https://cran.r-project.org) à l'aide des packages [`leaflet`](https://rstudio.github.io/leaflet/) (fond de carte, boutons de contrôle et représentation des données vectorielles), [`mapview`](https://environmentalinformatics-marburg.github.io/mapview/introduction.html) (inclusion de graphique HTML dans le popup d'une région de la carte), [`highcharter`](http://jkunst.com/highcharter/) (réalisation d'un graphique en HTML), [`htmltools`](https://cran.r-project.org/web/packages/htmltools/index.html) (personnalisation des _labels_) et [`htmlwidgets`](http://www.htmlwidgets.org) (exportation depuis R en version HTML).
