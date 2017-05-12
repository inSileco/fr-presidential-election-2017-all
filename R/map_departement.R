library(mapview)
library(htmltools)
library(htmlwidgets)

# indir <- '~/Documents/fr-departement-election'
indir <- '/Users/KevCaz/Codes/Github/fr-departement-election'


#### Import R objects
shp1 <- readRDS(paste0(indir, "/data/shp1.Rds"))
shp2 <- readRDS(paste0(indir, "/data/shp2.Rds"))
shp3 <- readRDS(paste0(indir, "/data/shp3.Rds"))
shp4 <- readRDS(paste0(indir, "/data/shp4.Rds"))

### Prepare popupGraph (PIECHARTS)
popupgraph1 <- readRDS(paste0(indir, "/data/popupgraph1.Rds"))
popupgraph1 %<>% lapply(function(x) gsub("src=.*graphs/tmp_", "src='piecharts/file-", x))
popupgraph1 %<>% lapply(function(x) gsub("leaflet-popup-content \\{",
  "leaflet-popup-content \\{ \twidth: 755px !important;", x))


### Prepare popupGraph (BARPLOTS)
popupgraph2 <- readRDS(paste0(indir, "/data/popupgraph2.Rds"))
popupgraph2 %<>% lapply(function(x) gsub("src=.*graphs/tmp_", "src='barplots/file-", x))
popupgraph2 %<>% lapply(function(x) gsub("leaflet-popup-content \\{",
  "leaflet-popup-content \\{ \twidth: 755px !important;", x))



### mapview() + leaflet()
map <- leaflet() %>%

  setView(lng = 2.25, lat = 46.50, zoom = 5.85) %>%

  ### VOTES 2ND TOUR
  garnishMap(addPolygons,
           data = shp3,
           group = 'Votes exprimés (2nd tour)',
           weight = .5,
           smoothFactor = 0.5,
           opacity = 1,
           fillOpacity = 1,
           color = "#212121",
           fillColor = as.character(shp3@data$color),
           highlightOptions = highlightOptions(color = "#212121", weight = 3, bringToFront = TRUE),
           label = mapply(function(x, y, z) {
               HTML(sprintf("%s<br /> %s : %s", htmlEscape(x), htmlEscape(y), htmlEscape(z))) },
               toupper(as.character(shp3@data[, 'departement'])), as.character(shp3@data[, 'graph']), gsub('\\.', ',', paste0(as.character(format(shp3@data[, 'winner'])), '%')),
               SIMPLIFY = FALSE, USE.NAMES = FALSE),
           popup = popupgraph1) %>%

  ### ABSTENSION 2ND TOUR
  garnishMap(addPolygons,
           data = shp4,
           group = 'Taux d\'abstention (2nd tour)',
           weight = .5,
           smoothFactor = .5,
           opacity = 1,
           fillOpacity = 1,
           color = "#212121",
           fillColor = as.character(shp4@data$color),
           highlightOptions = highlightOptions(color = "#212121", weight = 3, bringToFront = TRUE),
           label = mapply(function(x, y) {
               HTML(sprintf("%s<br />Abstention : %s", htmlEscape(x), htmlEscape(y))) },
               toupper(as.character(shp4@data[, 'departement'])), gsub('\\.', ',', paste0(as.character(format(shp4@data[, 'abstention'])), '%')),
               SIMPLIFY = FALSE, USE.NAMES = FALSE)) %>%

  ### VOTES 1ER TOUR
  garnishMap(addPolygons,
           data = shp1,
           group = 'Votes exprimés (1er tour)',
           weight = .5,
           smoothFactor = .5,
           opacity = 1,
           fillOpacity = 1,
           color = "#212121",
           fillColor = as.character(shp1@data$color),
           highlightOptions = highlightOptions(color = "#212121", weight = 3, bringToFront = TRUE),
           label = mapply(function(x, y, z) {
               HTML(sprintf("%s<br /> %s : %s", htmlEscape(x), htmlEscape(y), htmlEscape(z))) },
               toupper(as.character(shp1@data[, 'departement'])), as.character(shp1@data[, 'graph']), gsub('\\.', ',', paste0(as.character(format(shp1@data[, 'winner'])), '%')),
               SIMPLIFY = FALSE, USE.NAMES = FALSE),
           popup = popupgraph2) %>%

  ### ABSTENSION 1ER TOUR
  garnishMap(addPolygons,
           data = shp2,
           group = 'Taux d\'abstention (1er tour)',
           weight = .5,
           smoothFactor = .5,
           opacity = 1,
           fillOpacity = 1,
           color = "#212121",
           fillColor = as.character(shp2@data$color),
           highlightOptions = highlightOptions(color = "#212121", weight = 3, bringToFront = TRUE),
           label = mapply(function(x, y) {
               HTML(sprintf("%s<br />Abstention : %s", htmlEscape(x), htmlEscape(y))) },
               toupper(as.character(shp2@data[, 'departement'])), gsub('\\.', ',', paste0(as.character(format(shp2@data[, 'abstention'])), '%')),
               SIMPLIFY = FALSE, USE.NAMES = FALSE)) %>%
  addEasyButton(easyButton(icon = 'fa-home', title = 'Zoom initial',
                  onClick = JS('function(btn){ location.reload(); }'))) %>%
  addEasyButton(easyButton(icon = 'fa-globe', title = 'Résultats du vote des français établis à l\'étranger.',
                  onClick = JS("function(btn){window.location.href = 'https://letir.github.io/Elections_fr-042017/';}"))) %>%
  addLayersControl(baseGroups = paste0(c('Votes exprimés', 'Taux d\'abstention'),  rep(c(' (2nd tour)', ' (1er tour)'), each=2)),
                 options = layersControlOptions(collapsed = TRUE),
                 position = 'topleft')


### Export core html page
saveWidget(widget = map, file = paste0(indir, '/core.html'), selfcontained = FALSE)


### Change background color of leaflet map
html <- readLines(paste0(indir, '/core.html'))
html[grep('<div id=\"htmlwidget-', html)] <- gsub('width:100%;', 'width:100%;background-color:#212121;', html[grep('<div id=\"htmlwidget-', html)])
cat(paste0(html, collapse = '\n'), file = paste0(indir, '/core.html'))

system(paste0('open ', indir, '/index.html'))
