### Map by department of France (+ DOM-TOM)

# source('~/Documents/fr-departement-election/R/elections_department.R')

#### Packages
library(magrittr)
library(sp)
library(maptools)
library(rgeos)
library(leaflet)
library(mapview)
library(highcharter)
library(htmlwidgets)
library(htmltools)
library(xlsx)


# indir <- '~/Documents/fr-departement-election'
# indir <- getwd()
indir <- '/Users/KevCaz/Codes/Github/fr-departement-election'

myDirCreate <- . %>% paste0(indir,.) %>% dir.create(showWarnings = FALSE)
dirs <- c('/barplots', '/barplots/libs', '/piecharts', '/piecharts/libs')
tmp <- lapply(dirs, FUN = myDirCreate)


### Candidates' informations
cols <- c('#a41515', '#131413', '#464a4c', '#008abf', '#73bddd', '#f49ec4', '#ce9e76',
          '#84726e', '#ffd670', '#e12625', '#fb6e52', '#ffffff', '#3c3c3c', '#000000')

noms <- c('ARTHAUD', 'ASSELINEAU', 'CHEMINADE', 'DUPONTAIGNAN', 'FILLON', 'HAMON', 'LASSALLE',
          'LEPEN', 'MACRON', 'MELENCHON', 'POUTOU', 'BLANCS', 'NULS', 'ABSTENTION')

graph <- c('Arthaud', 'Asselineau', 'Cheminade', 'Dupont-Aignan', 'Fillon', 'Hamon',
           'Lassalle', 'Le Pen', 'Macron', 'Mélenchon', 'Poutou', 'Blanc', 'Nul', 'Abstention')

noms %<>% tolower
color <- data.frame(noms, cols, graph)




### Import elections results

### Loop on election tour
for (k in 1:2){

    ### Import data
    tab <- read.xlsx(file = paste0(indir, '/data/Presidentielle_2017_Resultats_Tour_', k, '_c.xls'),
                     sheetName = paste('Départements Tour', k),
                     startRow = ifelse(k == 1, 1, 4))

    ### Select general columns
    pos <- grep('département$|^Inscrits$|^Abstentions$|^Votants$|^Blancs$|^Nuls$|^Exprimés$', colnames(tab))
    dat <- tab[ , pos]
    colnames(dat) <- c('no_departement', 'departement', 'inscrits', 'abstention', 'votants', 'blancs', 'nuls', 'exprimes')

    ### Select candidates columns
    pos <- grep('^Voix|^Nom', colnames(tab))
    mat <- tab[ , pos]

    ### Transpose data
    tab <- data.frame()
    for (i in seq(1, ncol(mat), 2)){
        tmp <- data.frame(dat, nom = mat[ , i], voix = mat[ , i+1])
        tab <- rbind(tab, tmp)
    }

    ### Clean candidates names
    tab[ , 'nom'] <- tolower(gsub('-| ', '', tab[ , 'nom']))
    tab[ , 'nom'] <- tolower(gsub('é', 'e',  tab[ , 'nom']))
    candidats <- sort(unique(tab[ , 'nom']))

    ### Transpose data
    votes <- dat
    for (i in 1 : length(candidats)){
        dat <- tab[which(tab[ , 'nom'] == candidats[i]), c('no_departement', 'voix')]
        colnames(dat)[2] <- candidats[i]
        votes <- merge(votes, dat, by = 'no_departement', all = TRUE)
    }

    ### Export data
    saveRDS(votes, paste0(indir, '/data/votes_tour', k, '_departement.rds'))
}

votes1 <- readRDS(paste0(indir, '/data/votes_tour1_departement.rds'))
votes2 <- readRDS(paste0(indir, '/data/votes_tour2_departement.rds'))




### Import France department shapefile
shp <- readRDS(paste0(indir, '/data/FRA-DOM-composite.rds'))
shp@data <- shp@data[ , c(1, 8, 9)]
colnames(shp@data) <- c('noid', 'departement', 'code')
shp@data[ , 'code'] <- gsub('\\.', '-', shp@data[ , 'code'])

shp3 <- shp


### Clean attributes (Tour 1)
dat <- data.frame(shp@data)
dat <- merge(dat, votes1, by = 'departement', all.x = TRUE, all.y = FALSE)
dat <- dat[order(dat[ , 'noid']), c(2, 3, 1, 4:ncol(dat))]
dat[ , 'first'] <- as.character(apply(dat[, 11:ncol(dat)], 1, function(x) names(x)[which(x == max(x))]))
dat[ , 'color'] <- NA
for (i in 1:nrow(dat)) dat[i, 'color'] <- as.character(color[which(color[ , 'noms'] == dat[i, 'first']), 'cols'])
rownames(dat) <-  sapply(shp@polygons, function(x) x@ID)
shp@data <- dat


### Simplify geometry
shp %<>% gSimplify(tol = 0.01, topologyPreserve = TRUE)
shp %<>% SpatialPolygonsDataFrame(dat)


### Clean attributes (Tour 2)
dat <- data.frame(shp3@data)
dat <- merge(dat, votes2, by = 'departement', all.x = TRUE, all.y = FALSE)
dat <- dat[order(dat[ , 'noid']), c(2, 3, 1, 4:ncol(dat))]
dat[ , 'first'] <- as.character(apply(dat[, 11:ncol(dat)], 1, function(x) names(x)[which(x == max(x))]))
dat[ , 'color'] <- NA
for (i in 1 : nrow(dat)) dat[i, 'color'] <- as.character(color[which(color[ , 'noms'] == dat[i, 'first']), 'cols'])
rownames(dat) <-  sapply(shp3@polygons, function(x) x@ID)
shp3@data <- dat


### Simplify geometry
shp3 %<>% gSimplify(tol = 0.01, topologyPreserve = TRUE)
shp3 %<>% SpatialPolygonsDataFrame(dat)


### Add field
shp@data$winner <- shp@data$graph <- NA
for (i in 1:length(shp)){
    pos <- which(colnames(shp@data) == shp@data$first[i])
    shp@data$winner[i] <- round(100 * shp@data[i, pos] / shp@data$exprimes[i], 1)
    shp@data$graph[i]  <- as.character(color[which(color$noms == shp@data$first[i]), 'graph'])
}


### Add field
shp3@data$winner <- shp3@data$graph <- NA
for (i in 1:length(shp3)){
    pos <- which(colnames(shp3@data) == shp3@data$first[i])
    shp3@data$winner[i] <- round(100 * shp3@data[i, pos] / shp3@data$exprimes[i], 1)
    shp3@data$graph[i]  <- as.character(color[which(color$noms == shp3@data$first[i]), 'graph'])
}




### Create and export Tooltips (barplot in html)
ttips <- list()
for (j in 1:length(shp)){

    ### Data
    vals <- t(shp@data[j, 11:21])[ , 1]
    dat  <- data.frame(noms = names(vals), value = round(100*vals/sum(vals), 2))
    dat <- merge(dat, color, by = 'noms', all = TRUE)
    dat <- dat[which(!is.na(dat[ , 'value'])), ]
    dat <- dat[order(dat[ , 'value'], decreasing = TRUE), ]
    dat$group <- seq(1, nrow(dat))

    ### highchart() and The Economist theme
    ttips[[j]] <- highchart() %>%
    hc_add_series(data = dat$value, type = "bar", name = "Votes exprimés (%)", showInLegend = FALSE, colorByPoint = TRUE) %>%
    hc_yAxis(title = list(text = "Votes exprimés (%)"), allowDecimals = FALSE) %>%
    hc_xAxis(categories = as.character(dat$graph), tickmarkPlacement = "on", opposite = FALSE) %>%
    hc_title(text = as.character(shp@data[j, 'departement']), style = list(fontWeight = "bold")) %>%
    hc_subtitle(text = "Résultats du premier tour de l\'élection présidentielle française 2017") %>%
    hc_tooltip(valueDecimals = 2, pointFormat = "Vote : {point.y}%") %>%
    hc_credits(enabled = TRUE, text = "Source : data.gouv.fr", href = "http://www.data.gouv.fr/fr/posts/les-donnees-des-elections/", style = list(fontSize = "10px")) %>%
    hc_add_theme(hc_theme_economist())
    ttips[[j]]$x$theme$colors <- as.character(dat$cols)
    ttips[[j]]$x$conf_opts$lang$decimalPoint <- ','
    saveWidget(widget = ttips[[j]], file = paste0(indir, '/barplots/file-', j, '.html'), selfcontained = FALSE)
}


### Create and export Tooltips (barplot in html)
ttips2 <- list()
for (j in 1:length(shp3)){

    ### Data
    vals <- t(shp3@data[j, 11:(ncol(shp3@data)-4)])[ , 1]
    dat  <- data.frame(noms = names(vals), value = round(100*vals/sum(vals), 2))
    dat <- merge(dat, color, by = 'noms', all = TRUE)
    dat <- dat[which(!is.na(dat[ , 'value'])), ]
    dat <- dat[order(dat[ , 'value'], decreasing = TRUE), ]

    ### highchart() and The Economist theme
    ttips2[[j]] <- highchart() %>%
    hc_add_series_labels_values(labels = as.character(dat$graph),
                                values = dat$value,
                                colors = dat$cols,
                                type = "pie",
                                startAngle = -90, endAngle = 90, innerSize= '0%', center = c('50%', '75%'),
                                dataLabels = list(enabled = TRUE, distance = -50, fontWeight = 'bold', color = 'black')) %>%

    hc_tooltip(pointFormat = '<b>{point.percentage:.1f}%</b>') %>%
    hc_title(text = as.character(shp3@data[j, 'departement']), style = list(fontWeight = "bold")) %>%
    hc_subtitle(text = "Résultats du second tour de l\'élection présidentielle française 2017") %>%
    hc_tooltip(valueDecimals = 2, pointFormat = "Vote : {point.y}%") %>%
    hc_credits(enabled = TRUE, text = "Source : data.gouv.fr", href = "http://www.data.gouv.fr/fr/posts/les-donnees-des-elections/", style = list(fontSize = "10px")) %>%
    hc_add_theme(hc_theme_darkunica())
    ttips2[[j]]$x$conf_opts$lang$decimalPoint <- ','
    saveWidget(widget = ttips2[[j]], file = paste0(indir, '/piecharts/file-', j, '.html'), selfcontained = FALSE)
}


### Clean exported libs (size matter)
todel <- list.dirs(paste0(indir, '/barplots'), recursive = FALSE)
todel <- todel[grep('_files$', todel)]
system(paste0('cp -a ', todel[1], '/. ', indir, '/barplots/libs/'))
sapply(todel, function(x) system(paste0('rm -rf ', x)))


### Correct libs paths in tooltips html files
for (i in 1:length(todel)){
    html <- readLines(paste0(indir, '/barplots/file-', i, '.html'))
    html <- gsub(paste0('file-', i, '_files/'), 'libs/', html)
    html <- gsub('"padding":15|"padding":40', '"padding":5', html)
    cat(paste0(html, collapse = '\n'), file = paste0(indir, '/barplots/file-', i, '.html'))
}


### Clean exported libs (size matter)
todel <- list.dirs(paste0(indir, '/piecharts'), recursive = FALSE)
todel <- todel[grep('_files$', todel)]
system(paste0('cp -a ', todel[1], '/. ', indir, '/piecharts/libs/'))
sapply(todel, function(x) system(paste0('rm -rf ', x)))


### Correct libs paths in tooltips html files
for (i in 1 : length(todel)){
    html <- readLines(paste0(indir, '/piecharts/file-', i, '.html'))
    html <- gsub(paste0('file-', i, '_files/'), 'libs/', html)
    html <- gsub('"padding":15|"padding":40', '"padding":5', html)
    cat(paste0(html, collapse = '\n'), file = paste0(indir, '/piecharts/file-', i, '.html'))
}

absmax <- 40

### Abstention layer (tour 1)
shp2 <- shp
shp2@data$abstention <- round(100 * shp2@data$abstention / (shp2@data$abstention + shp2@data$inscrits), 1)

abs_pal <- c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026')

cols <- color_classes(seq(0, absmax, by = 5), colors = abs_pal)
for (i in 1 : length(cols)){
    shp2@data$color <- ifelse(shp2@data$abstention >= cols[[i]]$from &
                        shp2@data$abstention <  cols[[i]]$to,
                        yes = cols[[i]]$color,
                        no = shp2@data$color)
}


### Abstention layer (tour 2)
shp4 <- shp3
shp4@data$abstention <- round(100 * shp4@data$abstention / (shp4@data$abstention + shp4@data$inscrits), 1)

cols <- color_classes(seq(0, absmax, by = 5), colors = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026'))
for (i in 1 : length(cols)){
    shp4@data$color <- ifelse(shp4@data$abstention >= cols[[i]]$from &
                        shp4@data$abstention <  cols[[i]]$to,
                        yes = cols[[i]]$color,
                        no = shp4@data$color)
}

### mapview() + leaflet()
map <- leaflet() %>%
  setView(lng = 2.25, lat = 46.50, zoom = 5.85) %>%
# addProviderTiles(providers$CartoDB.Positron) %>%
  # votes 2nd tour
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
           popup = popupGraph(ttips2, type = 'html', width = 750, height = 415)) %>%
  # abstension 2nd tour
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
  # votes 1er tour
  garnishMap(addPolygons,
           data = shp,
           group = 'Votes exprimés (1er tour)',
           weight = .5,
           smoothFactor = .5,
           opacity = 1,
           fillOpacity = 1,
           color = "#212121",
           fillColor = as.character(shp@data$color),
           highlightOptions = highlightOptions(color = "#212121", weight = 3, bringToFront = TRUE),
           label = mapply(function(x, y, z) {
               HTML(sprintf("%s<br /> %s : %s", htmlEscape(x), htmlEscape(y), htmlEscape(z))) },
               toupper(as.character(shp@data[, 'departement'])), as.character(shp@data[, 'graph']), gsub('\\.', ',', paste0(as.character(format(shp@data[, 'winner'])), '%')),
               SIMPLIFY = FALSE, USE.NAMES = FALSE),
           popup = popupGraph(ttips, type = 'html', width = 750, height = 425)) %>%
  # abstension 1er tour
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
                  onClick = JS('function(btn){ location.reload(); }'))) %>%
  addLayersControl(baseGroups = paste0(c('Votes exprimés', 'Taux d\'abstention'),  rep(c(' (2nd tour)', ' (1er tour)'), each=2)),
                 options = layersControlOptions(collapsed = TRUE),
                 position = 'topleft')

#  mypop <- paste0("<image src='./fig/graph-", shp@data[, "HASC_2"], ".png' width=750 height=400>")


### Export core html page
saveWidget(widget = map, file = paste0(indir, '/core.html'), selfcontained = FALSE)


#### !!!!!!! Ça c'est pas cool...
### Correct libs paths in core html file
html <- readLines(paste0(indir, '/core.html'))
for (i in 1 : 101){
    html <- gsub(paste0('\\.\\./graphs/tmp_',   i, ".html' frameborder=0 width=750 height=415"), paste0('piecharts/file-',   i, ".html' frameborder=0 width=750 height=415"), html)
    html <- gsub(paste0('\\.\\./graphs/tmp_',   i, ".html' frameborder=0 width=750 height=425"), paste0('barplots/file-',   i, ".html' frameborder=0 width=750 height=425"), html)
}

html <- gsub('width:100%;height:400px;', 'width:100%;height:400px;background-color:#212121;', html)
cat(paste0(html, collapse = '\n'), file = paste0(indir, '/core.html'))


### Add CSS styles
css <- readLines(paste0(indir, '/core_files/leaflet-0.7.7/leaflet.css'))
css <- c(css, '', '.leaflet-popup { width:790px; }')
cat(paste0(css, collapse = '\n'), file = paste0(indir, '/core_files/leaflet-0.7.7/leaflet.css'))


system(paste0('open ', indir, '/index.html'))
