### Map by department of France (+ DOM-TOM)

# source('~/Documents/fr-departement-election/R/elections_department.R')

library(sp)
library(maptools)
library(rgeos)
library(leaflet)
library(mapview)
library(highcharter)
library(htmlwidgets)
library(htmltools)



indir <- '~/Documents/fr-departement-election'

dir.create(paste0(indir, '/barplots'), showWarnings = FALSE)
dir.create(paste0(indir, '/barplots/libs'), showWarnings = FALSE)
dir.create(paste0(indir, '/piecharts'), showWarnings = FALSE)
dir.create(paste0(indir, '/piecharts/libs'), showWarnings = FALSE)



### Candidates informations
cols <- c('#a41515', '#131413', '#464a4c', '#008abf', '#73bddd', '#f49ec4', '#ce9e76',
          '#84726e', '#ffd670', '#e12625', '#fb6e52', '#ffffff', '#3c3c3c', '#000000')

noms <- c('ARTHAUD', 'ASSELINEAU', 'CHEMINADE', 'DUPONTAIGNAN', 'FILLON', 'HAMON', 'LASSALLE',
          'LEPEN', 'MACRON', 'MELENCHON', 'POUTOU', 'BLANCS', 'NULS', 'ABSTENTION')

graph <- c('Arthaud', 'Asselineau', 'Cheminade', 'Dupont-Aignan', 'Fillon', 'Hamon',
           'Lassalle', 'Le Pen', 'Macron', 'Mélenchon', 'Poutou', 'Blanc', 'Nul', 'Abstention')

noms <- tolower(noms)
color <- data.frame(noms, cols, graph)


### Import elections results

### Loop on election tour
for (k in 1 : 2){

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
for (i in 1 : nrow(dat)) dat[i, 'color'] <- as.character(color[which(color[ , 'noms'] == dat[i, 'first']), 'cols'])
rownames(dat) <-  sapply(shp@polygons, function(x) x@ID)
shp@data <- dat


### Simplify geometry
shp <- gSimplify(shp, tol = 0.01, topologyPreserve = TRUE)
shp <- SpatialPolygonsDataFrame(shp, dat)


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
shp3 <- gSimplify(shp3, tol = 0.01, topologyPreserve = TRUE)
shp3 <- SpatialPolygonsDataFrame(shp3, dat)


### Add field
shp@data$winner <- shp@data$graph <- NA
for (i in 1 : length(shp)){
    pos <- which(colnames(shp@data) == shp@data$first[i])
    shp@data$winner[i] <- round(100 * shp@data[i, pos] / shp@data$exprimes[i], 1)
    shp@data$graph[i]  <- as.character(color[which(color$noms == shp@data$first[i]), 'graph'])
}


### Add field
shp3@data$winner <- shp3@data$graph <- NA
for (i in 1 : length(shp3)){
    pos <- which(colnames(shp3@data) == shp3@data$first[i])
    shp3@data$winner[i] <- round(100 * shp3@data[i, pos] / shp3@data$exprimes[i], 1)
    shp3@data$graph[i]  <- as.character(color[which(color$noms == shp3@data$first[i]), 'graph'])
}


### Create and export Tooltips (barplot in html)
ttips <- list()
for (j in 1 : length(shp)){

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
    htmlwidgets::saveWidget(widget = ttips[[j]], file = paste0(indir, '/barplots/file-', j, '.html'), selfcontained = FALSE)
}


### Create and export Tooltips (barplot in html)
ttips2 <- list()
for (j in 1 : length(shp3)){

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
    htmlwidgets::saveWidget(widget = ttips2[[j]], file = paste0(indir, '/piecharts/file-', j, '.html'), selfcontained = FALSE)
}


### Clean exported libs (size matter)
todel <- list.dirs(paste0(indir, '/barplots'), recursive = FALSE)
todel <- todel[grep('_files$', todel)]
system(paste0('cp -a ', todel[1], '/. ', indir, '/barplots/libs/'))
sapply(todel, function(x) system(paste0('rm -rf ', x)))


### Correct libs paths in tooltips html files
for (i in 1 : length(todel)){
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

cols <- color_classes(seq(0, absmax, by = 5), colors = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026'))
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

garnishMap(addPolygons,
           data = shp4,
           group = 'Taux d\'abstention (2nd tour)',
           weight = .5,
           smoothFactor = 0.5,
           opacity = 1,
           fillOpacity = 1,
           color = "#212121",
           fillColor = as.character(shp4@data$color),
           highlightOptions = highlightOptions(color = "#212121", weight = 3, bringToFront = TRUE),
           label = mapply(function(x, y) {
               HTML(sprintf("%s<br />Abstention : %s", htmlEscape(x), htmlEscape(y))) },
               toupper(as.character(shp4@data[, 'departement'])), gsub('\\.', ',', paste0(as.character(format(shp4@data[, 'abstention'])), '%')),
               SIMPLIFY = FALSE, USE.NAMES = FALSE)) %>%

garnishMap(addPolygons,
           data = shp,
           group = 'Votes exprimés (1er tour)',
           weight = .5,
           smoothFactor = 0.5,
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

garnishMap(addPolygons,
           data = shp2,
           group = 'Taux d\'abstention (1er tour)',
           weight = .5,
           smoothFactor = 0.5,
           opacity = 1,
           fillOpacity = 1,
           color = "#212121",
           fillColor = as.character(shp2@data$color),
           highlightOptions = highlightOptions(color = "#212121", weight = 3, bringToFront = TRUE),
           label = mapply(function(x, y) {
               HTML(sprintf("%s<br />Abstention : %s", htmlEscape(x), htmlEscape(y))) },
               toupper(as.character(shp2@data[, 'departement'])), gsub('\\.', ',', paste0(as.character(format(shp2@data[, 'abstention'])), '%')),
               SIMPLIFY = FALSE, USE.NAMES = FALSE)) %>%

addEasyButton(easyButton(icon = 'fa-globe', title = 'Reset zoom',
                         onClick = JS('function(btn){ location.reload(); }'))) %>%
addLayersControl(baseGroups = c('Votes exprimés (2nd tour)', 'Taux d\'abstention (2nd tour)', 'Votes exprimés (1er tour)', 'Taux d\'abstention (1er tour)'),
                 options = layersControlOptions(collapsed = TRUE),
                 position = 'topleft')



### Export core html page
htmlwidgets::saveWidget(widget = map, file = paste0(indir, '/core.html'), selfcontained = FALSE)


#### !!!!!!!
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



### Import data
# votes <- read.xlsx('data/elections_tour1_departement.xlsx', 1)
# voix  <- read.xlsx('data/elections_tour1_departement.xlsx', 2)
#
# voix[ , 'nom'] <- tolower(gsub('-| ', '', voix[ , 'nom']))
# voix[ , 'nom'] <- tolower(gsub('é', 'e', voix[ , 'nom']))
# candidats <- sort(unique(voix[ , 'nom']))
#
# for (i in 1 : length(candidats)){
#     dat <- voix[which(voix[ , 'nom'] == candidats[i]), c('no_departement', 'voix')]
#     colnames(dat)[2] <- candidats[i]
#     votes <- merge(votes, dat, by = 'no_departement', all = TRUE)
# }





### Barplot total
#
# pdf('graphs/elections-pays.pdf', width = 14, height = 8)
# par(mar = c(3, 6, 1, 1), family = 'serif', xaxs = 'i', yaxs = 'i')
# plot(0, type = 'n', xlim = c(0, 11500000), ylim = c(1-.5, nrow(x)+.5), axes = FALSE, ann = FALSE)
# rect(0, 0, 11500000, nrow(x)+.5, col = '#e7e7e7', border = '#e7e7e7')
# abline(v = seq(1000000, 11500000, by = 1000000), col = 'white', lty = 3)
# for (i in 1 : nrow(x)){
#     if (as.character(x[i, 'noms']) %in% c('abstention', 'nuls', 'blancs')){
#         rect(0, i-.33, x[i, 'votes'], i+.33, col = '#252525', border = NA, density = 10, angle = 45)
#         texte <- gsub('\\.', ',', paste0(as.character(format(round(100*x[i, 'votes']/sum(x[, 'votes']), 2))), '%'))
#     } else {
#         rect(0, i-.33, x[i, 'votes'], i+.33, col = as.character(x[i, 'cols']), border = as.character(x[i, 'cols']))
#         texte <- paste0(gsub('\\.', ',', paste0(as.character(format(round(100*x[i, 'votes']/sum(x[, 'votes']), 2))), '%')), ' (',
#                         gsub('\\.', ',', paste0(as.character(format(round(100*x[i, 'votes']/sum(votes[, 'exprimes']), 2))), '%')), ')')
#     }
#     text(x[i, 'votes'], i, texte, pos = 4, font = 2)
#
# }
# par(mgp = c(3, .25, 0))
# options(scipen = 16)
# axis(2, seq(1, nrow(x)), as.character(x[ , 'graph']), las = 1, lwd = 0)
# axis(1, seq(0, 11500000, by = 1000000), seq(0, 11500000, by = 1000000), lwd = 0)
# rect(7750000, 0, 11500000, 2.25, col = '#e7e7e7', border = '#e7e7e7')
# text(x = 7750000, y = 1.25, 'ÉLECTION PRÉSIDENTIELLE FRANÇAISE 2017\nRÉSULTAT DU PREMIER TOUR', pos = 4, cex = 1, font = 2)
# mtext(side = 1, line = 1.75, 'Nombre de voix obtenues', font = 2)
# dev.off()




### Export popup barplot by department
# for (j in 1 : length(shp)){
#     vals <- t(shp@data[j, 11:21])[ , 1]
#     dat  <- data.frame(noms = names(vals), value = round(100*vals/sum(vals), 2))
#     dat <- merge(dat, color, by = 'noms', all = TRUE)
#     dat <- dat[which(!is.na(dat[ , 'value'])), ]
#     dat <- dat[order(dat[ , 'value'], decreasing = FALSE), ]
#
#
#     png(paste0('graphs/graph-', shp@data[j, 'code'], '.png'), pointsize = 4, width = 960, height = 520, res = 300)
#     par(mar = c(2, 6, 1, 1), family = 'serif', xaxs = 'i', yaxs = 'i')
#     plot(0, type = 'n', xlim = c(0, 40), ylim = c(0.5, 11.5), axes = FALSE, ann = FALSE)
#     rect(0, 0, 40, 11.5, col = '#e7e7e7', border = '#e7e7e7')
#     abline(v = seq(5, 35, by = 5), col = 'white', lty = 3, lwd = .25)
#     for (i in 1 : length(vals)){
#         rect(0, i-.33, dat[i, 'value'], i+.33, col = as.character(dat[i, 'cols']), border = as.character(dat[i, 'cols']))
#         texte <- gsub('\\.', ',', paste0(dat[i, 'value'], '%'))
#         text(dat[i, 'value'], i, texte, pos = 4, font = 2)
#     }
#     par(mgp = c(3, .25, 0))
#     options(scipen = 16)
#     axis(2, seq(1, 11), as.character(dat[ , 'graph']), las = 1, lwd = 0)
#     axis(1, seq(0, 40, by = 5), seq(0, 40, by = 5), lwd = 0)
#     rect(7750000, 0, 40, 2.25, col = '#e7e7e7', border = '#e7e7e7')
#     text(x = 40, y = 1.25, labels = shp@data[j, 'departement'], pos = 2, cex = 1, font = 2)
#     dev.off()
# }
