### Map by department of France (+ DOM-TOM)

# library(xlsx)
library(sp)
library(maptools)
library(rgeos)
library(leaflet)
library(mapview)
library(highcharter)
library(htmlwidgets)



indir <- '~/Documents/fr-departement-election'

# dir.create(paste0(indir, '/graphs'), showWarnings = FALSE)
dir.create(paste0(indir, '/docs'), showWarnings = FALSE)
dir.create(paste0(indir, '/docs/barplots'), showWarnings = FALSE)
dir.create(paste0(indir, '/docs/barplots/libs'), showWarnings = FALSE)



### Candidates informations
cols <- c('#a41515', '#131413', '#464a4c', '#008abf', '#73bddd', '#f49ec4', '#ce9e76',
          '#84726e', '#ffd670', '#e12625', '#fb6e52', '#ffffff', '#3c3c3c', '#000000')

noms <- c('ARTHAUD', 'ASSELINEAU', 'CHEMINADE', 'DUPONTAIGNAN', 'FILLON', 'HAMON', 'LASSALLE',
          'LEPEN', 'MACRON', 'MELENCHON', 'POUTOU', 'BLANCS', 'NULS', 'ABSTENTION')

graph <- c('Arthaud', 'Asselineau', 'Cheminade', 'Dupont-Aignan', 'Fillon', 'Hamon',
           'Lassalle', 'Le Pen', 'Macron', 'Mélenchon', 'Poutou', 'Blanc', 'Nul', 'Abstention')

noms <- tolower(noms)
color <- data.frame(noms, cols, graph)



### Import data
votes <- readRDS(paste0(indir, '/data/votes_tour1_departement.rds'))



### Summary by candidates
x <- apply(votes[ , -c(1, 2, 3, 5, 8)], 2, sum)
x <- data.frame(noms = names(x), votes = x, row.names = NULL)
x <- merge(x, color, by = 'noms', all = TRUE)
x <- x[order(x[ , 'votes'], decreasing = FALSE), ]



### Import France department shapefile
shp1 <- readRDS(paste0(indir, '/data/FRA_adm2.rds'))
shp1@data <- shp1@data[ , c(1, 8, 9)]
colnames(shp1@data) <- c('noid', 'departement', 'code')
shp1@data[ , 'code'] <- gsub('\\.', '-', shp1@data[ , 'code'])



### Import DOMTOM shapefile
shp2 <- readRDS(paste0(indir, '/data/DOM_adm2.rds'))
shp2@data <- shp2@data[ , c(1, 7, 3)]
colnames(shp2@data) <- c('noid', 'departement', 'code')
shp2@data[ , 'code'] <- paste0('FR-', shp2@data[ , 'code'])
shp2@data[ , 'noid'] <- 97:107
shp2 <- spChFIDs(shp2, as.character(shp2@data$noid))



### Merge shapefiles
shp <- spRbind(shp1, shp2)
shp@data[which(shp@data$departement == 'Saint-Martin'), 'departement'] <- 'Saint-Martin/Saint-Barthélemy'
shp@data[which(shp@data$departement == 'Saint-Barthélemy'), 'departement'] <- 'Saint-Martin/Saint-Barthélemy'



### Clean attributes
dat <- data.frame(shp@data)
dat <- merge(dat, votes, by = 'departement', all.x = TRUE, all.y = FALSE)
dat <- dat[order(dat[ , 'noid']), c(2, 3, 1, 4:ncol(dat))]
dat[ , 'first'] <- as.character(apply(dat[, 12:ncol(dat)], 1, function(x) names(x)[which(x == max(x))]))
dat[ , 'color'] <- NA
for (i in 1 : nrow(dat)) dat[i, 'color'] <- as.character(x[which(x[ , 'noms'] == dat[i, 'first']), 'cols'])
rownames(dat) <- NULL
shp@data <- dat



### Simplify geometry
shp <- gSimplify(shp, tol = 0.01, topologyPreserve = TRUE)
shp <- SpatialPolygonsDataFrame(shp, dat)

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
    hc_credits(enabled = TRUE, text = "Source : data.gouv.fr", style = list(fontSize = "10px")) %>%
    hc_add_theme(hc_theme_economist())
    ttips[[j]]$x$theme$colors <- as.character(dat$cols)
    ttips[[j]]$x$conf_opts$lang$decimalPoint <- ','
    htmlwidgets::saveWidget(widget = ttips[[j]], file = paste0(indir, '/docs/barplots/file-', j, '.html'), selfcontained = FALSE)
}



### Clean exported libs (size matter)
todel <- list.dirs(paste0(indir, '/docs/barplots'), recursive = FALSE)
todel <- todel[grep('_files$', todel)]
system(paste0('cp -a ', todel[1], '/. ', indir, '/docs/barplots/libs/'))
sapply(todel, function(x) system(paste0('rm -rf ', x)))



### Correct libs paths in tooltips html files
for (i in 1 : length(todel)){
    html <- readLines(paste0(indir, '/docs/barplots/file-', i, '.html'))
    html <- gsub(paste0('file-', i, '_files/'), 'libs/', html)
    cat(paste0(html, collapse = '\n'), file = paste0(indir, '/docs/barplots/file-', i, '.html'))
}



### mapview() + leaflet()
map <- leaflet() %>%
setView(lng = 2.25, lat = 46.50, zoom = 6) %>%
addProviderTiles(providers$CartoDB.Positron) %>%
garnishMap(addPolygons,
           data = shp,
           weight = .5,
           smoothFactor = 0.5,
           opacity = 1,
           fillOpacity = 1,
           color = "white",
           fillColor = as.character(shp@data$color),
           highlightOptions = highlightOptions(color = "white", weight = 3, bringToFront = TRUE),
           label = as.character(shp@data[, 'departement']),
           popup = popupGraph(ttips, type = 'html', width = 750, height = 400))



### Export core html page
htmlwidgets::saveWidget(widget = map, file = paste0(indir, '/docs/core.html'), selfcontained = FALSE)



### Correct libs paths in core html file
html <- readLines(paste0(indir, '/docs/core.html'))
html <- gsub('\\.\\./graphs/tmp_', 'barplots/file-', html)
cat(paste0(html, collapse = '\n'), file = paste0(indir, '/docs/core.html'))

system(paste0('open ', indir, '/docs/index.html'))



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
