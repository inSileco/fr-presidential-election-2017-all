### Creates Shapefiles and plots to be used to make the Map

#### Packages
library(magrittr)
library(xlsx)
library(highcharter)
library(sp)
library(rgeos)
library(htmlwidgets)
library(mapview)

# indir <- '~/Documents/fr-departement-election'
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




### IMPORT ELECTIONS RESULTS

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




### Create and export Tooltips (BARPLOT in HTML)
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


### Create and export Tooltips (PIECHARTS in HTML)
ttips2 <- list()
for (j in 1:length(shp3)){

    ### Data
    vals <- t(shp3@data[j, 11:(ncol(shp3@data)-4)])[ , 1]
    dat  <- data.frame(noms = names(vals), value = round(100*vals/sum(vals), 2))
    dat <- merge(dat, color, by = 'noms', all = TRUE)
    dat <- dat[which(!is.na(dat[ , 'value'])), ]
    dat <- dat[order(dat[ , 'value'], decreasing = TRUE), ]

    ### highchart() and Dark Unica theme
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


### Keep one single libs
### Remove exported libs (size matter)
todel <- list.dirs(paste0(indir, '/barplots'), recursive = FALSE)
todel <- todel[grep('_files$', todel)]
system(paste0('cp -a ', todel[1], '/. ', indir, '/barplots/libs/'))
sapply(todel, function(x) system(paste0('rm -rf ', x)))


### Correct libs paths in tooltips html files
### And change padding of popups
for (i in 1:length(todel)){
    html <- readLines(paste0(indir, '/barplots/file-', i, '.html'))
    html <- gsub(paste0('file-', i, '_files/'), 'libs/', html)
    html <- gsub('"padding":15|"padding":40', '"padding":5', html)
    cat(paste0(html, collapse = '\n'), file = paste0(indir, '/barplots/file-', i, '.html'))
}


### Keep one single libs
### Remove exported libs (size matter)
todel <- list.dirs(paste0(indir, '/piecharts'), recursive = FALSE)
todel <- todel[grep('_files$', todel)]
system(paste0('cp -a ', todel[1], '/. ', indir, '/piecharts/libs/'))
sapply(todel, function(x) system(paste0('rm -rf ', x)))


### Correct libs paths in tooltips html files
### And change padding of popups
for (i in 1 : length(todel)){
    html <- readLines(paste0(indir, '/piecharts/file-', i, '.html'))
    html <- gsub(paste0('file-', i, '_files/'), 'libs/', html)
    html <- gsub('"padding":15|"padding":40', '"padding":5', html)
    cat(paste0(html, collapse = '\n'), file = paste0(indir, '/piecharts/file-', i, '.html'))
}


### Abstention layer (tour 1)
shp2 <- shp
shp2@data$abstention <- round(100 * shp2@data$abstention / (shp2@data$abstention + shp2@data$inscrits), 1)

abs_pal <- c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026')

cols <- color_classes(seq(0, 40, by = 5), colors = abs_pal)
for (i in 1 : length(cols)){
    shp2@data$color <- ifelse(shp2@data$abstention >= cols[[i]]$from &
                        shp2@data$abstention <  cols[[i]]$to,
                        yes = cols[[i]]$color,
                        no = shp2@data$color)
}


### Abstention layer (tour 2)
shp4 <- shp3
shp4@data$abstention <- round(100 * shp4@data$abstention / (shp4@data$abstention + shp4@data$inscrits), 1)

cols <- color_classes(seq(0, 40, by = 5), colors = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026'))
for (i in 1 : length(cols)){
    shp4@data$color <- ifelse(shp4@data$abstention >= cols[[i]]$from &
                        shp4@data$abstention <  cols[[i]]$to,
                        yes = cols[[i]]$color,
                        no = shp4@data$color)
}


###########
########### Save R objects used for the maps afterwards.
saveRDS(shp, paste0(indir, "/data/shp1.Rds"))
saveRDS(shp2, paste0(indir, "/data/shp2.Rds"))
saveRDS(shp3, paste0(indir, "/data/shp3.Rds"))
saveRDS(shp4, paste0(indir, "/data/shp4.Rds"))

### Prepare popupGraph (PIECHARTS and BARPLOTS)
popupgraph1 <- popupGraph(ttips2, type = 'html', width = 750, height = 425)
popupgraph2 <- popupGraph(ttips,  type = 'html', width = 750, height = 425)

saveRDS(popupgraph1, paste0(indir, "/data/popupgraph1.Rds"))
saveRDS(popupgraph2, paste0(indir, "/data/popupgraph2.Rds"))
