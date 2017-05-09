


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
