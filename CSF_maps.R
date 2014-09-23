library(sp)
library(maptools)
library(Hmisc)

# Adapted from function by John Colby 
# (http://www.colbyimaging.com/wiki/statistics/color-bars)
ColourBar <- function(lut, min, max=-min, nticks=6, ticks=seq(min, max, len=nticks), title="") {
  scale = (length(lut)-1)/(max-min)
  plot(c(0, 10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  axis(2, ticks, las=1)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0, y, 5, y+1/scale, col=lut[i], border=NA)
  }
}

CSFMaps <- function(UK, targets, preds, field = "CATCHMENT", 
                    sce = "base.out",
                    segs = 100, title = "", det){
  stopifnot(exists("ColourBar"))
  mapdata <- merge(targets, preds, by = field)
  column <- which(names(mapdata) == sce)
  startofreducts <- 18 # col number of reduction fields
  if (det == "Orthophosphate") colours <- colorRampPalette(c("#ffffcc", "#a1dab4", 
                                                             "#41b6c4", "#2c7fb8", 
                                                             "#253494"))
  if (det == "Ammoniacal Nitrogen") colours <- colorRampPalette(c("#ffffb2", "#fecc5c", 
                                                                  "#fd8d3c", "#f03b20", 
                                                                  "#bd0026"))
  if (det == "Total Phosphorus") colours <- colorRampPalette(c("#ffffcc", "#c2e699", 
                                                               "#78c679", "#31a354", 
                                                               "#006837"))
  if (det == "TON") colours <- colorRampPalette(c("#ffffd4", "#fed98e", 
                                                  "#fe9929", "#d95f0e", 
                                                  "#993404"))
  if (det == "Suspended Solids") colours <- colorRampPalette(c("#feebe2", "#fbb4b9", 
                                                               "#f768a1", "#c51b8a", 
                                                               "#7a0177"))
  mapdata@data$colour <- colours(segs)[as.numeric(cut(mapdata@data[, column], breaks = segs))]
  oldpar <- par(no.readonly = TRUE)
  par(mar = c(0, 0, 1, 0))
  plot(targets, col = mapdata@data$col, border = NA)
  lines(UK, col = "lightgrey")
  par(oldpar)
  usr <- par("usr")
  if (column >= startofreducts){
    legtext <- expression(paste("Percent improvement (*100)"))
  }
  else {legtext <- expression(paste("Concentration ", "mgl" ^ -1))}
  subplot(ColourBar(colours(segs), min = round(min(mapdata@data[, column]), 2), 
                    max = round(max(mapdata@data[, column]), 2), 
                    title = legtext),
          x = usr[2]/1.25, y = usr[4]/1, vadj = 1, hadj= 1, pars = list(cex = 0.7))
  text(x = usr[2]/8, y = usr[4]/1.3, labels = title, font = 2, cex = 1.2)
}

#UK <- readShapeLines("~/ShinyApps/CSF_web_regman/UK_line.shp")
UK <- readShapeLines("C:/Users/jdouglass/Desktop/RRegman test/UK_line.shp")
#targets <- readShapePoly("~/ShinyApps/CSF_web_regman/Phase123_TAs_merged.shp")
targets <- readShapePoly("C:/Users/jdouglass/Desktop/RRegman test/Phase123_TAs_merged.shp")

CSFMaps(UK = UK, targets = targets, preds = amm.preds, sce = "head.reduct", title = "test", 
        det  = "Ammoniacal Nitrogen")
names(amm.preds)

