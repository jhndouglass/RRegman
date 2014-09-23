library(shiny)
library(plotrix)
library(lme4)
library(sp)
library(maptools)
library(Hmisc)
library(GISTools)

# functions
csf_rect <- function(line.loc, labels, Title, det, limit = NA,
                     useLimit = FALSE, ...){
        require(plotrix)
        if (class(Title) != "character") {stop("Title must be character")}
        if (useLimit == FALSE) {limit <- NA}
        if (useLimit == TRUE && is.na(limit)) {limit <- 0}
        if (!det %in% c("Ammoniacal Nitrogen", "Orthophosphate",
                        "Total Phosphorus", "TON", "Suspended Solids")){
                stop("invalid 'det' string")
        }
        col <- switch(det, "Ammoniacal Nitrogen" = "#bd0026",
                      "Orthophosphate" = "#253494",
                      "Total Phosphorus" = "#006837",
                      "TON" = "#993404", "Suspended Solids" = "#7a0177")
        if (length(line.loc) != length(labels)) {
                stop("'lines' and 'labels' must be vectors of equal length")
        }
        if (class(line.loc) != "numeric" && class(labels) != "character"){
                stop("'lines' must of class 'numeric' and 'labels' of class
                     'character'")
        }
        #base plot
        bounds <- list(x1 = 0.1, x2 = 0.3, y1 = 0, y2 = 1)
        RectPlot(Title, col, bounds, ...)
        # plot details
        if (useLimit == TRUE){
                line.loc <- as.numeric(c(line.loc, limit))
                labels <- c(labels, "User limit")
        }
        high <- max(line.loc, na.rm = TRUE)
        scale <- 0.95
        plot.loc <- (line.loc - 0)/(high - 0) * scale
        for (i in 1:length(plot.loc)){
                lines(x = c(bounds[[1]], bounds[[2]]),
                      y = c(plot.loc[i], plot.loc[i]),
                      lwd = 1, col = "black")
        }
        lab.disp <- paste(round(line.loc, 3), labels)
        spread.labels.simple(x = rep((bounds[[2]]), length(plot.loc)),
                             y = as.numeric(plot.loc), labels = lab.disp)
}

RectPlot <- function(Title, col, bounds, ...) {
        plot.new()
        title(main = Title)
        x1 <- bounds[[1]]
        x2 <- bounds[[2]]
        y1 <- bounds[[3]]
        y2 <- bounds[[4]]
        gradient.rect(x1, y1, x2, y2, col = smoothColors("gray95", 999, col),
                      border = NA, gradient = "y")
        text(y = y1 + 0.02, x = x1 - 0.05,
             labels = expression(paste("0 mgl" ^-1)), ...)
        text(y = 0.5, x = x1 - 0.1,
             labels = expression(paste("Increasing concentration",
                                       "  (mgl" ^-1, ")")), srt = 90, ...)
        arrows(y0 = 0.2, x0 = x1 - 0.02, y1 = 0.8, length = 0.1, lwd = 1.5, ...)
}

spread.labels.simple <- function (x, y, labels = NULL, linecol = par("fg"),
                                  srt = 0, ...) {
        if (missing(x))
                stop("Usage: spread.labels(x,y,labels,...)")
        nx <- length(x)
        ny <- length(y)

        sort.index <- sort.list(y)
        x <- x[sort.index]
        y <- y[sort.index]
        newy <- seq(y[1], y[ny], length = length(labels))

        offsets <- rep(0.15, nx)

        segments(x + offsets, newy, x, y, col = "black", lwd = 1, lty = 3)
        text(x + offsets, newy, labels[sort.index], srt = srt,
             pos = 4, ...)

}

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
        copyright = "© Crown copyright and database rights 2013 Ordnance Survey 100024198
© Environment Agency copyright and/or database rights 2013. All rights reserved."
        oldpar <- par(no.readonly = TRUE)
        par(mar = c(2, 0, 3, 0))
        usr <- par("usr")
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
        plot(targets, col = mapdata@data$col, border = NA)
        lines(UK, col = "lightgrey")
        usr <- par("usr")
        north.arrow(x = usr[2]/ 1.2, y = usr[4]/10, len = 7000, "North")
        mtext(side = 1, text = copyright, cex = 0.5)
        title(main = title)
        if (column >= startofreducts){
                legtext <- expression(paste("Percent improvement (*100)"))
        }
        else {legtext <- expression(paste("Concentration ", "mgl" ^ -1))}
        subplot(ColourBar(colours(segs), min = round(min(mapdata@data[, column]), 2),
                          max = round(max(mapdata@data[, column]), 2),
                          title = legtext),
                x = usr[2]/1.1, y = usr[4]/1.1, vadj = 1, hadj= 1, pars = list(cex = 0.65))
        par(oldpar)
}

load("amm.rdata")
load("op.rdata")
load("tp.rdata")
load("ton.rdata")
load("ss.rdata")
load("moddat.rdata")
data_book <- read.csv("data_book.csv")

UK <- readShapeLines(paste0(getwd(), "/UK_line.shp"))
targets <- readShapePoly(paste0(getwd(), "/Phase123_TAs_merged.shp"))

source("TA_catches_predictions_v1.R")

disptext <- c("Target Area",
              "Baseline concentration", "Current concentration",
              "Extrapolated concentration", "Optimised concentration",
              "Maximum CSF concentration",
              "Maximum agricultural benefit concentration",
              "Headroom concentration",
              "Current percentage reduction", "Extrapolated percentage reduction",
              "Opmitmised percentage reduction",
              "Maximum CSF percentage reduction",
              "Maximum agricultural benefit percentage reduction",
              "Headroom percentage reduction")

choicetext = c("NULL_space", "base.out","curr.out", "extrap.out", "optim.out", "max.out",
               "maxben.out", "head.out", "curr.reduct",
               "extrap.reduct", "optim.reduct", "max.reduct", "maxben.reduct",
               "head.reduct")

shinyServer(function(input, output, session){

        datInput <- reactive({
                switch(input$det, "Orthophosphate" = opdat, "Total Phosphorus" = tpdat,
                       "Ammoniacal Nitrogen" = ammdat, "TON" = tondat,
                       "Suspended Solids" = ssdat)
        })

        predInput <- reactive({
                switch(input$det, "Orthophosphate" = op.preds, "Total Phosphorus" = tp.preds,
                       "Ammoniacal Nitrogen" = amm.preds, "TON" = ton.preds,
                       "Suspended Solids" = ss.preds)
        })

        modInput <- reactive({
                switch(input$det, "Orthophosphate" = op, "Total Phosphorus" = tp,
                       "Ammoniacal Nitrogen" = amm, "TON" = ton,
                       "Suspended Solids" = ss)
        })

        ###GRAPHS###
        output$graph_title_a <- renderText({
                paste("Catchment:   ", input$site)
        })

        output$graph_title_b <- renderText({
                paste("Determinand:   ", input$det)
        })

        output$graph_title_c <- renderText({
                paste("CSF scenarios summary plot. Each line
          represents the pedicted mean water quality across the target area
          for a given scenario.")
        })

        output$rect_graph <- renderPlot({
                par(mar = c(0, 0, 1, 0))
                dat <- predInput()
                dat <- dat[dat$CATCHMENT == input$site, c(2, 4, 6, 8, 10, 12, 14)]
                names(dat) <- c("baseline", "current", "extrapolated", "optimised",
                                "maximum CSF", "max agriculture benefit", "headroom")
                csf_rect(dat, labels = names(dat),
                         Title = paste(input$site, input$det),
                         det = input$det, limit = input$eqs, useLimit = input$lim)
        })

        output$graph_dl <- downloadHandler(
                filename = function() {
                        paste(Sys.Date(), input$det, input$site ,"_CSF_scenario_graph.png", sep = "")
                },
                content = function(file){
                        png(file, width = 1200, height = 1500, res = 175)
                        dat <- predInput()
                        dat <- dat[dat$CATCHMENT == input$site, c(2, 4, 6, 8, 10, 12, 14)]
                        names(dat) <- c("baseline", "current", "extrapolated", "optimised",
                                        "maximum CSF", "max agriculture benefit", "headroom")
                        csf_rect(dat, labels = names(dat),
                                 Title = paste(input$site, input$det),
                                 det = input$det, limit = input$eqs, useLimit = input$lim)
                        dev.off()
                }
        )

        ###TABLES###
        output$tab_title1a <- renderText({
                paste("Catchment:    ", input$site)
        })

        output$tab_title1b <- renderText({
                paste("Determinand:   ", input$det)

        })

        output$tab_title1c <- renderText({
                paste("CSF scenarios concentration summary table. Each row
          gives the pedicted mean water quality across the target area (in mg/l)")

        })

        output$tab_dat1 <- renderTable({
                disp.tab <- predInput()
                disp.tab <- disp.tab[disp.tab$CATCHMENT == input$site, c("CATCHMENT", "base.out","curr.out",
                                                                         "extrap.out", "optim.out",
                                                                         "max.out", "maxben.out",
                                                                         "head.out")]
                disp.tab[, c(2:8)] <- signif(disp.tab[, c(2:8)], 2)
                names(disp.tab) <- disptext[c(1:8)]
                disp.tab
        },
        include.rownames = FALSE, digits = 3)

        output$tab_title2a <- renderText({
                paste("Catchment:   ", input$site)
        })

        output$tab_title2b <- renderText({
                paste("Determinand:   ", input$det)
        })

        output$tab_title2c <- renderText({
                paste("CSF scenarios percent reductions summary table. Each row
    gives predicted percentage reduction from the baseline for a given scenario.")
        })

        output$tab_dat2 <- renderTable({
                disp.tab2 <- predInput()
                disp.tab2 <- disp.tab2[disp.tab2$CATCHMENT == input$site, c("CATCHMENT", "curr.reduct",
                                                                            "extrap.reduct", "optim.reduct",
                                                                            "max.reduct", "maxben.reduct",
                                                                            "head.reduct")]
                names(disp.tab2) <- disptext[c(1, 9:14)]
                disp.tab2[, c(2:7)] <- disp.tab2[, c(2:7)] * 100
                disp.tab2
        },
        include.rownames = FALSE)


        output$tab_dl <- downloadHandler(
                filename = function() {
                        paste(Sys.Date(), input$det, "_CSF_scenario.csv", sep = "")
                },
                content = function(file){
                        write.csv(x = predInput(), file, row.names = FALSE)
                }
        )
        output$datbook_dl <- downloadHandler(
                filename = function() {
                        paste("data_book.csv", sep = "")
                },
                content = function(file){
                        write.csv(x = data_book, file, row.names = FALSE)
                }
        )

        ###MAPS###
        output$map_title_a <- renderText({
                index <- which(choicetext == input$sce)
                paste("Scenario:   ", disptext[index])
        })

        output$map_title_b <- renderText({
                index <- which(choicetext == input$sce)
                paste("Determinand:   ", input$det)
        })

        output$map_title_c <- renderText({
                index <- which(choicetext == input$sce)
                paste("England and Wales map showing spatial
          pattern of concentrations or reductions for your chosen scenrio")
        })

        output$map <- renderPlot(expr = {
                index <- which(choicetext == input$sce)
                CSFMaps(UK, targets,  preds = predInput(), sce = input$sce,
                        title = paste(disptext[index], "\n", input$det), det = input$det)
        })

        output$map_dl <- downloadHandler(
                index <- which(choicetext == input$sce),
                filename = function() {
                        paste(Sys.Date(), "_", disptext[index], input$det,
                              "_CSF_scenario_map.png", sep = "")
                },
                content = function(file){
                        png(file, width = 3000, height = 3000, res = 250)
                        CSFMaps(UK, targets,  preds = predInput(), sce = input$sce,
                                title = paste(disptext[index], "\n", input$det), det = input$det)
                        dev.off()
                }
        )

        ###MODELS###
        output$mod_title <- renderText({
                paste(input$det, "model diagnostic plots and summary")
        })

        output$mod_diag <- renderPlot({
                par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))
                plot(modInput())
        })

        output$mod_summ <- renderPrint({
                summary(modInput())
        })

        output$mod_dl <- downloadHandler(
                filename = function() {
                        paste(Sys.Date(), "_", input$det, "_R_glm_object.rdata", sep = "")
                },
                content = function(file){
                        mod <- modInput()
                        save(x = mod, file = file)
                }
        )
})