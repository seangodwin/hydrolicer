# 3_fig2.R
# Author: Sean Godwin
# Date: 2026-05-10
# Description: Map of sampling sites

rm(list=ls())

## 0 [LOAD PACKAGES] -----------------------------------------------------------
library(here)        # file referencing
library(terra)       # spatial data manipulation
library(data.table)  # for fread()


## 1 [READ IN DATA] ------------------------------------------------------------
# Identify root directory
here::i_am("code/3_fig2.R")




### Old code below to adjust for this map later

# # Load sampling sites
# sites <- fread(here::here("./data/processed/herring_sites.csv"))
# 
# # Add jitter to latitudes protect site locations
# set.seed(3)
# sites[, lon := jitter(lon, amount = 0.015)]
# 
# # Convert to spatial vector
# sites <- vect(sites, geom = c("lon", "lat"), crs = "EPSG:4326")
# 
# # Read in spatial data
# states <- vect(here::here("data/processed/states.gpkg"))
# provinces <- vect(here::here("data/processed/provinces.gpkg"))
# bath <- rast(here::here("./data/processed/bathymetry.tif"))
# 
# 
# ## 2 [MAP INPUTS] --------------------------------------------------------------
# # Bathymetry colours
# col.bath <- rev(colorRampPalette(c("#E8F4F9", "#A3CDE2", "#31789D"))(100))
# 
# # Other plot inputs
# col.land.inset <- "grey18"
# col.land.main <- "grey25"
# col.border <- "white"   # Canada / US border
# col.label <- "white"
# col.tick <- "white"
# col.pt <- "#e69b99"
# col.sea <- "#c4E9FF"
# col.extent <- col.pt 
# col.scalebar <- "white"
# lwd.extent <- 4
# lwd.axis <- 0
# lwd.tick <- 2
# length.tick.sm <- 0.010
# length.tick.lg <- 0.025
# gap.tick.sm <- 0.1
# gap.tick.lg <- 1
# cex.pt <- 1.85
# cex.ctry <- 0.7
# alpha.pt <- 0.9
# pch.pt <- 23
# 
# # Plot extents (roughly, since dimensions will override)
# main.ext <- ext(-124.15, -122.8, 47.9, 48.68)
# inset.ext <- ext(-133, -120, 45, 55)
# 
# # Aspect ratios for geographic coordinates
# asp.main  <- 1 / cos(mean(main.ext[3:4])  * pi/180)
# asp.inset <- 1 / cos(mean(inset.ext[3:4]) * pi/180)
# 
# # Define major and minor tick positions
# lon.major <- seq(floor(main.ext[1])-1, ceiling(main.ext[2])+1, by=gap.tick.lg)
# lon.minor <- seq(floor(main.ext[1])-1, ceiling(main.ext[2])+1, by=gap.tick.sm)
# lat.major <- seq(floor(main.ext[3])-1, ceiling(main.ext[4])+1, by=gap.tick.lg)
# lat.minor <- seq(floor(main.ext[3])-1, ceiling(main.ext[4])+1, by=gap.tick.sm)
# 
# 
# ## 3 [MAP] ---------------------------------------------------------------------
# tiff(here::here("./outputs/figs/map.tiff"), width=10, height=7, units="in",
#      pointsize=20, res=600, compression="lzw")
# par(mar=c(0,0,0,0), tck=-0.03, mgp=c(3,0.5,0), family="sans")
# 
#   # Empty plot
#   plot(0, type="n", 
#        xlim=main.ext[1:2], ylim=main.ext[3:4], 
#        asp=asp.main,
#        xaxs="i", yaxs="i", 
#        axes=FALSE, xlab="", ylab="", frame.plot=FALSE)
# 
#   # Get real extent, since the plot dimensions override main.ext
#   real.ext <- ext(par("usr"))
#   
#   # Ocean (no bathymetry for this one, not enough variation anyway)
#   rect(xleft = xmin(main.ext)-1,
#        xright = xmax(main.ext)+1,
#        ybottom = ymin(main.ext)-1,
#        ytop = ymax(main.ext)+1,
#        col = col.sea,
#        border = NA)
#   
#   # States and provinces
#   plot(provinces, add=T,
#        col=col.land.main, border=NA)
#   plot(states, add=T,
#        col=col.land.main, border=NA)
#   
#   # Sampling sites
#   plot(sites, add=TRUE, pch=pch.pt, 
#        bg=adjustcolor(col.pt, alpha.f=alpha.pt), 
#        col=adjustcolor("black", alpha.f=alpha.pt), 
#        cex=cex.pt)
#   
#   # Scale bar
#   scalebar.km <- 20
#   lat.mid <- mean(real.ext[3:4])
#   km.per.deg.lon <- 111.32 * cos(lat.mid * pi/180)
#   scalebar.deg <- scalebar.km / km.per.deg.lon
#   
#   # Draw scale bar
#   x0 <- real.ext[1] + 0.5
#   y0 <- real.ext[3] + 0.04
#   segments(x0, y0, x0 + scalebar.deg, y0,
#            col=col.scalebar, lwd=3, lend="butt")
#   
#   # Scale bar end ticks
#   segments(x0 + 0.0015, y0, x0 + 0.0015, y0 + 0.01, col=col.scalebar, lwd=2)
#   segments(x0 + scalebar.deg - 0.0015, y0, x0 + scalebar.deg - 0.0015, y0 + 0.01,
#            col=col.scalebar, lwd=2)
#   
#   # Scale bar label
#   text(x0 + scalebar.deg/2, y0 + 0.02,
#        labels=paste0(scalebar.km, " km"),
#        col=col.scalebar, cex=0.6)
#   
#   # Bottom axis ticks
#   axis(1, at=lon.major, labels=FALSE, tck=length.tick.lg, 
#        col=col.tick, lwd=lwd.axis, lwd.ticks=lwd.tick)
#   axis(1, at=lon.minor, labels=FALSE, tck=length.tick.sm, 
#        col=col.tick, lwd=lwd.axis, lwd.ticks=lwd.tick)
#   
#   # Right axis ticks
#   axis(4, at=lat.major, labels=FALSE, tck=length.tick.lg, 
#        col=col.tick, lwd=lwd.axis, lwd.ticks=lwd.tick)
#   axis(4, at=lat.minor, labels=FALSE, tck=length.tick.sm, 
#        col=col.tick, lwd=lwd.axis, lwd.ticks=lwd.tick)
#   
#   # Border
#   box()
#   
#   ## Inset
#   # Define inset position (x1, x2, y1, y2 in figure coordinates)
#   par(fig=c(0.03, 0.28, 0.05, 0.57), new=TRUE, mar=c(0,0,0,0))
#   
#   # Empty inset plot
#   plot(0, type="n",
#        xlim=inset.ext[1:2],
#        ylim=inset.ext[3:4],
#        asp=asp.inset,
#        xaxs="i", yaxs="i",
#        axes=FALSE, xlab="", ylab="")
#   
#   # Add bathymetry
#   plot(bath, add=T,
#        col=col.bath, legend=F)
#   
#   # Plot land
#   plot(states, add=TRUE, col=col.land.inset, border=NA)
#   plot(provinces, add=TRUE, col=col.land.inset, border=NA)
#   
#   # Canada / US border, starting from Blaine
#   lines(x=c(-122.758159, -110),
#         y=c(49, 49),
#         col=col.border,
#         lwd=0.6)
#   
#   # Highlight study region
#   rect(xleft = real.ext[1],
#        xright = real.ext[2],
#        ybottom = real.ext[3],
#        ytop = real.ext[4],
#        border = col.extent, lwd = lwd.extent)
#   rect(xleft = real.ext[1],
#        xright = real.ext[2],
#        ybottom = real.ext[3],
#        ytop = real.ext[4],
#        border = "black", lty=3, lwd=1)
#   
#   # Country labels
#   text(x=-123, y=52, labels = "Canada", family="sans", 
#        cex=cex.ctry, col=col.label)
#   text(x=-121.5, y=46, labels = "USA", family="sans", 
#        cex=cex.ctry, col=col.label)
#   
#   # Inset border
#   box()
#   
# dev.off()