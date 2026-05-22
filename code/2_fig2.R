# 2_fig2.R
# Author: Sean Godwin
# Date: 2026-05-22
# Description: Map of sampling sites

rm(list=ls())

## 0 [LOAD PACKAGES] -----------------------------------------------------------
library(here)        # file referencing
library(terra)       # spatial data manipulation
library(data.table)  # for fread()


## 1 [READ IN DATA] ------------------------------------------------------------
# Identify root directory
here::i_am("code/2_fig2.R")

# Load sampling sites
sites <- fread(here::here("./data/raw/farm_locations.csv"))

# Convert to spatial vector
sites <- vect(sites, geom = c("lon", "lat"), crs = "EPSG:4326")

# Read in spatial data
states <- vect(here::here("./data/processed/states.gpkg"))
provinces <- vect(here::here("./data/processed/provinces.gpkg"))
bath <- rast(here::here("./data/processed/bathymetry.tif"))


## 2 [MAP INPUTS] --------------------------------------------------------------
# Bathymetry colours
col.bath <- rev(colorRampPalette(c("#E8F4F9", "#A3CDE2", "#31789D"))(100))

# # Other plot inputs
col.land.inset <- "grey25"
col.land.main <- "grey25"
col.border <- "grey90"   # Canada / US border
col.label <- "white"
col.tick <- "white"
col.pt <- "#f2af4a"
col.sea <- "#ADD2E5" #"#c4E9FF"
col.extent <- col.pt
col.scalebar <- "white"
lwd.extent <- 4
lwd.extent.dashed <- 2
lwd.axis <- 0
lwd.tick <- 2
length.tick.sm <- 0.01
length.tick.lg <- 0.02
gap.tick.sm <- 0.5
gap.tick.lg <- 1
cex.pt <- 1.4
cex.ctry <- 1.2
cex.region <- 0.8
alpha.pt <- 1
pch.pt <- 21

# Plot extents (roughly, since image dimensions will override)
main.ext <- ext(-128.5, -122.5, 47, 53.5)
cs.ext <- ext(-126.3, -125.55, 49.27, 49.35)
ba.ext <- ext(-126.9, -126.1, 50.5, 51.02)

# Aspect ratios for geographic coordinates
asp.main  <- 1 / cos(mean(main.ext[3:4])  * pi/180)
asp.cs <- 1 / cos(mean(cs.ext[3:4]) * pi/180)
asp.ba <- 1 / cos(mean(ba.ext[3:4]) * pi/180)

# Define major and minor tick positions
lon.major <- seq(floor(main.ext[1])-5, ceiling(main.ext[2])+5, by=gap.tick.lg)
lon.minor <- seq(floor(main.ext[1])-5, ceiling(main.ext[2])+5, by=gap.tick.sm)
lat.major <- seq(floor(main.ext[3])-5, ceiling(main.ext[4])+5, by=gap.tick.lg)
lat.minor <- seq(floor(main.ext[3])-5, ceiling(main.ext[4])+5, by=gap.tick.sm)


# ## 3 [MAP] ---------------------------------------------------------------------
tiff(here::here("./outputs/figs/fig2.tiff"), width=10, height=7, units="in",
     pointsize=20, res=600, compression="lzw")
par(mar=c(0,0,0,0), tck=-0.03, mgp=c(3,0.5,0), family="sans")

  ## Main map
  # Empty plot
  plot(0, type="n",
       xlim=main.ext[1:2], ylim=main.ext[3:4],
       asp=asp.main,
       xaxs="i", yaxs="i",
       axes=FALSE, xlab="", ylab="", frame.plot=FALSE)
  
  # Bathymetry
  plot(bath, add = TRUE,
       col = col.bath, legend=F)
  # 
  # Get real extent, since the plot dimensions override main.ext
  real.ext <- ext(par("usr"))

  # # Ocean (no bathymetry)
  # rect(xleft = xmin(main.ext)-5,
  #      xright = xmax(main.ext)+1,
  #      ybottom = ymin(main.ext)-1,
  #      ytop = ymax(main.ext)+1,
  #      col = col.sea,
  #      border = NA)

  # States and provinces
  plot(provinces, add=T,
       col=col.land.main, border=NA)
  plot(states, add=T,
       col=col.land.main, border=NA)
  
  # Canada / US border, starting from Blaine
  lines(x=c(-122.758159, -110),
        y=c(49, 49),
        col=col.border,
        lwd=0.6)
  
  # Country labels
  text(x=-121.5, y=50, labels = "Canada", family="sans",
       cex=cex.ctry, col=col.label)
  text(x=-121, y=48, labels = "USA", family="sans",
       cex=cex.ctry, col=col.label)

  # Calculate scale bar
  scalebar.km <- 100
  lat.mid <- mean(real.ext[3:4])
  km.per.deg.lon <- 111.32 * cos(lat.mid * pi/180)
  scalebar.deg <- scalebar.km / km.per.deg.lon

  # Draw scale bar
  x0 <- real.ext[2] - 2
  y0 <- real.ext[3] + 0.3
  segments(x0, y0, x0 + scalebar.deg, y0,
           col=col.scalebar, lwd=3, lend="butt")

  # Scale bar end ticks
  segments(x0 + 0.0015, y0, x0 + 0.0015, y0 + 0.01, col=col.scalebar, lwd=2)
  segments(x0 + scalebar.deg - 0.0015, y0, x0 + scalebar.deg - 0.0015, y0 + 0.01,
           col=col.scalebar, lwd=2)

  # Scale bar label
  text(x0 + scalebar.deg/2, y0 + 0.12,
       labels=paste0(scalebar.km, " km"),
       col=col.scalebar, cex=0.6)

  # Bottom axis ticks
  axis(1, at=lon.major, labels=FALSE, tck=length.tick.lg,
       col=col.tick, lwd=lwd.axis, lwd.ticks=lwd.tick)
  axis(1, at=lon.minor, labels=FALSE, tck=length.tick.sm,
       col=col.tick, lwd=lwd.axis, lwd.ticks=lwd.tick)

  # Right axis ticks
  axis(4, at=lat.major, labels=FALSE, tck=length.tick.lg,
       col=col.tick, lwd=lwd.axis, lwd.ticks=lwd.tick)
  axis(4, at=lat.minor, labels=FALSE, tck=length.tick.sm,
       col=col.tick, lwd=lwd.axis, lwd.ticks=lwd.tick)

  # Save plotting state for later
  main.par <- par(no.readonly=TRUE)
  
  
  ## Clayoquot Inset
  # Define inset position (x1, x2, y1, y2 in figure coordinates)
  par(fig=c(0.03, 0.35, 0.05, 0.4), new=TRUE, mar=c(0,0,0,0))

  # Empty plot
  plot(0, type="n",
       xlim=cs.ext[1:2],
       ylim=cs.ext[3:4],
       asp=asp.cs,
       xaxs="i", yaxs="i",
       axes=FALSE, xlab="", ylab="")

  # Ocean (minimum of bathymetry anyway)
  rect(xleft = xmin(main.ext)-5,
       xright = xmax(main.ext)+1,
       ybottom = ymin(main.ext)-1,
       ytop = ymax(main.ext)+1,
       col = col.sea,
       border = NA)

  # Plot land
  plot(states, add=TRUE, col=col.land.inset, border=NA)
  plot(provinces, add=TRUE, col=col.land.inset, border=NA)
  
  # Sampling sites
  plot(sites, add=TRUE, pch=pch.pt,
       bg=adjustcolor(col.pt, alpha.f=alpha.pt),
       col=adjustcolor("black", alpha.f=alpha.pt),
       cex=cex.pt)

  # Region label
  usr <- par("usr")
  cs.real.ext <- ext(usr)  # actual rendered extent (for later)
  text(x = mean(usr[1:2]), y = usr[4] - 0.06 * diff(usr[3:4]), 
       labels = "Clayoquot Sound", family="sans",
       cex=cex.region, col=col.label)
  
  # Border
  box()
  
  
  ## Broughton Inset
  # Define inset position (x1, x2, y1, y2 in figure coordinates)
  par(fig=c(0.62, 0.95, 0.62, 0.97), new=TRUE, mar=c(0,0,0,0))
  
  # Empty plot
  plot(0, type="n",
       xlim=ba.ext[1:2],
       ylim=ba.ext[3:4],
       asp=asp.ba,
       xaxs="i", yaxs="i",
       axes=FALSE, xlab="", ylab="")
  
  # Ocean (minimum of bathymetry anyway)
  rect(xleft = xmin(main.ext)-5,
       xright = xmax(main.ext)+1,
       ybottom = ymin(main.ext)-1,
       ytop = ymax(main.ext)+1,
       col = col.sea,
       border = NA)
  
  # Plot land
  plot(states, add=TRUE, col=col.land.inset, border=NA)
  plot(provinces, add=TRUE, col=col.land.inset, border=NA)
  
  # Sampling sites
  plot(sites, add=TRUE, pch=pch.pt,
       bg=adjustcolor(col.pt, alpha.f=alpha.pt),
       col=adjustcolor("black", alpha.f=alpha.pt),
       cex=cex.pt)
  
  # Region label
  usr <- par("usr")
  ba.real.ext <- ext(usr)  # actual rendered extent (for later)
  text(x = mean(usr[1:2]), y = usr[4] - 0.06 * diff(usr[3:4]), 
       labels = "Broughton Archipelago", family="sans",
       cex=cex.region, col=col.label)
  
  # Border
  box()
  
  
  ## Inset extent# 2_fig2.R
# Author: Sean Godwin
# Date: 2026-05-22
# Description: Map of sampling sites

rm(list=ls())

## 0 [LOAD PACKAGES] -----------------------------------------------------------
library(here)        # file referencing
library(terra)       # spatial data manipulation
library(data.table)  # for fread()


## 1 [READ IN DATA] ------------------------------------------------------------
# Identify root directory
here::i_am("code/2_fig2.R")

# Load sampling sites
sites <- fread(here::here("./data/raw/farm_locations.csv"))

# Convert to spatial vector
sites <- vect(sites, geom = c("lon", "lat"), crs = "EPSG:4326")

# Read in spatial data
states <- vect(here::here("./data/processed/states.gpkg"))
provinces <- vect(here::here("./data/processed/provinces.gpkg"))
bath <- rast(here::here("./data/processed/bathymetry.tif"))


## 2 [MAP INPUTS] --------------------------------------------------------------
# Bathymetry colours
col.bath <- rev(colorRampPalette(c("#E8F4F9", "#A3CDE2", "#31789D"))(100))

# # Other plot inputs
col.land.inset <- "grey25"
col.land.main <- "grey25"
col.border <- "grey90"   # Canada / US border
col.label <- "white"
col.tick <- "white"
col.pt <- "#f2af4a"
col.sea <- "#ADD2E5" #"#c4E9FF"
col.extent <- col.pt
col.scalebar <- "white"
lwd.extent <- 4
lwd.extent.dashed <- 2
lwd.axis <- 0
lwd.tick <- 2
length.tick.sm <- 0.01
length.tick.lg <- 0.02
gap.tick.sm <- 0.5
gap.tick.lg <- 1
cex.pt <- 1.4
cex.ctry <- 1.2
cex.ext.label <- 0.55
cex.region <- 0.8
alpha.pt <- 1
pch.pt <- 21

# Plot extents (roughly, since image dimensions will override)
main.ext <- ext(-128.5, -122.5, 47, 53.5)
cs.ext <- ext(-126.3, -125.55, 49.27, 49.35)
ba.ext <- ext(-126.9, -126.1, 50.5, 51.02)

# Aspect ratios for geographic coordinates
asp.main  <- 1 / cos(mean(main.ext[3:4])  * pi/180)
asp.cs <- 1 / cos(mean(cs.ext[3:4]) * pi/180)
asp.ba <- 1 / cos(mean(ba.ext[3:4]) * pi/180)

# Define major and minor tick positions
lon.major <- seq(floor(main.ext[1])-5, ceiling(main.ext[2])+5, by=gap.tick.lg)
lon.minor <- seq(floor(main.ext[1])-5, ceiling(main.ext[2])+5, by=gap.tick.sm)
lat.major <- seq(floor(main.ext[3])-5, ceiling(main.ext[4])+5, by=gap.tick.lg)
lat.minor <- seq(floor(main.ext[3])-5, ceiling(main.ext[4])+5, by=gap.tick.sm)


# ## 3 [MAP] ---------------------------------------------------------------------
tiff(here::here("./outputs/figs/fig2.tiff"), width=10, height=7, units="in",
     pointsize=20, res=600, compression="lzw")
par(mar=c(0,0,0,0), tck=-0.03, mgp=c(3,0.5,0), family="sans")

  ## Main map
  # Empty plot
  plot(0, type="n",
       xlim=main.ext[1:2], ylim=main.ext[3:4],
       asp=asp.main,
       xaxs="i", yaxs="i",
       axes=FALSE, xlab="", ylab="", frame.plot=FALSE)
  
  # Bathymetry
  plot(bath, add = TRUE,
       col = col.bath, legend=F)
  
  # Get real extent, since the plot dimensions override main.ext
  real.ext <- ext(par("usr"))

  # # Ocean (no bathymetry)
  # rect(xleft = xmin(main.ext)-5,
  #      xright = xmax(main.ext)+1,
  #      ybottom = ymin(main.ext)-1,
  #      ytop = ymax(main.ext)+1,
  #      col = col.sea,
  #      border = NA)

  # States and provinces
  plot(provinces, add=T,
       col=col.land.main, border=NA)
  plot(states, add=T,
       col=col.land.main, border=NA)
  
  # Canada / US border, starting from Blaine
  lines(x=c(-122.758159, -110),
        y=c(49, 49),
        col=col.border,
        lwd=0.6)
  
  # Country labels
  text(x=-121.5, y=50, labels = "Canada", family="sans",
       cex=cex.ctry, col=col.label)
  text(x=-121, y=48, labels = "USA", family="sans",
       cex=cex.ctry, col=col.label)

  # Calculate scale bar
  scalebar.km <- 100
  lat.mid <- mean(real.ext[3:4])
  km.per.deg.lon <- 111.32 * cos(lat.mid * pi/180)
  scalebar.deg <- scalebar.km / km.per.deg.lon

  # Draw scale bar
  x0 <- real.ext[2] - 2
  y0 <- real.ext[3] + 0.3
  segments(x0, y0, x0 + scalebar.deg, y0,
           col=col.scalebar, lwd=3, lend="butt")

  # Scale bar end ticks
  segments(x0 + 0.0015, y0, x0 + 0.0015, y0 + 0.01, col=col.scalebar, lwd=2)
  segments(x0 + scalebar.deg - 0.0015, y0, x0 + scalebar.deg - 0.0015, y0 + 0.01,
           col=col.scalebar, lwd=2)

  # Scale bar label
  text(x0 + scalebar.deg/2, y0 + 0.12,
       labels=paste0(scalebar.km, " km"),
       col=col.scalebar, cex=0.6)

  # Bottom axis ticks
  axis(1, at=lon.major, labels=FALSE, tck=length.tick.lg,
       col=col.tick, lwd=lwd.axis, lwd.ticks=lwd.tick)
  axis(1, at=lon.minor, labels=FALSE, tck=length.tick.sm,
       col=col.tick, lwd=lwd.axis, lwd.ticks=lwd.tick)

  # Right axis ticks
  axis(4, at=lat.major, labels=FALSE, tck=length.tick.lg,
       col=col.tick, lwd=lwd.axis, lwd.ticks=lwd.tick)
  axis(4, at=lat.minor, labels=FALSE, tck=length.tick.sm,
       col=col.tick, lwd=lwd.axis, lwd.ticks=lwd.tick)

  # Border
  box()
  
  # Save plotting state for later
  main.par <- par(no.readonly=TRUE)
  
  
  ## Clayoquot Inset
  # Define inset position (x1, x2, y1, y2 in figure coordinates)
  par(fig=c(0.03, 0.35, 0.05, 0.4), new=TRUE, mar=c(0,0,0,0))

  # Empty plot
  plot(0, type="n",
       xlim=cs.ext[1:2],
       ylim=cs.ext[3:4],
       asp=asp.cs,
       xaxs="i", yaxs="i",
       axes=FALSE, xlab="", ylab="")

  # Ocean (minimum of bathymetry anyway)
  rect(xleft = xmin(main.ext)-5,
       xright = xmax(main.ext)+1,
       ybottom = ymin(main.ext)-1,
       ytop = ymax(main.ext)+1,
       col = col.sea,
       border = NA)

  # Plot land
  plot(states, add=TRUE, col=col.land.inset, border=NA)
  plot(provinces, add=TRUE, col=col.land.inset, border=NA)
  
  # Sampling sites
  plot(sites, add=TRUE, pch=pch.pt,
       bg=adjustcolor(col.pt, alpha.f=alpha.pt),
       col=adjustcolor("black", alpha.f=alpha.pt),
       cex=cex.pt)

  # Region label
  usr <- par("usr")
  cs.real.ext <- ext(usr)  # actual rendered extent (for later)
  text(x = mean(usr[1:2]), y = usr[4] - 0.06 * diff(usr[3:4]), 
       labels = "Clayoquot Sound", family="sans",
       cex=cex.region, col=col.label)
  
  # Inset label
  text(x = usr[2] - 0.03 * diff(usr[1:2]), y = usr[4] - 0.04 * diff(usr[3:4]), 
       labels = "†", family="sans",
       cex=cex.ext.label, col=col.label)
  
  # Border
  box()
  
  
  ## Broughton Inset
  # Define inset position (x1, x2, y1, y2 in figure coordinates)
  par(fig=c(0.62, 0.95, 0.62, 0.97), new=TRUE, mar=c(0,0,0,0))
  
  # Empty plot
  plot(0, type="n",
       xlim=ba.ext[1:2],
       ylim=ba.ext[3:4],
       asp=asp.ba,
       xaxs="i", yaxs="i",
       axes=FALSE, xlab="", ylab="")
  
  # Ocean (minimum of bathymetry anyway)
  rect(xleft = xmin(main.ext)-5,
       xright = xmax(main.ext)+1,
       ybottom = ymin(main.ext)-1,
       ytop = ymax(main.ext)+1,
       col = col.sea,
       border = NA)
  
  # Plot land
  plot(states, add=TRUE, col=col.land.inset, border=NA)
  plot(provinces, add=TRUE, col=col.land.inset, border=NA)
  
  # Sampling sites
  plot(sites, add=TRUE, pch=pch.pt,
       bg=adjustcolor(col.pt, alpha.f=alpha.pt),
       col=adjustcolor("black", alpha.f=alpha.pt),
       cex=cex.pt)
  
  # Region label
  usr <- par("usr")
  ba.real.ext <- ext(usr)  # actual rendered extent (for later)
  text(x = mean(usr[1:2]), y = usr[4] - 0.06 * diff(usr[3:4]), 
       labels = "Broughton Archipelago", family="sans",
       cex=cex.region, col=col.label)
  
  # Inset label
  text(x = usr[2] - 0.03 * diff(usr[1:2]), y = usr[4] - 0.04 * diff(usr[3:4]), 
       labels = "‡", family="sans",
       cex=cex.ext.label, col=col.label)
  
  # Border
  box()
  
  
  ## Inset extents on main map
  # Reset to main plot parameters
  par(main.par)
  
  # Clayoquot extent
  rect(xleft = cs.real.ext[1],
       xright = cs.real.ext[2],
       ybottom = cs.real.ext[3],
       ytop = cs.real.ext[4],
       border = col.extent, lwd = lwd.extent)
  rect(xleft = cs.real.ext[1],
       xright = cs.real.ext[2],
       ybottom = cs.real.ext[3],
       ytop = cs.real.ext[4],
       border = "black", lty=3, lwd=lwd.extent.dashed)
  
  # Clayoquot label
  text(x = cs.real.ext[2] - 0.14 * diff(cs.real.ext[1:2]), 
       y = cs.real.ext[4] - 0.25 * diff(cs.real.ext[3:4]), 
       labels = "†", family="sans",
       cex=cex.ext.label, col=col.label)  
  
  # Broughton
  rect(xleft = ba.real.ext[1],
       xright = ba.real.ext[2],
       ybottom = ba.real.ext[3],
       ytop = ba.real.ext[4],
       border = col.extent, lwd = lwd.extent)
  rect(xleft = ba.real.ext[1],
       xright = ba.real.ext[2],
       ybottom = ba.real.ext[3],
       ytop = ba.real.ext[4],
       border = "black", lty=3, lwd=lwd.extent.dashed)
  
  # Broughton label
  text(x = ba.real.ext[2] - 0.10 * diff(ba.real.ext[1:2]), 
       y = ba.real.ext[4] - 0.18 * diff(ba.real.ext[3:4]), 
       labels = "‡", family="sans",
       cex=cex.ext.label, col=col.label)  

dev.off()