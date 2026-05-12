# 1_fig1.R
# Author: Sean Godwin
# Date: 2026-05-10
# Description: Plot louse treatments on BC salmon farms over time

rm(list=ls())

## 0 [LOAD PACKAGES] -----------------------------------------------------------
library(here)        # for file referencing
library(tidyverse)   # for data manipulation
library(PNWColors)   # for colour palettes


## 1 [READ IN DATA] ------------------------------------------------------------
# Remember to change your path
here::i_am("code/2_fig1.R")

# Read in data
# Farm treatments:
#   https://open.canada.ca/data/en/dataset/fdba4d10-51aa-40df-884e-09e1c29049d8
# Farmed salmon inventory: 
#   https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3210010701
treat <- read.csv(here::here("./data/raw/farm_treatments.csv"))
inv <- read.csv(here::here("./data/raw/farm_biomass.csv"), header=F)


## 2 [CLEAN DATA] --------------------------------------------------------------
# Adjust column names
colnames(treat) <- c("date", "facility.num", "company", "facility.name", "lat",
                     "lon", "region", "treat.type")

# Fiddle with some columns
treat$year <- as.numeric(substr(treat$date, 1, 4))
treat$date <- as.Date(treat$date, format="%Y-%m-%d")

# Reorder columns
treat <- treat %>%
         select(date, year, region, company, facility.num,
                facility.name, lat, lon, treat.type)

# Remove the partial 2010 year and the year for which we don't have biomass
treat <- treat[treat$year >= 2011 & treat$year <= 2024,]

# Remove harvest, so it's just treatments
treat <- treat[treat$treat.type != "Harvest",]

# Re-name treatments (ugly for now)
treat$treat.type[treat$treat.type=="In-Feed Treatment"] <- "Emamectin benzoate"
treat$treat.type[treat$treat.type=="Medicinal Bath Treatment"] <- "Medicinal bath"
treat$treat.type[treat$treat.type=="Non-Medicinal Bath Treatment"] <- "Freshwater bath"
treat$treat.type[treat$treat.type=="Mechanical Removal"] <- "Hydrolicer"

# Clean inventory dataframe (ugly for now)
inv <- as.data.frame(t(inv[-1]))
row.names(inv) <- NULL
colnames(inv) <- c("year", "biomass", "value")
inv$biomass <- as.numeric(gsub(",", "", inv$biomass))
inv$value <- as.numeric(gsub(",", "", inv$value))

# Adjust biomass so that it's in thousands of tonnes
inv$biomass <- inv$biomass / 1000


## 3 [SUMMARISE] ---------------------------------------------------------------
summ <- as.data.frame(treat %>% 
                      group_by(year, treat.type) %>% 
                      summarise(n=n()))

# Add biomass
summ$biomass <- inv$biomass[match(summ$year, inv$year)]

# Number of treatments per unit biomass
summ$n.per <- summ$n / summ$biomass


## 4 [PLOT] --------------------------------------------------------------------
# Set up dataframe for plot
summ.wide <- reshape(
  summ[, c("year", "treat.type", "n.per")],
  timevar = "year",
  idvar = "treat.type",
  direction = "wide"
)
rownames(summ.wide) <- summ.wide$treat.type  # make treat type the row names
summ.wide <- summ.wide[, -1]                 # remove treat type column
summ.wide[is.na(summ.wide)] <- 0             # convert NAs to zeros

# Re-order according to how we want to visualise (ugly for now)
summ.wide <- summ.wide[c("Hydrolicer",
                         "Freshwater bath",
                         "Medicinal bath",
                         "Emamectin benzoate"),]

# Colours
col.bars <- rev(pnw_palette(name="Sunset2", 
                            n=4, 
                            type="discrete"))

# Plot
tiff(here::here("./outputs/figs/fig1.tiff"),
     width=8, height=4, units="in", 
     pointsize=13, res=600, compression="lzw")
par(mar=c(3,4.6,1,0), tck=-0.025, mgp=c(1,0.7,0), family="sans")

  bp <- barplot(as.matrix(summ.wide),
                beside = FALSE,
                col = col.bars, #border=NA,
                ann=F, xaxt="n", yaxt="n", 
                legend.text = rownames(summ.wide),
                args.legend = list(x=5.4, y=2.08, bty="n"),
                ylim=c(0,2), xlim = c(0.62, ncol(summ.wide) + 2.5))
  
  #  Y axis
  axis(side=2,
       at=seq(0,2,0.5),
       las=1)
  
  # X axis 
  par(mgp=c(2.5,0.6,0))
  axis(side=1, 
       at=bp, 
       labels=paste("'", 
                    substr(seq(min(summ$year), max(summ$year), 1), 3, 4),
                    sep=""))
  
  # Axes titles
  mtext("Number of treatments\nper thousand tonnes of fish",
        side=2, line=2.2, outer=F, cex=1.1)
  mtext("Year", 
        side=1, line=1.75, outer=F, cex=1.1)

dev.off()
  