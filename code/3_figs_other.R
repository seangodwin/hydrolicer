# 3_figs_other.R
# Author: Sean Godwin
# Date: 2026-05-11
# Description: Louse-count data from effluent vs controls

rm(list=ls())

## 0 [PACKAGES AND INPUTS] -----------------------------------------------------
library(here)        # for file referencing
library(tidyverse)   # for data manipulation
library(PNWColors)  # for colour palettes
set.seed(666)


## 1 [READ IN DATA] ------------------------------------------------------------
# Remember to change your path
here::i_am("code/3_figs_other.R")

# Read in data
data <- read.csv(here::here("./data/processed/all_vessel_sampling.csv"))

# Combine "effluent" and control.type to make plotting easier
data$control.type[is.na(data$control.type) & 
                  data$treat.type=="hydrolicer"] <- "hydrolicer"
data$control.type[is.na(data$control.type) & 
                  data$treat.type=="freshwater"] <- "freshwater"
data$control.type <- factor(data$control.type, 
                            levels=c("temporal", "spatial", 
                                     "hydrolicer", "freshwater"))



## 2 [FIGURE 3] ----------=-----------------------------------------------------
palette(adjustcolor(c(rev(pnw_palette(name="Sunset2", 
                        n=20, 
                        type="continuous"))[c(17,20)],
                    rev(pnw_palette(name="Sunset2", 
                          n=4, 
                          type="discrete"))[1:2]),
                    alpha.f=0.8))
x.shift <- -0.28

tiff(here::here("./outputs/figs/test.tiff"), width=8, height=4.5, units="in",
     pointsize=14, res=600, compression="lzw")
par(mar=c(3, 4.2, 0.5, 0.5), tck=-0.025, mgp=c(1,0.8,0), family="sans")

  plot(x = jitter(as.numeric(data$control.type)/2, amount=0.15) +
             x.shift,
       y = data$lice.extrap,
       ann=F, xaxt="n", yaxt="n",
       xlim=c(0, 1.95), ylim=c(0,1667),
       bg=data$control.type,
       pch = 21, cex=1.5)

  # Y axis
  axis(side=2, at=seq(0,1500,500), las=1)
  
  # X axis
  par(mgp=c(2.5,1.6,0))
  axis(side=1, at=seq(0.5,2,0.5)+x.shift, 
       labels=c("Temporal\ncontrol", "Spatial\ncontrol",
                "Hydrolicer\n", "Freshwater\nbath"))
  
  # Axes titles
  mtext("Estimated number of sea lice in sample",
        side=2, line=3, outer=F, cex=1.1)
  
dev.off()


## 3 [FIGURE 4] ----------=-----------------------------------------------------
# Plot inputs
col.bars <- pnw_palette(name="Starfish", n=6, type="continuous")[1:5]

# Summarise data for plot
summ.temp <- as.data.frame(data[data$sample.type == "effluent",] %>% 
                           group_by(id = paste(mesh, treat.type, region)) %>% 
                           summarise(egg = sum(egg)/sum(lice),
                                     naup = sum(naup)/sum(lice),
                                     cope = sum(cope)/sum(lice),
                                     chal = sum(chal)/sum(lice),
                                     mot = sum(mot)/sum(lice)))
summ <- as.data.frame(t(summ.temp[-1]))
colnames(summ) <- summ.temp$id

# Plot
tiff(here::here("./outputs/figs/fig4.tiff"), width=8, height=4, units="in",
     pointsize=13, res=600, compression="lzw")
par(mar=c(3.9,3.6,1,0), tck=-0.025, mgp=c(1,0.7,0), family="sans")

  bp <- barplot(as.matrix(summ),
                beside = FALSE,
                col = col.bars, #border=NA,
                ann=F, xaxt="n", yaxt="n", 
                space=0.18,
                legend.text = c("egg", "nauplius", "copepodite", 
                                "chalimus", "motile"),
                args.legend = list(x=5.86, y=0.9, bty="n"),
                ylim=c(0,1), xlim = c(0.25, ncol(summ) + 1.7))
  
  #  Y axis
  axis(side=2, at=seq(0,1,0.2), labels = seq(0,100,20), las=1)
  
  # X axis (brute force for now)
  par(mgp=c(2.5,2.5,0))
  axis(side=1, 
       at=bp[seq(1,4,2)], 
       labels=c("150 μm net\nBroughton\nFreshwater bath",
                "150 μm net\nBroughton\nHydrolicer",
                "20 μm net\nClayoquot\nHydrolicer",
                "250 μm net\nClayoquot\nHydrolicer")[seq(1,4,2)])
  axis(side=1, 
       at=bp[seq(2,4,2)], 
       labels=c("150 μm net\nBroughton\nFreshwater bath",
                "150 μm net\nBroughton\nHydrolicer",
                "20 μm net\nClayoquot\nHydrolicer",
                "250 μm net\nClayoquot\nHydrolicer")[seq(2,4,2)])
  
  # Axes titles
  mtext("Sea-louse life stage proportion (%)",
        side=2, line=2.3, outer=F, cex=1.1)

dev.off()


## 4 [FIGURE S1] ---------------------------------------------------------------


## 5 [TABLE 1] ----------=------------------------------------------------------
# Change sample type just to make it easier (ugly for now)
data$control.type <- as.character(data$control.type)

data$sample.type[data$sample.type=="control"] <- 
  data$control.type[data$sample.type=="control"]

# Make some adjustments
data$control.type[data$control.type %in% 
                   c("hydrolicer", "freshwater")] <- "effluent"
data$region <- factor(data$region,
                      levels = c("Clayoquot Sound", "Broughton Archipelago"))

# Make the table
tab <- as.data.frame(
           data %>% 
           group_by(region, treat.type, control.type) %>% 
           summarise(unique_ids =
                       if (first(control.type) == "effluent") {
                         n_distinct(treat.id, na.rm = TRUE)
                       } else {
                         n_distinct(control.id, na.rm = TRUE)
                       },
                     unique_days = n_distinct(date, na.rm = TRUE),
                     n.sample = n(),
                     n.positive = sum(lice.extrap > 0, na.rm = TRUE),
                     # pct.positive = round(n.positive / n.sample * 100, 0),
                     mean.lice = as.character(round(mean(lice.extrap, 
                                                         na.rm=T),0)),
                     max.lice = as.character(max(lice.extrap, na.rm=T)),
                     mean.inf.lice = as.character(round(mean(inf.lice.extrap, 
                                                         na.rm=T),0)),
                     max.inf.lice = as.character(max(inf.lice.extrap, na.rm=T))))

# Final adjustments (ugly for now)
tab$mean.lice[tab$mean.lice == "4"] <- "4*"
tab$max.lice[tab$max.lice == "67"] <- "67*"
tab$mean.inf.lice[tab$mean.inf.lice == "4"] <- "4*"
tab$max.inf.lice[tab$max.inf.lice == "67"] <- "67*"
tab$control.type[tab$control.type=="spatial"] <- "spatial control"
tab$control.type[tab$control.type=="temporal"] <- "temporal control"


colnames(tab) <- c("Region", "Treatment type", "Sample category",
                   "Number of sampling events", "Number of sampling days", 
                   "Number of samples", "Number of positive samples", 
                   # "Percent positive samples", 
                   "Mean number of lice", "Maximum number of lice", 
                   "Mean number of infective lice", 
                   "Maximum number of infective lice")


## 6 [WRITE CSV] ---------------------------------------------------------------
write.csv(tab, "./outputs/tables/table1.csv", 
          row.names=F, quote=F)
