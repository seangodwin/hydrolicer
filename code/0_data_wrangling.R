# 0_data_wrangling.R
# Author: Sean Godwin
# Date: 2026-05-11
# Description: Clean and combine sea-louse data from zoop tows around vessels

rm(list=ls())

## 0 [PACKAGES AND INPUTS] -----------------------------------------------------------
library(here)        # for file referencing
library(tidyverse)   # for data manipulation

net.dims <- data.frame(region = c("CS", "CS", "QCS"),  
                       mesh = c(20, 250, 150),   # mesh size in microns
                       diam = c(0.3, 0.5, 0.5),  # diameter of net in meters
                       conv = c(rep(26873/999999,2), NA))   # conversion for dist


## 1 [READ IN DATA] ------------------------------------------------------------
# Remember to change your path
here::i_am("code/0_data_wrangling.R")

# Read in data
cs <- read.csv(here::here("./data/raw/cs_vessel_sampling.csv"))
qcs <- read.csv(here::here("./data/raw/qcs_vessel_sampling.csv"))


## 2 [CLEAN CLAYOQUOT SOUND DATA] ----------------------------------------------
# Change column names
colnames(cs) <- c("sample.id", "date", "time", "site.name", "sample.type", 
                  "control.type", "tow.type", "treat.type", "crew", "enumerator",
                  "lat", "lon", "mesh", "sample.vol", "num.split", 
                  "subsample.vol", "total.lice.extrapolated", 
                  "total.lice.counted", "egg", "naup", "l.cope", "c.cope",
                  "c.chal", "l.chal.a", "l.chal.b", "l.mot", "c.mot",
                  "temp1", "temp2", "temp3", "temp4", "flow.in", 
                  "flow.out", "flow.diff", "distance", "volume", "comments")

# Fix dates
slash.dates <- grepl("/", cs$date)
cs$date[slash.dates] <- as.character(as.Date(cs$date[slash.dates], 
                                             format="%d/%m/%Y"))
cs$date[!slash.dates] <- as.character(as.Date(cs$date[!slash.dates], 
                                              format="%m.%d.%Y"))
cs$date <- as.Date(cs$date)

# Adjust some columns
cs$mesh <- as.numeric(gsub("[^0-9.]", "", cs$mesh))    # remove units
cs$sample.id <- gsub(" ", "", cs$sample.id)            # remove spaces
cs$sample.vol <- 1000                                  # actualy 1L
cs$subsample.vol <- cs$sample.vol / (2 ^ cs$num.split) # recalculate
cs$time[cs$time=="na"] <- NA                           # convert to actual NAs
cs$time <- as.numeric(gsub(":","", cs$time))           # convert to numeric

# Add some columns (including some calculations)
cs$tow.duration <- cs$tow.depth <- cs$temp <- cs$sal <- NA
cs$region <- "Clayoquot Sound"
cs$l.chal <- rowSums(cs[,c("l.chal.a", "l.chal.b")], na.rm=T) # combine l.chals

# Convert all louse NAs to zeros
cs.louse.cols <- grep("egg|naup|cope|chal|mot", names(cs))
cs[,cs.louse.cols][is.na(cs[,cs.louse.cols])] <- 0

# Add lice together
cs$cope <- rowSums(cs[,c("l.cope", "c.cope")], na.rm=T)       # all copes
cs$chal <- rowSums(cs[,c("l.chal", "c.chal")], na.rm=T)       # all chals
cs$mot <- rowSums(cs[,c("l.mot", "c.mot")], na.rm=T)          # all mots
cs$lice <- rowSums(cs[,c("egg", "naup", "cope",               
                         "chal","mot")], na.rm=T)             # all lice
cs$lice.extrap <- round(cs$lice * cs$sample.vol / 
                        cs$subsample.vol, 0)     # estimated lice in full sample

# Calculation for volume of water that flowed through zoop net
cs.flow.diff <- cs$flow.out - cs$flow.in
cs.distance <- cs.flow.diff * net.dims$conv[match(cs$mesh, net.dims$mesh)]
cs$flow.vol <- cs.distance * pi * 
               (net.dims$diam[match(cs$mesh, net.dims$mesh)])^2

# Reorder and remove columns
cs <- cs[,c("region", "sample.id", "date", "time", "site.name", "lat", "lon",
            "treat.type", "sample.type", "control.type", "tow.type",
            "mesh", "tow.duration", "tow.depth", "flow.vol", 
            "sample.vol", "subsample.vol", 
            "egg", "naup", "l.cope", "c.cope", "cope",
            "l.chal", "c.chal", "chal", 
            "l.mot", "c.mot", "mot", 
            "lice", "lice.extrap", "comments")]

# Convert blanks to NAs
cs[cs==""] <- NA


## 2 [CLEAN QUEEN CHARLOTTE STRAIT DATA] ---------------------------------------
# Change column names
colnames(qcs) <- c("treat.type", "vessel", "site.name", "sample.type",
                   "control.type", "sample.num", "time", "tow.duration",
                   "tow.depth", "speed", "day", "day.examined", "temp", "sal",
                   "month", "year", "egg", "naup", "cope", "chal", "mot",
                   "comments", "scales", "undeveloped.eggs", "ruptured.eggs",
                   "ruptured.strings")

# Add some columns
qcs$region <- "Queen Charlotte Strait"
qcs$sample.id <- paste("QCS", seq(1:nrow(qcs)), sep="")
qcs$date <- as.Date(paste(qcs$year, qcs$month, qcs$day, sep="-"), 
                    format="%Y-%m-%d")
qcs$sample.vol <- 1000                                  
qcs$subsample.vol <- 15

# Convert all louse NAs to zeros
qcs.louse.cols <- grep("egg|naup|cope|chal|mot", names(qcs))
qcs[,qcs.louse.cols][is.na(qcs[,qcs.louse.cols])] <- 0

# Add lice together
qcs$lice <- rowSums(qcs[,c("egg", "naup", "cope",               
                         "chal","mot")], na.rm=T)      # all lice
qcs$lice.extrap <- round(qcs$lice * qcs$sample.vol / 
                         qcs$subsample.vol, 0)  # estimated lice in full sample

# Adjust some columns
qcs$sample.type <- gsub(" ", "", qcs$sample.type)

# Fix some temp entries
qcs$temp[qcs$temp=="6..5"] <- 6.5
qcs$temp[qcs$temp=="45 F"] <- (45 - 32) * 5/9
qcs$temp <- round(as.numeric(qcs$temp),1)



## 2 [COMBINE AND CLEAN] ---------------------------------------
# Combine
data <- bind_rows(cs, qcs)[,c(1:ncol(cs))]

# Convert blanks to NAs
data[data==""] <- NA

# Make type entries consistent
data[c("treat.type", "sample.type", "control.type", "tow.type")] <-
  lapply(data[c("treat.type", "sample.type", "control.type", "tow.type")], 
         tolower)

# Fix site names
data$site.name <- str_to_title(data$site.name)
data$site.name <- gsub(" Farm", "", str_trim(data$site.name, side = "left"))
