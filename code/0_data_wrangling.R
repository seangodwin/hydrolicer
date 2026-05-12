# 0_data_wrangling.R
# Author: Sean Godwin
# Date: 2026-05-11
# Description: Clean and combine sea-louse data from zoop tows around vessels

rm(list=ls())

## 0 [PACKAGES AND INPUTS] -----------------------------------------------------
library(here)        # for file referencing
library(tidyverse)   # for data manipulation

net.dims <- data.frame(region = c("CS", "CS", "ba"),  
                       mesh = c(20, 250, 150),   # mesh size in microns
                       diam = c(0.3, 0.5, 0.5),  # diameter of net in meters
                       conv = c(rep(26873/999999,2), NA))  # conversion for dist


## 1 [READ IN DATA] ------------------------------------------------------------
# Remember to change your path
here::i_am("code/0_data_wrangling.R")

# Read in data
cs <- read.csv(here::here("./data/raw/cs_vessel_sampling.csv"))
ba <- read.csv(here::here("./data/raw/ba_vessel_sampling.csv"))
farm.locs <- read.csv(here::here("./data/raw/farm_locations.csv"))


## 2 [CLEAN CLAYOQUOT SOUND DATA] ----------------------------------------------
# Change column names
colnames(cs) <- c("sample.id", "date", "time", "farm.name", "sample.type", 
                  "control.type", "tow.type", "treat.type", "crew", 
                  "enumerator", "lat", "lon", "mesh", "sample.vol", "num.split", 
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

# Fix a couple date entry errors
cs$date[is.na(cs$farm.name)==F & 
        cs$farm.name=="Bawden Bay"] <- as.Date("2022-03-24")
cs$control.type[cs$farm.name == "Bawden Bay" & 
                cs$date == "2022-03-24"] <- "spatial"

# Reorder and remove columns
cs <- cs[,c("region", "sample.id", "date", "time", "farm.name", "lat", "lon",
            "treat.type", "sample.type", "control.type", "tow.type",
            "mesh", "tow.duration", "tow.depth", "flow.vol", 
            "sample.vol", "subsample.vol", 
            "egg", "naup", "l.cope", "c.cope", "cope",
            "l.chal", "c.chal", "chal", 
            "l.mot", "c.mot", "mot", 
            "lice", "lice.extrap", "comments")]

# Convert blanks to NAs
cs[cs==""] <- NA


## 3 [CLEAN BROUGHTON DATA] ----------------------------------------------------
# Change column names
colnames(ba) <- c("treat.type", "vessel", "farm.name", "sample.type",
                   "control.type", "sample.num", "time", "tow.duration",
                   "tow.depth", "speed", "day", "day.examined", "temp", "sal",
                   "month", "year", "egg", "naup", "cope", "chal", "mot",
                   "comments", "scales", "undeveloped.eggs", "ruptured.eggs",
                   "ruptured.strings")

# Add some columns
ba$region <- "Broughton Archipelago"
ba$sample.id <- paste("BA", seq(1:nrow(ba)), sep="")
ba$date <- as.Date(paste(ba$year, ba$month, ba$day, sep="-"), 
                    format="%Y-%m-%d")
ba$sample.vol <- 1000                                  
ba$subsample.vol <- 15
ba$mesh <- 150
ba$tow.type <- "horizontal"

# Convert all louse NAs to zeros
ba.louse.cols <- grep("egg|naup|cope|chal|mot", names(ba))
ba[,ba.louse.cols][is.na(ba[,ba.louse.cols])] <- 0

# Add lice together
ba$lice <- rowSums(ba[,c("egg", "naup", "cope",               
                         "chal","mot")], na.rm=T)      # all lice
ba$lice.extrap <- round(ba$lice * ba$sample.vol / 
                         ba$subsample.vol, 0)  # estimated lice in full sample

# Adjust some columns
ba$sample.type <- gsub(" ", "", ba$sample.type)

# Fix a misentered date
ba$date[ba$date == "2012-10-06"] <- as.Date("2021-10-06")

# Add correct site names for those missing
ba$farm.name[ba$farm.name == "" & ba$date %in% 
             c("2021-09-23", "2021-10-12")] <- "Swanson"
ba$farm.name[ba$farm.name == "" & ba$date %in% 
               c("2021-10-01", "2021-10-06")] <- "Midsummer"

# Remove Sir Ed and Cypress samples; neither spatial nor temporal controsl
ba <- ba[!(ba$farm.name %in% c("Sir Edmund", "Cypress")),]

# Fix a site name and add a clarifying comment for it
ba$comment[ba$farm.name == "Mitchell Bay"] <- "sample taken 13 km away after 
                                               realising no control was taken"
ba$farm.name[ba$farm.name == "Mitchell Bay"] <- "Midsummer"

# Fix some temp entries
ba$temp[ba$temp=="6..5"] <- 6.5
ba$temp[ba$temp=="45 F"] <- (45 - 32) * 5/9
ba$temp <- round(as.numeric(ba$temp),1)


## 4 [COMBINE AND CLEAN] -------------------------------------------------------
# Combine
data <- bind_rows(cs, ba)[,c(1:ncol(cs))]

# Convert blanks to NAs
data[data==""] <- NA

# Make type entries consistent
data[c("treat.type", "sample.type", "control.type", "tow.type")] <-
  lapply(data[c("treat.type", "sample.type", "control.type", "tow.type")], 
         tolower)

# Fix site names (ugly for now)
data$farm.name <- str_to_title(data$farm.name)
data$farm.name <- gsub(" Farm", "", str_trim(data$farm.name, side = "left"))
data$farm.name[data$farm.name == "Bawden Bay"] <- "Bawden"
data$farm.name[data$farm.name == "Forture Channel"] <- "Fortune Channel"
data$farm.name[data$farm.name == "Saranac Island"] <- "Saranac"

# Replace lat lons with farm lat lons (since none at all for BA)
data$lat <- farm.locs$lat[match(data$farm.name, farm.locs$farm.name)]
data$lon <- farm.locs$lon[match(data$farm.name, farm.locs$farm.name)]

# Replace commas in comments so it doesn't mess up csv file
data$comments <- gsub(",", ";", data$comments)




# Add a treatment ID column
data$treat.id[data$sample.type == "effluent"] <- 
    paste(data$farm.name, 
          data$sample.type,
          substr(data$date,1,7))[data$sample.type == "effluent"]
data$treat.id <- gsub(" ", "_", data$treat.id)

# Add a control ID column
data$control.id[is.na(data$control.type)==F & 
                data$sample.type == "control"] <- 
  paste(data$farm.name, 
        data$control.type,
        substr(data$date,1,7))[is.na(data$control.type)==F & 
                                 data$sample.type == "control"]
data$control.id <- gsub(" ", "_", data$control.id)

# Reorder columns
data <- data %>%
  relocate(treat.id, control.id, .after = region)


## 5 [WRITE CSV] ---------------------------------------------------------------
write.csv(data, "./data/processed/all_vessel_sampling.csv", row.names=F, quote=F)
