## Started 21 August 2023 ##
## By Lizzie ##

## This file reads in all the cleaning for our GREPHON meta-analysis ##

# housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/grephon/grephon/analyses")
} else if (length(grep("ailene", getwd()))>0) 
{setwd("boomboom")
}

## packages
require(data.table)
require(tidyverse)
require(stringr)

# Step 1: Read in the data

# After double entry meetings and final table update (we hope)
rdmf <- fread("input/round7/grephontable_rdm_fb_5.4.csv") # need to update!
akej <- fread("input/round7/grephontable_JHRLAKE.csv") 
achinc <- fread("input/round7/grephon table Alana and Cat final round.csv") 
emw <- fread("input/round7/grephontable_emw.csv")
kp <- fread("input/round7/grephontable_kp_NEW_updatedExogenous.csv")

# cleaning up some issues before merging data
akej$V46 <- NULL
akej$V47 <- NULL

dall <- rbind(rdmf, akej, achinc, emw, kp)
d <- dall

# Step 2: Clean up some issues in entries
source("source/clean_AKEJHRL.R") 

# Step 3: Clean growth, GSL (and start/end) and a bunch of other misc stuff
source("source/cleangrowthgsl.R")

# Step 4: Merge in ageclass info
source("source/cleanageclass.R")

# Step 5: Clean who looked at endo and ext factors
source("source/clean_methodexoendo.R") 

# Step 6: Clean species
source("source/cleanspecies.R") 

# Step 7 (prime!): Write it out ...
write.csv(d, "output/grephontable.csv", row.names=FALSE)