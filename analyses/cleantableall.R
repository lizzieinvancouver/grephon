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

# Step 2: Clean growth, GSL (and start/end) and a bunch of other misc stuff
source("cleangrowthgsl.R")

# Step 3: Clean up some issues in entries
source("clean_AKEJHRL.R")

# Step X ... merge in ageclass table?
tryme <- read.csv("input/ageclass.csv")
tryme <- tryme[!duplicated(tryme),]
worryabout <- aggregate(tryme["age_class"], tryme["paper_id"], FUN=length)
worryabout[which(worryabout$age_class >1),]

# Step X: Clean who looked at endo and ext factors
source("clean_methodexoendo.R") # NEED to check this given above AKEJHRL cleaning

# Step X: Clean spcies
source("species.R") 

# Step X: Write it out ...
write.csv(d, "output/grephontable.csv", row.names=FALSE)