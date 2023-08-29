## Started 28 August 2023 ##
## By Lizzie so far ##

# housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/grephon/grephon/analyses/growthxelevationetc")
} else if (length(grep("ailene", getwd()))>0) 
{setwd("boomboom")
}

# packages

# get the data
d <- read.csv("input/classicalrefs_datascraped.csv")

# fix the growth in cm
d$growthmm <- d$growth_value
d$growthmm[which(d$growth_units=="cm/yr-1")] <- d$growth_value[which(d$growth_units=="cm/yr-1")]/10

# subset to elevation-ish
unique(d$predictor_type)
delev <- subset(d, predictor_type!="latitude")

library(ggplot2)
ggplot(delev, aes(x=predictor_value, y=growthmm, color=dataset_id)) +
    geom_point() +
    geom_smooth(method="lm")