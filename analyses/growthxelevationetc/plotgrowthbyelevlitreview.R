## Started 28 August 2023 ##
## By Lizzie so far ##

# housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/grephon/grephon/analyses/growthxelevationetc")
} else if (length(grep("ailene", getwd()))>0) {
  setwd("boomboom")
} else if (length(grep("britanywuuu", getwd()))>0) {
  setwd("~/Documents/ubc/year5/TemporalEcologyLab/grephon/analyses/growthxelevationetc")
}

# packages
library(ggplot2)
library(viridis)
# get the data
d <- read.csv("input/classicalrefs_datascraped.csv")

# fix the growth in cm and make error numeric, or 0
d$growthmm <- d$growth_value
d$growthmm[which(d$growth_units=="cm/yr-1")] <- d$growth_value[which(d$growth_units=="cm/yr-1")]*10
d$error <- as.numeric(d$growth_errorvalue)
d$error[which(is.na(d$error)==TRUE)] <- 0

# subset to elevation-ish
unique(d$predictor_type)
delev <- subset(d, predictor_type!="latitude")
dlat <- subset(d, predictor_type=="latitude")


# one study (huang2010) has multiple species
table(d$species, d$dataset_id)

ggplot(delev, aes(x=predictor_value, y=growthmm, color=dataset_id)) +
    geom_point() +
    geom_smooth(method="lm")

# zhu2018 seems weird, but I think latitude may also vary with elevation according to Table 1?
# Will confirm and for now remove ...

delevsm <- subset(delev, dataset_id!="zhu2018" & dataset_id!="oleksyn1998")

ggplot(delevsm, aes(x=predictor_value, y=growthmm, color=species)) +
    geom_point() +
    geom_smooth(method="lm")

# Hmm, the error we do have seems big
ggplot(delevsm, aes(x=predictor_value, y=growthmm, color=species, ymin = growthmm-error, ymax = growthmm + error)) +
    geom_point() +
    geom_errorbar(width = 0.2) +
    geom_smooth(method="lm")

# Latitude
ggplot(dlat, aes(x=predictor_value, y=growthmm, color=species)) +
    geom_smooth(method="lm", aes(group = species)) +
  geom_point() +
  scale_color_viridis_d(option = "viridis", name = "Species") +
  xlab("Elevation (m)") +
  ylab("Growth (mm)") +
  scale_x_continuous(breaks = seq(46, 54.5, by = 2))+
  ylim(c(0,3.2)) +
  theme(legend.position = c(0.8,0.85), 
        legend.key.size = unit(0.2, "cm"), 
        legend.text = element_text(face = "italic"),
        legend.title = element_text(size = 10))
ggsave("output/growthbyelevation_plot.pdf", dpi = 300)
