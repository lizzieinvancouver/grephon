## Started 2 May 2023 ##
## By Lizzie so far ##

## On the train into Zurich, we made the connection! ##

# housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

## packages
library(data.table)

setwd("~/Documents/git/projects/grephon/analyses")

rdm <- fread("input/grephontable_rdm.csv")
ake <- fread("input/grephontable_Ailene.csv")
achin <- fread("input/grephontable_Alana.csv")
emw <- fread("input/grephontable_emw.csv")
fb <- fread("input/grephontable_FB.csv")
kp <- fread("input/grephontable_kp.csv")
cjc <- fread("input/grephontable_cjc.csv")
jhrl <- fread("input/grephontable-jhrl.csv")

d <- rbind(rdm, ake, achin, emw, fb, kp, cjc, jhrl)

# Go through consistency of entries...
table(d$biome)
table(d$paper_id)
table(d$study_type)
table(d$growth_metric)
table(d$gsl_metric)
subset(d, gsl_metric=="estimated start to estimated end")
unique(d$species_list)

papernum <- length(unique(d$paper_id))

## Simplify some columns

d$study_type[which(d$study_type=="continental scale long-term observational study of phenology combined with model, checked against remote-sensed data")] <- "continental scale obs phenology with model"


# authors think about gsl x growth
d$simple.authorsthink.gslxgrowth <- NA
d$simple.authorsthink.gslxgrowth[grep("yes", d$authorsthink_evidence_gslbygrowth)] <- "yes"
d$simple.authorsthink.gslxgrowth[which(d$authorsthink_evidence_gslbygrowth=="no")] <- "no"
d$simple.authorsthink.gslxgrowth[which(d$authorsthink_evidence_gslbygrowth=="no (or atleast, its weaker than others think) ")] <- "no"
d$simple.authorsthink.gslxgrowth[grep("unsure", d$authorsthink_evidence_gslbygrowth)] <- "unsure"
d$simple.authorsthink.gslxgrowth[grep("not sure", d$authorsthink_evidence_gslbygrowth)] <- "unsure"
d$simple.authorsthink.gslxgrowth[which(d$authorsthink_evidence_gslbygrowth=="not mentioned")] <- "not mentioned"
table(d$simple.authorsthink.gslxgrowth )

# you think about gsl x growth
d$simple.wethink.gslxgrowth <- d$youthink_evidence_gslxgrowth
d$simple.wethink.gslxgrowth[which(d$youthink_evidence_gslxgrowth=="yes, but their path model is quite weird and may have problems")] <- "unsure"
d$simple.wethink.gslxgrowth[which(d$youthink_evidence_gslxgrowth=="not sure")] <- "unsure"
table(d$simple.wethink.gslxgrowth)

# growth metric
d$simple.growth.metric <- d$growth_metric
d$simple.growth.metric[grep("intra-annual", d$growth_metric)] <- "intra-annual core"
d$simple.growth.metric[grep("cell development metrics", d$growth_metric)] <- "intra-annual core"
d$simple.growth.metric[grep("growth anomalies", d$growth_metric)] <- "intra-annual core"
d$simple.growth.metric[which(d$growth_metric=="annual core")] <- "annual core"
d$simple.growth.metric[grep("photosynthe", d$growth_metric)] <- "photosynthesis"
d$simple.growth.metric[grep("biomass", d$growth_metric)] <- "biomass/height/R:S"
d$simple.growth.metric[grep("root", d$growth_metric)] <- "biomass/height/R:S"
d$simple.growth.metric[grep("height", d$growth_metric)] <- "biomass/height/R:S"
d$simple.growth.metric[grep("NDVI", d$growth_metric)] <- "NDVI/LAI"
d$simple.growth.metric[grep("LAI", d$growth_metric)] <- "NDVI/LAI"
d$simple.growth.metric[grep("NPP", d$growth_metric)] <- "other"
d$simple.growth.metric[grep("LMA", d$growth_metric)] <- "other"
d$simple.growth.metric[grep("stomatal conductance", d$growth_metric)] <- "other"
d$simple.growth.metric[grep("water-use efficiencty", d$growth_metric)] <- "other"


table(d$simple.growth.metric)
table(d$simple.growth.metric, d$simple.authorsthink.gslxgrowth)
table(d$simple.authorsthink.gslxgrowth, d$simple.wethink.gslxgrowth)

# table(d$study_type, d$simple.growth.metric)

# How many papers found relationship?
authorsyes <- subset(d, simple.authorsthink.gslxgrowth=="yes")
yespapers <- length(unique(authorsyes$paper_id))

# Write it out ...
write.csv(d, "output/grephontable.csv", row.names=FALSE)
