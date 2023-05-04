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

d <- rbind(rdm, ake, achin, emw, fb, kp)

# Go through consistency of entries...
table(d$biome)
table(d$paper_id)
table(d$study_type)
table(d$growth_metric)
table(d$gsl_metric)
subset(d, gsl_metric=="estimated start to estimated end")

# Messing around ... 
table(d$authorsthink_evidence_gslbygrowth)
table(d$authorsthink_evidence_gslbygrowth, d$youthink_evidence_gslxgrowth)

# How many found relationship?
authorsyes <- subset(d, authorsthink_evidence_gslbygrowth=="yes")

## Simplify some columns

# authors think about gsl x growth
d$simple.authorsthink.gslxgrowth <- NA
d$simple.authorsthink.gslxgrowth[grep("yes", d$authorsthink_evidence_gslbygrowth)] <- "yes"
d$simple.authorsthink.gslxgrowth[which(d$authorsthink_evidence_gslbygrowth=="no")] <- "no"
d$simple.authorsthink.gslxgrowth[which(d$authorsthink_evidence_gslbygrowth=="no (or atleast, its weaker than others think) ")] <- "no"
d$simple.authorsthink.gslxgrowth[grep("unsure", d$authorsthink_evidence_gslbygrowth)] <- "unsure"
d$simple.authorsthink.gslxgrowth[grep("not sure", d$authorsthink_evidence_gslbygrowth)] <- "unsure"
d$simple.authorsthink.gslxgrowth[which(d$authorsthink_evidence_gslbygrowth=="not mentioned")] <- "not mentioned"

# you think about gsl x growth
d$simple.wethink.gslxgrowth <- d$youthink_evidence_gslxgrowth
d$simple.wethink.gslxgrowth[which(d$youthink_evidence_gslxgrowth=="yes, but their path model is quite weird and may have problems")] <- "unsure"

# growth metric
d$simple.growth.metric <- d$growth_metric
d$simple.growth.metric[grep("intra-annual", d$growth_metric)] <- "intra-annual core"
d$simple.growth.metric[grep("cell development metrics", d$growth_metric)] <- "intra-annual core"
d$simple.growth.metric[grep("growth anomalies", d$growth_metric)] <- "intra-annual core"
d$simple.growth.metric[which(d$growth_metric=="annual core")] <- "annual core"
d$simple.growth.metric[grep("photosynthesis", d$growth_metric)] <- "photosynthesis"
d$simple.growth.metric[grep("NDVI", d$growth_metric)] <- "other"
d$simple.growth.metric[grep("LAI", d$growth_metric)] <- "other"
d$simple.growth.metric[grep("root", d$growth_metric)] <- "other"
d$simple.growth.metric[grep("NPP", d$growth_metric)] <- "other"

table(d$simple.growth.metric)
table(d$simple.growth.metric, d$simple.authorsthink.gslxgrowth)
table(d$simple.authorsthink.gslxgrowth, d$simple.wethink.gslxgrowth)

# table(d$study_type, d$simple.growth.metric)
