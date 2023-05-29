## Started 2 May 2023 ##
## By Lizzie so far ##

## On the train into Zurich, we made the connection! ##

## Updated 22 May 2023, hoping to ..
# (1) Check how double entry is going
# (2) Review what we have found
# DID NOT make it past (1) so need to work on that and do (2) someday.

# housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

## packages
library(data.table)
library(stringr)


setwd("~/Documents/git/projects/grephon/analyses")

## needed functions
comparedoubentry <- function(df, suffixeshere, columnname){
    column1 <- paste(columnname, suffixeshere[1], sep=".")
    column2 <- paste(columnname, suffixeshere[2], sep=".")
    columnz <- c("paperid", column1, column2)
    print(df[,..columnz])
    }
          

# round 2
rdm2 <- fread("input/round2/grephontable_rdm2.0.csv")
ake2 <- fread("input/round2/grephontable_AKE.csv")
achin2 <- fread("input/round2/grephontable_Alana.csv")
emw2 <- fread("input/round2/grephontable_emw.csv")
# fb <- fread("input/round2/XXX.csv")
kp2 <- fread("input/round2/grephontable_kp.csv")
# cjc <- fread("input/round2/XXX.csv")
jhrl2 <- fread("input/round2/grephontable_jhrl2.csv")

# round 3
rdm3ALL <- fread("input/round3/grephontable_rdm2.1.csv")
ake3 <- fread("input/round3/grephontable_Ailene_20May2023.csv")
achin3 <- fread("input/round3/grephontable_Alana.csv")
emw3 <- fread("input/round3/grephontable_emw.csv")
# fb <- fread("input/round3/XXX.csv")
kp3 <- fread("input/round3/grephontable_kp_2.csv")
kp4 <- fread("input/round3/grephontable_kp_3.csv")
kp4$V39 <- NULL
kp3 <- rbind(kp3, kp4)
# cjc <- fread("input/round3/XXX.csv")
jhrl3 <- fread("input/round3/grephontable_jhrl3.csv")
# slim down any where they sent last time's data
rdm3 <- rdm3ALL[which(!rdm3ALL$paper_id %in% unique(rdm2$paper_id)),]

dr2 <- rbind(rdm2, ake2, achin2, emw2, kp2, jhrl2)
dr3 <- rbind(rdm3, ake3, achin3, emw3, kp3, jhrl3)

# okay, step 1 is to get the double enteries separated out
dr2 <- subset(dr2, paper_id!="") # empty
dr2$paperid  <- tolower(str_replace_all(dr2$paper_id, " ", ""))
dr3$paperid  <- tolower(str_replace_all(dr3$paper_id, " ", ""))
# manually fix some ...
dr3$paperid[which(dr3$paperid=="keenanetal2014")] <- "keenan2014"
dr3$paperid[which(dr3$paperid=="mckownetal2016")] <- "mckown2016"
dr2$paperid[which(dr2$paperid=="zohner2023")] <- "zohnerpreprint"
# lists
sort(unique(dr2$paperid))
sort(unique(dr3$paperid))
allids <- c(unique(dr2$paperid), unique(dr3$paperid))
sort(allids[!duplicated(allids)])

# round2 doublentry should include:
doublentryexp <- c("camarero2022", "desauvage2022", "dow2022", "etzold2021", "gao2022",
"mckown2022", "silvestro2023", "soolananayakanahally2013", "vitasse2009",
"zani2020", "zohnerpreprint", "francon2020", "eckes-sherpard2020",
 "stridbeck2022", "zhu2021", "finzi2021", "chen2000", 
"ren2019")

doublentryids <- unique(dr2$paperid)[which(unique(dr2$paperid) %in% unique(dr3$paperid))]
sort(doublentryexp)
sort(doublentryids)

doubleentrydf <- rbind(
    dr2[which(dr2$paperid %in% doublentryids),],
    dr3[which(dr3$paperid %in% doublentryids),]
)

# need all unique columns to reshape ...
table(doubleentrydf$paperid, doubleentrydf$who_entered)
multirows <- c("vitasse2009", "zani2020", "zohnerpreprint")
doubleentrydfsm <- doubleentrydf[which(!doubleentrydf$paperid %in% multirows),]
table(doubleentrydfsm$paperid, doubleentrydfsm$who_entered)

# look at pairs ...
doubleentrydfsmAKEJHRL <- subset(doubleentrydfsm, who_entered=="Ailene" |
                                                  who_entered=="JHRL")
doubleentrydfsmKPEMW <- subset(doubleentrydfsm, who_entered=="KP" |
                                 who_entered=="emw")


# reshaping
dekavyalizziedfwide <- reshape(doubleentrydfsmKPEMW, idvar=c("paper_id", "paperid"), timevar="who_entered", direction = "wide")
deailenejannekedfwide <- reshape(doubleentrydfsmAKEJHRL, idvar=c("paper_id", "paperid"), timevar="who_entered", direction = "wide")

# need to work on this ... https://stackoverflow.com/questions/24561936/grep-to-search-column-names-of-a-dataframe
dekavyalizziedfwide[,c("paperid", "gsl_start_metric.KP", "gsl_start_metric.emw")]

dekavyalizziedfwide[,grep("gsl_start_metric", names(dekavyalizziedfwide), value=TRUE)]

          
comparedoubentry(dekavyalizziedfwide, c("KP", "emw"),  "gsl_start_metric")
comparedoubentry(dekavyalizziedfwide, c("KP", "emw"),  "authorsthink_evidence_gslxgrowth")
comparedoubentry(dekavyalizziedfwide, c("KP", "emw"),  "growth_metric")
comparedoubentry(dekavyalizziedfwide, c("KP", "emw"),  "youthink_evidence_gslxgrowth")

comparedoubentry(deailenejannekedfwide, c("Ailene", "JHRL"),  "gsl_start_metric")
comparedoubentry(deailenejannekedfwide, c("Ailene", "JHRL"),  "authorsthink_evidence_gslxgrowth")
comparedoubentry(deailenejannekedfwide, c("Ailene", "JHRL"),  "growth_metric")
comparedoubentry(deailenejannekedfwide, c("Ailene", "JHRL"),  "youthink_evidence_gslxgrowth")


# Just look at them all ...
for (i in names(dr3)[3:14]){
    comparedoubentry(dekavyalizziedfwide, c("KP", "emw"),  i)
}



## checking a few general things...
d <- rbind(dr2, dr3)
table(d$growth_metric)
table(d$gsl_start_metric)
subset(d, growth_metric=="photosynthesis")



## OLDER CODE, need to update once all cleaned ... 

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
