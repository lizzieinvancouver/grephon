## Started 27 August 2023 ##
## Organizing code that is not cleaning ##

## 31 Jan 2024: Lizzie added in hypotheses table ###

# housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/grephon/grephon/analyses")
} else if (length(grep("ailene", getwd()))>0) 
{setwd("C:/Users/ailene.ettinger/Documents/GitHub/grephon/analyses")
}

## packages
library(tidyverse)

# get the data
d <- read.csv("output/grephontable.csv")


##################################
##  Looking at what we've found ##
## This should get moved eventually ## 
######################################
## code from growthgsl.R

papernum <- length(unique(d$paper_id))
studynum <- nrow(d)

# Which papers have data but do not test it?
d$paper_id[d$gsxgrowthourdef=="not tested but have data"]
withdatanottested <- d[which(d$gsxgrowthourdef=="not tested but have data"),]
numwithdatanottested <- length(unique(withdatanottested$paper_id))

# Any trends with year? No.
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

d$year <- as.numeric(substrRight(d$paper_id, 4))
plot(year~as.factor(gsxgrowthourdef), data=d)

minyr <- min(d$year, na.rm=TRUE)
maxyr <- max(d$year, na.rm=TRUE)
totyr <- maxyr-minyr

# Number of growth metrics and GSL metrics
sort(unique(d$growthclean))
sort(unique(d$gs_start_metricclean))
sort(unique(d$gs_end_metricclean))
d$gsstartendclean <- paste(d$gs_start_metricclean, d$gs_end_metricclean, sep="x")
sort(unique(d$gsstartendclean))

nstartmetrics <- length(unique(d$gs_start_metricclean))
nendmetrics <- length(unique(d$gs_end_metricclean))
ngslmetrics <- length(unique(d$gsstartendclean))
ngrowthmetrics <- length(unique(d$growthclean))


# What types of studies find evidence for this relationship in any way?
table(d$gsxgrowth)
table(d$gsxgrowthourdef)

eviany <- subset(d, gsxgrowth=="yes")
eviour <- subset(d, gsxgrowthourdef=="yes")

noeviany <- subset(d, gsxgrowth=="no")
noeviour <- subset(d, gsxgrowthourdef=="no")

papersnumanyevi <- length(unique(eviany$paper_id))
papersnumourevi <- length(unique(eviour$paper_id))

perceanyevi <- round((length(unique(eviany$paper_id))/length(unique(d$paper_id)))*100, 0)
percenoevi <- round((length(unique(noeviany$paper_id))/length(unique(d$paper_id)))*100, 0)


papersnumnoevi <- length(unique(noeviany$paper_id))
papersnumnoourevi <- length(unique(noeviour$paper_id))

table(eviany$gslsimple)
table(eviour$gslsimple)

table(eviany$gs_metric_used)
table(eviour$gs_metric_used)

table(eviany$growthsimple)
table(eviour$growthsimple)

table(eviany$study_level)
table(eviour$study_level)

table(eviany$gslxgrowth)
table(eviour$gslxgrowth)

# Does country matter? (No, but highly biased)
unique(d$country)
unique(eviany$country)
unique(eviour$country)


# Does species matter? Seems unlikely....
sort(unique(d$species_list))
sort(unique(eviany$species_list))
sort(unique(eviour$species_list))

# .. and biome
 sort(unique(d$biome))

# What do annual cores find? The three yes ones are where gsl is not mentioned or they use temperature or snow metric
annualcores <- subset(d, growth=="annual core")
table(annualcores$gsxgrowth)
table(annualcores$gsxgrowthourdef)

# What do wood phenology studies find?
woodphen <- subset(d, gsl=="wood phenology")
woodphen[,c(1:2,48:51)]

# What types of studies look at external factors vs endogenous ones?
exoyes <- d[grep("yes", d$authorslooked_externalfactors),]
endoyes <- d[grep("yes", d$authorslooked_endogenousfactors),]

exono <- d[grep("no", d$authorslooked_externalfactors),]
endono <- d[grep("no", d$authorslooked_endogenousfactors),]

table(exoyes$gsl)
table(endoyes$gsl)

table(exoyes$gs_metric_used)
table(endoyes$gs_metric_used)

table(exoyes$growth)
table(endoyes$growth)

# Simpler metrics ...
table(exoyes$growthsimple)
table(endoyes$growthsimple)
# compare to these totals
table(d$growthsimple)

table(exoyes$gslsimple)
table(endoyes$gslsimple)
# compare to these totals
table(d$gslsimple)

## Some stuff on latitude
latitudestuff <- c("MAT in origin as related to adaptation to latitude/temperature",
    "yes - for temperature x latitude relationships with radial growth")

latstudies <- d[which(d$authorsthink_ALTteststatistic %in% latitudestuff),] # hmm, missing Vitasse

# Wondering what the model studies find
dmodel <- subset(d, growthmodeled=="yes")
dmodel[,48:51]

# Which metrics find it? 
table(d$growth, d$gsxgrowthourdef) # of the common ones, annual cores do not find our definition
table(d$growth, d$gsxgrowth) # and annual cores more often than not do not find their own definition; ecosystem fluxes and height are often yes

# Endo and external factors: CHECK!
table(d$authorslooked_externalfactors)
table(d$authorslooked_endogenousfactors)


##################################
##  Code from clean_methodexoendo
######################################


# Filter and look ---------------------------------------------------------

#make new dfs
unique(d$look.ext)
yes_growth<- filter(d, look.ext  == "yes - growth" |look.ext  == "yes - growth and growing season")
yes_ext.gsl<- filter(d, look.ext  == "yes - growing season" |look.ext  == "yes - growth and growing season")
yes_ext <- rbind(yes_growth, yes_ext.gsl)
no_ext<- filter(d, look.ext  == "no" )
#not sure where to include  "yes - photosythesis or carbon flux"


found_ext<- filter(yes_growth, find.ext  =="yes - growth"|find.ext  == "yes - growth and growing season" )
found_ext.gsl<- filter(yes_ext.gsl, find.ext  == "yes - growing season" |find.ext  == "yes - growth and growing season")#not sure this is the filter we want
not.found_ext<- filter(yes_growth, find.ext  == "no" )

#what factors were found?
unique(d$what.ext)
as.data.frame(table(found_ext$what.ext))
as.data.frame(table(not.found_ext$what.ext))

#who looks for external factors?
all <- as.data.frame(table(d$method))
all
as.data.frame(table(yes_ext$method))
as.data.frame(table(no_ext$method))

#who finds external factors?
looked <- as.data.frame(table(yes_ext$method))
found <- as.data.frame(table(found_ext$method))
as.data.frame(table(found_ext.gsl$method))
as.data.frame(table(not.found_ext$method))

# Merge the tables based on Var1
who.looked <- merge(all, looked, by = "Var1", suffixes = c("_all", "_looked"))
who.looked

who.found <- merge(looked, found, by = "Var1", suffixes = c("_looked", "_found"))
who.found

###looking at growth a bit more for Box on measurements
unique(d$growth_metric)
unique(d$growthsimple)
table(d$method,d$growthsimple)
boxmetrics <- colSums(table(d$method,d$growthsimple))
boxradg <- unname(boxmetrics["radial growth"])
boxbiomassg <- unname(boxmetrics["biomass height stems"])
boxcaccg <- unname(boxmetrics["putative C assimilation"])
boxrootshootg <- unname(boxmetrics["root:shoot ratio"])



######################################
##  Hypotheses table               
## This was generated by a re-review  in fall 2023 to get the specific hypotheses mentioned             ## 
######################################
hypdall <- read.csv("output/hyp_mergeable.csv") # built in hypothesest.R
hypdall$paper_id <- tolower(gsub(" ", "", hypdall$addressed.in.which.Grephon.paper))

# Make sure we don't have duplicate hypotheses, unless we should... 
hypd <- subset(hypdall, select=c("paper_id", "wording_figure"))
hypd <- hypd[!duplicated(hypd), ] # many papers have more than 1 hypothesis
sort(unique(hypd$wording_figure))
subset(hypd, wording_figure=="") # three have no hypotheses?

# Match the names efforts so I can merge ...
sort(unique(hypd$paper_id))
sort(unique(d$paper_id))

# Basic cleanup ... I am matching to d, even though some spellings are wrong (arghh)
hypd$paper_id[hypd$paper_id=="camarrero2022"] <- "camarero2022"
hypd$paper_id[hypd$paper_id=="drew2018"] <- "drew & downes 2018"
hypd$paper_id[hypd$paper_id=="keenan2014"] <- "keenan et al 2014"
hypd$paper_id[hypd$paper_id=="vitasseetal.2009"] <- "vitasse2009"
hypd$paper_id[hypd$paper_id=="zohneretal2023"] <- "zohner2023"
hypd$paper_id[hypd$paper_id=="zuetal2021"] <- "zhu2021"
hypd$paper_id[hypd$paper_id=="soolanayakanahally2013"] <- "soolananayakanahally2013"
hypd$paper_id[hypd$paper_id=="sebastian-alconza2020"] <- "sebastian-azcona2020"

# Now about to merge ... 
sort(unique(d$paper_id))
sort(unique(hypd$paper_id))

dhyp <- merge(d, hypd, by=c("paper_id"))

## Now get numbers for the paper!

## What percent of tree ring studies report the drought hypothesis? 

treerings <- subset(dhyp, growth=="annual core")
radialgrowth <- subset(dhyp, growthsimple=="radial growth")

droughthyp <- subset(dhyp, wording_figure=="More temp = more drought (drought limitation)")
unique(paste(droughthyp$paper_id, droughthyp$growth)) # only one study is NOT radial growth
treeringsdrought  <- subset(treerings, wording_figure=="More temp = more drought (drought limitation)")
# Same answer whether you look at % of radial growth or tree ring studies!
perdroughtreering <- round(length(unique(treeringsdrought$paper_id))/length(unique(treerings$paper_id))*100)
round(length(unique(droughthyp$paper_id))/length(unique(radialgrowth$paper_id))*100)

## What types of studies report pop/sp specific responses?
# Surprised soolananayakanahally2013 did not get pop-specific response (but I think it's because it did grow more with longer season)
popsphyp  <- dhyp[which(dhyp$wording_figure %in% c("species-specific responses", "shift in allocation")),]
longernotmore  <- dhyp[which(dhyp$wording_figure %in% c("Longer growing season != more growth")),]
# But these are rarely tree ring studies ....
table(longernotmore$growth)
table(popsphyp$growth)



