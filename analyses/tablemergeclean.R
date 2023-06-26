## Started 2 May 2023 ##
## By Lizzie so far ##

## On the train into Zurich, we made the connection! ##

## Updated 22 June 2023, hoping to ..
# (1) Check the files submitted
# (2) Try to report out to people our overal findings and what they are reporting

# housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

## packages
library(data.table)
library(stringr)


setwd("~/Documents/git/projects/grephon/grephon/analyses")

## needed functions

## Flags
suppressrichardson <- TRUE
  

# after double entry meetings and final table update (we hope)
rdmfprep <- fread("input/round6/grephontable_rdm_fb_5.2.csv")
akej <- fread("input/round6/grephontable_JHRLAKE-fin-25Jun2023.csv") 
achinc <- fread("input/round6/grephon table Alana and Cat final round.csv") 
emw <- fread("input/round6/grephontable_emw.csv")
kp <- fread("input/round6/grephontable_kp_NEW.csv")

# cleaning up some issues before merging data
rdmf <- subset(rdmfprep, paper_id!="")
if(suppressrichardson){
    akej <- akej[which(akej$paper_id!="Richardson2020"),]
}

dall <- rbind(rdmf, akej, achinc, emw, kp) 

sort(unique(dall$paper_id))
# expecting 37 I thought ... amd we have 38
# so I should check my original counting

d <- dall
names(d)[names(d)=="authorsthink_ALTteststatistic:"] <- "authorsthink_ALTteststatistic"
d$paper_id <- tolower(d$paper_id) # who knew that R sorts capital letters first, then lowercase?

# Go through consistency of entries...
table(d$growth_metric)

papernum <- length(unique(d$paper_id))

## Simplify some columns

## Deal with multiple growth metrics (questions: are we okay with number of rows?)
subset(d, growth_metric=="height,  root:shoot ratio") # Soolananayakanahally2013; this paper already seems broken out a lot so I think okay
subset(d, growth_metric=="stem density; proportion flowering; proportion fruiting") #  Wheeler2016: going with stem density 


## Deal with gsl metrics 
sort(unique(d$gsl_metric))
sort(unique(d$gs_start_metric))
sort(unique(d$gs_end_metric))

# growth metric
d$growth <- d$growth_metric
d$growth[grep("intra-annual", d$growth_metric)] <- "intra-annual core (xylogeneis)"
d$growth[grep("intraannual cores", d$growth_metric)] <- "intra-annual core (xylogeneis)"
d$growth[grep("photosynthe", d$growth_metric)] <- "photosynthesis" # doing this first, so we re-assign NEP ones below
d$growth[grep("ring width", d$growth_metric)] <- "annual core" # CHECK
# The below line overwrites "dendrometer, intra-annual core (xylogeneis)" to dendrometer
d$growth[grep("dendrometer", d$growth_metric)] <- "dendrometer/circumference" # doing first so we get intra-annual core for Wheeler below
d$growth[grep("circumference at breast height", d$growth_metric)] <- "dendrometer/circumference" # doing first so we get intra-annual core for Wheeler below
d$growth[grep("stem density", d$growth_metric)] <- "stem density"
# The below overwrites "height,  root:shoot ratio" 
d$growth[grep("height", d$growth_metric)] <- "height"
d$growth[grep("growth anomalies", d$growth_metric)] <- "growth anomalies"
d$growth[grep("NDVI/green", d$growth_metric)] <- "NDVI/greenness"

# A ton of stuff falling under ecosystem fluxes
d$growth[grep("carbon flux", d$growth_metric)] <- "ecosystem fluxes"
d$growth[grep("net ecosystem production", d$growth_metric)] <- "ecosystem fluxes"
d$growth[grep("in terms of both NEP and gross ecosystem photosynthesis", d$growth_metric)] <- "ecosystem fluxes"
d$growth[grep("CO2 assimilation", d$growth_metric)] <- "ecosystem fluxes"
d$growth[grep("NEP, GEP", d$growth_metric)] <- "ecosystem fluxes"
d$growth[grep("NPP", d$growth_metric)] <- "ecosystem fluxes"



# CHECK
# Need to discuss with Janneke and Ailene this entry ... they probably should have used gs_metric_other to limit the numnber of entries here. 
d[grep("(calcuated from flux towers)", d$growth_metric)]
# Ask Fredi's opinion here
d[grep("(simulated to intraannual)", d$growth),]

# growth metric is modeled or not?
d$growthmodeled <- "no"
d$growthmodeled[grep("model", d$growth_metric)] <- "yes"
d$growthmodeled[grep("simulated", d$growth_metric)] <- "yes"


# Question .... which categories can we combine?
# NEP is similar to NPP?
unique(sort(d$growth)) 

# GSL metric
d$gsl <- d$gsl_metric
d$gsl[grep("fluxnet derived", d$gsl_metric)] <- "flux related"
d$gsl[grep("plant vegetative phenology", d$gsl_metric)] <- "plant vegetative phenology"
d$gsl[grep("the period when the forest was in leaf", d$gsl_metric)] <- "plant vegetative phenology"
d$gsl[grep("GDD", d$gsl_metric)] <- "temperature or snow metric"
d$gsl[grep("snowmelt day", d$gsl_metric)] <- "temperature or snow metric"
d$gsl[grep("monthly temperature data", d$gsl_metric)] <- "temperature or snow metric"
d$gsl[grep("Mar-May Temperature", d$gsl_metric)] <- "temperature or snow metric"
d$gsl[grep("minimum temperature", d$gsl_metric)] <- "temperature or snow metric"
d$gsl[grep("spring \\(mar-may\\) temperature", d$gsl_metric)] <- "temperature or snow metric"
d$gsl[grep("climate related to satellite phenology data", d$gsl_metric)] <- "temperature or snow metric" # this is Dow
d$gsl[grep("all days with an average daily temperature above DTMIN", d$gsl_metric)] <- "temperature or snow metric"
d$gsl[grep("satellite derived", d$gsl_metric)] <- "satellite derived"
d$gsl[grep("NDVI", d$gsl_metric)] <- "satellite derived"
d$gsl[grep("none", d$gsl_metric)] <- "not measured"

sort(unique(d$gsl))

# Compound metric
d$gslxgrowth <- paste(d$gsl, d$growth, sep=" x ")
table(d$gslxgrowth)

# Standardize GSL x growth 
table(d$authorsthink_evidence_gsxgrowth)
d$gsxgrowth <- d$authorsthink_evidence_gsxgrowth

# CHECK -- this is when I gave up on dealing with the richardson paper ... this feels not super useful
d[grep("yes \\(both sites\\)", d$authorsthink_evidence_gsxgrowth),]

# Questions: What to do about not sure?
gsxgrowthmaybe <- subset(d, gsxgrowth=="not sure")
gsxgrowthmaybe[,1:3] # my weird bruening2017 paper, could be a no (as it's assumed basically)

# Now look at our definition of gsl x growth ... 
table(d$ourdefinition_evidence_gslxgrowth)
d$ourdefinition_evidence_gslxgrowth[which(d$ourdefinition_evidence_gslxgrowth=="negative relationships")] <- "negative relationship"
d$gsxgrowthours <- d$ourdefinition_evidence_gslxgrowth
d$gsxgrowthours[grep("not tested but have data", d$ourdefinition_evidence_gslxgrowth)] <- "not tested but have data"

table(d$gsxgrowthours)

# Studies with ourdefinition_evidence_gslxgrowth as yes or no MUST have the right metrics, check ... 
# Questions here ... 
# Do we want to add "satellite derived" as an okay metric of vegetative phenology? I think so. 
checking <- subset(d, gsxgrowthours=="yes" | gsxgrowthours=="no")
seemswrong <- subset(checking, gsl!= "plant vegetative phenology" & gsl!="wood phenology" 
    & gsl!="satellite derived")
seemswrong[,c("paper_id", "who_entered", "gsxgrowth", "gsl", "gs_start_metric", "gs_end_metric", "gsxgrowthours")] # When this is no rows, we say hurrah!

# Wondering what the model studies find
dmodel <- subset(d, growthmodeled=="yes")
dmodel[,48:51]

# Endo and external factors: CHECK!
table(d$authorslooked_externalfactors)
table(d$authorslooked_endogenousfactors)
# ... a bunch of these lack the gsl or growth part
missingstuffendoexo <- subset(d, authorslooked_externalfactors=="yes" | authorslooked_endogenousfactors=="yes" )
missingstuffendoexo[,c("paper_id", "who_entered")]

# Which papers have data but do not test it?
d$paper_id[d$gsxgrowthours=="not tested but have data"]

# What types of studies find evidence for this relationship in any way?
eviany <- subset(d, gsxgrowth=="yes")
eviour <- subset(d, gsxgrowthours=="yes")

noeviany <- subset(d, gsxgrowth=="no")
noeviour <- subset(d, gsxgrowthours=="no")

papersnumanyevi <- length(unique(eviany$paper_id))
papersnumourevi <- length(unique(eviour$paper_id))

table(eviany$gsl)
table(eviour$gsl)

table(eviany$gs_metric_used)
table(eviour$gs_metric_used)

table(eviany$growth)
table(eviour$growth)

table(eviany$study_level)
table(eviour$study_level)

table(eviany$gslxgrowth)
table(eviour$gslxgrowth)

# What do annual cores find? The three yes ones are where gsl is not mentioned or they use temperature or snow metric
annualcores <- subset(d, growth=="annual core")
table(annualcores$gsxgrowth)
table(annualcores$gsxgrowthours)

# What do wood phenology studies find?
woodphen <- subset(d, gsl=="wood phenology")
woodphen[,c(1:2,48:51)]
subset(d, paper_id=="oddi2022") # CHECK: Not clear to me how this has "no data for this" for the last entry for this paper

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


## Below not yet updated ... 
if(FALSE){
# More questions (esp. what does 'no' 'not sure' mean?)
unique(d$authorsthink_ALTteststatistic)
subset(d, authorsthink_ALTteststatistic=="Fig 4") # oh dear, that's me
subset(d, authorsthink_ALTteststatistic=="no") # Alana
subset(d, authorsthink_ALTteststatistic=="not sure") # Alana


latitudestuff <- c("MAT in origin as related to adaptation to latitude/temperature",
    "yes - for temperature x latitude relationships with radial growth")


latstudies <- d[which(d$authorsthink_ALTteststatistic %in% latitudestuff),] # hmm, missing Vitasse

## Questions ... specific (if time allows)
subset(d, growth_metric=="") ## did Alana and Cat mean to leave this empty in desauvage2022?
# Kavya has 3 rows for wheeler, did she mean it?
subset(d, growth_metric=="height,  root:shoot ratio") # Soolananayakanahally2013
subset(d, authorsthink_evidence_gslxgrowth=="yes, no") # Alana!



##
table(d$gsl_metric)
subset(d, gsl_metric=="estimated start to estimated end")
unique(d$species_list)
}


# Write it out ...
# write.csv(d, "output/grephontable.csv", row.names=FALSE)
