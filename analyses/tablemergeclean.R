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
library(tidyverse)
library(stringr)


setwd("~/Documents/git/projects/grephon/grephon/analyses")

## needed functions

## Flags
suppressrichardson <- FALSE
  

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
# d$paper_id[which(d$gs_end_metric=="end vegetative")] # lots of these end vegetative, but they are across papers

# growth metric
d$growth <- d$growth_metric
d$growth[grep("intra-annual", d$growth_metric)] <- "intra-annual core (xylogeneis)"
d$growth[grep("intraannual cores", d$growth_metric)] <- "intra-annual core (xylogeneis)"
d$growth[grep("photosynthe", d$growth_metric)] <- "photosynthesis" # doing this first, so we re-assign NEP ones below
d$growth[grep("ring width", d$growth_metric)] <- "annual core" # Alana confirmed this is correct
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

# Which metrics find it? 
table(d$growth, d$gsxgrowthours) # of the common ones, annual cores do not find our definition
table(d$growth, d$gsxgrowth) # and annual cores more often than not do not find their own definition; ecosystem fluxes and height are often yes

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


## Below from Alana
# compress some shit ------------------------------------------------------


#fix look.ext factors
d$look.ext <- d$authorslooked_externalfactors
unique (d$authorslooked_externalfactors)

#CHECK: does "yes" mean "yes - growth" I think we all need to look at our entries with this in mind, maybe as a group we can sit and address table queries so we can discuss and be consitent?
d$look.ext[grep("yes - growth", d$authorslooked_externalfactors)] <- "yes"
d$look.ext[grep("yes - gsl", d$authorslooked_externalfactors)] <- "yes.gsl"
d$look.ext[grep("yes - gsl and growth", d$authorslooked_externalfactors)] <- "yes"#CHECK: At this point I have this as one row, coded as growth only because the "authorsFound" was unclear if they found for growth or gsl, but everyone should look at this and maybe we should add rows if needed
d$look.ext[grep("yes - length of growing season", d$authorslooked_externalfactors)] <- "yes.gsl"
#did I do that right?
unique(d$look.ext)

#fix find.ext factors
d$find.ext <- d$authorsfoundevidencefor_externalfactors
unique (d$authorsfoundevidencefor_externalfactors)

#CHECK: does "yes" mean "yes - growth"?
d$find.ext[grep("yes - growth", d$authorsfoundevidencefor_externalfactors)] <- "yes"
d$find.ext[grep("yes -growth", d$authorsfoundevidencefor_externalfactors)] <- "yes"
d$find.ext[grep("yes - growth (NPP)", d$authorsfoundevidencefor_externalfactors)] <- "yes"
d$find.ext[grep("precipitation", d$authorsfoundevidencefor_externalfactors)] <- "yes" ##CHECK
d$find.ext[grep("Tmin, Tmax, Tmean, relative humidity, precipitation, vpd", d$authorsfoundevidencefor_externalfactors)] <- "yes" ##CHECK
d$find.ext[grep("temperature, precipitation, soil moisture", d$authorsfoundevidencefor_externalfactors)] <- "yes" ##CHECK
d$find.ext[grep("precipitation, min temperature", d$authorsfoundevidencefor_externalfactors)] <- "yes" ##CHECK
d$find.ext[grep("yes - length of growing season", d$authorsfoundevidencefor_externalfactors)] <- "yes.gsl"
d$find.ext[grep("yes - gsl", d$authorsfoundevidencefor_externalfactors)] <- "yes.gsl"
#did I do that right?
unique(d$find.ext)

#fix what.ext factors
d$what.ext <- d$ifyes_whichexternal
unique (d$ifyes_whichexternal)

##CHECK - I think this is another section to review as a group, definitely made some executive decisions in the recoding
d$what.ext[grep("temp", d$ifyes_whichexternal)] <- "temperature"
d$what.ext[grep("temperature, GDD", d$ifyes_whichexternal)] <- "temperature"
d$what.ext[grep( "seasonal mean temp (separate from GSL)" , d$ifyes_whichexternal)] <- "temperature"
d$what.ext[grep( "autumn temperature, winter chilling, spring temperature", d$ifyes_whichexternal)] <- "temperature"
d$what.ext[grep(  "high summer temperatures"  , d$ifyes_whichexternal)] <- "temperature"
d$what.ext[grep(  "temperature (pre solstice)"   , d$ifyes_whichexternal)] <- "temperature"
d$what.ext[grep(  "temperature at SOS"    , d$ifyes_whichexternal)] <- "temperature"

d$what.ext[grep("temperature (summer, autumn), CO2", d$ifyes_whichexternal)] <- "temperature&CO2"
d$what.ext[grep("temperature, CO2, N, disturbance" , d$ifyes_whichexternal)] <- "temperature&CO2"
d$what.ext[grep( "CO2, warming and their interaction"  , d$ifyes_whichexternal)] <- "temperature&CO2"

d$what.ext[grep("soil water content"  , d$ifyes_whichexternal)] <- "water"
d$what.ext[grep("reduced soil moisture"   , d$ifyes_whichexternal)] <- "water"
d$what.ext[grep("timing of rainfall impacts growth not gsl"  , d$ifyes_whichexternal)] <- "water"
d$what.ext[grep("mean GDD and mean growing-season precipitation"    , d$ifyes_whichexternal)] <- "water"

d$what.ext[grep("Seasonal precip and May temp" , d$ifyes_whichexternal)] <- "temperature&precipitation"
d$what.ext[grep("spring temperature mainly, but also elevation, precipication (they also looked at land cover and other climate variables)", d$ifyes_whichexternal)] <- "temperature&precipitation"
d$what.ext[grep("VPD, SWP, Temp, Precip" , d$ifyes_whichexternal)] <- "temperature&precipitation"
d$what.ext[grep("temperature and soil water content"  , d$ifyes_whichexternal)] <- "temperature&precipitation"
d$what.ext[grep("temperature (pre solstice) , shade/light, nutrients, drought"   , d$ifyes_whichexternal)] <- "temperature&precipitation"

d$what.ext[grep("latitude"  , d$ifyes_whichexternal)] <- "site difference" 
d$what.ext[grep("elevation"  , d$ifyes_whichexternal)] <- "site difference" 

d$what.ext[grep( "day length"  , d$ifyes_whichexternal)] <- "other"
d$what.ext[grep( "sun vs. shade"    , d$ifyes_whichexternal)] <- "other"
d$what.ext[grep("stand density and thinning method"   , d$ifyes_whichexternal)] <- "other"

#did I do that right?
unique(d$what.ext)

#fix method factors
d$method <- d$study_type
unique(d$study_type)
 

##CHECK - this whole section is something we should all agree on the way in which factors are merged
d$method[grep("tree ring \\(simulated , vs-lite model\\)", d$study_type)] <- "tree ring"# had to escape the damn parentheses
d$method[grep("tree ring \\(growth\\), phenology observations \\(for GSL\\)", d$study_type)] <- "tree ring"
d$method[grep("tree-ring", d$study_type)] <- "tree ring"

d$method[grep("greenhouse or chamber (technically CHN terrace)", d$study_type)] <- "greenhouse or chamber"
d$method[grep("greenhouse", d$study_type)] <- "greenhouse or chamber"
d$method[grep("greenhouse or chamber experiment", d$study_type)] <- "greenhouse or chamber"

d$method[grep("permanent plot?" , d$study_type)] <- "radial growth"  ###CHECK re-check that this is a plot?
d$method[grep("permanent plot"  , d$study_type)] <- "radial growth"  ###CHECK not sure about this designation either....

d$method[grep("provenance"  , d$study_type)] <- "provenance trial"
d$method[grep( "common garden"  , d$study_type)] <- "provenance trial"###CHECK -is this correct?

d$method[grep("satellite-based phenology data"  , d$study_type)] <- "remote"
d$method[grep("satellite"   , d$study_type)] <- "remote"
d$method[grep( "remote sensing, flux tower"  , d$study_type)] <- "remote"

d$method[grep(  "whole forest experiment \\(eddy covariance\\)" , d$study_type)] <- "eddy covariance"
d$method[grep( "whole forest experiment, eddy covariance"   , d$study_type)] <- "eddy covariance"

#did I do that right?
unique(d$method)



# Filter and look ---------------------------------------------------------



#make new dfs
yes_ext<- filter(d, look.ext  == "yes" )
yes_ext.gsl<- filter(d, look.ext  == "yes.gsl" )
no_ext<- filter(d, look.ext  == "no" )


found_ext<- filter(yes_ext, find.ext  == "yes" )
found_ext.gsl<- filter(yes_ext.gsl, find.ext  == "yes.gsl" )#not sure this is the filter we want
not.found_ext<- filter(yes_ext, find.ext  == "no" )

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
write.csv(d, "output/grephontable.csv", row.names=FALSE)
