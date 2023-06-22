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
  

# after double entry meetings and final table update (we hope)
rdmf <- fread("input/round6/grephontable_rdm_fb_5.1.csv")
# akej <- fread("input/round6/grephontable_JHRLAKE-fin.csv")
achinc <- fread("input/round6/grephon table Alana and Cat final round.csv") # missing what should be col 19 "gs_metric_other"
emw <- fread("input/round6/grephontable_emw.csv")
kp <- fread("input/round6/grephontable_kp_NEW.csv")

dall <- rbind(rdmf, achinc, emw, kp) # akej

sort(unique(dall$paper_id))
# expecting 37


d <- dall
names(d)[names(d)=="authorsthink_ALTteststatistic:"] <- "authorsthink_ALTteststatistic"

# Go through consistency of entries...
table(d$growth_metric)


papernum <- length(unique(d$paper_id))

## Simplify some columns

## Deal with multiple growth metrics (questions: are we okay with number of rows?)
subset(d, growth_metric=="height,  root:shoot ratio") # Soolananayakanahally2013; this paper already seems broken out a lot so I think okay
subset(d, growth_metric=="stem density; proportion flowering; proportion fruiting") #  Wheeler2016: going with stem density 


## Deal with gsl metrics 
sort(unique(d$gsl_metric))
sort(unique(d$gsl_start_metric))
sort(unique(d$gsl_end_metric))

# growth metric
d$growth <- d$growth_metric
d$growth[grep("intra-annual", d$growth_metric)] <- "intra-annual core"
d$growth[grep("intraannual cores", d$growth_metric)] <- "intra-annual core"
d$growth[grep("photosynthe", d$growth_metric)] <- "photosynthesis" # doing this first, so we re-assign NEP ones below
d$growth[grep("dendrometer", d$growth_metric)] <- "dendrometer/circumference" # doing first so we get intra-annual core for Wheeler below
d$growth[grep("circumference at breast height", d$growth_metric)] <- "dendrometer/circumference" # doing first so we get intra-annual core for Wheeler below
d$growth[grep("carbon flux", d$growth_metric)] <- "ecosystem fluxes"
d$growth[grep("net ecosystem production", d$growth_metric)] <- "ecosystem fluxes"
d$growth[grep("in terms of both NEP and gross ecosystem photosynthesis", d$growth_metric)] <- "ecosystem fluxes"
d$growth[grep("NPP", d$growth_metric)] <- "NPP"
d$growth[grep("stem density", d$growth_metric)] <- "stem density"
d$growth[grep("height", d$growth_metric)] <- "height"

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
d$gsl[grep("all days with an average daily temperature above DTMIN", d$gsl_metric)] <- "temperature or snow metric"
d$gsl[grep("satellite derived", d$gsl_metric)] <- "satellite derived"
d$gsl[grep("NDVI", d$gsl_metric)] <- "satellite derived"
d$gsl[grep("none", d$gsl_metric)] <- "not measured"

sort(unique(d$gsl))

# Standardize GSL x growth 
table(d$authorsthink_evidence_gsxgrowth)
d$gsxgrowth <- d$authorsthink_evidence_gsxgrowth

# Questions: What to do about not sure?
gsxgrowthmaybe <- subset(d, gsxgrowth=="not sure")
gsxgrowthmaybe[,1:3] # my weird bruening2017 paper, could be a no (as it's assumed basically)

# Now look at our definition of gsl x growth ... 
table(d$ourdefinition_evidence_gslxgrowth)
d$ourdefinition_evidence_gslxgrowth[which(d$ourdefinition_evidence_gslxgrowth=="negative relationships")] <- "negative relationship"
d$gsxgrowthours <- d$ourdefinition_evidence_gslxgrowth


# Studies with ourdefinition_evidence_gslxgrowth as yes or no MUST have the right metrics, check ... 
# Questions here ... 
# Do we want to add "satellite derived" as an okay metric of vegetative phenology? I think so. 
checking <- subset(d, gsxgrowthours=="yes" | gsxgrowthours=="no")
seemswrong <- subset(checking, gsl!= "plant vegetative phenology" & gsl!="wood phenology" 
    & gsl!="satellite derived")
seemswrong[,c("paper_id", "who_entered", "gsxgrowth", "gsl", "gs_start_metric", "gs_end_metric", "gsxgrowthours")]




## Below not yet updated ... 
if(FALSE){
# More questions (esp. what does 'no' 'not sure' mean?)
unique(d$authorsthink_ALTteststatistic)
subset(d, authorsthink_ALTteststatistic=="Fig 4") # oh dear, that's me
subset(d, authorsthink_ALTteststatistic=="no") # Alana
subset(d, authorsthink_ALTteststatistic=="not sure") # Alana

# More questions -- what is the no here? And the 'correlation between height growth duration and height'?
table(d$authorsthink_evidence_gslxgrowth_suitabledays_or_starttoend)

# Endo and external factors
table(d$authorslooked_externalfactors)
table(d$authorslooked_endogenousfactors)


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
