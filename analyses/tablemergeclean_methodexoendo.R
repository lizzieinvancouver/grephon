## Cleaning up method and external/internal ##
## By Alana mostly so far ##

## This was in a file Alana sent Lizzie, then Lizzie put in tablemergeclean.R ...
## then Lizzie pulled it out so we can clean things for a while, then organize. ## 

# housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

## packages
library(data.table)
library(tidyverse)
library(stringr)


# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/grephon/grephon/analyses")
} else if (length(grep("ailene", getwd()))>0) 
{setwd("boomboom")
}

# Get the semi-cleaned file
d <- read.csv("output/grephontablesemiclean.csv")

## Below from Alana
# compress some shit ------------------------------------------------------

#fix look.ext factors
d$look.ext <- d$authorslooked_externalfactors
unique (d$authorslooked_externalfactors)

#CHECK: does "yes" mean "yes - growth" I think we all need to look at our entries with this in mind, maybe as a group we can sit and address table queries so we can discuss and be consitent?
d$look.ext[grep("yes - growth", d$authorslooked_externalfactors)] <- "yes"
d$look.ext[grep("yes - gsl", d$authorslooked_externalfactors)] <- "yes.gsl"
d$look.ext[grep("yes - gsl and growth", d$authorslooked_externalfactors)] <- "yes" #CHECK: At this point I have this as one row, coded as growth only because the "authorsFound" was unclear if they found for growth or gsl, but everyone should look at this and maybe we should add rows if needed
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

write.csv(d, "output/grephontable.csv", row.names=FALSE)
