## Cleaning up method and external/internal ##
## By Alana mostly so far ##

## This was in a file Alana sent Lizzie, then Lizzie put in tablemergeclean.R ...
## then Lizzie pulled it out so we can clean things for a while, then organize. ## 


## Below from Alana
# compress some shit ------------------------------------------------------

#fix look.ext factors
d$look.ext <- d$authorslooked_externalfactors
unique (d$authorslooked_externalfactors)


d$look.ext[grep("yes - length of growing season", d$authorslooked_externalfactors)] <- "yes - growing season"  
 d$look.ext[grep("yes - photosynthesis" , d$authorslooked_externalfactors)] <- "yes - photosythesis or carbon flux"
 d$look.ext[grep( "yes - carbon flux" , d$authorslooked_externalfactors)] <- "yes - photosythesis or carbon flux" 
 
# d$look.ext[grep("yes - length of growing season", d$authorslooked_externalfactors)] <- "yes.gsl"
# #did I do that right?
unique(d$look.ext)
#CHECK - can "yes - [gs_end_metric]" be included in  "yes - growing season" ??? see other query below

#fix find.ext factors
d$find.ext <- d$authorsfoundevidencefor_externalfactors
unique (d$authorsfoundevidencefor_externalfactors)


d$find.ext[grep("yes - length of growing season", d$authorsfoundevidencefor_externalfactors)] <- "yes - growing season"
d$find.ext[grep("yes - carbon flux" , d$authorsfoundevidencefor_externalfactors)] <- "yes - photosythesis or carbon flux"
d$find.ext[grep("yes - photosynthesis" , d$authorsfoundevidencefor_externalfactors)] <- "yes - photosythesis or carbon flux"
# d$find.ext[grep("precipitation", d$authorsfoundevidencefor_externalfactors)] <- "yes" ##CHECK
# d$find.ext[grep("Tmin, Tmax, Tmean, relative humidity, precipitation, vpd", d$authorsfoundevidencefor_externalfactors)] <- "yes" ##CHECK
# d$find.ext[grep("temperature, precipitation, soil moisture", d$authorsfoundevidencefor_externalfactors)] <- "yes" ##CHECK
# d$find.ext[grep("precipitation, min temperature", d$authorsfoundevidencefor_externalfactors)] <- "yes" ##CHECK
# d$find.ext[grep("yes - length of growing season", d$authorsfoundevidencefor_externalfactors)] <- "yes.gsl"
# d$find.ext[grep("yes - gsl", d$authorsfoundevidencefor_externalfactors)] <- "yes.gsl"
 
# #did I do that right?
unique(d$find.ext)
#CHECK -- what is "yes - [gs_end_metric]"  and can it be turned to  "yes - growing season" 

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
d$what.ext[grep("Tmin, Tmax, Tmean, relative humidity, precipitation, vpd"  , d$ifyes_whichexternal)] <- "temperature&precipitation"

d$what.ext[grep("latitude"  , d$ifyes_whichexternal)] <- "latitudeORelevation" 
d$what.ext[grep("elevation"  , d$ifyes_whichexternal)] <- "latitudeORelevation" 

d$what.ext[grep( "day length"  , d$ifyes_whichexternal)] <- "other"
d$what.ext[grep( "sun vs. shade"    , d$ifyes_whichexternal)] <- "other"
d$what.ext[grep("stand density and thinning method"   , d$ifyes_whichexternal)] <- "other"

#did I do that right?
unique(d$what.ext)
#CHECK - can we merge  "latitudeORelevation" into   "site difference" ???


###fix method factors##

#fix delpierre to not be blank
reword <- "radial growth"
d[45, 8] <- reword 
reword <- "height"
d[44, 8] <- reword 

#fix Zani to not say permanent plot
reword <- "remote"# in this case remote photosynthesis with ground truthing??
d[18, 8] <- reword

d$method <- d$study_type
unique(d$study_type)
 

##CHECK - this whole section is something we should all agree on the way in which factors are merged
d$method[grep("tree ring \\(simulated , vs-lite model\\)", d$study_type)] <- "tree ring"# had to escape the damn parentheses
d$method[grep("tree ring \\(growth\\), phenology observations \\(for GSL\\)", d$study_type)] <- "tree ring"
d$method[grep("tree-ring", d$study_type)] <- "tree ring"

d$method[grep("greenhouse or chamber (technically CHN terrace)", d$study_type)] <- "greenhouse or chamber"
d$method[grep("greenhouse", d$study_type)] <- "greenhouse or chamber"
d$method[grep("greenhouse or chamber experiment", d$study_type)] <- "greenhouse or chamber"

d$method[grep("permanent plot?" , d$study_type)] <- "radial growth" 
d$method[grep("permanent plot"  , d$study_type)] <- "radial growth"  
d$method[grep( "permanent plot"   , d$study_type)] <- "radial growth"

d$method[grep("provenance"  , d$study_type)] <- "provenance trial"
d$method[grep( "common garden"  , d$study_type)] <- "provenance trial"

d$method[grep("satellite-based phenology data"  , d$study_type)] <- "remote"
d$method[grep("satellite"   , d$study_type)] <- "remote"
d$method[grep( "remote sensing, flux tower"  , d$study_type)] <- "remote"

d$method[grep(  "whole forest experiment \\(eddy covariance\\)" , d$study_type)] <- "eddy covariance"
d$method[grep( "whole forest experiment, eddy covariance"   , d$study_type)] <- "eddy covariance"

d$method[grep( "repeated measurements on individuals"  , d$study_type)] <- "phenology observation"

#did I do that right?
unique(d$method)


###Fix endogenous factors ##CHECK - this whole section is new
names(d)
d$endo_found <- d$"authorsfoundevidencefor_endogenousfactors"
unique(d$authorsfoundevidencefor_endogenousfactors)

d$what.endo <- d$"ifyes_whichendogenous"
unique(d$ifyes_whichendogenous)

d$what.endo[grep("daylength, species", d$ifyes_whichendogenous)] <- "taxanomic variation"
d$what.endo[grep("functional types"  , d$ifyes_whichendogenous)] <- "taxanomic variation"
d$what.endo[grep("species", d$ifyes_whichendogenous)] <- "taxanomic variation"
d$what.endo[grep("differences in relationship between SOS and spring NEP due to species composition of the site"   , d$ifyes_whichendogenous)] <- "taxanomic variation"
d$what.endo[grep( "differences in relationship between EOS and autumn NEP due to species composition of the site"  , d$ifyes_whichendogenous)] <- "taxanomic variation"
d$what.endo[grep("species composition and lagged productivity effects. Specifically, NEP of ENF, evergreen needle leaved forests was less sensitive to earlier SOS and later EOS than that of DBF, deciduous broadleaved forests. Additionally, they found stronger relationships when looking at spatial differences in growing season metrics / NEP than when looking at temporal variation. Finally, there were carry over effects such that an anomoly in GEP in one year resulted in an anomoly in GEP in the next nest year.", d$ifyes_whichendogenous)] <- "taxanomic variation"##CHECK - maybe this should be two rows?

d$what.endo[grep("early season GPP has a negative effect on autumn (as measured by photosynthesis declines), whlle late season GPP had a positive effect"    , d$ifyes_whichendogenous)] <- "photosynthetic timing"
d$what.endo[grep("greater photosynthesis early in the growing season is associated with earlier senescence, while greater photosynthesis late in the growing season is associated with later senescence"     , d$ifyes_whichendogenous)] <- "photosynthetic timing"
d$what.endo[grep("greater GPP early in the growing season is associated with earlier EOS" , d$ifyes_whichendogenous)] <- "photosynthetic timing"
d$what.endo[grep("greater photosynthesis early in the growing season is associated with earlier senescence" , d$ifyes_whichendogenous)] <- "photosynthetic timing"
d$what.endo[grep("early season GPP has a negative effect on autumn \\(as measured by photosynthesis declines\\), whlle late season GPP had a positive effect"   , d$ifyes_whichendogenous)] <- "photosynthetic timing"

d$what.endo[grep("provenance. Specifically they found evidence that provenances differed in both growth and phenology, but they did not test the gsl x growth \\(table 3\\) at the provenace level" , d$ifyes_whichendogenous)] <- "provenance"
d$what.endo[grep("elevation" , d$ifyes_whichendogenous)] <- NA
d$what.endo[grep(" no for latitude and bud burst timing and lammas are not realted to growth allocation, early buds = height, late buds = more roots"   , d$ifyes_whichendogenous)] <- NA

#check
unique(d$what.endo)



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
