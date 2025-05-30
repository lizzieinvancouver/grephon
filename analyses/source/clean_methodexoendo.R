## Cleaning up method and external/internal ##
## By Alana mostly so far ##

## This was in a file Alana sent Lizzie, then Lizzie put in tablemergeclean.R ...
## then Lizzie pulled it out so we can clean things for a while, then organize. ## 
## Now (late August 2023) it is part of the cleaning workflow ##


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

d$what.ext[grep("temp", d$ifyes_whichexternal)] <- "temperature"
d$what.ext[grep("temperature, GDD", d$ifyes_whichexternal)] <- "temperature"
d$what.ext[grep( "seasonal mean temp (separate from GSL)" , d$ifyes_whichexternal)] <- "temperature"
d$what.ext[grep( "autumn temperature, winter chilling, spring temperature", d$ifyes_whichexternal)] <- "temperature"
d$what.ext[grep(  "high summer temperatures"  , d$ifyes_whichexternal)] <- "temperature"
d$what.ext[grep(  "temperature (pre solstice)"   , d$ifyes_whichexternal)] <- "temperature"
d$what.ext[grep(  "temperature at SOS"    , d$ifyes_whichexternal)] <- "temperature"

d$what.ext[grep("temperature (summer, autumn), CO2", d$ifyes_whichexternal)] <- "temperature & CO2"
d$what.ext[grep("temperature, CO2, N, disturbance" , d$ifyes_whichexternal)] <- "temperature & CO2"
d$what.ext[grep( "CO2, warming and their interaction"  , d$ifyes_whichexternal)] <- "temperature & CO2"

d$what.ext[grep("soil water content"  , d$ifyes_whichexternal)] <- "water"
d$what.ext[grep("reduced soil moisture"   , d$ifyes_whichexternal)] <- "water"
d$what.ext[grep("timing of rainfall impacts growth not gsl"  , d$ifyes_whichexternal)] <- "water"
d$what.ext[grep("mean GDD and mean growing-season precipitation"    , d$ifyes_whichexternal)] <- "water"

d$what.ext[grep("Seasonal precip and May temp" , d$ifyes_whichexternal)] <- "temperature & precipitation"
d$what.ext[grep("spring temperature mainly, but also elevation, precipication (they also looked at land cover and other climate variables)", d$ifyes_whichexternal)] <- "temperature & precipitation"
d$what.ext[grep("VPD, SWP, Temp, Precip" , d$ifyes_whichexternal)] <- "temperature & precipitation"
d$what.ext[grep("temperature and soil water content"  , d$ifyes_whichexternal)] <- "temperature & precipitation"
d$what.ext[grep("temperature (pre solstice) , shade/light, nutrients, drought"   , d$ifyes_whichexternal)] <- "temperature & precipitation"
d$what.ext[grep("Tmin, Tmax, Tmean, relative humidity, precipitation, vpd"  , d$ifyes_whichexternal)] <- "temperature & precipitation"

d$what.ext[grep("latitude"  , d$ifyes_whichexternal)] <- "latitude or elevation" 
d$what.ext[grep("elevation"  , d$ifyes_whichexternal)] <- "latitude or elevation" 

d$what.ext[grep( "day length"  , d$ifyes_whichexternal)] <- "other"
d$what.ext[grep( "sun vs. shade"    , d$ifyes_whichexternal)] <- "other"
d$what.ext[grep("stand density and thinning method"   , d$ifyes_whichexternal)] <- "other"


# check
unique(d$what.ext)
sort(unique(paste(d$method, d$paper_id, d$study_type, d$growth_metric)))
#CHECK - can we merge  "latitude or elevation" into   "site difference" ???

###fix method factors##
## Lizzie updated these in June 2024, as they used to reference row numbers and were not correct
#fix delpierre to not be blank
d$study_type[which(d$paper_id=="delpierre2017" & d$growth_metric=="circumference at breast height")] <- "forest plots: radial growth"
d$study_type[which(d$paper_id=="delpierre2017" & d$growth_metric=="height")] <- "forest plots: other"

#fix Zani to not say permanent plot
d$study_type[which(d$paper_id=="zani2020" & 
	d$growth_metric=="photosynthesis (total growing-season net daytime photosynthesis, modelled)")] <- "remote"

d$method <- d$study_type
unique(d$study_type)
 

## Merging factors: First major work by Alana, then Lizzie checked in June 2024
d$method[grep("tree ring \\(simulated , vs-lite model\\)", d$study_type)] <- "tree ring"# had to escape the damn parentheses
d$method[grep("tree ring \\(growth\\), phenology observations \\(for GSL\\)", d$study_type)] <- "tree ring"
d$method[grep("tree-ring", d$study_type)] <- "tree ring"

d$method[grep("greenhouse or chamber (technically CHN terrace)", d$study_type)] <- "greenhouse or similar"
d$method[grep("greenhouse", d$study_type)] <- "greenhouse or similar"
d$method[grep("greenhouse or chamber experiment", d$study_type)] <- "greenhouse or similar"

d$method[which(d$study_type=="radial growth")] <- "forest plots: radial growth" 
d$method[grep("permanent plot?" , d$study_type)] <- "forest plots: radial growth" 
d$method[grep("permanent plot"  , d$study_type)] <- "forest plots: radial growth"  
d$method[grep( "permanent plot"   , d$study_type)] <- "forest plots: radial growth"

d$method[grep("provenance"  , d$study_type)] <- "provenance trial"
d$method[grep( "common garden"  , d$study_type)] <- "provenance trial"

d$method[grep("satellite-based phenology data"  , d$study_type)] <- "remote"
d$method[grep("satellite"   , d$study_type)] <- "remote"
d$method[grep( "remote sensing, flux tower"  , d$study_type)] <- "remote"

d$method[grep(  "whole forest experiment \\(eddy covariance\\)" , d$study_type)] <- "eddy covariance"
d$method[grep( "whole forest experiment, eddy covariance"   , d$study_type)] <- "eddy covariance"

d$method[grep( "repeated measurements on individuals"  , d$study_type)] <- "synthesis" # Lizzie renamed in June 2024 (this is Zohner merging LPJ model with long-term phenology observations)
d$method[which(d$paper_id=="dow2022" & d$growth_metric=="annual core")] <- "tree ring" # Lizzie renamed in June 2024
d$method[which(d$paper_id=="wheeler2016" & d$growth_metric=="stem density; proportion flowering; proportion fruiting")] <- "forest plots: other" # category covers height also
d$method[which(d$paper_id=="zohner2023" & d$growth_metric=="GPP")] <- "eddy covariance" # 10 fluxnet sites mostly located in North America and Europe

#Check
unique(d$method)


###Fix endogenous factors 
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

