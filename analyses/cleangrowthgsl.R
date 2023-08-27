## Started 2 May 2023 ##
## By Lizzie so far ##

## On the train into Zurich, we made the connection! ##


sort(unique(dall$paper_id))
# expecting 37 I thought ... but we have 38, but we have Soolananayakanahally2013 duplicate (fixed below)

d <- dall
names(d)[names(d)=="authorsthink_ALTteststatistic:"] <- "authorsthink_ALTteststatistic"
d$paper_id <- tolower(d$paper_id) # who knew that R sorts capital letters first, then lowercase?


###########################
##  Cleaning up metrics  ##
## By making new columns ##
###########################

# Go through consistency of entries...

# First is growth metric, we make a new column called growth
table(d$growth_metric)

## Check by CJC 4Aug:
sool <- d[(d$paper_id %in% c("soolananayakanahally2013", "soolananayakanahally2014")),]
table(sool$growth_metric, sool$paper_id)
## Updated by CJC 9Aug:
d$paper_id <- ifelse(d$paper_id == "soolananayakanahally2014", "soolananayakanahally2013", d$paper_id)
sort(unique(d$paper_id))


## Deal with multiple growth metrics (questions: are we okay with number of rows?)
subset(d, growth_metric=="height,  root:shoot ratio") # Soolananayakanahally2013; this paper already seems broken out a lot so I think okay
## CJC 4Aug comment: Soolananayakanahally2013 has "height" and "height, root:shoot ratio", I think we can separate the two maybe?
subset(d, growth_metric=="stem density; proportion flowering; proportion fruiting") #  Wheeler2016: going with stem density (emw reviewed this paper)

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
d$growth[grep("NEP", d$growth_metric)] <- "ecosystem fluxes"
d$growth[grep("NPP", d$growth_metric)] <- "ecosystem fluxes"
d$growth[grep("GPP", d$growth_metric)] <- "ecosystem fluxes"

# Stuff I am calling 'biomass height stems'
d$growth[which(d$growth=="height")] <- "biomass height stems"
d$growth[which(d$growth=="stem density")] <- "biomass height stems"
d$growth[which(d$growth=="biomass")] <- "biomass height stems"


# CHECK: Ask Fredi's opinion here
d[grep("(simulated to intraannual)", d$growth),]
# but for now ... 
d$growth[which(d$growth=="annual core (simulated to intraannual)")] <- "annual core"

# Getting down to fewer ... 
d$growth[which(d$growth=="cell production (number of cells)")] <- "intra-annual core (xylogeneis)"
d$growth[which(d$growth=="growth anomalies")] <- "intra-annual core (xylogeneis)"


# growth metric is modeled or not?
d$growthmodeled <- "no"
d$growthmodeled[grep("model", d$growth_metric)] <- "yes"
d$growthmodeled[grep("simulated", d$growth_metric)] <- "yes"

# Question .... which categories can we combine?
# NEP is similar to NPP?
unique(sort(d$growth)) 

# growth metric: simplified further
d$growthsimple <- d$growth
d$growthsimple[which(d$growth=="annual core")] <- "radial growth"
d$growthsimple[which(d$growth=="dendrometer/circumference")] <- "radial growth"
d$growthsimple[which(d$growth=="intra-annual core (xylogeneis)")] <- "radial growth"
d$growthsimple[which(d$growth=="ecosystem fluxes")] <- "putative C assimilation"
d$growthsimple[which(d$growth=="NDVI/greenness")] <- "putative C assimilation"
d$growthsimple[which(d$growth=="photosynthesis")] <- "putative C assimilation"

unique(sort(d$growthsimple)) 


## Next! Deal with gsl metrics 
sort(unique(d$gsl_metric))
sort(unique(d$gs_start_metric))
sort(unique(d$gs_end_metric))
# d$paper_id[which(d$gs_end_metric=="end vegetative")] # lots of these end vegetative, but they are across papers

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

# gsl metric: simplified further
d$gslsimple <- d$gsl
d$gslsimple[which(d$gsl=="plant vegetative phenology")] <- "vegtative phenology"
d$gslsimple[which(d$gsl=="satellite derived")] <-  "vegtative phenology"
d$gslsimple[which(d$gsl=="temperature or snow metric")] <- "climate and date"
d$gslsimple[which(d$gsl=="date")] <- "climate and date"

sort(unique(d$gslsimple))

# Compound metric
d$gslxgrowthsimple <- paste(d$gslsimple, d$growthsimple, sep=" x ")
table(d$gslxgrowthsimple)

# Standardize GSL x growth 
table(d$authorsthink_evidence_gsxgrowth)
d$gsxgrowth <- d$authorsthink_evidence_gsxgrowth

# Questions: What to do about not sure?
gsxgrowthmaybe <- subset(d, gsxgrowth=="not sure")
gsxgrowthmaybe[,1:3] # my weird bruening2017 paper, could be a no (as it's assumed basically)

# Now look at our definition of gsl x growth ... 
table(d$ourdefinition_evidence_gslxgrowth)
d$ourdefinition_evidence_gslxgrowth[which(d$ourdefinition_evidence_gslxgrowth=="negative relationships")] <- "negative relationship"
d$gsxgrowthourdef <- d$ourdefinition_evidence_gslxgrowth
d$gsxgrowthourdef[grep("not tested but have data", d$ourdefinition_evidence_gslxgrowth)] <- "not tested but have data"

table(d$gsxgrowthourdef)
### CJC 4Aug: I'm curious about the blank entry...
subset(d[(d$gsxgrowthourdef==""),]) ## I think this should be "no"


###########################
## Misc cleaning ##
###########################

# Studies with ourdefinition_evidence_gslxgrowth as yes or no MUST have the right metrics, check ... 
# Questions here ... 
# Do we want to add "satellite derived" as an okay metric of vegetative phenology? I think so. 
checking <- subset(d, gsxgrowthourdef=="yes" | gsxgrowthourdef=="no")
seemswrong <- subset(checking, gsl!= "plant vegetative phenology" & gsl!="wood phenology" 
    & gsl!="satellite derived")
seemswrong[,c("paper_id", "who_entered", "gsxgrowth", "gsl", "gs_start_metric", "gs_end_metric", "gsxgrowthourdef")] # When this is no rows, we say hurrah!

# Wondering what the model studies find
dmodel <- subset(d, growthmodeled=="yes")
dmodel[,48:51]

# Which metrics find it? 
table(d$growth, d$gsxgrowthourdef) # of the common ones, annual cores do not find our definition
table(d$growth, d$gsxgrowth) # and annual cores more often than not do not find their own definition; ecosystem fluxes and height are often yes

# Endo and external factors: CHECK!
table(d$authorslooked_externalfactors)
table(d$authorslooked_endogenousfactors)
# ... a bunch of these lack the gsl or growth part
missingstuffendoexo <- subset(d, authorslooked_externalfactors=="yes" | authorslooked_endogenousfactors=="yes" )
missingstuffendoexo[,c("paper_id", "who_entered")]




