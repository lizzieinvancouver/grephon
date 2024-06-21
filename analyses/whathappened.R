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
#plot(year~as.factor(gsxgrowthourdef), data=d)

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

# Trends across years analysis
quantile(noeviany$year)
quantile(eviany$year)
yesandnoeviany <- subset(d, gsxgrowth=="no"|gsxgrowth=="yes")
yesandnoeviany$gsxgrowthdummy <- yesandnoeviany$gsxgrowth
yesandnoeviany$gsxgrowthdummy[which(yesandnoeviany$gsxgrowth=="yes")] <- 1
yesandnoeviany$gsxgrowthdummy[which(yesandnoeviany$gsxgrowth=="no")] <- 0
yesandnoeviany$gsxgrowthdummy <- as.numeric(yesandnoeviany$gsxgrowthdummy)
if(FALSE){
library(rstanarm)
fityr <- stan_glm(gsxgrowthdummy ~ year, family=binomial(link="logit"), data=yesandnoeviany)
print(fityr)
plot(gsxgrowthdummy~year, data= yesandnoeviany)
curve(invlogit(coef(fityr)[1] + coef(fityr)[2]*x), add=TRUE)
}

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
##  Species cleaning (from plotspecies.R)
######################################

#do some cleaning
spd<-subset(d,select=c("paper_id","study_type","continent","species_num","species_list","authorsthink_evidence_gsxgrowth"))
spd$species_list[which(spd$species_list=="ITRDB and ITPCAS")]<-NA
spd$species_list[which(spd$species_list=="no specific species")]<-NA
spd$species_list[which(spd$species_list=="BSI (broad leaved summer green shade-intolerant), BST (broad-leaved summer-green shade-tolerant), NS (needle-leaved summer green)")]<-NA
spd$species_list[which(spd$species_list=="Quercus robur, Quercus petraea and Fagus sylvatica ")]<-"Quercus robur, Quercus petraea, Fagus sylvatica" 
spd$species_list[which(spd$species_list=="Populus tremuloides (dominant), Populus balsamifera, Corylus cornuta (understorey), other shrubs")]<-"Populus tremuloides, Populus balsamifera, Corylus cornuta"
spd$species_list[which(spd$species_list=="Pinus sylvestris (Scots pine), Norway spruce (Picea abies), Downy birch (Betula pubescens), European beech (Fagus sylvatica), European oak (Quercus robur), Betula pendula (Silver birch). First three species were used for tree ring analyses, all species for phenology assessment, but birch data merged.")]<-"Pinus sylvestris, Picea abies, Betula pubescens, Fagus sylvatica, Quercus robur, Betula pendula"
spd$species_list[which(spd$species_list=="Aesculus hippocastanum, Betula pendula, Fagus sylvatic, Quercus robur")]<-"Aesculus hippocastanum, Betula pendula, Fagus sylvatica, Quercus robur" 
spd$species_list[which(spd$species_list=="Rhododendron ferrugineum L")]<-"Rhododendron ferrugineum"

allspp <- unlist(lapply(spd$species_list, function(x) strsplit(x, ", ")[[1]]))
sort(unique(allspp, na.ignore=TRUE)) 
allgenera <- gsub("^([^\\ ]+).*", "\\1", allspp)
sort(unique(allgenera, na.ignore=TRUE)) 

sppnum <- length(unique(allspp, na.ignore=TRUE))
gennum <- length(unique(allgenera, na.ignore=TRUE))


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
hypd <- subset(hypdall, select=c("paper_id", "hypothesis_in_paper"))
hypd <- hypd[!duplicated(hypd), ] # many papers have more than 1 hypothesis
sort(unique(hypd$hypothesis_in_paper))
subset(hypd, hypothesis_in_paper=="") # three have no hypotheses? (Some get fixed below.)

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

# Fix the ones that were missing but got fixed in the issue... (around 13 Feb 2024)
hypd$hypothesis_in_paper[which(hypd$paper_id=="brand2022")] <- "warmer temperatures = more drought (drought limitation)"
hypd$hypothesis_in_paper[which(hypd$paper_id=="zhu2021")] <- "warmer temperatures = more drought (drought limitation)"

# Now about to merge ... 
sort(unique(d$paper_id))
sort(unique(hypd$paper_id))

dhyp <- merge(d, hypd, by=c("paper_id"))

## Now get numbers for the paper!
moretimedf <- subset(dhyp, hypothesis_in_paper=="longer season = more growth")
moretimenum  <- length(unique(moretimedf$paper_id)) # number of studies reporting time as driver
moretempdf <- subset(dhyp, hypothesis_in_paper=="warmer temperatures = more growth")
moretempnum  <- length(unique(moretempdf$paper_id)) # number of studies reporting warner as driver
notfasterdf <- subset(dhyp, hypothesis_in_paper=="effect of growth rate not equal to growth duration")
notfasternum  <- length(unique(notfasterdf$paper_id)) # number of studies saying rates may not change enough with longer seasons

## What percent of tree ring studies report the drought hypothesis? 

treerings <- subset(dhyp, growth=="annual core")
radialgrowth <- subset(dhyp, growthsimple=="radial growth")

droughthyp <- subset(dhyp, hypothesis_in_paper=="warmer temperatures = more drought (drought limitation)")
unique(paste(droughthyp$paper_id, droughthyp$growth)) # only one study is NOT radial growth
treeringsdrought  <- subset(treerings, hypothesis_in_paper=="warmer temperatures = more drought (drought limitation)")
# Same answer whether you look at % of radial growth or tree ring studies!
perdroughtreering <- round(length(unique(treeringsdrought$paper_id))/length(unique(treerings$paper_id))*100)
round(length(unique(droughthyp$paper_id))/length(unique(radialgrowth$paper_id))*100)

# internal contraints in xylo and lab studies
xylostud <- dhyp[which(dhyp$growth %in% c("intra-annual core (xylogeneis)")),]
labstud <- dhyp[which(dhyp$method %in% c("greenhouse or chamber")),]
which(unique(xylostud$paper_id) %in% unique(labstud$paper_id))
which(unique(labstud$paper_id) %in% unique(xylostud$paper_id)) # no overlap so rbind
xylolabstud <- rbind(xylostud, labstud)
internalconstraintsrelated  <- xylolabstud[which(xylolabstud$hypothesis_in_paper %in% c("species-specific responses", 
  "internal constraints (including popl'n, photoperiod)", "shift of whole pheno sequence")),]
intconslabxylo <- round(length(unique(internalconstraintsrelated$paper_id))/length(unique(xylolabstud$paper_id))*100)

# How many papers had more than one hypothesis (in supp)?
hypperpaper <- as.data.frame(table(hypd$paper_id))
morethanonehyp <- subset(hypperpaper, Freq>1)
nrow(hypperpaper) 
papernum # just checking these agree

#adding another check of hypotheses across the table and our original summaries:
checkhyps<-as.data.frame(cbind(dhyp$paper_id,dhyp$authorslooked_externalfactors,dhyp$authorslooked_endogenousfactors,dhyp$hypothesis_in_paper))
colnames(checkhyps)<-c("paper_id","authorslooked_externalfactors","authorslooked_endogenousfactors","hypothesis_in_paper")
write.csv(checkhyps,"output/checkhyps.csv")

##########################################################
##### Make a Ref table with hypotheses for Manuscript ####
##########################################################
htab2<-table(hypd$paper_id,hypd$hypothesis_in_paper)
#Calculate how many papers test each hypothesis
#consolitdate duplicate hypotheses/paper: replace cells >1 with 1
htab2[htab2 > 1] <- 1 
thtab2<-t(htab2)

ht.df<-as.data.frame(cbind(names(colSums(htab2)),
                           colSums(htab2)))
colnames(ht.df)<-c("hypothesis","num.studies")
ht.df$studnames<-NA
for(i in 1:length(ht.df$studnames)){
  x<-thtab2[i,]
  xst<-paste(names(x[x>0]), collapse=",")
  ht.df$studnames[i]<-xst
}
ht.df$num.studies<-as.integer(ht.df$num.studies)
ht.df<-ht.df[order(ht.df$num.studies,decreasing=TRUE),]

#sub in the citation names for the study names
ht.df$ref<-ht.df$studnames
ht.df$ref<-gsub("buermann2018","buermann2018widespread", ht.df$ref)
ht.df$ref<-gsub("camarero2022","camarero2022decoupled", ht.df$ref)
ht.df$ref<-gsub("chen1998","chen1999effects", ht.df$ref)
ht.df$ref<-gsub("cuny2012","cuny2012life", ht.df$ref)
ht.df$ref<-gsub("cufar2014","vcufar2015variations", ht.df$ref)
ht.df$ref<-gsub("delpierre2017","delpierre2017tree", ht.df$ref)
ht.df$ref<-gsub("desauvage2022","de2022temperature", ht.df$ref)
ht.df$ref<-gsub("dow2022","dow2022warm", ht.df$ref)
ht.df$ref<-gsub("drew & downes 2018","drew2018growth", ht.df$ref)
ht.df$ref<-gsub("eckes-shephard2020","eckes2021", ht.df$ref)
ht.df$ref<-gsub("etzold2021","etzold2022number", ht.df$ref)
#finzi is all set
#francon is all set
ht.df$ref<-gsub("gao2022","gao2022earlier", ht.df$ref)
ht.df$ref<-gsub("grossiord2022","grossiord2022warming", ht.df$ref)
ht.df$ref<-gsub("keenan et al 2014","keenan2014net", ht.df$ref)
ht.df$ref<-gsub("kolar2016","kolavr2016response", ht.df$ref)
ht.df$ref<-gsub("michelot2012","michelot2012comparing", ht.df$ref)
ht.df$ref<-gsub("mckown2016","mckown2016impacts", ht.df$ref)
ht.df$ref<-gsub("moser2019","moser2010timing", ht.df$ref)
ht.df$ref<-gsub("oddi2022","oddi2022contrasting", ht.df$ref)
#ren is all set
ht.df$ref<-gsub("richardson2020","richardson2010influence", ht.df$ref)
ht.df$ref<-gsub("sebastian-azcona2020","sebazc2020", ht.df$ref)
ht.df$ref<-gsub("silvestro2023","silvestro2023longer", ht.df$ref)
ht.df$ref<-gsub("soolananayakanahally2013","soolanayakanahally2013timing", ht.df$ref)
ht.df$ref<-gsub("stridbeck2022","stridbeck2022", ht.df$ref)
ht.df$ref<-gsub("vitasse2009","vitasse2009altitudinal", ht.df$ref)
ht.df$ref<-gsub("wheeler2016","wheeler2016snow", ht.df$ref)
ht.df$ref<-gsub("zhang2021","zhang2021drought", ht.df$ref)
ht.df$ref<-gsub("zani2020","zani2020increased", ht.df$ref)
ht.df$ref<-gsub("zohner2020","zohner2020interactive", ht.df$ref)
ht.df$ref<-gsub("zhu2021","zhu2021different", ht.df$ref)
ht.df$ref<-gsub("zohner2023","zohner2023effect", ht.df$ref)

#add text for citing refs in sweave:

ht.df$ref<-paste("\\citep{",ht.df$ref,"}",sep="")
htabrefs<-subset(ht.df,select=c("hypothesis","num.studies","ref"))

# write.csv(htabrefs,"output/hyptable_withrefs.csv", row.names=FALSE)

#################################
##  Subsetting to data to post ## 
#################################
columnstokeep <- c( 
  "paper_id",
  "continent",
  "year",
  "method",
  "species_list",
  "species_num",
  "age_class",
  "gs_start_metricclean",
  "gs_end_metricclean",
  "gslsimple",
  "growth",
  "growthsimple",
  "gslxgrowthsimple",
  "gsxgrowth",
  "gsxgrowthourdef",
  "authorslooked_externalfactors",
  "authorslooked_endogenousfactors",
  "what.ext",
  "what.endo")

dpost <- d[,which(names(d) %in% columnstokeep)]
dpost  <- d[,columnstokeep]
write.csv(dpost,"output/grephontableKNB.csv", row.names=FALSE)
