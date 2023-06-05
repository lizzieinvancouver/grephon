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


setwd("~/Documents/git/projects/grephon/grephon/analyses")

## needed functions
comparedoubentry <- function(df, suffixeshere, columnname){
    column1 <- paste(columnname, suffixeshere[1], sep=".")
    column2 <- paste(columnname, suffixeshere[2], sep=".")
    columnz <- c("paperid", column1, column2)
    print(df[,..columnz])
    }
          

# after double entry meetings
rdmf <- fread("input/round4_doubled/grephontable_rdm2.4.csv")
akej <- fread("input/round4_doubled/grephontable_JHRLAKE-fin.csv")
achinc <- fread("input/round4_doubled/grephontable_AlanaCat.csv")
emw <- fread("input/round4_doubled/grephontable_emw.csv")
kp <- fread("input/round4_doubled/grephon_doubleEntry_kp.csv")

dall <- rbind(rdmf, akej, achinc, emw, kp)

sort(unique(dall$paper_id))
# expecting 37
# waiting on Buermann_etal_2018 from kp

d <- dall


# Go through consistency of entries...
table(d$growth_metric)


papernum <- length(unique(d$paper_id))

## Simplify some columns

## Deal with multiple growth metrics 
subset(d, growth_metric=="dendrometer diameter AND intra-annual core (xylogeneis)" ) # michelot2012; hmm, I think we could do either; not both?
subset(d, growth_metric=="height,  root:shoot ratio") # Soolananayakanahally2013; this paper already seems broken out a lot -- which one?
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
table(d$authorsthink_evidence_gslxgrowth)
d$gslxgrowth <- d$authorsthink_evidence_gslxgrowth
d$gslxgrowth[grep("maybe", d$authorsthink_evidence_gslxgrowth)] <- "not sure"

# Studies with authorsthink_evidence_gslxgrowth as yes or no MUST have the right metrics, check ... 
checking <- subset(d, gslxgrowth=="yes" | gslxgrowth=="no")
seemswrong <- subset(checking, gsl!= "plant vegetative phenology" & gsl!="wood phenology")


## Quetions
subset(d, growth_metric=="") ## did Alana and Cat mean to leave this empty in desauvage2022?
# Kavya has 3 rows for wheeler, did she mean it?
subset(d, growth_metric=="height,  root:shoot ratio") # Soolananayakanahally2013
subset(d, authorsthink_evidence_gslxgrowth=="yes, no") # Alana!

## Questions for everyone
# NEP is similar to NPP?

##
table(d$gsl_metric)
subset(d, gsl_metric=="estimated start to estimated end")
unique(d$species_list)



# Write it out ...
# write.csv(d, "output/grephontable.csv", row.names=FALSE)
