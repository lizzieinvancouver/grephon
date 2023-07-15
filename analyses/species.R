## Started 10 July 2023 ##
## by Ailene ##

## On the plane to Angola! ##

## Goals are to:
# (1) Pull out species 
# (2) Check for trends in:
#   -how diverse are spp studied?
#   -which species are most studied?
#   -which species show trends vs not?
#   -any useful ways to categorize spp? (e.g. functional group, Grime CSR, freeze/shade tolerance)

# housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

## packages
library(tidyverse)
library(dplyr)
library(tidyr)

setwd("~/Documents/git/projects/grephon/grephon/analyses")

## needed functions

## Flags

d <- read.csv("output/grephontable.csv", header=TRUE)
head(d)

# First look at number of spp column
# cleaning
d$species_num[which(d$species_num=="No species listed")]<-"no spp"
d$species_num[which(d$species_num=="3 functional types")]<-"no spp"
d$species_num[which(d$species_num=="no species listed")]<-"no spp"

sptab<-table(d$species_num)
namord<-c(5,1,8,2,9,3,4,6,7,10)
sptab<-sptab[order(namord)]

#Next look at species_list column
sort(unique(d$species_list))

#do some cleaning
d$species_list[which(d$species_list=="Quercus robur, Quercus petraea and Fagus sylvatica ")]
spd<-subset(d,select=c("paper_id","study_type","species_num","species_list","authorsthink_evidence_gsxgrowth"))
spd2<-spd %>%
  separate(species_list,c("sp1","sp2","sp3","sp4","sp5","sp6","sp7","sp8","sp9","sp10"),",")
            
as.data.frame(strsplit(d$species_list,"," ))

#Make a barplot of this
pdf("../figures/numsppplot.pdf",width=7,height=5)
barplot(sptab, ylab="# of studies",xlab="# of species",
        col="lightblue")
dev.off()

papernum <- length(unique(d$paper_id))
