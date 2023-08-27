## Started 10 July 2023 ##
## by Ailene ##
## 27 August 2023 -- plotting code moved to plotspecies.R ##

## On the plane to Angola! ##

## Goals are to:
# (1) Pull out species 
# (2) Check for trends in:
#   -how diverse are spp studied?
#   -which species are most studied?
#   -which species show trends vs not?
#   -any useful ways to categorize spp? (e.g. functional group, Grime CSR, freeze/shade tolerance)
# (3) add continent
# housekeeping

# First look at number of spp column
# cleaning
d$species_num[which(d$species_num=="No species listed")]<-"no spp"
d$species_num[which(d$species_num=="3 functional types")]<-"no spp"
d$species_num[which(d$species_num=="no species listed")]<-"no spp"

#add continent
d$continent<-"1Europe"
d$continent[d$country=="USA"]<-"2North.America" 
d$continent[d$country=="Canada"]<-"2North.America" 
d$continent[d$country=="China"]<-"3Asia" 
d$continent[d$country=="China, USA"]<-"7Asia,North.America" 
d$continent[d$country=="northern hemisphere"]<-"8Northern hemisphere"
d$continent[d$country=="China"]<-"3Asia" 
d$continent[d$country=="Northern hemisphere"]<-"8Northern hemisphere"
d$continent[d$country=="north hemisphere vegetation"]<-"8Northern hemisphere"
d$continent[d$country=="Australia"]<-"4Australia"
d$continent[d$country=="Argentina"]<-"5South.America"
d$continent[d$country=="global, but phenology data mainly from Germany, Switzerland, Austria "]<-"9Global"
d$continent[d$country=="21 sites mostly in Europe and North America"]<-"6Europe, North America"
d$continent[d$country=="North America (USA, Canada), Europe"]<-"6Europe, North America"