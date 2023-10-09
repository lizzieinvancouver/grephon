######Figures for grephon analyses of overlaps between itrdb and pep725
#####RDM+LW, 07.10.2023

## Data is not ours and quite large, so not posted to repo
## If you need the data, ask Lizzie

#load libraries (careful, raster and rgdal will soon be outdated)
library(raster)
library(maps)
library(ggmap)
library(mapdata)
library(geodata)
library(plyr)
library(ggplot2)
library(rgdal)

###1. LOAD TREE RING DATA - METADATA, UPDATED AS OF 2022 (LAST FTP ACCESSIBLE: https://www.ncei.noaa.gov/pub/data/paleo/treering/)

setwd('D:/Universidad/2023/grephon table/itrdb metadata')
data.itrdb = read.csv("ITRDBmetadata12January2022.txt", header=T, sep='\t')

#remove historical and subfossil data (only post 1900)
data.itrdb2 = subset(data.itrdb, LastYear > 1900)
plot(data.itrdb2$Latitude~data.itrdb2$Longitude) #all seems fine

###2. LOAD PEP725 DATA AND OBTAIN COORDINATES (how many in pep725 are not woody or agricultural?)
setwd("D:/Universidad/2023/grephon table/PEP725_records")
#data stations2 is without the names. they are a total mess of invalid characters and are useless for us rigth now
data725 = read.csv('PEP725_stations.csv', header=T, sep=';', dec='.')
#quick check that dimensions and locations seem correct
plot(data725$LON,data725$LAT)#seems fine


###3. Figure a:SIMPLE MAP COMPARING THEIR DISTRIBUTION

##need some backgroung info: distribution of forests (forest.cover. height, raster by simard pinto publication)
forest.cover<-raster('D:/Universidad/2018/ITRDB database cleaning/Environmental data/Forest cover/Simard_Pinto_3DGlobalVeg_L3C.tif')
plot(forest.cover)

#mapping
color.1='darkolivegreen3'
plot(forest.cover, 
     xlim=c(-15,50), 
     ylim=c(35,75), 
     axes=F, 
     col=c(rgb(0,0,0,0), color.1, color.1),
     main='Spatial distribution'
)
axis(1)
axis(2,las=2)
map('world', 
    fill = T, 
    col = 'white',
    add=T
)
plot(forest.cover,
     axes=F,
     xlim=c(-15,50), 
     ylim=c(35,75), 
     col=c(rgb(0,0,0,0), color.1,color.1),
     add=T
)
map('world', 
    fill = F, 
    col = 'grey50',
    add=T
    
)
points(data725$LON, data725$LAT, col='steelblue3', bg='steelblue3', pch=21, cex=0.8)
points(data.itrdb2$Longitude,data.itrdb2$Latitude, col='orange', bg='orange', pch=21,cex=0.8)

#add notes legends
legend('bottomleft', col=color.1, pch=15, legend='Vegetation > 10m', bty='n')
legend('topleft', col=c('steelblue3','orange'), pch=19, legend=c('PEP725','ITRDB'), bty='n')




###4.Figure b: Time distribution of both
#let's take the stard and end date of each ITRDB
##first subset only european ones by coordinates
itrdb.eu = subset(data.itrdb2, data.itrdb2$Longitude > (-15))
itrdb.eu = subset(itrdb.eu, itrdb.eu$Longitude < 45)
itrdb.eu = subset(itrdb.eu, itrdb.eu$Latitude < 75)
itrdb.eu = subset(itrdb.eu, itrdb.eu$Latitude > 35)

#move it to a data.frame
time.itrdb = data.frame('FirstYear' = itrdb.eu$FirstYear,
                   'LastYear' = itrdb.eu$LastYear, 
                   'All.years' = rep(0, length(itrdb.eu$FirstYear)))

#remove the very likely annoying weird dates (pre 1900, plus some people that use other calendars)
time.itrdb$FirstYear[time.itrdb$FirstYear < 1900] <- 1900
time.itrdb$FirstYear[time.itrdb$FirstYear > 2024] <- NA
time.itrdb$LastYear[time.itrdb$LastYear < 1900] <- NA
time.itrdb$LastYear[time.itrdb$LastYear > 2024] <- NA
hist(time.itrdb$LastYear) #looks fine
hist(time.itrdb$FirstYear) #looks fine

time.itrdb=na.omit(time.itrdb) #remove the NAs of the step before for hte loop to work

for(i in 1:length(time.itrdb$FirstYear)){
  #expands into the sequence of years in between
  time.itrdb$All.years[i] = paste(seq(time.itrdb$FirstYear[i],time.itrdb$LastYear[i]), collapse=';')
}

head(time.itrdb) #looks ok
time.den.itrdb = table(unlist(strsplit(time.itrdb$All.years, ';'))) #split them in columns, then do a tablecount
barplot(time.den.itrdb) 


###code from Lizzie to extract names and species for the pep data
# which species and phasecodes show up?
phendata <- read.csv2("PEP725_data.csv", skip=1, header=FALSE,col.names=c("PEP_ID", "PLANT_ID", "CULT_ID", "BBCH", "YEAR", "DAY"))
head(phendata) ###does this collate everything?

time.pep725= table(phendata$YEAR)

head(time.den.itrdb)
head(time.pep725)
par(mar=c(5,5,5,8))
plot(time.den.itrdb, type='l',
     axes=F,
     xlab='Year',
     ylab='Number of Tree Ting Chronologies',
     ylim=c(0,1400),
     col='orange',
     lwd=4)
polygon(x=c(as.numeric(min(names(time.den.itrdb))),
            as.numeric(names(time.den.itrdb)),
            as.numeric(max(names(time.den.itrdb)))),
        y=c(0,
            time.den.itrdb,
            0),
        col=rgb(1,0.6,0,0.5),
        border=F)
axis(1)
axis(2, col.axis='orange',las=2, col.ticks='orange')
box(lwd=3)
par(new=T)

plot(time.pep725, type='l',
     axes=F,
     col='steelblue3',
     lwd=3,
     ylim=c(0,300000),
     ylab='Single Phenological Observations')
polygon(x=c(as.numeric(min(names(time.pep725))),
            as.numeric(names(time.pep725)),
            as.numeric(max(names(time.pep725)))),
        y=c(0,
            time.pep725,
            0),
        col=rgb(0.31,0.6,0.8,0.5),
        border=F)
axis(4, col.axis='steelblue3',las=2, col.ticks='steelblue3')

par(mar=c(4,4,4,4))

###Part C- taxonomical focus
#tree ring part

tr.sp = table(itrdb.eu$Species)
tr.sp = tr.sp[order(tr.sp)]
dim(tr.sp)
tr.sp = tr.sp[(55-25):55]



#code from lizzie to calculate the most common species
goo <- ddply(phendata, c("PLANT_ID", "CULT_ID", "BBCH"), summarise,
             N=length(PLANT_ID),
             meanyr=mean(YEAR)
)

plantz <- read.csv2("PEP725_plant.csv", header=TRUE)

names(plantz)[1] = 'PLANT_ID'

goo <- goo[order(-goo$N),] # order by most to least N
gooo = head(goo,25)


head(gooo)

pep.sp = merge(gooo, plantz)
pep.sp = pep.sp[order(pep.sp$N),]
head(pep.sp)

par(mfcol=c(1,2))
barplot(tr.sp,
        xlim=c(0,350),
        horiz=T,
        cex.names=0.8,
        las=2,
        border=F,
        space=0,
        col='orange3')
box(lwd=3)
barplot(pep.sp$N,
        horiz=T,
        cex.names=0.8,
        las=2,
        border=F,
        space=0,
        col='steelblue3',
        names=pep.sp$sci_name)
box(lwd=3)




###


##let's do a quick spatial interpolation of both 

library(spatstat)

coord.itrdb=data.frame(data.itrdb2$Longitude, 
                       data.itrdb2$Latitude)
coord.pep725 = data.frame(data725$LON, 
                          data725$LAT)
coord.itrdb=unique(coord.itrdb)
coord.pep725 = unique(coord.pep725)


names(coord.itrdb) = c('lon','lat')
names(coord.pep725) = c('lon','lat')

ppp.itrdb = as.ppp(ppp(coord.itrdb$lon,
                       coord.itrdb$lat,
                       window=owin(c(-15,40),c(30,75))))
ppp.pep725 = as.ppp(ppp(coord.pep725$lon,
                        coord.pep725$lat,
                        window=owin(c(-15,40),c(30,75))))
cols = colorRampPalette(c('grey20','grey','white','cornflowerblue','brown2'))(50)


#

plot(density(ppp.itrdb, sigma=2), col=cols, main='ITRDB density')
map('world', add=T, xlim=c(-15,35), ylim=c(30,75), col='white')
text(-10,70,'ITRDB', col='white')
#plot(ppp.itrdb, add=T, pch=16, axes=T, cex=.3, col=rgb(0.1,0.1,0.1,0.4))
#con=contour(density(amountppp,2), axes=T,add=T, col='pink')
plot(density(ppp.pep725, sigma=2), col=cols, main='PEP725 density')
map('world', add=T, xlim=c(-15,35), ylim=c(30,75), col='white')
#plot(ppp.pep725, add=T, pch=16, axes=T, cex=.3, col=rgb(0.1,0.1,0.1,0.4))
text(-7,70,'PEP725', col='white')

par(mfcol=c(1,1))
it=density(ppp.itrdb,sigma=2)
it=it/(max(it))
plot(it)
plot(pe)
pe=density(ppp.pep725, sigma=2)
pe=pe/(max(pe))
overlap = (it+pe)/2
diff = it-pe

plot(overlap, col=cols, main='Main Normalized overlap')
map('world', add=T, xlim=c(-15,35), ylim=c(30,75), col='white')
plot(diff, main=c(paste('Normalized differences'), 'negative = more PEP725, positive more ITRDB'))
map('world', add=T, xlim=c(-15,35), ylim=c(30,75), col='white')

###plot elevational coverage
data.itrdb2$elevation[data.itrdb2$elevation=="-999"] <- 'NA'
histpep = hist(data725$ALT, breaks = seq(from=-100, to=2000, by=100))
histitrdb = hist(as.numeric(data.itrdb2$elevation),breaks = seq(from=0, to=4500, by=100))
plot(histpep, col=color.2, 
     xlim=c(0,4500), ylim=c(0,4000),
     main='Distributional elevations',
     xlab='Elevation(m)',
     axes=F,
     border='steelblue3')
axis(1)
axis(2, las=2)
plot(histitrdb,col=color.1, add=T, border=color.1)
abline(h=0)
text(950,3600, '11062 obs')
lines(x=c(600,100),y=c(3700,4000))
legend('topright', col=c(color.1,color.2), pch=16, legend=c('PEP725','ITRDB'), bty='n')

##


setwd("D:/Universidad/2018/ITRDB database cleaning/Environmental data/Ecoregions")
ecoregions<-read.table('KGcoords.txt', sep='\t', header=T)
head(ecoregions)
ecoregions$ecoreg = paste(ecoregions$Main.Clim,ecoregions$SecondaryClim,sep='-')
main.eco<-ecoregions[,c(1,2,5)]

# create spatial points data frame
spg <- main.eco
coordinates(spg) <- ~ Lon + Lat
# coerce to SpatialPixelsDataFrame
gridded(spg) <- TRUE
# coerce to raster
main.eco.map <- raster(spg)
plot(main.eco.map, xlim=c(-15,35), ylim=c(30,75))

#obtain distribution
coord.itrdb.eu= subset(coord.itrdb,coord.itrdb$lon > -15)
coord.itrdb.eu= subset(coord.itrdb.eu,coord.itrdb.eu$lon < 35)
coord.itrdb.eu= subset(coord.itrdb.eu,coord.itrdb.eu$lat > 30)
coord.itrdb.eu= subset(coord.itrdb.eu,coord.itrdb.eu$lat < 75)
plot(coord.itrdb.eu)


ecoreg.itrdb = extract(main.eco.map, data.frame(coord.itrdb.eu$lon,
                                                coord.itrdb.eu$lat))
ecoreg.pep725 = extract(main.eco.map, data.frame(coord.pep725$lon,
                                                 coord.pep725$lat))
freq.itrdb = data.frame(table(ecoreg.itrdb))
freq.pep725 = data.frame(table(ecoreg.pep725))
names(freq.itrdb) = c('ecoregion.code', 'ITRDB')
names(freq.pep725) = c('ecoregion.code', 'PEP725')

dists= merge(freq.itrdb, freq.pep725, all.x=T, all.y=T, incomparables=NA)
par(mfcol=c(2,1))
barplot(sort(table(ecoreg.itrdb)),
        space=0, axes=F, col=color.1, border=color.1)
axis(2,las=2)
barplot(sort(table(ecoreg.pep725)), space=0, axes=F,
        col=color.2, border=color.2)
axis(2,las=2)
levels(main.eco.map)


####

#taxonomical
rev(sort(table(data.itrdb2$speciesCode)))
setwd("D:/Universidad/2023/grephon table/PEP725_records")
#data stations2 is without the names. they are a total mess of invalid characters and are useless for us rigth now
data725 = read.csv('PEP725_data.csv', header=T, sep=';')
#quick check that dimensions and locatioons seem correct



