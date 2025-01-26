## Started 31 December 2024 ##
## Happy New Year's Eve! ##
## By Lizzie ##

## Trying to make conceptual figure of rates versus longer, warmer seasons ##


# housekeeping 
rm(list=ls()) 
options(stringsAsFactors = FALSE)

setwd("~/Documents/git/projects/grephon/grephon/analyses/ratesconceptfig/")

# Get Wang and Engel f(x) to use for non-linear rates
source("/Users/Lizzie/Documents/git/projects/vinmisc/vassalphen/analyses/wangengel/Script_functions_pheno_models.R")

longmire <- read.delim("input/longmireGHCN.txt", skip=19, header=FALSE, sep="")
colnames(longmire) <- c("year", "month", "day", "meantemp")

paradise <- read.delim("input/paradiseGHCN.txt", skip=19, header=FALSE, sep="")
colnames(paradise) <- c("year", "month", "day", "meantemp")

longmire$date <- as.Date(paste(longmire$year, longmire$month, longmire$day, sep="-"), format="%Y-%m-%d")
longmire$doy <- as.numeric(format(longmire$date , "%j"))
paradise$date <- as.Date(paste(paradise$year, paradise$month, paradise$day, sep="-"), format="%Y-%m-%d")
paradise$doy <- as.numeric(format(paradise$date , "%j"))


long1979 <- subset(longmire, year==1979) # data starts in late 1978
para1979 <- subset(paradise, year==1979)

long1981 <- subset(longmire, year==1981)
para1981 <- subset(paradise, year==1981)

long2021 <- subset(longmire, year==2021)
para2021 <- subset(paradise, year==2021)

long80s <- subset(longmire, year>1979 & year<1990)
para80s <- subset(paradise, year>1979 & year<1990)

longrec <- subset(longmire, year>2013 & year<2024)
pararec <- subset(paradise, year>2013 & year<2024)

long80smean <- aggregate(long80s["meantemp"], long80s["doy"], FUN=mean)
para80smean <- aggregate(para80s["meantemp"], para80s["doy"], FUN=mean)

longrecmean <- aggregate(longrec["meantemp"], longrec["doy"], FUN=mean)
pararecmean <- aggregate(pararec["meantemp"], pararec["doy"], FUN=mean)

par(mfrow=c(1,3))
plot(meantemp~doy, data=long1979, type="l", ylim=c(-20,30), main="1979")
lines(meantemp~doy, data=para1979, col="dodgerblue")

plot(meantemp~doy, data=long2021, type="l", ylim=c(-20,30), main="2021")
lines(meantemp~doy, data=para2021, col="dodgerblue")

quartz()
par(mfrow=c(2,2))
plot(meantemp~doy, data=para1979, type="l", ylim=c(-20,30), main="paradise: 1979 vs 2021")
lines(meantemp~doy, data=para2021, col="red")

plot(meantemp~doy, data=long1979, type="l", ylim=c(-20,30), main="longmire: 1979 vs 2021")
lines(meantemp~doy, data=long2021, col="red")

plot(meantemp~doy, data=para1981, type="l", ylim=c(-20,30), main="paradise: 1981 vs 2021")
lines(meantemp~doy, data=para2021, col="red")

plot(meantemp~doy, data=long1981, type="l", ylim=c(-20,30), main="longmire: 1981 vs 2021")
lines(meantemp~doy, data=long2021, col="red")

growfx <- 0.0334

# copied from makevassalcurve.R 
testclim.min <- seq(-5, 40, by=0.25)
testclim.max <- seq(-4, 41, by=0.25) # higher than 41 and the curve freaks out
testclim.avg <- (testclim.min+testclim.max)/2
wangeng <- WangEngelfx(0, 41, 30, 2.85, testclim.avg) 

wangengclim <- data.frame(we=wangeng[,1], tempC=testclim.avg)
pdf("figures/wecurve.pdf", width=8, height=6)
par(mfrow=c(1,1))
plot(we~testclim.avg, data=wangengclim, type="l", xlim=c(0,40), 
	ylim=c(0,1), ylab="Rate", xlab="Temperature")
dev.off()


## Plot comparing SINGLE years
pdf("figures/mora1981vs2021.pdf", width=8, height=6)
par(mfrow=c(2,2))
# Showing 1981
wepara1981 <- WangEngelfx(0, 41, 30, 2.85, para1981$meantemp) 
welong1981 <- WangEngelfx(0, 41, 30, 2.85, long1981$meantemp) 
plot(wepara1981[,1]~para1981$doy, type="l", ylim=c(0,1), xlab="day of year in 1981", ylab="Growth rate")
lines(welong1981[,1]~long1981$doy, col="orange")
plot(wepara1981[,2]~para1981$doy, type="l", ylim=c(0,45), xlab="day of year in 1981", ylab="Accumulated growth")
lines(welong1981[,2]~long1981$doy, col="orange")
# Showing 2021
wepara2021 <- WangEngelfx(0, 41, 30, 2.85, para2021$meantemp) 
welong2021 <- WangEngelfx(0, 41, 30, 2.85, long2021$meantemp) 
plot(wepara2021[,1]~para2021$doy, type="l", ylim=c(0,1), xlab="day of year in 2021", ylab="Growth rate")
lines(welong2021[,1]~long2021$doy, col="orange")
plot(wepara2021[,2]~para2021$doy, type="l", ylim=c(0,45), xlab="day of year in 2021", ylab="Accumulated growth")
lines(welong2021[,2]~long2021$doy, col="orange")
dev.off()

## Plot comparing decades
pdf("figures/moracomparedecades.pdf", width=8, height=6)
par(mfrow=c(2,2))
wepara1980s <- WangEngelfx(0, 41, 30, 2.85, para80smean$meantemp) 
welong1980s <- WangEngelfx(0, 41, 30, 2.85, long80smean$meantemp) 
plot(wepara1980s[,1]~para80smean$doy, type="l", ylim=c(0,0.55), xlab="day of year: mean of 1980s", 
	ylab="Imaginary growth rate (temp curve figure)")
lines(welong1980s[,1]~long80smean$doy, col="orange")
plot(wepara1980s[,2]~para80smean$doy, type="l", ylim=c(0,40), xlab="day of year: mean of 1980s", ylab="Accumulated imaginary growth")
lines(welong1980s[,2]~long80smean$doy, col="orange")

wepararecent <- WangEngelfx(0, 41, 30, 2.85, pararecmean$meantemp) 
welongrecent <- WangEngelfx(0, 41, 30, 2.85, longrecmean$meantemp) 
plot(wepararecent[,1]~pararecmean$doy, type="l", ylim=c(0,0.55), xlab="day of year: mean of 2013-2023", 
	ylab="Imaginary growth rate (temp curve figure)")
lines(welongrecent[,1]~longrecmean$doy, col="orange")
plot(wepararecent[,2]~pararecmean$doy, type="l", ylim=c(0,40), xlab="day of year: mean of 2013-2023", 
	ylab="Accumulated imaginary growth")
lines(welongrecent[,2]~longrecmean$doy, col="orange")
dev.off()

# Also get GSL for days >5 C for PLOTTING
long80smean$daysabove5 <- ifelse(long80smean[["meantemp"]]>4.99, 22, "NA")
para80smean$daysabove5 <- ifelse(para80smean[["meantemp"]]>4.99, 21, "NA")
longrecmean$daysabove5 <- ifelse(longrecmean[["meantemp"]]>4.99, 22, "NA")
pararecmean$daysabove5 <- ifelse(pararecmean[["meantemp"]]>4.99, 21, "NA")

# Also get GSL for days >5 C for summing
long80smean$daysabove5forsum <- ifelse(long80smean[["meantemp"]]>4.99, 1, 0)
para80smean$daysabove5forsum  <- ifelse(para80smean[["meantemp"]]>4.99, 1, 0)
longrecmean$daysabove5forsum  <- ifelse(longrecmean[["meantemp"]]>4.99, 1, 0)
pararecmean$daysabove5forsum  <- ifelse(pararecmean[["meantemp"]]>4.99, 1, 0)

par(mfrow=c(1,2))
plot(long80smean$daysabove5~long80smean$doy)
points(para80smean$daysabove5~para80smean$doy, col="orange")

sum(long80smean$daysabove5forsum)
sum(para80smean$daysabove5forsum)
sum(longrecmean$daysabove5forsum)
sum(pararecmean$daysabove5forsum)

# Get temp ranges over these windows ...
min(long80smean$meantemp[which(long80smean$daysabove5forsum==1)])
max(long80smean$meantemp[which(long80smean$daysabove5forsum==1)])
mean(long80smean$meantemp[which(long80smean$daysabove5forsum==1)])

min(para80smean$meantemp[which(para80smean$daysabove5forsum==1)])
max(para80smean$meantemp[which(para80smean$daysabove5forsum==1)])
mean(para80smean$meantemp[which(para80smean$daysabove5forsum==1)])

min(longrecmean$meantemp[which(longrecmean$daysabove5forsum==1)])
max(longrecmean$meantemp[which(longrecmean$daysabove5forsum==1)])

min(pararecmean$meantemp[which(pararecmean$daysabove5forsum==1)])
max(pararecmean$meantemp[which(pararecmean$daysabove5forsum==1)])


## Better version?
# Plot comparing decades with GSL and temperatures...
pdf("figures/moracomparedecades6panel.pdf", width=8, height=9)
par(mfrow=c(3,2))

plot(para80smean$meantemp~para80smean$doy, type="l", ylim=c(-5,22),  xlab="day of year", 
	ylab="Mean daily temperature", main="1980s")
text(110, -4, "Paradise (high elev)")
text(60, 10, "Lomgmire (low elev)", col="orange")
lines(long80smean$meantemp~long80smean$doy, col="orange")
points(long80smean$daysabove5~long80smean$doy, col="orange", cex=0.5)
points(para80smean$daysabove5~para80smean$doy, col="black", cex=0.5)

plot(pararecmean$meantemp~pararecmean$doy, type="l", ylim=c(-5,22), xlab="day of year ", 
	ylab="Mean daily temperature", main="2013-2023")
lines(longrecmean$meantemp~longrecmean$doy, col="orange")
points(longrecmean$daysabove5~longrecmean$doy, col="orange", cex=0.5)
points(pararecmean$daysabove5~pararecmean$doy, col="black", cex=0.5)

plot(wepara1980s[,1]~para80smean$doy, type="l", ylim=c(0,0.55), xlab="day of year", 
	ylab="Imaginary growth rate (temp curve figure)")
lines(welong1980s[,1]~long80smean$doy, col="orange")

plot(wepararecent[,1]~pararecmean$doy, type="l", ylim=c(0,0.55), xlab="day of year", 
	ylab="Imaginary growth rate (temp curve figure)")
lines(welongrecent[,1]~longrecmean$doy, col="orange")

plot(wepara1980s[,2]~para80smean$doy, type="l", ylim=c(0,40), xlab="day of year", 
	ylab="Accumulated imaginary growth")
lines(welong1980s[,2]~long80smean$doy, col="orange")

plot(wepararecent[,2]~pararecmean$doy, type="l", ylim=c(0,40), xlab="day of year", 
	ylab="Accumulated imaginary growth")
lines(welongrecent[,2]~longrecmean$doy, col="orange")

dev.off()

## Plot with different curves for different species

ABAMopt <- 21 # 18
THPLopt <- 23 # 20

# First, set up the Wang & Engel
wangengABAM <- WangEngelfx(0, 35, ABAMopt, 2.85, testclim.avg) 
wangengABAMclim <- data.frame(we=wangengABAM[,1], tempC=testclim.avg)

wangengTHPL <- WangEngelfx(0, 35, THPLopt, 2.85, testclim.avg) 
wangengwangengTHPLclim <- data.frame(we=wangengTHPL[,1], tempC=testclim.avg)

wepara1980sABAM <- WangEngelfx(0, 35, ABAMopt, 2.85, para80smean$meantemp) 
wepara1980sTHPL <- WangEngelfx(0, 35, THPLopt, 2.85, para80smean$meantemp) 
welong1980sABAM <- WangEngelfx(0, 35, ABAMopt, 2.85, long80smean$meantemp) 
welong1980sTHPL <- WangEngelfx(0, 35, THPLopt, 2.85, long80smean$meantemp) 
wepararecentABAM <- WangEngelfx(0, 35, ABAMopt, 2.85, pararecmean$meantemp) 
wepararecentTHPL <- WangEngelfx(0, 35, THPLopt, 2.85, pararecmean$meantemp) 
welongrecentABAM <- WangEngelfx(0, 35, ABAMopt, 2.85, longrecmean$meantemp) 
welongrecentTHPL <- WangEngelfx(0, 35, THPLopt, 2.85, longrecmean$meantemp) 

library(ggplot2) # for alpha
colz <- c("springgreen4", alpha("springgreen4", 0.45), # THPL
	"steelblue4", alpha("steelblue4", 0.45)) # ABAM

pdf("figures/wecurve2spp.pdf", width=8, height=6)
par(mfrow=c(1,1))
plot(we~testclim.avg, data=wangengwangengTHPLclim, type="l", xlim=c(0,31), 
	ylim=c(0,1), ylab="Rate", xlab="Temperature", col=colz[1])
lines(we~testclim.avg, data=wangengABAMclim, 
	col=colz[3])
dev.off()

# Could add different season lengths for each sp. but seems like to TMI
long80smean$daysaboveTHPL <- ifelse(long80smean[["meantemp"]]>5.99, 1.1, "NA")
para80smean$daysaboveTHPL  <- ifelse(para80smean[["meantemp"]]>5.99, 1.05, "NA")
long80smean$daysaboveABAM <- ifelse(long80smean[["meantemp"]]>2.99, 1, "NA")
para80smean$daysaboveABAM  <- ifelse(para80smean[["meantemp"]]>2.99, 0.95, "NA")

pdf("figures/moracomparedecades4panelwspp.pdf", width=8, height=7)
par(mfrow=c(2,2))
# Rates 1980s
plot(wepara1980sTHPL[,1]~para80smean$doy, type="l", ylim=c(0,1), xlab="day of year", 
	ylab="Imaginary growth rate (temp curve figure)", col=colz[2])
lines(welong1980sTHPL[,1]~long80smean$doy, , col=colz[1])
lines(wepara1980sABAM[,1]~para80smean$doy, , col=colz[4])
lines(welong1980sABAM[,1]~long80smean$doy, , col=colz[3])
if(FALSE){ # make ylim up to 1.1 if adding this back in 
points(long80smean$daysaboveTHPL~long80smean$doy, col=colz[1], cex=0.5)
points(para80smean$daysaboveTHPL~para80smean$doy, col=colz[2], cex=0.5)
points(long80smean$daysaboveABAM~long80smean$doy, col=colz[3], cex=0.5)
points(para80smean$daysaboveABAM~para80smean$doy, col=colz[4], cex=0.5)
}

# Rates recent
plot(wepararecentTHPL[,1]~pararecmean$doy, type="l", ylim=c(0,1.1), xlab="day of year", 
	ylab="Imaginary growth rate (temp curve figure)", col=colz[2])
lines(welongrecentTHPL[,1]~longrecmean$doy, , col=colz[1])
lines(wepararecentABAM[,1]~pararecmean$doy, , col=colz[4])
lines(welongrecentABAM[,1]~longrecmean$doy, , col=colz[3])

# Accumulated 1980s
plot(wepara1980sTHPL[,2]~para80smean$doy, type="l", ylim=c(0,100), xlab="day of year", 
	ylab="Accumulated imaginary growth", col=colz[2])
lines(welong1980sTHPL[,2]~long80smean$doy, , col=colz[1])
lines(wepara1980sABAM[,2]~para80smean$doy, , col=colz[4])
lines(welong1980sABAM[,2]~long80smean$doy, , col=colz[3])

# Accumulated recent
plot(wepararecentTHPL[,2]~pararecmean$doy, type="l", ylim=c(0,100), xlab="day of year", 
	ylab="Imaginary growth rate (temp curve figure)", col=colz[2])
lines(welongrecentTHPL[,2]~longrecmean$doy, , col=colz[1])
lines(wepararecentABAM[,2]~pararecmean$doy, , col=colz[4])
lines(welongrecentABAM[,2]~longrecmean$doy, , col=colz[3])
dev.off()

colz <- c("springgreen3","springgreen4",  # THPL
	"steelblue3", "steelblue4") # ABAM

pdf("figures/wecurve2sppaltcol.pdf", width=8, height=6)
par(mfrow=c(1,1))
plot(we~testclim.avg, data=wangengwangengTHPLclim, type="l", xlim=c(0,31), 
	ylim=c(0,1), ylab="Rate", xlab="Temperature", col=colz[1])
lines(we~testclim.avg, data=wangengABAMclim, 
	col=colz[3])
dev.off()

## Comparing elevations with 1980s data
pdf("figures/moracompareelevations1980s4panelwspp.pdf", width=8, height=7)
par(mfrow=c(2,2))
# Rates 1980s, low elevation
plot(welong1980sTHPL[,1]~long80smean$doy, type="l", ylim=c(0,0.9), xlab="day of year", 
	ylab="Imaginary growth rate (temp curve figure)", col=colz[1])
lines(welong1980sABAM[,1]~long80smean$doy, , col=colz[3])

# Rates 1980s, high elevation
plot(wepara1980sTHPL[,1]~para80smean$doy, type="l", ylim=c(0,0.9), xlab="day of year", 
	ylab="Imaginary growth rate (temp curve figure)", col=colz[2])
lines(wepara1980sABAM[,1]~para80smean$doy, , col=colz[4])

# Accumulated 1980s, low 
plot(welong1980sTHPL[,2]~long80smean$doy, type="l", ylim=c(0,80), xlab="day of year", 
	ylab="Accumulated imaginary growth", col=colz[1])
lines(welong1980sABAM[,2]~long80smean$doy, , col=colz[3])

# Accumulated 1980s, high 
plot(wepara1980sTHPL[,2]~para80smean$doy, type="l", ylim=c(0,80), xlab="day of year", 
	ylab="Accumulated imaginary growth", col=colz[1])
lines(wepara1980sABAM[,2]~para80smean$doy, , col=colz[3])

dev.off()

## Comparing elevations with recent data
pdf("figures/moracompareelevationsrec4panelwspp.pdf", width=8, height=7)
par(mfrow=c(2,2))
# Rates recent, low elevation
plot(welongrecentTHPL[,1]~longrecmean$doy, type="l", ylim=c(0,1), xlab="day of year", 
	ylab="Imaginary growth rate (temp curve figure)", col=colz[1])
lines(welongrecentABAM[,1]~longrecmean$doy, , col=colz[3])

# Rates recent, high elevation
plot(wepararecentTHPL[,1]~pararecmean$doy, type="l", ylim=c(0,1), xlab="day of year", 
	ylab="Imaginary growth rate (temp curve figure)", col=colz[2])
lines(wepararecentABAM[,1]~pararecmean$doy, , col=colz[4])

# Accumulated recent, low 
plot(welongrecentTHPL[,2]~longrecmean$doy, type="l", ylim=c(0,100), xlab="day of year", 
	ylab="Accumulated imaginary growth", col=colz[1])
lines(welongrecentABAM[,2]~longrecmean$doy, , col=colz[3])

# Accumulated recent, high 
plot(wepararecentTHPL[,2]~pararecmean$doy, type="l", ylim=c(0,100), xlab="day of year", 
	ylab="Accumulated imaginary growth", col=colz[2])
lines(wepararecentABAM[,2]~pararecmean$doy, , col=colz[4])

dev.off()