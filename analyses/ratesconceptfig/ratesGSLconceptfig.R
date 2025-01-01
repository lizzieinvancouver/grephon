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
plot(we~testclim.avg, data=wangengclim, type="l", xlim=c(0,40))

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
lines(welong1980s[,1]~long80smean$doy, col="orange")
lines(welongrecent[,1]~longrecmean$doy, col="orange")
plot(wepararecent[,2]~pararecmean$doy, type="l", ylim=c(0,40), xlab="day of year: mean of 2013-2023", 
	ylab="Accumulated imaginary growth")
lines(welongrecent[,2]~longrecmean$doy, col="orange")
dev.off()

# Also get GSL for days >5 C
long80smean$daysabove5 <- ifelse(long80smean[["meantemp"]]>4.99, 1, 0)
para80smean$daysabove5 <- ifelse(para80smean[["meantemp"]]>4.99, 1, 0)
longrecmean$daysabove5 <- ifelse(longrecmean[["meantemp"]]>4.99, 1, 0)
pararecmean$daysabove5 <- ifelse(pararecmean[["meantemp"]]>4.99, 1, 0)

par(mfrow=c(1,2))
plot(long80smean$daysabove5~long80smean$doy)
points(para80smean$daysabove5~para80smean$doy, col="orange")

sum(long80smean$daysabove5)
sum(para80smean$daysabove5)
sum(longrecmean$daysabove5)
sum(pararecmean$daysabove5)
