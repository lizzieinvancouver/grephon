## Plotting Tree Growth (annual ring widt, bai) vs Elevation
## Mt. Rainier data
## Code Yields a table called "dat" of tree ring data averaged between two cores per tree (for trees with 2 cores collected, if only one core- it is a single core). Each row is a different tree. "dat.df" is the same data, but in dataframe form."rngdat" contains ONLY the tree ring data
# housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

#load packages
library(dplyr)

##Read in data
setwd("C:/Users/ailene.ettinger/Documents/GitHub/grephon/analyses")

#read in data
rawdat1<-read.csv("../data/mora.dat/SouthSideCoresX.csv", header=TRUE)
climate<-read.csv("../data/mora.dat/Clim.csv", header=TRUE)

#first average cores from same trees, get rid of 1st and last year; go back 1850
rawdat<-rawdat1[,c(1:3,5:6,9:169)] #only columns we need
tags<-unique(rawdat[,3])
inddat<-matrix(NA, nrow=length(tags), ncol=5)
dimnames(inddat)<-list(c(), c("stand","species","tag","DBH","lastyear"))
rngdat<-c()

#Code to get averages from 2 trees: NOTE THIS WILL ONLY WORK WITH 2007 AND 2008 CORES MIXED
for(i in 1:length(tags)){
	tmpdat<-rawdat[rawdat[,3]==tags[i],]
	lastring<-min(tmpdat[,5])-1
	if(lastring==2007){firstyr<-8}
	if(lastring==2008){firstyr<-7}
	lastyr<-c()
	for(j in 1:dim(tmpdat)[1]){
		rngs<-tmpdat[j,6:dim(tmpdat)[2]]
		tmp<-which(is.na(rngs)==TRUE)
		tmp<-tmp[tmp>3]
		if(length(tmp)==0){mintmp<-161}
		if(length(tmp)>0){mintmp<-min(tmp)}
		lastyr<-c(lastyr,mintmp+4)
	}
	lastyr<-min(lastyr)
	avgrng<-colMeans(tmpdat[,firstyr:lastyr])
	for(j in 1:4){
		inddat[i,j]<-as.character(tmpdat[1,j])}
	inddat[i,5]<-lastring
	if(lastring==2007){
		treerng<-c(NA,avgrng,rep(NA, times=dim(tmpdat)[2]-lastyr))}
	if(lastring==2008){
		treerng<-c(avgrng,rep(NA, times=dim(tmpdat)[2]-lastyr))}
	rngdat<-rbind(rngdat,treerng)
}

dimnames(rngdat)<-list(c(), seq(2008,1849))
dat<-cbind(inddat,rngdat[,1:159]) #merge data together
dat.df<-as.data.frame(cbind(inddat,rngdat[,1:159]))
rownames(rngdat)<-dat[,3]

####Now analyze tree growth and climate relationships, using lmer
stand<-c("TO04","TB13","AG05","AV06","AX15","AM16","AR07","AE10","PARA")
elev<-c(704,851,950,1064,1091,1197,1454,1460,1603)
standelev<-cbind(stand,elev)
dat2.df<-left_join(dat.df, standelev,copy=TRUE)
dat2.df$mean.rw<-rowMeans(rngdat[,1:159])
dat2.df$elev<-as.numeric(dat2.df$elev)
for(i in unique(dat2.df$species)){
spdat<-dat2.df[dat2.df$species==i,]
spplotname=paste("../figures/mora",i,"rwelev.pdf", sep="")
pdf(spplotname,height=5,width=8)
boxplot(spdat$mean.rw~spdat$elev,xlab="Elevation (m)", ylab="Average ring width (cm)", main=paste(i))
dev.off()
}
#########################################################
###############################@@@@@@@@@@@@@@@@@@@@@@@@@@@
## If we decide we want climate data (isntead of just elevation) could start with the bwlo:
#summarize climate data: up to 2007 growing season
minyr<-min(climate[,1])+1; maxyr<-2007 #WE SHOULD GET MORE RECENT DATA
MAT<-c(); TP<-c(); Tsnow<-c(); GP<-c(); DP<-c(); GT<-c(); DT<-c()
for(i in minyr:maxyr){
	tmpclim<-climate[climate[,1]==i|climate[,1]==i-1,]
	climyr<-tmpclim[10:21,]
	tmpTP<-sum(climyr[,3]); TP<-c(TP,tmpTP)
	tmpMAT<-mean(climyr[,4]); MAT<-c(MAT,tmpMAT)
	tmpTsnow<-sum(climyr[,5]); Tsnow<-c(Tsnow,tmpTsnow)
	tmpGP<-sum(climyr[8:12,3]); GP<-c(GP,tmpGP)
	tmpDP<-sum(climyr[2:6,3]); DP<-c(DP,tmpDP)
	tmpGT<-mean(climyr[8:12,4]); GT<-c(GT,tmpGT)
	tmpDT<-mean(climyr[2:6,4]); DT<-c(DT,tmpDT)
}
clim<-cbind(seq(minyr,maxyr),MAT,TP,Tsnow,GP,DP,GT,DT)
dimnames(clim)<-list(c(), c("year","MAT","TPrecip","Tsnow","GPrecip","DPrecip","GTemp","DTemp"))
MAT<-clim[,2]
TP<-clim[,3]
GP<-clim[,5]
DP<-clim[,6]
GT<-clim[,7]
DT<-clim[,8]

#Next correlate with tree growth
#standardize climatic variables, if desired
MAT2<-(MAT-mean(MAT))/sd(MAT)
TP2<-(TP-mean(TP))/sd(TP)
Tsnow2<-(Tsnow-mean(Tsnow))/sd(Tsnow)
GP2<-(GP-mean(GP))/sd(GP)
DP2<-(DP-mean(DP))/sd(DP)
GT2<-(GT-mean(GT))/sd(GT)
DT2<-(DT-mean(DT))/sd(DT)
climst<-cbind(seq(minyr,maxyr),MAT2,TP2,Tsnow2,GP2,DP2,GT2,DT2)
dimnames(climst)<-list(c(), c("year","MATST","TPrecipST","TsnowST","GPrecipST","DPrecipST","GTempST","DTempST"))
#reorder clim to match tree ring order
climst<-climst[order(climst[,1], decreasing=TRUE),]
head(climst)
MATST<-climst[,2]
TPST<-climst[,3]
GPST<-climst[,5]
DPST<-climst[,6]
GTST<-climst[,7]
DTST<-climst[,8]




#Kevin's climate data estimates for each stand, which are based on PRISM estimates
standtemp<-read.csv("tree_plot_climate_temp.csv", header=TRUE)
standprecip<-read.csv("tree_plot_climate_precip.csv", header=TRUE)
unique(standprecip[,"Plot"])
dimnames(standprecip)
stands<-standtemp[,1]
month<-rep(1:12,length.out=1204)
year<-rep(1909:2009, each=12,length.out=1204)
monthyear<-cbind(year,month)
sttemp<-standtemp[,5:1208]
sttemp<-t(sttemp)
stdtemp<-cbind(monthyear,sttemp)
stdtemp<-stdtemp[61:1186,]#because snow data only goes back to 1914 and tree ring data ends at 2007 for most of the trees
colnames(stdtemp)<-c("year","month","AB08","AE10","AG05","AM16","AO03","AR07","AV02","AV06","AV14","AX15","PARA","PP17","TA01","TB13","TO04","TO11")
stprecip<-standprecip[,5:1208]
stprecip<-t(stprecip)
stdprecip<-cbind(monthyear,stprecip)
stdprecip<-stdprecip[61:1186,]
colnames(stdprecip)<-c("year","month","AB08","AE10","AG05","AM16","AO03","AR07","AV02","AV06","AV14","AX15","PARA","PP17","TA01","TB13","TO04","TO11")
row.names(stdtemp)<-NULL
row.names(stdprecip)<-NULL
sites<-c("PARA","AE10","AR07","AM16","AX15","AV06","AG05","TB13","TO04")
stdswe<-read.csv("tree_plot_climate_swe.csv", header=TRUE)
stdsnowdur<-read.csv("tree_plot_climate_snowdur.csv", header=TRUE)
stdgrdd5<-read.csv("tree_plot_climate_grdd5.csv", header=TRUE)
dim(stdsnowdur)
dim(stdswe)
dim(stdtemp)

#create a for loop to get all climate variables for each stand and examine correlations between them
for (i in 1:length(sites)){
	sitetemp<-cbind(stdtemp[,1:2],stdtemp[,dimnames(stdtemp)[[2]]==sites[i]])
	mntemp<-tapply(sitetemp[,3],sitetemp[,1],mean)
	sitetempgr<-sitetemp[sitetemp[,2]>5&sitetemp[,2]<11,]
	growtemp<-tapply(sitetempgr[,3],sitetempgr[,1],mean)
	sitetempdr<-sitetemp[sitetemp[,2]<5,]
	sitetempdr2<-sitetemp[sitetemp[,2]==12,]
	sitetempdr2[,1]<-sitetempdr2[,1]+1
	sitetempdr<-rbind(sitetempdr,sitetempdr2)
	dortemp<-tapply(sitetempdr[,3],sitetempdr[,1],mean)
	#now precip
	siteprecip<-cbind(stdprecip[,1:2],stdprecip[,dimnames(stdprecip)[[2]]==sites[i]])
	totprecip<-tapply(siteprecip[,3],siteprecip[,1],sum)
	siteprecipgr<-siteprecip[siteprecip[,2]>5&siteprecip[,2]<11,]
	growprecip<-tapply(siteprecipgr[,3],siteprecipgr[,1],sum)
	siteprecipdr<-siteprecip[siteprecip[,2]<5,]
	siteprecipdr2<-siteprecip[siteprecip[,2]==12,]
	siteprecipdr2[,1]<-siteprecipdr2[,1]+1
	siteprecipdr<-rbind(siteprecipdr,siteprecipdr2)
	dorprecip<-tapply(siteprecipdr[,3],siteprecipdr[,1],sum)
	#now snowdata&growingdegreedays(5c threshold)
	siteswe<-stdswe[,dimnames(stdswe)[[2]]==sites[i]]
	names(siteswe)<-stdswe[,1]
	swe<-siteswe[1:94]
	sitesnowdur<-stdsnowdur[,dimnames(stdsnowdur)[[2]]==sites[i]]
	names(sitesnowdur)<-stdsnowdur[,1]
	snowdur<-sitesnowdur[1:94]
	sitegrowdd<-stdgrdd5[,dimnames(stdgrdd5)[[2]]==sites[i]]
	names(sitegrowdd)<-stdgrdd5[,1]
	growdd<-sitegrowdd[1:94]
	#create matrix with all climate variables clim using cbind
	climsite<-cbind(mntemp,growtemp,dortemp,totprecip,growprecip,dorprecip,swe,snowdur,growdd)
	climsite<-climsite[order(dimnames(climsite)[[1]],decreasing=TRUE),]#to reorder climsite so that it is in the same order as the tree ring data!
	#use command pairs to look at data; cor to get correlations
	quartz(width=10, height=7)
	pairs(climsite,main=sites[i])
	print(sites[i])
	print(cor(climsite))#print and save correlations between climate variables
	#standardize site climatic variables, if desired
STMAT2<-(mntemp-mean(mntemp))/sd(mntemp)
STTP2<-(totprecip-mean(totprecip))/sd(totprecip)
STGP2<-(growprecip-mean(growprecip))/sd(growprecip)
STDP2<-(dorprecip-mean(dorprecip))/sd(dorprecip)
STGT2<-(growtemp-mean(growtemp))/sd(growtemp)
STDT2<-(dortemp-mean(dortemp))/sd(dortemp)
STSWE2<-(swe-mean(swe))/sd(swe)
STSNDUR2<-(snowdur-mean(snowdur))/sd(snowdur)
STGROWDD2<-(growdd-mean(growdd))/sd(growdd)
climsitestd<-cbind(seq(1914,2007),STMAT2,STTP2,STGP2,STDP2,STGT2,STDT2,STSWE2,STSNDUR2,STGROWDD2)
dimnames(climsitestd)<-list(c(), c("year","MATST","TPrecipST","GPrecipST","DPrecipST","GTempST","DTempST","SWEST","SnowDurST","GrowDDST"))
#reorder clim
climsitestd<-climsitestd[order(climsitestd[,1], decreasing=TRUE),]
head(climsitestd)
	#now save correlations and climate data for each site
	if(sites[i]=="PARA"){corclimPARA<-cor(climsite)}
	if(sites[i]=="AE10"){corclimAE10<-cor(climsite)}
	if(sites[i]=="AR07"){corclimAR07<-cor(climsite)}
	if(sites[i]=="AV06"){corclimAV06<-cor(climsite)}
	if(sites[i]=="AM16"){corclimAM16<-cor(climsite)}
	if(sites[i]=="AX15"){corclimAX15<-cor(climsite)}
	if(sites[i]=="AG05"){corclimAG05<-cor(climsite)}
	if(sites[i]=="TB13"){corclimTB13<-cor(climsite)}
	if(sites[i]=="TO04"){corclimTO04<-cor(climsite)}
	#create matrices for each stand's climate data(unstand & stand)
	if(sites[i]=="PARA"){climstdPARA<-climsitestd}
	if(sites[i]=="AE10"){climstdAE10<-climsitestd}
	if(sites[i]=="AR07"){climstdAR07<-climsitestd}
	if(sites[i]=="AV06"){climstdAV06<-climsitestd}
	if(sites[i]=="AM16"){climstdAM16<-climsitestd}
	if(sites[i]=="AX15"){climstdAX15<-climsitestd}
	if(sites[i]=="AG05"){climstdAG05<-climsitestd}
	if(sites[i]=="TB13"){climstdTB13<-climsitestd}
	if(sites[i]=="TO04"){climstdTO04<-climsitestd}
	
	if(sites[i]=="PARA"){climPARA<-climsite}
	if(sites[i]=="AE10"){climAE10<-climsite}
	if(sites[i]=="AR07"){climAR07<-climsite}
	if(sites[i]=="AV06"){climAV06<-climsite}
	if(sites[i]=="AM16"){climAM16<-climsite}
	if(sites[i]=="AX15"){climAX15<-climsite}
	if(sites[i]=="AG05"){climAG05<-climsite}
	if(sites[i]=="TB13"){climTB13<-climsite}
	if(sites[i]=="TO04"){climTO04<-climsite}
	}
#Next correlate with tree growth
MATST<-climsitestd[,2]
TPST<-climsitestd[,3]
GPST<-climsitestd[,4]
DPST<-climsitestd[,5]
GTST<-climsitestd[,6]
DTST<-climsitestd[,7]
SWEST<-climsitestd[,8]
SNDST<-climsitestd[,9]
GDDST<-climsitestd[,10]
