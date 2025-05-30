# housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/grephon/grephon/analyses")
} else if (length(grep("ailene", getwd()))>0) 
{setwd("C:/Users/ailene.ettinger/Documents/GitHub/grephon/analyses")
}

# packages
library(tidyverse)

# get the data
d <- read.csv("output/grephontable.csv")

sptab<-table(d$species_num)
namord<-c(1,8,2,9,3,4,5,6,7,10)
sptab<-sptab[order(namord)]

#Next look at species_list column
#sort(unique(d$species_list))

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
#Pull out the species and genera to do answer the questions
spd2<-spd %>%
  separate(species_list,c("sp1","sp2","sp3","sp4","sp5","sp6","sp7","sp8","sp9","sp10"),", ")
gen1<-spd2 %>%
  separate(sp1,c("gen1","spe1")," ")
gen2<-gen1 %>%
  separate(sp2,c("gen2","spe2")," ")
gen3<-gen2 %>%
  separate(sp3,c("gen3","spe3")," ")
gen4<-gen3 %>%
  separate(sp4,c("gen4","spe4")," ")
gen5<-gen4 %>%
  separate(sp5,c("gen5","spe5")," ")
gen6<-gen5 %>%
  separate(sp6,c("gen6","spe6")," ")
gen7<-gen6 %>%
  separate(sp7,c("gen7","spe7")," ")
gen8<-gen7 %>%
  separate(sp8,c("gen8","spe8")," ")
gen9<-gen8 %>%
  separate(sp9,c("gen9","spe9")," ")
gen10<-gen9 %>%
  separate(sp10,c("gen10","spe10")," ")
gen<-subset(gen10,select=c("paper_id","study_type","species_num","authorsthink_evidence_gsxgrowth","gen1","gen2","gen3","gen4","gen5","gen6","gen7","gen8","gen9","gen10"))
countgen<-sort(table(c(as.matrix(gen[,5:14]))), decreasing=TRUE)

gencols<-rep("lightgreen", times=length(names(countgen)))
gencols[which(names(countgen)=="Pinus")]<-"darkgreen"
gencols[which(names(countgen)=="Picea")]<-"darkgreen"
gencols[which(names(countgen)=="Abies")]<-"darkgreen"
gencols[which(names(countgen)=="Larix")]<-"darkgreen"
gencols[which(names(countgen)=="Juniperus")]<-"darkgreen"
gencols[which(names(countgen)=="Tsuga")]<-"darkgreen"

#  countgen<-countgen[sort(names(countgen))]
  
  #Make a barplot of genera
  pdf("../figures/genusnums.pdf",width=12,height=8)
  par(mar=c(10,5,1,1))
  barplot(countgen, ylab="# of studies (rows)",xlab=" ",
          col=gencols, ylim=c(0,25),
          las=3,cex.lab=1.3, cex.axis=1.3, cex.names=1.3)
  legend("topright",legend=c("angiosperm","gymnosperm"),
         fill =c("lightgreen","darkgreen"), cex=1.5)
  
  dev.off()
  
#sptab<-sptab[order(names(sptab))]
  
#Make a barplot of sp numbers
pdf("../figures/numsppplot.pdf",width=7,height=5)
barplot(sptab, ylab="# of studies (rows)",xlab="# of species",
        col="lightblue")
dev.off()

spd1<- subset(spd2,select=c("paper_id","study_type","authorsthink_evidence_gsxgrowth","continent","sp1","sp2","sp3","sp4","sp5","sp6","sp7","sp8","sp9","sp10"))
spd1_long<-gather(spd1,key="species", value="species_name",sp1,sp2,sp3,sp4,sp5,sp6,sp7,sp8,sp9,sp10)
spd1_long<-spd1_long[-which(is.na(spd1_long$species_name)),]

countspp<-sort(table(c(spd1_long$species_name)), decreasing=TRUE)
sppcols<-rep("lightgreen", times=length(names(countspp)))
sppcols[grepl("Pinus",names(countspp))]<-"darkgreen"
sppcols[grepl("Tsuga",names(countspp))]<-"darkgreen"
sppcols[grepl("Picea",names(countspp))]<-"darkgreen"
sppcols[grepl("Larix",names(countspp))]<-"darkgreen"
sppcols[grepl("Juniperus",names(countspp))]<-"darkgreen"
sppcols[grepl("Abies",names(countspp))]<-"darkgreen"


#Make a barplot of genera
pdf("../figures/speciesnums.pdf",width=14,height=8)
par(mar=c(15,5,1,1))
barplot(countspp, ylab="# of studies (rows)",xlab=" ",
        col=sppcols, ylim=c(0,20),
        las=3,cex.lab=1.2, cex.axis=1.2, cex.names=1.1)
legend("topright",legend=c("angiosperm","gymnosperm"),
       fill =c("lightgreen","darkgreen"), cex=1.5)

dev.off()

#Nowlook at whether authors found relationship in atleast one instance
#first do some cleaning
#spd1_long$authorsthink_evidence_gsxgrowth[spd1_long$authorsthink_evidence_gsxgrowth=="yes (only 1 / 2 sites)"]<-"yes"
#spd1_long$authorsthink_evidence_gsxgrowth[spd1_long$authorsthink_evidence_gsxgrowth=="yes (1 of 2 sites)"]<-"yes"
spd1_long$authorsthink_evidence_gsxgrowth[spd1_long$authorsthink_evidence_gsxgrowth=="negative relationship"]<-"yes"
unique(spd1_long$authorsthink_evidence_gsxgrowth)
spd1_long$authorsthink_evidence_gsxgrowth[is.na(spd1_long$authorsthink_evidence_gsxgrowth)]<-"not tested"
spd1_long$authorsthink_evidence_gsxgrowth[spd1_long$authorsthink_evidence_gsxgrowth=="not mentioned"]<-"not tested"

#what does NA mean? that either GSL or growth were not tested, i think
findcols<-c("salmon","lightgray","darkblue")
spnames<-colnames(t(table(spd1_long$species_name,spd1_long$authorsthink_e)))
spnames[which(spnames=="Acer")]<-"Acer sp."
spnames[which(spnames=="Betula")]<-"Betula sp."
spnames[which(spnames=="Carya")]<-"Carya sp."
spnames[which(spnames=="Magnolia")]<-"Magnolia sp."

spnames[which(spnames=="Magnolia")]<-"Magnolia sp."
spnames[which(spnames=="Nyssa")]<-"Nyssa sp."
spnames[which(spnames=="Populus")]<-"Populus sp."
spnames[which(spnames=="Prunus")]<-"Prunus sp."
spnames[which(spnames=="Quercus")]<-"Quercus sp."

pdf("../figures/speciesnums_finds.pdf",width=14,height=8)
#x11()
par(mar=c(15,5,1,1))
p<-barplot(t(table(spd1_long$species_name,spd1_long$authorsthink_e)),
        ylab="# of studies",xlab=" ",
        col=findcols, ylim=c(0,20),
        names.arg=rep("", times=length(spnames)),
        las=3,cex.lab=2, cex.axis=2, cex.names=1.2)

legend("topright",legend=c("yes","no","not tested"),
       fill =c("darkblue","salmon","lightgray"), cex=2, bty="n")
text(p, par("usr")[3]-0.25, 
     srt = 60, adj = 1, xpd = TRUE,
     labels = paste(spnames),font=3, cex = 1)
dev.off()

##species nums by continent

contcols<-c("green4","darkblue","goldenrod","salmon","lightblue","purple3")

 countsppcont<-table(spd1_long$species_name,spd1_long$cont)
#colSums(countsppcont)
#Make a barplot of genera
pdf("../figures/speciesnumsbycont.pdf",width=15,height=5)
par(mar=c(15,5,1,1))
barplot(countsppcont,beside=TRUE,
        ylab="# of studies (rows)",xlab=" ",
        col="darkblue", ylim=c(0,18),
        las=3,cex.lab=1.2, cex.axis=1.2, cex.names=1.2)
#legend("topright",legend=c("Europe","North America","Asia","Australia","South America", "Asia & North America"),
#       fill =contcols, cex=1.5, bty="n")

dev.off()

countsppfinds <- table(spd1_long$species_name,spd1_long$authorsthink_e)

write.csv(countsppfinds, "output/countsppfinds.csv", row.names=TRUE)
write.csv(countsppcont, "output/countsppcont.csv", row.names=TRUE)

