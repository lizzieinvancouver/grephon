## Started 31 August 2023 ##
## By Lizzie so far ##

## Combine lit review and Mount Rainier data ##

# housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/grephon/grephon/analyses/growthxelevationetc")
} else if (length(grep("ailene", getwd()))>0) 
{setwd("~/GitHub/grephon/analyses/growthxelevationetc")
}

# packages 
library(viridis)
library(egg)
library(ggplot2)

# get the data
d <- read.csv("input/classicalrefs_datascraped.csv")
mora <- read.csv("output/dat2.df.csv")

# fix the growth in cm and make error numeric, or 0
d$growthmm <- d$growth_value
d$growthmm[which(d$growth_units=="cm/yr-1")] <- d$growth_value[which(d$growth_units=="cm/yr-1")]*10
d$error <- as.numeric(d$growth_errorvalue)
d$error[which(is.na(d$error)==TRUE)] <- 0

# subset to elevation-ish
unique(d$predictor_type)
delev <- subset(d, predictor_type!="latitude")

# zhu2018 seems weird, but I think latitude may also vary with elevation according to Table 1?
# Will confirm and for now remove ...
delevsm <- subset(delev, dataset_id!="zhu2018")
delevsm$spstudy <- paste(delevsm$species, "from", delevsm$dataset_id)
#Convert bai to cm
mora$bai20cm<-mora$bai20/100
# checks
unique(delevsm$predictor_units)


plotelevsm <- ggplot(data = delevsm, aes(x = predictor_value, y = growthmm, 
    colour = spstudy, fill=spstudy)) +
  geom_point() + 
  geom_smooth(method="lm") +
  xlab("Elevation (m)") + 
  ylab("Ring growth (mm)") +
  theme_classic() +
  theme(legend.position = c(0.8,0.8))

ggsave("figures/growthxelev4studies.pdf", plotelevsm, width=8, height=5)

pdf("figures/growthxelev2part.pdf", width=8, height=5)
par(mfrow=c(1,2))
plot(growthmm~predictor_value, data=delevsm, type="n",
    xlab="Elevation (m)", ylab="Growth (mm)")
colz <- viridis(4, alpha = 0.5)
for(i in c(1:length(unique(delevsm$spstudy)))){
    datahere <- subset(delevsm, spstudy==unique(delevsm$spstudy)[i])
    points(growthmm~predictor_value, data=datahere, col=colz[i], pch=16)
    abline(lm(growthmm~predictor_value, data=datahere), col=colz[i])
}
legend("topright", unique(delevsm$spstudy), cex=0.75, col=colz[1:4], pch=16, bty="n")
colz <- viridis(length(unique(mora$species)), alpha = 0.5)
plot(mean.rw~elev, data=mora, type="n",
    xlab="Elevation (m?)", ylab="Growth (mean.rw)")
for(i in c(1:length(unique(mora$species)))){
    datahere <- subset(mora, species==unique(mora$species)[i])
    points(mean.rw~elev, data=datahere, col=colz[i], pch=16)
    abline(lm(mean.rw~elev, data=datahere), col=colz[i])
}
legend("topright", unique(mora$species), col=colz, pch=16, bty="n")
dev.off()


#Now make these look prettier, convert to a 2 or 3 paneled, add error, constrain lines to where data occurs
delevsm$species[grep("Fagus",delevsm$species)] <- 'Fagus sylvatica'

p1 <- ggplot(delevsm, aes(x=predictor_value, y=growthmm, color=species)) +
  geom_point() +
  geom_smooth(method="lm")+
  theme_article() + theme(legend.position = 'right') + scale_color_brewer(palette = "PuOr") +
  xlab("Elevation (m)") + ylab("Growth- ring width (mm)")


mora$species[mora$species=="Abam"] <- 'Abies amabilis'
mora$species[mora$species=="Psme"] <- 'Pseudotsuga menziesii'
mora$species[mora$species=="Thpl"] <- 'Thuja plicata'
mora$species[mora$species=="Tshe"] <- 'Tsuga heterophylla'
mora$species[mora$species=="Tsme"] <- 'Tsuga mertensiana'
mora$species[mora$species=="Xano"] <- 'Xanthocyparus nootkatensis'
p2 <- ggplot(mora, aes(x=elev, y=mean.rw, color=species)) +
  geom_point() +
  geom_smooth(method="lm")+
  theme_article() + scale_color_brewer(palette = "YlGnBu") +
  xlab("Elevation (m)") + ylab("Growth- ring width (mm)")

p3 <- ggplot(mora, aes(x=elev, y=bai20cm, color=species)) +
  geom_point() +
  geom_smooth(method="lm")+
  theme_article() + theme(legend.position = 'right') + scale_color_brewer(palette = "YlGnBu") +
  xlab("Elevation (m)") + ylab("Growth - basal area increment (sq.cm)")

ggarrange(p1,p3)
ggsave("figures/grbyelev.png", plot = ggarrange(p1,p3))

ggarrange(p2,p3)
ggsave("figures/grbyelev_rwvsbai.png", plot = ggarrange(p2,p3))
