## Started 12 January 205 ##
## By Lizzie but ...
## Copied from https://github.com/DeirdreLoughnan/pheno_bc/blob/master/rcode/get_tree_phenobc.R and  ..
## 

# housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/grephon/grephon/analyses")
} else if (length(grep("ailene", getwd()))>0) 
{setwd("C:/Users/ailene.ettinger/Documents/GitHub/grephon/analyses")
}

# I commented out some of the packages Deirdre had, not sure I need all those that I left
# library(tidyverse)
library(stringr)
library(ape)
library(phytools)
library(geiger)
# library(pez)
# library(caper)
# library(phangorn)


colnames <- c("latbi", "Europe", "NorthAm", "Asia", "Australia", "SouthAmerica", "NorthAmAsia")
spp <- read.csv("output/countsppcont.csv")
names(spp) <- colnames

temp <- str_split_fixed(spp$latbi, " ", 2)
spp$phylo.name <- paste(temp[,1], temp[,2], sep="_")
spp$genus <- temp[,1]
spp$species <- temp[,2]

sps.list <- sort(unique(spp$phylo.name))
genus.list=sort(unique(spp$genus))

## load phylo (from Smith and Brown 2019)
phy.plants<-read.tree("input/ALLMB.tre")

## getting a list of genera in S&B's phylo
phy.genera<-unlist(
  lapply(strsplit(phy.plants$tip.label, "_"),function(x){return(x[1])}))

phy.genera.uniq<-sort(unique(phy.genera))

## how many grephon species are in the phylogeny?
phenosp.genus.inphylo <- genus.list[which(genus.list %in% phy.genera.uniq)]

## first prune the phylogeny to include only these genera
phy.genera.grephon <- drop.tip(phy.plants,
                            which(!phy.genera %in% phenosp.genus.inphylo)) 

rm(phy.plants)
View(sort(phy.genera.grephon$tip.label))

# now prune just the species I want
phy.plants.grephon <-  drop.tip(phy.genera.grephon,
                            which(!phy.genera.grephon$tip.label %in% sps.list))

# Some QUICK plots ... # figures/phylo.pdf
plot(phy.plants.grephon,cex=1.25)
plot(phy.plants.grephon,cex=1.25, type="f")


# save phylogeny
# write.tree(phy.plants.grephon,"output/SBphylo_grephon.tre")