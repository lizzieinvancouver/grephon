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

# All genera are in tree
length(phenosp.genus.inphylo)
length(unique(spp$genus))

## first prune the phylogeny to include only these genera
phy.genera.grephon <- drop.tip(phy.plants,
                            which(!phy.genera %in% phenosp.genus.inphylo)) 

rm(phy.plants)
View(sort(phy.genera.grephon$tip.label))

# What's not going to merge?
sps.list[which(!sps.list %in% phy.genera.grephon$tip.label)]
phy.genera.grephon$tip.label[grep("sylvatic", phy.genera.grephon$tip.label)]

sps.list.tomerge <- sps.list
sps.list.tomerge[which(sps.list.tomerge=="Fagus_sylvatica")] <- "Fagus_sylvatica_subsp._orientalis"
sps.list.tomerge[which(sps.list.tomerge=="Abies_balsamia")] <- "Abies_balsamea"
sps.list.tomerge[which(sps.list.tomerge=="Acer_rubra")] <- "Acer_rubrum"
sps.list.tomerge[which(sps.list.tomerge=="Quercus_pubesence")] <- "Quercus_pubescens_subsp._pubescens"
sps.list.tomerge[which(sps.list.tomerge=="Quercus_petraea")] <- "Quercus_petraea_subsp._petraea" # 3 options; should check paper! 
sps.list.tomerge[which(sps.list.tomerge=="Quercus_petreaea")] <- "Quercus_petraea_subsp._petraea" # 3 options; should check paper! 
sps.list.tomerge[which(sps.list.tomerge=="Quercus_robur")] <- "Quercus_robur_subsp._robur"
sps.list.tomerge[which(sps.list.tomerge=="Acer_pennsylvanicum")] <- "Acer_pensylvanicum" 
sps.list.tomerge[which(sps.list.tomerge=="Betula_allegheniensis")] <- "Betula_alleghaniensis"
sps.list.tomerge[which(sps.list.tomerge=="Fagus_grandifolia")] <- "Fagus_grandifolia_var._caroliniana"
sps.list.tomerge[which(sps.list.tomerge=="Juniperus_przewlaskii")] <- "Juniperus_przewalskii"
sps.list.tomerge[which(sps.list.tomerge=="Nothofagus_pomilio")] <- "Nothofagus_pumilio" # fix!
sps.list.tomerge[which(sps.list.tomerge=="Spirea_japonica")] <- "Spirea_montana" # close enough...
sps.list.tomerge[which(sps.list.tomerge=="xxx")] <- "xxx"

sps.list.tomerge[which(!sps.list.tomerge %in% phy.genera.grephon$tip.label)] 
# of these remaining, the following genera are unique (so we could splice in a sp.)
# "Carya_" Liriodendron_" "Magnolia_" "Prunus_"

# now prune just the species I want
phy.plants.grephon <-  drop.tip(phy.genera.grephon,
                            which(!phy.genera.grephon$tip.label %in% sps.list))

# Some QUICK plots ... # figures/phylo.pdf
plot(phy.plants.grephon,cex=1.25)
plot(phy.plants.grephon,cex=1.25, type="f")


# save phylogeny
# write.tree(phy.plants.grephon,"output/SBphylo_grephon.tre")