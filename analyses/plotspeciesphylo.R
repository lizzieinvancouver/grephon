## Started 12 January 2025 ##
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


colnames <- c("latbi", "no", "not.tested", "yes")
sppfull <- read.csv("output/countsppfinds.csv")
names(sppfull) <- colnames

temp <- str_split_fixed(sppfull$latbi, " ", 2)
sppfull$phylo.name <- paste(temp[,1], temp[,2], sep="_")

# Fix things that won't merge with tree 
sppfull$phylo.name[which(sppfull$phylo.name=="Fagus_sylvatica")] <- "Fagus_sylvatica_subsp._orientalis"
sppfull$phylo.name[which(sppfull$phylo.name=="Abies_balsamia")] <- "Abies_balsamea"
sppfull$phylo.name[which(sppfull$phylo.name=="Acer_rubra")] <- "Acer_rubrum"
sppfull$phylo.name[which(sppfull$phylo.name=="Quercus_pubesence")] <- "Quercus_pubescens_subsp._pubescens"
sppfull$phylo.name[which(sppfull$phylo.name=="Quercus_petraea")] <- "Quercus_petraea_subsp._petraea" # 3 options; should check paper! 
sppfull$phylo.name[which(sppfull$phylo.name=="Quercus_petreaea")] <- "Quercus_petraea_subsp._petraea" # 3 options; should check paper! 
sppfull$phylo.name[which(sppfull$phylo.name=="Quercus_robur")] <- "Quercus_robur_subsp._robur"
sppfull$phylo.name[which(sppfull$phylo.name=="Acer_pennsylvanicum")] <- "Acer_pensylvanicum" 
sppfull$phylo.name[which(sppfull$phylo.name=="Betula_allegheniensis")] <- "Betula_alleghaniensis"
sppfull$phylo.name[which(sppfull$phylo.name=="Fagus_grandifolia")] <- "Fagus_grandifolia_var._caroliniana"
sppfull$phylo.name[which(sppfull$phylo.name=="Juniperus_przewlaskii")] <- "Juniperus_przewalskii"
sppfull$phylo.name[which(sppfull$phylo.name=="Nothofagus_pomilio")] <- "Nothofagus_pumilio" # fix!
sppfull$phylo.name[which(sppfull$phylo.name=="Spirea_japonica")] <- "Spirea_montana" # close enough...

# But now we need to re-summarize because some of these corrected names already existed!
library(plyr)
spp <-
      ddply(sppfull, c("phylo.name"), summarise,
      no = sum(no),
      not.tested = sum(not.tested),
      yes = sum(yes))


# Okay, back to where I was ...
temp <- str_split_fixed(spp$phylo.name, "_", 2)
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

# What's not going to merge? I fix most of these ABOVE ... but here's how I did it.
sps.list[which(!sps.list %in% phy.genera.grephon$tip.label)]
phy.genera.grephon$tip.label[grep("sylvatic", phy.genera.grephon$tip.label)]
# of these remaining, the following genera are unique (so we could splice in a sp.)
# "Carya_" Liriodendron_" "Magnolia_" "Prunus_"

sps.list.tomerge <- sps.list
spp.inphylo  <- spp[which(spp$phylo.name %in% phy.genera.grephon$tip.label),]

sps.list.tomerge[which(!sps.list.tomerge %in% phy.genera.grephon$tip.label)] 


# now prune just the species I want
phy.plants.grephon <-  drop.tip(phy.genera.grephon,
                            which(!phy.genera.grephon$tip.label %in% sps.list))

# Some QUICK plots ... # figures/phylo.pdf
plot(phy.plants.grephon,cex=1.25)
plot(phy.plants.grephon,cex=1.25, type="f")

# Now try to make stacked barplot
# Trying to crib off http://blog.phytools.org/2017/01/plottreebarplot-with-more-user-options.html
cols <- c("darkblue","salmon","lightgray")
tree <- phy.plants.grephon
Z <- spp.inphylo[,2:4]
row.names(Z) <- spp.inphylo$phylo.name

pdf("..//figures/speciesnumsphylo/phylosppcounts.pdf", width=12, height=8)
plotTree.barplot(tree, Z[,1:3], args.barplot=list(col=cols, 
    legend.text=TRUE))
dev.off()


# save phylogeny
# write.tree(phy.plants.grephon,"output/SBphylo_grephon.tre")