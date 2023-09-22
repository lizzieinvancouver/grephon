#### Create Heatmaps for manuscript
## Started 22 September 2023 by Cat

# housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/grephon/grephon/analyses")
} else if (length(grep("c.chamberlain", getwd()))>0) 
{setwd("/Users/c.chamberlain/Documents/git/grephon/analyses")
}



##Load in required libraries and data
library(gplots)
library(RColorBrewer)
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyr)

### Add in the cleaned GREPHON data
greph <- read.csv("output/grephontable.csv")

######## To ask the group, but some of the gslsimple and gsl are listed as NA instead of "not measured", I am changing
## NA to "not measured" for now
greph <- greph %>%
  mutate(gslsimple = ifelse(is.na(gslsimple), "not measured", gslsimple),
         gsl = ifelse(is.na(gsl), "not measured", gsl))

## Prep for Simple descriptors
length(unique(greph$growthsimple)) ## determine number of rows needed for simple heatmap
growthtypes_simple <- unique(greph$growthsimple)
length(unique(greph$gslsimple)) ## determine number of columns needed for simple heatmap
gsltypes_simple <- unique(greph$gslsimple)

## Prep for complete list of descriptors
length(unique(greph$growth)) ## determine number of rows needed for complete heatmap
growthtypes <- unique(greph$growth)
length(unique(greph$gsl)) ## determine number of columns needed for complete heatmap
gsltypes <- unique(greph$gsl)


##HEATMAPS-----------------------------------------------------------------------------------------

### Start with Simple Heatmaps
###Count linkages between growth metrics and GSL metrics
io_countsgreph = matrix(nrow=5, ncol=4) ## based on above lengths (lines 35 & 37)
rownames(io_countsgreph) <- growthtypes_simple 
colnames(io_countsgreph) <-gsltypes_simple 

for (i in gsltypes_simple){ # i = "climate and date"
  for (j in growthtypes_simple){ # j = "radial growth"
    subsetgreph <- filter(greph, growthsimple == j, gslsimple == i)
    io_countsgreph[j,i] <- n_distinct(subsetgreph$paper_id)
  }
}

palette_final <- colorRampPalette(c("#e5f5f9", "#d9f0a3","#41ab5d","#004529")) (n=20)

pdf(file="../figures/heatmap_gslxgrowth_simple.pdf", width=11, height=8.5)
mar <- c(14, 14)
heatmap.2(io_countsgreph, Colv=NA, dendrogram="none", 
          col=palette_final, cellnote=io_countsgreph, notecol="black", 
          trace="none", key=FALSE, Rowv=NA, notecex = 1.5,
          margins = mar, lwid=c(0.1,6), lhei=c(0.1,4), cexRow = 1.2, cexCol = 1.2) 
dev.off()



################################################################################
### Next, build the more complex heatmaps
###Count linkages between growth metrics and GSL metrics
io_countsgreph = matrix(nrow=9, ncol=6) ## based on above lengths (lines 41 & 43)
rownames(io_countsgreph) <- growthtypes
colnames(io_countsgreph) <-gsltypes

for (i in gsltypes){ # i = "climate and date"
  for (j in growthtypes){ # j = "radial growth"
    subsetgreph <- filter(greph, growth == j, gsl == i)
    io_countsgreph[j,i] <- n_distinct(subsetgreph$paper_id)
  }
}

palette_final <- colorRampPalette(c("#e5f5f9", "#d9f0a3","#41ab5d","#004529")) (n=20)

pdf(file="../figures/heatmap_gslxgrowth_complete.pdf", width=11, height=8.5)
mar <- c(15, 15)
heatmap.2(io_countsgreph, Colv=NA, dendrogram="none", 
          col=palette_final, cellnote=io_countsgreph, notecol="black", 
          trace="none", key=FALSE, Rowv=NA, notecex = 1.5,
          margins = mar, lwid=c(0.1,6), lhei=c(0.1,4), cexRow = 1.2, cexCol = 1.2) 
dev.off()

