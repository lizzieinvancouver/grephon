## Trying to create  endo/eco heatmaps for manuscript
## Started 2 October 2023 by Lizzie
## Following heatmaps.R, but WAY less pretty.

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

## data!
greph <- read.csv("output/grephontable.csv")


### Update the heatmaps for Supp to better match the heatmaps in the main document

grephsub <- greph %>%
  select(paper_id, method, what.ext, what.endo) %>%
  mutate(method = ifelse(method=="", "height", method)) %>%
  distinct() %>%
  group_by(method, what.ext) %>%
  mutate(totcounts.ext = n()) %>%
  ungroup() %>%
  group_by(method, what.endo) %>%
  mutate(totcounts.endo = n()) %>%
  select(method, what.ext, what.endo, totcounts.ext, totcounts.endo) 

totalpossible.ext <- grephsub %>%
  ungroup() %>%
  expand(method, what.ext) 

totalpossible.endo <- grephsub %>%
  ungroup() %>%
  expand(method, what.endo) 

allcombos.ext <- grephsub %>%
  ungroup() %>%
  select(method, what.ext, totcounts.ext) %>%
  full_join(totalpossible.ext) %>%
  mutate(totcounts.ext = ifelse(is.na(totcounts.ext), 0, totcounts.ext)) %>%
  distinct() %>%
  filter(!is.na(what.ext))

allcombos.endo <- grephsub %>%
  ungroup() %>%
  select(method, what.endo, totcounts.endo) %>%
  full_join(totalpossible.endo) %>%
  mutate(totcounts.endo = ifelse(is.na(totcounts.endo), 0, totcounts.endo)) %>%
  distinct() %>%
  filter(!is.na(what.endo))

ext <- ggplot(allcombos.ext, aes(x = what.ext, y = method, fill = totcounts.ext)) +
  geom_tile(color = "black") + geom_text(aes(label=ifelse(totcounts.ext==0, "", totcounts.ext))) +
  theme_classic() + 
  scale_fill_continuous(low = "white", high = "#99000D")+ 
  xlab("") + ylab("") + ggtitle("External factors") + 
  theme(legend.position = "none") + 
  scale_x_discrete(guide = guide_axis(angle = 45), expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) + coord_equal() 

endo <- ggplot(allcombos.endo, aes(x = what.endo, y = method, fill = totcounts.endo)) +
  geom_tile(color = "black") + geom_text(aes(label=ifelse(totcounts.endo==0, "", totcounts.endo))) +
  theme_classic() + 
  scale_fill_continuous(low = "white", high = "#084594")+ 
  xlab("") + ylab("") + ggtitle("Internal factors") + 
  theme(legend.position = "none",
        axis.text.y=element_blank()) + 
  scale_x_discrete(guide = guide_axis(angle = 45), expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) + coord_equal() 

pdf(file=paste0("../figures/heatmaps/heatmap_combined_endo&exo.pdf"), width=11, height=8.5)
grid::grid.draw(cbind(ggplotGrob(ext), ggplotGrob(endo)))
dev.off()




# For external...

## By growth ... 
growthtypes_simple <- sort(unique(greph$growth))
totrows <- length(growthtypes_simple) ## determine number of rows needed for complete heatmap
whatexttypes_simple <- sort(unique(greph$what.ext))
totcols <- length(whatexttypes_simple) ## determine number of columns needed for complete heatmap

io_countsgreph = matrix(nrow=totrows, ncol=totcols) 
   rownames(io_countsgreph) <- growthtypes_simple 
   colnames(io_countsgreph) <- whatexttypes_simple
 for (i in whatexttypes_simple){ 
   for (j in growthtypes_simple){ 
     subsetgreph <- filter(greph, growthsimple == j, what.ext == i)
     io_countsgreph[j,i] <- n_distinct(subsetgreph$paper_id)
   }
}

pdf(file=paste0("../figures/heatmaps/heatmap_extbygrowth.pdf"), width=11, height=8.5)
mar <- c(14, 14)
heatmap.2(io_countsgreph, Colv=NA, dendrogram="none", 
          cellnote=io_countsgreph, notecol="black", # col=palette_final, 
          trace="none", key=FALSE, Rowv=NA, notecex = 1.5,
          margins = mar, lwid=c(0.1,6), lhei=c(0.1,4), cexRow = 1.2, cexCol = 1.2) 
dev.off()


###
# Now do external by METHOD
greph$method[which(greph$method=="")] <- "height"
method_simple <- sort(unique(greph$method))
totrows <- length(method_simple) ## determine number of rows needed for complete heatmap
whatexttypes_simple <- sort(unique(greph$what.ext))
totcols <- length(whatexttypes_simple) ## determine number of columns needed for complete heatmap

io_countsgreph = matrix(nrow=totrows, ncol=totcols) 
   rownames(io_countsgreph) <- method_simple
   colnames(io_countsgreph) <- whatexttypes_simple
 for (i in whatexttypes_simple){ 
   for (j in method_simple){ 
     subsetgreph <- filter(greph, method == j, what.ext == i)
     io_countsgreph[j,i] <- n_distinct(subsetgreph$paper_id)
   }
}

pdf(file=paste0("../figures/heatmaps/heatmap_extbymethod.pdf"), width=11, height=8.5)
mar <- c(14, 14)
heatmap.2(io_countsgreph, Colv=NA, dendrogram="none", 
          cellnote=io_countsgreph, notecol="black", # col=palette_final, 
          trace="none", key=FALSE, Rowv=NA, notecex = 1.5,
          margins = mar, lwid=c(0.1,6), lhei=c(0.1,4), cexRow = 1.2, cexCol = 1.2) 
dev.off()


###
# Now try external by GSL ...
gsl_simple <- sort(unique(greph$gslsimple))
totrows <- length(gsl_simple ) ## determine number of rows needed for complete heatmap
whatexttypes_simple <- sort(unique(greph$what.ext))
totcols <- length(whatexttypes_simple) ## determine number of columns needed for complete heatmap

io_countsgreph = matrix(nrow=totrows, ncol=totcols) 
   rownames(io_countsgreph) <- gsl_simple 
   colnames(io_countsgreph) <- whatexttypes_simple
 for (i in whatexttypes_simple){ 
   for (j in gsl_simple ){ 
     subsetgreph <- filter(greph, gslsimple == j, what.ext == i)
     io_countsgreph[j,i] <- n_distinct(subsetgreph$paper_id)
   }
}

pdf(file=paste0("../figures/heatmaps/heatmap_extbygsl.pdf"), width=11, height=8.5)
mar <- c(14, 14)
heatmap.2(io_countsgreph, Colv=NA, dendrogram="none", 
          cellnote=io_countsgreph, notecol="black", # col=palette_final, 
          trace="none", key=FALSE, Rowv=NA, notecex = 1.5,
          margins = mar, lwid=c(0.1,6), lhei=c(0.1,4), cexRow = 1.2, cexCol = 1.2) 
dev.off()



# For endogenous ...

###
# Now do endo by METHOD
method_simple <- sort(unique(greph$method))
totrows <- length(method_simple) ## determine number of rows needed for complete heatmap
whatendtypes_simple <- sort(unique(greph$what.endo))
totcols <- length(whatendtypes_simple) ## determine number of columns needed for complete heatmap

io_countsgreph = matrix(nrow=totrows, ncol=totcols) 
   rownames(io_countsgreph) <- method_simple
   colnames(io_countsgreph) <- whatendtypes_simple
 for (i in whatendtypes_simple){ 
   for (j in method_simple){ 
     subsetgreph <- filter(greph, method == j, what.endo == i)
     io_countsgreph[j,i] <- n_distinct(subsetgreph$paper_id)
   }
}

pdf(file=paste0("../figures/heatmaps/heatmap_endobymethod.pdf"), width=11, height=8.5)
mar <- c(14, 14)
heatmap.2(io_countsgreph, Colv=NA, dendrogram="none", 
          cellnote=io_countsgreph, notecol="black", # col=palette_final, 
          trace="none", key=FALSE, Rowv=NA, notecex = 1.5,
          margins = mar, lwid=c(0.1,6), lhei=c(0.1,4), cexRow = 1.2, cexCol = 1.2) 
dev.off()


## By growth ... 
growthtypes_simple <- sort(unique(greph$growth))
totrows <- length(growthtypes_simple) ## determine number of rows needed for complete heatmap
whatendtypes_simple <- sort(unique(greph$what.endo))
totcols <- length(whatendtypes_simple) ## determine number of columns needed for complete heatmap

io_countsgreph = matrix(nrow=totrows, ncol=totcols) 
   rownames(io_countsgreph) <- growthtypes_simple 
   colnames(io_countsgreph) <- whatendtypes_simple
 for (i in whatendtypes_simple){ 
   for (j in growthtypes_simple){ 
     subsetgreph <- filter(greph, growthsimple == j, what.endo == i)
     io_countsgreph[j,i] <- n_distinct(subsetgreph$paper_id)
   }
}

pdf(file=paste0("../figures/heatmaps/heatmap_endobygrowth.pdf"), width=11, height=8.5)
mar <- c(14, 14)
heatmap.2(io_countsgreph, Colv=NA, dendrogram="none", 
          cellnote=io_countsgreph, notecol="black", # col=palette_final, 
          trace="none", key=FALSE, Rowv=NA, notecex = 1.5,
          margins = mar, lwid=c(0.1,6), lhei=c(0.1,4), cexRow = 1.2, cexCol = 1.2) 
dev.off()


###
# Now try external by GSL ...
gsl_simple <- sort(unique(greph$gslsimple))
totrows <- length(gsl_simple ) ## determine number of rows needed for complete heatmap
whatendtypes_simple  <- sort(unique(greph$what.endo))
totcols <- length(whatendtypes_simple) ## determine number of columns needed for complete heatmap

io_countsgreph = matrix(nrow=totrows, ncol=totcols) 
   rownames(io_countsgreph) <- gsl_simple 
   colnames(io_countsgreph) <- whatendtypes_simple
 for (i in whatendtypes_simple){ 
   for (j in gsl_simple ){ 
     subsetgreph <- filter(greph, gslsimple == j, what.endo == i)
     io_countsgreph[j,i] <- n_distinct(subsetgreph$paper_id)
   }
}

pdf(file=paste0("../figures/heatmaps/heatmap_endobygsl.pdf"), width=11, height=8.5)
mar <- c(14, 14)
heatmap.2(io_countsgreph, Colv=NA, dendrogram="none", 
          cellnote=io_countsgreph, notecol="black", # col=palette_final, 
          trace="none", key=FALSE, Rowv=NA, notecex = 1.5,
          margins = mar, lwid=c(0.1,6), lhei=c(0.1,4), cexRow = 1.2, cexCol = 1.2) 
dev.off()
