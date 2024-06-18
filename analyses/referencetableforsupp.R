#### Create Reference table for Supp if useful
## Started 15 December 2023 by Cat
## Update 18 June 2024: AKE ended up adding refs for hypotheses in supp, see whathappened.R

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
library(gtable)
library(grid)

greph <- read.csv("output/grephontable.csv")



table <- grid.arrange(greph %>% select(paper_id, country, biome) %>%  
               rename(`Reference` = paper_id) %>%
               rename(`Country` = country) %>%
               rename(`Biome` = biome) %>%
               distinct() %>%
               tableGrob(theme = ttheme_default(
                 core = list(bg_params=list(fill=c("grey90", "white"))),
                 colhead = list(fg_params=list(col="white"),
                             bg_params=list(fill="#084594"))), rows = NULL))


table <- gtable_add_grob(table,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 2, b = nrow(table), l = 1, r = ncol(table))
table <- gtable_add_grob(table,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 1, l = 1, r = ncol(table)) 


pdf(file=paste0("../figures/referencetable.pdf"), width=12, height=13)
grid.draw(table)
dev.off()

