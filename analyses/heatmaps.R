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

################################################################################
################################################################################
################################################################################
#### IMPORTANT!!! Pay close attention to lines 28-52 to build the right map ####

#### Adding in Flags
### Start with type of heatmap you are interested in building: full count, just yesses or just nos
## You can only select one of the flags from lines 31-37
numberofinstances = FALSE ## if this is TRUE, this looks at all "yes" and "no" total # of instances reported

authorsyes = TRUE ## if this is TRUE, this selects all the times the authors report a gslxgrowth relationship
authorsno = FALSE ## if this is TRUE, this selects all the times the authors report there is not a gslxgrowth relationship

weyes = FALSE ## if this is TRUE, this selects all the times we report a gslxgrowth relationship
weno = FALSE ## if this is TRUE, this selects all the times we report there is not a gslxgrowth relationship

if(numberofinstances==TRUE & authorsyes | numberofinstances==TRUE & authorsno |
   numberofinstances==TRUE  & weyes | numberofinstances==TRUE & weno | authorsyes & authorsno |
   authorsyes & weyes | authorsno & weno | authorsyes & weno | authorsno & weyes){
  warning("You can only select one flag to be true, otherwise it will result in an error with the code")
}

### Next, decide if you want to look at the full list of gsl and growth metrics or the simplified version
fulllist = TRUE

#### Finally, add in figure name. If fulllist == TRUE, then use "complete" section, if FALSE use "simple" section
## List of options: "simple", "simple_authoryes", "simple_authorno", "simple_weyes", "simple_weno",
##                  "complete", "complete_authoryes", "complete_authorno", "complete_weyes", "complete_weno"

figname <- "complete_authoryes"

################################################################################
################################################################################
################################################################################
### Add in the cleaned GREPHON data
greph <- read.csv("output/grephontable.csv")

######## To ask the group, but some of the gslsimple and gsl are listed as NA instead of "not measured", I am changing
## NA to "not measured" for now
if(numberofinstances==TRUE){
  
  greph = greph ## nothing to change
  palette_final <- colorRampPalette(rev(c("#084594", "#2171B5", "#4292C6", "#6BAED6", "#9ECAE1", "#C6DBEF", "#DEEBF7", "#F7FBFF"))) (n=20)

}else if(authorsyes==TRUE){
  
  greph <- greph %>%
    filter(authorsthink_evidence_gsxgrowth == "yes")
  palette_final <- colorRampPalette(rev(c("#005A32", "#238B45", "#41AB5D", "#74C476", "#A1D99B", "#C7E9C0", "#E5F5E0", "#F7FCF5")))(20)
  
}else if(authorsno==TRUE){
  
  greph <- greph %>%
    filter(authorsthink_evidence_gsxgrowth == "no")
  palette_final <- colorRampPalette(rev(c("#99000D", "#CB181D", "#EF3B2C", "#FB6A4A", "#FC9272", "#FCBBA1", "#FEE0D2", "#FFF5F0")))(20)
  
}else if(weyes==TRUE){
  
  greph <- greph %>%
    filter(youthink_evidence_gsxgrowth == "yes")
  palette_final <- colorRampPalette(rev(c("#005A32", "#238B45", "#41AB5D", "#74C476", "#A1D99B", "#C7E9C0", "#E5F5E0", "#F7FCF5")))(20)
  
}else if(weno==TRUE){
  
  greph <- greph %>%
    filter(youthink_evidence_gsxgrowth == "no")
  palette_final <- colorRampPalette(rev(c("#99000D", "#CB181D", "#EF3B2C", "#FB6A4A", "#FC9272", "#FCBBA1", "#FEE0D2", "#FFF5F0")))(20)
  
}

## Prep designing the matrix
if(fulllist == TRUE){
  
  totrows <- length(unique(greph$growth)) ## determine number of rows needed for complete heatmap
  growthtypes_simple <- sort(unique(greph$growth))
  totcols <- length(unique(greph$gsl)) ## determine number of columns needed for complete heatmap
  gsltypes_simple <- sort(unique(greph$gsl))
  
  io_countsgreph = matrix(nrow=totrows, ncol=totcols) 
  rownames(io_countsgreph) <- growthtypes_simple 
  colnames(io_countsgreph) <-gsltypes_simple 
  
  for (i in gsltypes_simple){ 
    for (j in growthtypes_simple){ 
      subsetgreph <- filter(greph, growthsimple == j, gslsimple == i)
      io_countsgreph[j,i] <- n_distinct(subsetgreph$paper_id)
    }
  }
  
}else if(fulllist == FALSE){
  
  totrows <- length(unique(greph$growthsimple)) ## determine number of rows needed for simple heatmap
  growthtypes_simple <- sort(unique(greph$growthsimple))
  totcols <- length(unique(greph$gslsimple)) ## determine number of columns needed for simple heatmap
  gsltypes_simple <- sort(unique(greph$gslsimple))
  
  io_countsgreph = matrix(nrow=totrows, ncol=totcols) 
  rownames(io_countsgreph) <- growthtypes 
  colnames(io_countsgreph) <-gsltypes 
  
  for (i in gsltypes){  
    for (j in growthtypes){ 
      subsetgreph <- filter(greph, growth == j, gsl == i)
      io_countsgreph[j,i] <- n_distinct(subsetgreph$paper_id)
    }
  }
  
}

## Finally, build the actual map!
pdf(file=paste0("../figures/heatmaps/heatmap_gslxgrowth_", figname, ".pdf"), width=11, height=8.5)
mar <- c(14, 14)
heatmap.2(io_countsgreph, Colv=NA, dendrogram="none", 
          col=palette_final, cellnote=io_countsgreph, notecol="black", 
          trace="none", key=FALSE, Rowv=NA, notecex = 1.5,
          margins = mar, lwid=c(0.1,6), lhei=c(0.1,4), cexRow = 1.2, cexCol = 1.2) 
dev.off()


################################################################################
################################################################################
################################################################################
### Next, attempt to consolidate the Yesses and Nos into one combined map
## Ideally, slice the bins in half to show each
greph <- read.csv("output/grephontable.csv")

unique(greph$youthink_evidence_gsxgrowth)
#"no"                              NA                                "yes"                            
# "not sure"                        "yes - growth and growing season"

unique(greph$ourdefinition_evidence_gslxgrowth)
#[1] "no data for this"                               "not mentioned"                                 
#[3] "not tested but have data"                       "no"                                            
#[5] "yes"                                            "not tested but have data (satellite phenology)"
#[7] NA                                               "not sure"                                      
#[9] "negative relationship"  

grephsub <- greph %>%
  select(paper_id, gslsimple, growthsimple, ourdefinition_evidence_gslxgrowth, ourdefinition_evidence_gslxgrowth_notsure) %>%
  mutate(gslsimple = ifelse(is.na(gslsimple), "not measured", gslsimple),
         ourdefinition_evidence_gslxgrowth = 
           ifelse(ourdefinition_evidence_gslxgrowth=="not tested but have data (satellite phenology)", 
                  "not tested",
                  ourdefinition_evidence_gslxgrowth),
         ourdefinition_evidence_gslxgrowth = 
           ifelse(ourdefinition_evidence_gslxgrowth %in% c("not tested but have data", "no data for this",
                                                           "not mentioned"),"not tested",
                  ourdefinition_evidence_gslxgrowth),
         ourdefinition_evidence_gslxgrowth = 
           ifelse(ourdefinition_evidence_gslxgrowth == "negative relationship", "negative\nrelationship", 
                  ourdefinition_evidence_gslxgrowth),
         ourdefinition_evidence_gslxgrowth = 
           ifelse(ourdefinition_evidence_gslxgrowth == "not sure", "yes", ourdefinition_evidence_gslxgrowth),
         ourdefinition_evidence_gslxgrowth = 
           ifelse(is.na(ourdefinition_evidence_gslxgrowth), "not tested", ourdefinition_evidence_gslxgrowth),
         gslxgrowth = paste(gslsimple, growthsimple),
         ourdef.f = factor(ourdefinition_evidence_gslxgrowth, levels=c('yes','no','not tested',
                                                                       'negative\nrelationship'))) %>%
  group_by(gslxgrowth) %>%
  mutate(totcounts = n(),
         uniqueid = paste(gslxgrowth, ourdefinition_evidence_gslxgrowth)) %>%
  group_by(uniqueid) %>%
  mutate(subcounts = n()) %>%
  select(gslxgrowth, gslsimple, growthsimple, ourdef.f, subcounts, totcounts) 

totalpossiblebydef <- grephsub %>%
  ungroup() %>%
  expand(gslsimple, growthsimple, ourdef.f) 

totalpossible <- grephsub %>%
  ungroup() %>%
  expand(gslsimple, growthsimple) 

allcombosbydef <- grephsub %>%
  full_join(totalpossiblebydef) %>%
  mutate(subcounts = ifelse(is.na(subcounts), 0, subcounts)) #,ourdef.f = ifelse(is.na(ourdef.f), "not tested", ourdef.f) 

allcombos <- grephsub %>%
  full_join(totalpossible) %>%
  mutate(totcounts = ifelse(is.na(totcounts), 0, totcounts))

  
totalcountpal <- colorRampPalette(rev(c("#084594", "#2171B5", "#4292C6", "#6BAED6", "#9ECAE1", "#C6DBEF", "#DEEBF7", "#F7FBFF"))) (n=9)
yessespal <- colorRampPalette(rev(c("#005A32", "#238B45", "#41AB5D", "#74C476", "#A1D99B", "#C7E9C0", "#E5F5E0", "#F7FCF5")))(4)
nospal <- colorRampPalette(rev(c("#99000D", "#CB181D", "#EF3B2C", "#FB6A4A", "#FC9272", "#FCBBA1", "#FEE0D2", "#FFF5F0")))(5)

yessesandnos <- ggplot(data = allcombosbydef, mapping = aes(x = gslsimple,
                                       y = growthsimple,
                                       fill = ourdef.f, alpha=subcounts)) +
  geom_tile(color = "black") + theme_classic() + 
  scale_fill_manual(values = c("#005A32", "#99000D", "#084594", "purple4")) +
  scale_alpha_continuous(range=c(0, 2)) +
  xlab("") + ylab("") +
  facet_grid(~ ourdef.f) +
  theme(strip.placement = "outside", legend.position = "none",
        strip.background = element_rect(colour="black",
                                        fill="white"),
        axis.text.y=element_blank()) + geom_text(aes(label=subcounts)) +
  scale_x_discrete(guide = guide_axis(angle = 45), expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) + coord_equal()

all <- ggplot(allcombos, aes(x = gslsimple, y = growthsimple, fill = totcounts)) +
  geom_tile(color = "black") + geom_text(aes(label=ifelse(totcounts==0, "", totcounts))) +
  theme_classic() + #scale_fill_manual(values=totalcountpal) +
  scale_fill_continuous(low = "white", high = "#084594")+ 
  xlab("") + ylab("") + ggtitle("Total # of studies") + 
  theme(legend.position = "none") + 
  scale_x_discrete(guide = guide_axis(angle = 45), expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) + coord_equal() 

pdf(file=paste0("../figures/heatmaps/combinedheatmap_gslxgrowth_simple.pdf"), width=11, height=7)
gridExtra::grid.arrange(all, yessesandnos, ncol=2,
                        widths = c(1, 2.1),
                        layout_matrix = rbind(c(1, 2)))
dev.off()





