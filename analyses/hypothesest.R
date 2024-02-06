## Started by Ailene (later 2023) ##
## Updates in Feb 2024 by Lizzie ##

# housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# Setting working directory. Add in your own path in an if statement for your file structure
if(length(grep("lizzie", getwd())>0)) { 
  setwd("~/Documents/git/projects/grephon/grephon")
} else if (length(grep("ailene", getwd()))>0) 
{setwd("C:/Users/ailene.ettinger/Documents/GitHub/grephon")
}

# packages
library(tidyverse)

# get the data
hall <- read.csv("data/Grephon.hypotheses.csv")
head(hall)
unique(hall$hypothesis_for_fig)#10 hypotheses

# delete some papers we excluded or were not part of the Grephon analysis
h <- hall
# delete papers not part of GREPHON lit review (I think Fredi added them)
h <- h[-which(h$addressed.in.which.Grephon.paper=="x"),] 
# delete bruening2017 (which we remove because it was treeline)
h <- h[-which(h$addressed.in.which.Grephon.paper=="bruening 2017"),] 

# Now sort the papers and get numbers for figure
sort(unique(h$addressed.in.which.Grephon.paper))# 35 grephon papers
htab<-table(h$addressed.in.which.Grephon.paper,h$hypothesis_for_fig)
#Calculate how many papers test each hypothesis
#consolitdate duplicate hypotheses/paper: replace cells >1 with 1
htab[htab > 1] <- 1 
thtab<-t(htab)
#Make table with hypotheses, number of studies, and list of studies addressing
htab.df<-as.data.frame(cbind(names(colSums(htab)),
                             colSums(htab)))
colnames(htab.df)<-c("hypothesis","num.studies")
htab.df$studnames<-NA
for(i in 1:length(htab.df$studnames)){
  x<-thtab[i,]
  xst<-paste(names(x[x>0]), collapse=",")
htab.df$studnames[i]<-xst
}
#add in hypohteses from figure
hypfig<-subset(h, select=c("group_final","wording_figure"))
hypfigdist = hypfig%>% distinct(wording_figure,.keep_all = TRUE)
colnames(hypfigdist)[2]<-"hypothesis"

# htab.df2<-left_join(htab.df,hypfigdist)
htab.df$num.studies<-as.integer(htab.df$num.studies)
#  htab.df3<-subset(htab.df2, select=c(hypothesis,num.studies,studnames))   

 htab.df2<-htab.df[order(htab.df$num.studies,decreasing=TRUE),]
# htab.df2
write.csv(htab.df2,"analyses/output/hyp_summarytab2.csv",row.names=FALSE)

# Now make a simplified version to merge into other grephon data
htab.tomerge <- subset(h, select=c("addressed.in.which.Grephon.paper", 
  "hypothesis_for_fig", 
  "wording_figure",
  "group_final"))
write.csv(htab.tomerge,"analyses/output/hyp_mergeable.csv",row.names=FALSE)
