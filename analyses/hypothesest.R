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
h <- read.csv("data/Grephon.hypotheses.csv")
head(h)
unique(h$group_final)#10 hypotheses
sort(unique(h$addressed.in.which.Grephon.paper))#35 grephon papers
htab<-table(h$addressed.in.which.Grephon.paper,h$group_final)
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

htab.df2<-left_join(htab.df2,)
write.csv(htab.df,"analyses/output/hyp_summarytab.csv",row.names=FALSE)
