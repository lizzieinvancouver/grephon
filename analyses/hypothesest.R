## Started by Ailene (later 2023) ##
## Updates in Feb and May 2024 by Lizzie ##

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
unique(hall$hypothesis_for_fig) # 10 hypotheses

# delete some papers we excluded or were not part of the Grephon analysis
h <- hall
# delete papers not part of GREPHON lit review (I think Fredi added them)
h <- h[-which(h$addressed.in.which.Grephon.paper=="x"),] 
# delete bruening2017 (which we remove because it was treeline)
h <- h[-which(h$addressed.in.which.Grephon.paper=="bruening 2017"),] 

## On 16 May 2024, Lizzie built a new column more aligned with how we organized the hypotheses in the paper
# Lots of related discussion in: https://github.com/lizzieinvancouver/grephon/issues/29

# The following all work so start with a new column based off this one:
# More temp = more drought (drought limitation)
# Longer growing season = more growth
# Higher temp = more growth (temp limitation)
# population-specific responses
# species-specific responses
# "shift in allocation"
h$hypothesis_in_paper <- h$wording_figure
# Next, we want effect of growth rate not equal to growth duration (this is in group_final)
h$hypothesis_in_paper[which(h$group_final=="effect of growth rate not equal to growth duration")] <- 
  "effect of growth rate not equal to growth duration" # These were converted to: Longer growing season != more growth
h$hypothesis_in_paper[which(h$group_final=="shift of whole pheno sequence")] <- 
"shift of whole pheno sequence" # These were converted to: ..
# Earlier!= longer growing season (and I just think this phrasing is easier to match to our figure)
h$hypothesis_in_paper[which(h$group=="PP control over growth")] <- "internal constraints (including photoperiod)"
# I reviewed Zani2020 and zohner and this is VERY similar to their language...
# "plants to progress through their seasonal cycle more rapidly", and more informative to me than 'sink limitation'
h$hypothesis_in_paper[which(h$addressed.in.which.Grephon.paper=="Zani2020")] <- "shift of whole pheno sequence"
h$hypothesis_in_paper[which(h$addressed.in.which.Grephon.paper=="zohner2020")] <- "shift of whole pheno sequence"
# I reviewed the below paper and it seems about sink limitation through soil moisture 
h$hypothesis_in_paper[which(h$addressed.in.which.Grephon.paper=="oddi 2022" & 
  h$wording_figure=="Longer growing season != more growth")] <- "More temp = more drought (drought limitation)"
# This paper is about one species and does not seem to be a good fit to 'species-specific responses' 
# ... especially compared to the others: michelot2012, cuny 2012, etzold2021 which really are about this. 
# This leaves this paper as shift in allocation only. 
h <- h[-which(h$addressed.in.which.Grephon.paper=="McKown 2016" & h$wording_figure=="species-specific responses"),] 
# Finally, merge internal and population ... 
h$hypothesis_in_paper[which(h$hypothesis_in_paper=="internal constraints (including photoperiod)")] <- "internal constraints (including pop, photo)"
h$hypothesis_in_paper[which(h$hypothesis_in_paper=="population-specific responses")] <- "internal constraints (including pop, photo)"

#25 May 2024 Ailene addressed questions in https://github.com/lizzieinvancouver/grephon/issues/29
h$hypothesis_in_paper[h$addressed.in.which.Grephon.paper=="chen 1998"]<-"Carbon fertilization"
h$hypothesis_in_paper[h$addressed.in.which.Grephon.paper=="Richardson2020" & h$hypothesis_in_paper=="Longer growing season != more growth"]<-"shift of whole pheno sequence" 
newRichardsonhyp<-c(rep("",times=7),"Richardson2020",rep("",times=5),"AKE","entered by AKE May 2024, see github issue 29","species-specific responses")
h<-rbind(h,newRichardsonhyp)
         
table(h$hypothesis_in_paper)
unique(sort(h$hypothesis_in_paper))



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
  "hypothesis_in_paper", 
  "wording_figure",
  "group_final"))
write.csv(htab.tomerge,"analyses/output/hyp_mergeable.csv",row.names=FALSE)
