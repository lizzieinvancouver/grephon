# Started mostly 27 August 2023 #
# Adding in nice table on adult/juv for each study from Alana #

ageclassall <- read.csv("input/ageclass.csv")
ageclassall <- subset(ageclassall, paper_id!="")
ageclass <- ageclassall[!duplicated(ageclassall),]
worryabout <- aggregate(ageclass["age_class"], ageclass["paper_id"], FUN=length)
worryabout[which(worryabout$age_class >1),]
# So we'll delete one entry and fix these later
ageclass <- ageclass[which(!ageclass$paper_id=="zani2020"|ageclass$paper_id=="zohnerpreprint"),]
d <- merge(d, ageclass, by="paper_id", all.x=TRUE)

d$age_class[which(d$paper_id=="zani2020" & d$study_type=="permanent plot")] <- "adult"
d$age_class[which(d$paper_id=="zani2020" & d$study_type=="common garden")] <- "adult"
d$age_class[which(d$paper_id=="zani2020" & d$study_type=="greenhouse or chamber (technically CHN terrace)")] <- "juvenile"
d$age_class[which(d$paper_id=="zohner2023" & d$study_scale!="One common garden (CHN terrace, central Switzerland)")] <- "adult"
d$age_class[which(d$paper_id=="zohner2023" & d$study_scale=="One common garden (CHN terrace, central Switzerland)")] <- "juvenile"
