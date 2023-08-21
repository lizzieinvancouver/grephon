## Started 10 Aug 2023 ##
## by Ailene ##

## Goal is to check and correct "authorsfoundevidencefor_endogenousfactors" columnrows added by Ailene & Janneke. 

#select out and clean Finzi2020 (previously said "yes" only)
d$authorsfoundevidencefor_endogenousfactors[d$who_entered=="AKE&JHRL" & d$paper_id=="finzi2020"]<- 
    c("yes - growth [spring NEP] and gs_start_metric relationship differences due to species composition",
      "yes - growth [autumn NEP] and gs_end_metric relationship differences due to species composition")

#select out and clean zani2020 (previously said "yes" only)
d$authorsfoundevidencefor_endogenousfactors[d$who_entered=="AKE&JHRL" & d$paper_id=="zani2020"]<- 
  c("yes - growth and gs_end_metric [greater photosynthesis early in growing season associated with earlier senescence]",
    "yes - growth and gs_end_metric [greater photosynthesis early in growing season associated with earlier senescence]",
    "yes - growth and gs_end_metric [greater photosynthesis early in growing season associated with earlier senescence]")

#select out and clean zani2020 (previously said "yes" only)
d$authorsfoundevidencefor_endogenousfactors[d$who_entered=="AKE&JHRL" & d$paper_id=="zani2020"]<- 
  c("yes - [gs_end_metric]",
    "yes - [gs_end_metric]",
    "yes - [gs_end_metric]")

#select out and clean vitasse2009 (previously said "yes" only)
d$authorsfoundevidencefor_endogenousfactors[d$who_entered=="AKE&JHRL" & d$paper_id=="vitasse2009"]<- 
  c("growth and growing season")
    
#select out and clean zohner2020 (previously said "yes" only)
  d$authorsfoundevidencefor_endogenousfactors[d$who_entered=="AKE&JHRL" & d$paper_id=="zohner2020"]<- 
      c( "yes - growth")
    
#select out and clean richardson2020 (previously said "yes" only)
    d$authorsfoundevidencefor_endogenousfactors[d$who_entered=="AKE&JHRL" & d$paper_id=="richardson2020"]<- 
      c("yes - growth and growing season","yes - growth and growing season","yes - growth and growing season")
    
#zohner2023 already done)
   
# fixing zohner2020 external factors, which appears to be incorrect (was "yes= length of growing season" but i think it should be "yes= growth") 
    d$authorsfoundevidencefor_externalfactors[d$who_entered=="AKE&JHRL" & d$paper_id=="zohner2020"]<-
    "yes - growth"
    