#2/6/2020
#Kayla Blincow

#Make all the priority network visualiations!!!
#ccm_plot is in NetworkVisFnctn.R!
#SST Chord plots are in different script file (ChordDiagram_SSTCombo.R)

#NO Vessels Daily Warm/Cool/Norm COMBO
ccm_plot(infile = "TCombo_CoolDaily_ccm_webs_SCB3.RData",
         siglevel = 0.05,
         edgecolor = "blue",
         outfile = "NetworkVis/NO_Daily_TComboCool_SCB3.png")

chord(infile = "TCombo_CoolDaily_ccm_webs_SCB3.RData",
      outfile = "NetworkVis/Chord_Cool.png")


ccm_plot(infile = "TCombo_WarmDaily_ccm_webs_SCB3.RData",
         siglevel = 0.05,
         edgecolor = "red",
         outfile = "NetworkVis/NO_Daily_TComboWarm_SCB3.png")

chord(infile = "TCombo_WarmDaily_ccm_webs_SCB3.RData", 
      outfile = "NetworkVis/Chord_Warm.png")


ccm_plot(infile = "Tcombo_NormDaily_ccm_webs_SCB3.RData",
         siglevel = 0.05,
         edgecolor = "grey20",
         outfile = "NetworkVis/TCombo_NO_Daily_SSTNorm_SCB3.png")

chord(infile = "Tcombo_NormDaily_ccm_webs_SCB3.RData",
      outfile = "NetworkVis/Chord_Norm.png")



#NO Vessels Daily Warm/Cool/Norm TU split
ccm_plot(infile = "TSplt_CoolDaily_ccm_webs_SCB3.RData",
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_TSpltCool_SCB3.png")


ccm_plot(infile = "TSplt_WarmDaily_ccm_webs_SCB3.RData",
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_TSpltWarm_SCB3.png")


ccm_plot(infile = "TSplt_NormDaily_ccm_webs_SCB3.RData",
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_SSTNorm_SCB3.png")







#OLD HIGH PRIORITY NETWORK VIS... (3/8/2019)

#NO Vessels Daily Gradient (Yearly SST average)
ccm_plot(infile = "CoolGradSsnDaily2_ccm_webs.RData",
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_CoolGradSsn2.png")


ccm_plot(infile = "WarmGradSsnDaily2_ccm_webs.RData",
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_WarmGradSsn2.png")


ccm_plot(infile = "HotGradSsnDaily2_ccm_webs.RData",
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_HotGradSsn2.png.png")


ccm_plot(infile = "ColdGradSsnDaily2_ccm_webs.RData",
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_ColdGradSsn2.png")




#NO Vessels Daily Warm/Cool/Norm
ccm_plot(infile = "CoolDaily_ccm_webs_SCB3.RData",
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_Cool_SCB3.png")


ccm_plot(infile = "WarmDaily_ccm_webs_SCB3.RData",
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_Warm_SCB3.png")


ccm_plot(infile = "SSTNormDaily_ccm_webs_SCB3.RData",
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_SSTNorm_SCB3.png")


#NO Vessels Daily Gradient
ccm_plot(infile = "CoolGradDaily_ccm_webs.RData",
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_CoolGrad.png")


ccm_plot(infile = "WarmGradDaily_ccm_webs.RData",
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_WarmGrad.png")


ccm_plot(infile = "HotGradDaily_ccm_webs.RData",
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_HotGrad.png")


ccm_plot(infile = "ColdGradDaily_ccm_webs.RData",
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_ColdGrad.png")

#NO Vessels Daily Gradient (Yearly SST average)
ccm_plot(infile = "CoolGradYrDaily_ccm_webs.RData",
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_CoolGradYr.png")


ccm_plot(infile = "WarmGradYrDaily_ccm_webs.RData",
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_WarmGradYr.png")


ccm_plot(infile = "HotGradYrDaily_ccm_webs.RData",
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_HotGradYr.png")


ccm_plot(infile = "ColdGradYrDaily_ccm_webs.RData",
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_ColdGradYr.png")



ccm_plot(infile = "CoolDaily_ccm_webs.RData",
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_Cool.png")

ccm_plot(infile = "CoolDaily_ccm_webs_3.RData",
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_Cool_3.png")



ccm_plot(infile = "WarmDaily_ccm_webs.RData",
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_Warm.png")

ccm_plot(infile = "WarmDaily_ccm_webs_3.RData",
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_Warm_3.png")


ccm_plot(infile = "SSTNormDaily_ccm_webs.RData",
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_SSTNorm.png")
ccm_plot(infile = "SSTNormDaily_ccm_webs_SIO3.RData",
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_SSTNorm_SIO3.png")

#NO Vessels Daily Pre/Post Regs (5 and 10 yr pre)
ccm_plot(infile = "Mar_NOPreRegDaily10yrs_ccm_webs.Rdata", 
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_PreReg_10yrs.png")
#probs want to stick showing off this one

ccm_plot(infile = "Mar_NOPreRegDaily5yrs_ccm_webs.Rdata", 
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_PreReg_5yrs.png")

ccm_plot(infile = "Mar_NOPostRegDaily_ccm_webs.Rdata", 
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_PostReg.png")

# ^^^ Build Networks with just nearshore species?? ^^^

#NO Vessels Daily Nino/Nina/Norm
ccm_plot(infile = "Mar_NINODaily_ccm_webs.Rdata", 
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_NINO.png")
ccm_plot(infile = "Mar_NINADaily_ccm_webs.Rdata", 
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_NINA.png")
ccm_plot(infile = "Mar_NORMDaily_ccm_webs.Rdata", 
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_NORM.png")

#NO Vessels Daily PDO Warm/Cool
ccm_plot(infile = "Mar_PDOCoolDaily_ccm_webs.Rdata", 
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_PDOCool.png")

ccm_plot(infile = "Mar_PDOWarmDaily_ccm_webs.Rdata", 
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Daily_PDOWarm.png")
#no clue what that means...



ccm_plot(infile= "FULLWeekly_ccm_webs.Rdata",
         siglevel= 0.05,
         outfile = "NetworkVis/FULL_weekly.png")

#NO Vessels Weekly Nino/Nina/Norm
ccm_plot(infile = "NINOWeekly_ccm_webs.Rdata", 
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Weekly_NINO.png")
ccm_plot(infile = "NINAWeekly_ccm_webs.Rdata", 
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Weekly_NINA.png")
ccm_plot(infile = "NORMWeekly_ccm_webs.Rdata", 
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Weekly_NORM.png")

#NO Vessels Weekly Pre/Post Regs
ccm_plot(infile = "NOPreRegWeekly_ccm_webs.Rdata", 
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Weekly_PreReg.png")
ccm_plot(infile = "NOPostRegWeekly_ccm_webs.Rdata", 
         siglevel = 0.05,
         outfile = "NetworkVis/NO_Weekly_PostReg.png")


#NT Vessels US Weekly Pre/Post Regs
ccm_plot(infile = "NTUSPreRegWeekly_ccm_webs.Rdata", 
         siglevel = 0.05,
         outfile = "NetworkVis/NT_Weekly_PreReg_US.png")
ccm_plot(infile = "NTUSPostRegWeekly_ccm_webs.Rdata", 
         siglevel = 0.05,
         outfile = "NetworkVis/NT_Weekly_PostReg_US.png")
#this plot had no significant maps

#NT Vessels US Daily Pre/Post Regs
ccm_plot(infile = "NTUSPreRegDaily_ccm_webs.Rdata", 
         siglevel = 0.05,
         outfile = "NetworkVis/NT_Daily_PreReg_US.png")
ccm_plot(infile = "NTUSPostRegDaily_ccm_webs.Rdata", 
         siglevel = 0.05,
         outfile = "NetworkVis/NT_Daily_PostReg_US.png")


#OT Vessels Weekly Nino/Nina/Norm
ccm_plot(infile = "OTNINOWeekly_ccm_webs.Rdata", 
         siglevel = 0.05,
         outfile = "NetworkVis/OT_Weekly_NINO.png")
ccm_plot(infile = "OTNINAWeekly_ccm_webs.Rdata", 
         siglevel = 0.05,
         outfile = "NetworkVis/OT_Weekly_NINA.png")
ccm_plot(infile = "OTNORMWeekly_ccm_webs.Rdata", 
         siglevel = 0.05,
         outfile = "NetworkVis/OT_Weekly_NORM.png")

#OT Vessels Daily Nino/Nina (Norm was too much for the computer)
ccm_plot(infile = "OTNINODaily_ccm_webs.Rdata", 
         siglevel = 0.05,
         outfile = "NetworkVis/OT_Daily_NINO.png")
ccm_plot(infile = "OTNINADaily_ccm_webs.Rdata", 
         siglevel = 0.05,
         outfile = "NetworkVis/OT_Daily_NINA.png")

