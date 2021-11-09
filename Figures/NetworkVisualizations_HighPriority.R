#2/6/2020
#Kayla Blincow

#Make all the priority network visualiations!!!
#ccm_plot is in NetworkVisFnctn.R!
#SST Chord plots are in different script file (ChordDiagram_SSTCombo.R)

#NO Vessels Daily Warm/Cool/Norm COMBO
ccm_plot(infile = "TCombo_CoolDaily_ccm_webs_SCB3.RData",
         siglevel = 0.05,
         edgecolor = "blue",
         outfile = "NetworkVis/NO_Daily_TComboCool_SCB3_new2.png") #same as before..

ccm_plot(infile = "TCombo_WarmDaily_ccm_webs_SCB3.RData",
         siglevel = 0.05,
         edgecolor = "red",
         outfile = "NetworkVis/NO_Daily_TComboWarm_SCB3_new2a.png")

ccm_plot(infile = "Tcombo_NormDaily_ccm_webs_SCB3.RData",
         siglevel = 0.05,
         edgecolor = "grey20",
         outfile = "NetworkVis/TCombo_NO_Daily_SSTNorm_SCB3_new2.png")


