###this script will share the code to create the Venn Diagram of exclusion overlap between participants excluded across the different T1w QC approaches and rs-fMRI QC
###this script was used for all datasets with the input DFs changed


#1. making a DF of just the excluded participants
StandVis_Exc$Exc_Group <- "Standard Visual QC Exclusion"
StandVisMet_Exc$Exc_Group <- "Standard Visual + Metric QC Exclusion"
Automated_Exc$Exc_Group <- "Automated QC Exclusion"
QC_fMRI_Exc$Exc_Group <- "fMRI QC Exclusion" 

###need to make a DF that accounts for people excluded by more than one approach 
Exc_T1w_fMRI <- rbind.fill(StandVis_Exc, StandVisMet_Exc, Automated_Exc, QC_fMRI_Exc)


#2. prepping data for the venn diagram
###making smaller DFs
StandVis_Exc_small <- StandVis_Exc[, c('ID', 'Exc_Group')]
StandVisMet_Exc_small <- StandVisMet_Exc[, c('ID', 'Exc_Group')]
Automated_Exc_small <- Automated_Exc[, c('ID', 'Exc_Group')]
fMRI_Exc_small <- QC_fMRI_Exc[, c('ID', 'Exc_Group')]

##getting the Sub IDs to be characters
StandVis_Exc_ID <- as.character(StandVis_Exc_small$ID)
StandVisMet_Exc_ID <- as.character(StandVisMet_Exc$ID)
Automated_Exc_ID <- as.character(Automated_Exc_small$ID)
fMRI_Exc_ID <- as.character(fMRI_Exc_small$ID)


#3. Venn Diagram code 
library(VennDiagram)

venn.diagram(
  x = list(StandVis_Exc_ID, StandVisMet_Exc_ID, Automated_Exc_ID, fMRI_Exc_ID),
  category.names = c("StandVis_Exc_ID" ,  "StandVisMet_Exc_ID",  "Automated_Exc_ID", "fMRI_Exc_ID"),
  filename = '#POND_fMRI_Exclusion_VennDiagram.png',
  height = 30, 
  width = 38,
  units = "cm",
  resolution = 300,
  # Circles
  lwd = 2,
  lty = 'blank',
  fill = c("#999999", "#E69F00", "#56B4E9", "#009E73"),
  # Numbers
  cex = 2,
  fontface = "italic",
  # Set names
  cat.cex = 2,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.dist = c(0.055, 0.055, 0.1, 0.1)
)





