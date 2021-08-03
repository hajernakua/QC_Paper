###This script will perform the tetrachoric correlation between the rs-fMRI QC and T1w for the POND dataset which addresses the 3rd aim of the paper 
### this analysis was the same across all three datasets. The only difference was the input DFs of participants excluded. 



#1. Creating a Dataframe of participants who were excluded by one or more QC approaches. Participants who were not excluded from any of the QC approaches were not included in this dataframe or analysis.
#1.1. binding all the dataframes of the excluded participants from each T1w QC approach 
Exc_T1w_fMRI <- rbind(StandVis_Exc, StandVisMet_Exc, StringVis_Exc, StringVisMet_Exc, Automated_Exc, fMRI_Exc)

#1.2. using the tabyl from the janitor package to create a DF filled with 1s and 0s for each of the 5 QC approaches. The 1s indicate a participant was excluded from that specific QC approaches, and a 0 indicates they were not.  
library(janitor)
Exc_T1w_fMRI_Groups <- tabyl(Exc_T1w_fMRI, ID, Exc_Group)
##The Exc_Group variable is a categorical variable that identifies which QC approach that subject was excluded group 


#2. performing the polychoric correlations 
library(psych)
Mixed_cor_T1_fMRI_QC <- mixedCor(Exc_T1w_fMRI_Groups, d=c(1:6)) 
Mixed_cor_T1_fMRI_QC_DF <- as.matrix(Mixed_cor_T1_fMRI_QC$rho)

#3. Plotting the correlation matrix
library(corrplot)
library(RColorBrewer)
col23 <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
Mixed_cor_T1_fMRI_QC_corplot <- corrplot.mixed(Mixed_cor_T1_fMRI_QC_DF, lower.col = "#BB4444")
