###this script performs a correlation analysis to assess the relationships between the main clinical traits of interest, the IQMs used for visual + metric QC (i.e., SNR, CNR, CJV), and mean FD
### this same script was used for all the datasets with dataset-specific input variables

#0. loading necessary library 
library(corrplot)

#1. making a smaller DF with only the variables we're interested in
MRIQC_fMRI_Clin_Small <- MRIQC_fMRI_Clin[, c('fd_mean', 'age_scan', 'CB68EPTOT', 'CB68IPTOT', 'CB68TPTOT', 'AB21GCCS', 'IQ', 'CB684TOT', 'snr', 'cnr', 'cjv')]

#2. renaming the variables so the plot is more interpretable 
names(MRIQC_fMRI_Clin_Small)[names(MRIQC_fMRI_Clin_Small) == "CB68EPTOT"] <- "Ext. Beh"
names(MRIQC_fMRI_Clin_Small)[names(MRIQC_fMRI_Clin_Small) == "CB68IPTOT"] <- "Int. Beh"
names(MRIQC_fMRI_Clin_Small)[names(MRIQC_fMRI_Clin_Small) == "CB68TPTOT"] <- "Total CBCL"
names(MRIQC_fMRI_Clin_Small)[names(MRIQC_fMRI_Clin_Small) == "AB21GCCS"] <- "ABAS"
names(MRIQC_fMRI_Clin_Small)[names(MRIQC_fMRI_Clin_Small) == "CB684TOT"] <- "ADHD score"
names(MRIQC_fMRI_Clin_Small)[names(MRIQC_fMRI_Clin_Small) == "age_scan"] <- "Age"

#3. creating the correlation matrix
MRIQC_fMRI_Clin_Small_Cor <- cor(MRIQC_fMRI_Clin_Small, use="complete.obs")


#4. getting the p-values of the zero order correlations:
p.mat <- cor_pmat(MRIQC_fMRI_Clin_Small_Cor)


##plotting this correlation table
MRIQC_fMRI_Clin_Small_Cor_plot <- ggcorrplot(MRIQC_fMRI_Clin_Small_Cor, hc.order = TRUE, type = "lower",
           outline.col = "white", p.mat = p.mat)


#5. final plot used in paper 
MRIQC_fMRI_Clin_Small_Cor_plot_v3 <- ggcorrplot(
  MRIQC_fMRI_Clin_Small_Cor,
  p.mat = p.mat,
  hc.order = TRUE,
  lab = TRUE,
  type = "lower"
)


#6. downloading the plot 
tiff("MRIQC_fMRI_Clin_Small_Cor_plot_v3.tiff", width = 24, height = 20, units = "cm", res = 300)
print(MRIQC_fMRI_Clin_Small_Cor_plot_v3)
dev.off()







