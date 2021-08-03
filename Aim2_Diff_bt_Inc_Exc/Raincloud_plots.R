###this script will create the raincloud plots used in the main paper & supplementary section. The function to create the plots is consistent across different datasets and QC approach with the only differences being the input data. 


#1. Loading the necessary packages:
  library(cowplot)
  library(readr)
  library(ggplot2)
  library(dplyr)
  library(RColorBrewer)
  library(ggsignif)
  source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
  
  
  #2. Creating the raincloud function 
  
  Raincloud_Age_StandVis <- ggplot(data = POND_StandVis, aes(x = Decision, y = age_scan, color = Decision), na.rm = TRUE) +
    geom_flat_violin(position = position_nudge(x = .25, y = 0), adjust = 2, size = 2, trim = FALSE) +
    geom_point(position = position_jitter(width = 0.15), size = 0.75) +
    geom_boxplot(width = .2, guides = FALSE, outlier.shape = NA, alpha = 0.5, size= 2) +
    geom_signif(comparisons = list(c("Standard Visual QC:Passed", "Standard Visual QC:Failed")), map_signif_level = TRUE, size = 1.5) +
    labs(y = "Age of participants (in years)", x = "Inclusion/Exclusion Decision") +
    scale_colour_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    theme_classic() +
    theme(text = element_text(size = 20)) +
    theme(axis.text = element_text(size = 20))
  return(Plot)
}

tiff("Raincloud_Age_StandVis.tiff", width = 24, height = 20, units = "cm", res = 300)
print(Raincloud_Age_StandVis)
dev.off()


Raincloud_ABAS_StandVis <- ggplot(data = POND_StandVis, aes(x = Decision, y = AB21GCCS, color = Decision), na.rm = TRUE) +
    geom_flat_violin(position = position_nudge(x = .25, y = 0), adjust = 2, size = 2, trim = FALSE) +
    geom_point(position = position_jitter(width = 0.15), size = 0.75) +
    geom_boxplot(width = .2, guides = FALSE, outlier.shape = NA, alpha = 0.5, size= 2) +
    geom_signif(comparisons = list(c("Standard Visual QC:Passed", "Standard Visual QC:Failed")), map_signif_level = TRUE, size = 1.5) +
    labs(y = "ABAS General Composite Score", x = "Inclusion/Exclusion Decision") +
    scale_colour_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    theme_classic() +
    theme(text = element_text(size = 20)) +
    theme(axis.text = element_text(size = 20))
  return(Plot)
}

tiff("Raincloud_ABAS_StandVis.tiff", width = 24, height = 20, units = "cm", res = 300)
print(Raincloud_ABAS_StandVis)
dev.off()


Raincloud_IQ_StandVis <- ggplot(data = POND_StandVis, aes(x = Decision, y = IQ, color = Decision), na.rm = TRUE) +
    geom_flat_violin(position = position_nudge(x = .25, y = 0), adjust = 2, size = 2, trim = FALSE) +
    geom_point(position = position_jitter(width = 0.15), size = 0.75) +
    geom_boxplot(width = .2, guides = FALSE, outlier.shape = NA, alpha = 0.5, size= 2) +
    geom_signif(comparisons = list(c("Standard Visual QC:Passed", "Standard Visual QC:Failed")), map_signif_level = TRUE, size = 1.5) +
    labs(y = "Age dependent IQ", x = "Inclusion/Exclusion Decision") +
    scale_colour_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    theme_classic() +
    theme(text = element_text(size = 20)) +
    theme(axis.text = element_text(size = 20))
  return(Plot)
}

tiff("Raincloud_IQ_StandVis.tiff", width = 24, height = 20, units = "cm", res = 300)
print(Raincloud_IQ_StandVis)
dev.off()


Raincloud_TotCBCL_LenVis <- ggplot(data = POND_StandVis, aes(x = Decision, y = CB68TPTOT, color = Decision), na.rm = TRUE) +
    geom_flat_violin(position = position_nudge(x = .25, y = 0), adjust = 2, size = 2, trim = FALSE) +
    geom_point(position = position_jitter(width = 0.15), size = 0.75) +
    geom_boxplot(width = .2, guides = FALSE, outlier.shape = NA, alpha = 0.5, size= 2) +
    geom_signif(comparisons = list(c("Standard Visual QC:Passed", "Standard Visual QC:Failed")), map_signif_level = TRUE, size = 1.5) +
    labs(y = "CBCL Total Score", x = "Inclusion/Exclusion Decision") +
    scale_colour_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    theme_classic() +
    theme(text = element_text(size = 20)) +
    theme(axis.text = element_text(size = 20))

tiff("Raincloud_TotCBCL_LenVis.tiff", width = 24, height = 20, units = "cm", res = 300)
print(Raincloud_TotCBCL_LenVis)
dev.off()


###these same functions were used for the other QC approaches and datasets. 

