### this script will examine whether brain metrics differ between the included and excluded groups of each QC approach
### this same script was used for all the datasets with dataset-specific outliers 

#1. Loading brain metric data 
Cortical_BrainMeasures <- read.csv("/path/to/csv")
Subcortical_BrainMeasures <- read.csv("/path/to/csv")

BrainMeasures <- merge(Cortical_BrainMeasures, Subcortical_BrainMeasures, by = "ID")

#2. merging brain data with clinical data
POND_StandVis_Brain <- merge(POND_StandVis, BrainMeasures_AllPOND, by = "ID")

POND_StandVisMet_Brain <- merge(POND_StandVisMet, BrainMeasures_AllPOND, by = "ID")

POND_StringVis_Brain <- merge(POND_StringVis, BrainMeasures_AllPOND, by = "ID")

POND_StringVisMet_Brain <- merge(POND_StringVisMet, BrainMeasures_AllPOND, by = "ID")

POND_Auto_Brain <- merge(POND_Auto, BrainMeasures_AllPOND, by = "ID")


#3. Performing the ANOVA comparing the various cortical thickness & subcortical ROI metrics between the included and excluded groups.
####While this code was used across the three datasets, there were slight dataset-specific differences such as different covariates (specified in the paper)
###only showing the function for the standard visual QC but it was the same function for each QC approach 

CT_StandVis_AllPOND <- function(POND_LenVis_Brain) {
  library(multcomp)
  Sample.Data <- POND_StandVis_Brain[, c('Decision', 'age_scan', 'ICV', 'GENDER', 'AB21GCCS', 'IQ', 'scanner', 'L_caudalanteriorcingulate_thickavg', 'L_caudalmiddlefrontal_thickavg', 'L_cuneus_thickavg', 'L_entorhinal_thickavg', 
                                      'L_fusiform_thickavg', 'L_inferiorparietal_thickavg', 'L_inferiortemporal_thickavg', 'L_isthmuscingulate_thickavg', 'L_lateraloccipital_thickavg', 'L_lateralorbitofrontal_thickavg', 
                                      'L_lingual_thickavg', 'L_medialorbitofrontal_thickavg', 'L_middletemporal_thickavg', 'L_parahippocampal_thickavg', 'L_paracentral_thickavg', 'L_parsopercularis_thickavg', 
                                      'L_parsorbitalis_thickavg', 'L_parstriangularis_thickavg', 'L_pericalcarine_thickavg', 'L_postcentral_thickavg', 'L_posteriorcingulate_thickavg', 'L_precentral_thickavg', 
                                      'L_precuneus_thickavg', 'L_rostralanteriorcingulate_thickavg', 'L_rostralmiddlefrontal_thickavg', 'L_superiorfrontal_thickavg', 'L_superiorparietal_thickavg', 'L_superiortemporal_thickavg', 
                                      'L_supramarginal_thickavg', 'L_frontalpole_thickavg', 'L_temporalpole_thickavg', 'L_transversetemporal_thickavg', 'L_insula_thickavg', 'R_bankssts_thickavg', 'R_caudalanteriorcingulate_thickavg', 
                                      'R_caudalmiddlefrontal_thickavg', 'R_entorhinal_thickavg', 'R_cuneus_thickavg', 'R_fusiform_thickavg', 'R_inferiorparietal_thickavg', 'R_inferiortemporal_thickavg', 'R_isthmuscingulate_thickavg', 
                                      'R_lateraloccipital_thickavg', 'R_lateralorbitofrontal_thickavg', 'R_lingual_thickavg', 'R_medialorbitofrontal_thickavg', 'R_middletemporal_thickavg', 'R_parahippocampal_thickavg', 'R_paracentral_thickavg', 
                                      'R_parsopercularis_thickavg', 'L_temporalpole_thickavg', 'R_parsorbitalis_thickavg', 'R_parstriangularis_thickavg', 'R_pericalcarine_thickavg', 'R_postcentral_thickavg', 'R_posteriorcingulate_thickavg', 
                                      'R_precentral_thickavg', 'R_precuneus_thickavg', 'R_rostralanteriorcingulate_thickavg', 'R_rostralmiddlefrontal_thickavg', 'R_superiorfrontal_thickavg', 'R_superiorparietal_thickavg', 'R_superiortemporal_thickavg', 
                                      'R_supramarginal_thickavg', 'R_frontalpole_thickavg', 'R_temporalpole_thickavg', 'R_transversetemporal_thickavg', 'R_insula_thickavg')]
  aov(Sample.Data[,9] ~ Decision + age_scan + ICV + GENDER + AB21GCCS + IQ + scanner, data = Sample.Data, na.action = na.omit, p.adjust = "bonferroni")
  Loop2 <- for (i in 9:length(Sample.Data)) {
    cat("Printing the pairwise t.test comparing cortical thickness measures across diagnosis without QC in", (colnames(Sample.Data)[i]), "\n")
    TT <- aov(Sample.Data[,i] ~ Decision + age_scan + ICV + GENDER + AB21GCCS + IQ + scanner, data = Sample.Data, na.action = na.omit, p.adjust = "bonferroni")
    print(summary(TT))
  }
  return(Loop2)
}


SV_StandVis_AllPOND <- function(POND_StandVis_Brain) {
  library(multcomp)
  library(lsr)
  Sample.Data <- POND_LenVis_Brain[, c('Decision', 'age_scan', 'ICV', 'GENDER', 'AB21GCCS', 'IQ', 'scanner', 'Lthal', 'Rthal', 'Lamyg', 'Ramyg', 'Lcaud', 'Rcaud', 'Lput', 'Rput', 'Lpal', 'Rpal', 'Lhippo', 'Rhippo', 'Laccumb', 'Raccumb')]
  aov(Sample.Data[,9] ~ Decision + age_scan + ICV + GENDER + AB21GCCS + IQ + CB68VITOT + scanner, data = Sample.Data, na.action = na.omit, p.adjust = "bonferroni")
  Loop2 <- for (i in 9:length(Sample.Data)) {
    cat("Printing the pairwise t.test comparing cortical thickness measures across diagnosis without QC in", (colnames(Sample.Data)[i]), "\n")
    TT <- aov(Sample.Data[,i] ~ Decision + age_scan + ICV + GENDER + AB21GCCS + IQ + scanner, data = Sample.Data, na.action = na.omit, p.adjust = "bonferroni")
    print(summary(TT))
  }
  return(Loop2)
}


#4. Correcting for multiple comparisons - corrections were made per QC approach per dataset 

#4.1. cortical thickness correction 
Small_StandVis.POND <- POND_StandVis_Brain[, c('age_scan', 'ICV', 'GENDER', 'Decision', 'IQ', 'AB21GCCS', 'scanner', 'L_bankssts_thickavg', 'L_caudalanteriorcingulate_thickavg', 'L_caudalmiddlefrontal_thickavg', 'L_cuneus_thickavg', 'L_entorhinal_thickavg', 'L_fusiform_thickavg', 'L_inferiorparietal_thickavg', 'L_inferiortemporal_thickavg', 'L_isthmuscingulate_thickavg', 'L_lateraloccipital_thickavg', 'L_lateralorbitofrontal_thickavg', 'L_lingual_thickavg', 'L_medialorbitofrontal_thickavg', 'L_middletemporal_thickavg', 'L_parahippocampal_thickavg', 'L_paracentral_thickavg', 'L_parsopercularis_thickavg', 'L_parsorbitalis_thickavg', 'L_parstriangularis_thickavg', 'L_pericalcarine_thickavg', 'L_postcentral_thickavg', 'L_posteriorcingulate_thickavg', 'L_precentral_thickavg', 'L_precuneus_thickavg', 'L_rostralanteriorcingulate_thickavg', 'L_rostralmiddlefrontal_thickavg', 'L_superiorfrontal_thickavg', 'L_superiorparietal_thickavg', 'L_superiortemporal_thickavg', 'L_supramarginal_thickavg', 'L_frontalpole_thickavg', 'L_temporalpole_thickavg', 'L_transversetemporal_thickavg', 'L_insula_thickavg', 'R_bankssts_thickavg', 'R_caudalanteriorcingulate_thickavg', 'R_caudalmiddlefrontal_thickavg', 'R_entorhinal_thickavg', 'R_cuneus_thickavg', 'R_fusiform_thickavg', 'R_inferiorparietal_thickavg', 'R_inferiortemporal_thickavg', 'R_isthmuscingulate_thickavg', 'R_lateraloccipital_thickavg', 'R_lateralorbitofrontal_thickavg', 'R_lingual_thickavg', 'R_medialorbitofrontal_thickavg', 'R_middletemporal_thickavg', 'R_parahippocampal_thickavg', 'R_paracentral_thickavg', 'R_parsopercularis_thickavg', 'L_temporalpole_thickavg', 'R_parsorbitalis_thickavg', 'R_parstriangularis_thickavg', 'R_pericalcarine_thickavg', 'R_postcentral_thickavg', 'R_posteriorcingulate_thickavg', 'R_precentral_thickavg', 'R_precuneus_thickavg', 'R_rostralanteriorcingulate_thickavg', 'R_rostralmiddlefrontal_thickavg', 'R_superiorfrontal_thickavg', 'R_superiorparietal_thickavg', 'R_superiortemporal_thickavg', 'R_supramarginal_thickavg', 'R_frontalpole_thickavg', 'R_temporalpole_thickavg', 'R_transversetemporal_thickavg', 'R_insula_thickavg')]

BrainMetricList <- c('L_bankssts_thickavg', 'L_caudalanteriorcingulate_thickavg', 'L_caudalmiddlefrontal_thickavg', 'L_cuneus_thickavg', 'L_entorhinal_thickavg', 'L_fusiform_thickavg', 'L_inferiorparietal_thickavg', 'L_inferiortemporal_thickavg', 'L_isthmuscingulate_thickavg', 'L_lateraloccipital_thickavg', 'L_lateralorbitofrontal_thickavg', 'L_lingual_thickavg', 'L_medialorbitofrontal_thickavg', 'L_middletemporal_thickavg', 'L_parahippocampal_thickavg', 'L_paracentral_thickavg', 'L_parsopercularis_thickavg', 'L_parsorbitalis_thickavg', 'L_parstriangularis_thickavg', 'L_pericalcarine_thickavg', 'L_postcentral_thickavg', 'L_posteriorcingulate_thickavg', 'L_precentral_thickavg', 'L_precuneus_thickavg', 'L_rostralanteriorcingulate_thickavg', 'L_rostralmiddlefrontal_thickavg', 'L_superiorfrontal_thickavg', 'L_superiorparietal_thickavg', 'L_superiortemporal_thickavg', 'L_supramarginal_thickavg', 'L_frontalpole_thickavg', 'L_temporalpole_thickavg', 'L_transversetemporal_thickavg', 'L_insula_thickavg', 'R_bankssts_thickavg', 'R_caudalanteriorcingulate_thickavg', 'R_caudalmiddlefrontal_thickavg', 'R_entorhinal_thickavg', 'R_cuneus_thickavg', 'R_fusiform_thickavg', 'R_inferiorparietal_thickavg', 'R_inferiortemporal_thickavg', 'R_isthmuscingulate_thickavg', 'R_lateraloccipital_thickavg', 'R_lateralorbitofrontal_thickavg', 'R_lingual_thickavg', 'R_medialorbitofrontal_thickavg', 'R_middletemporal_thickavg', 'R_parahippocampal_thickavg', 'R_paracentral_thickavg', 'R_parsopercularis_thickavg', 'L_temporalpole_thickavg', 'R_parsorbitalis_thickavg', 'R_parstriangularis_thickavg', 'R_pericalcarine_thickavg', 'R_postcentral_thickavg', 'R_posteriorcingulate_thickavg', 'R_precentral_thickavg', 'R_precuneus_thickavg', 'R_rostralanteriorcingulate_thickavg', 'R_rostralmiddlefrontal_thickavg', 'R_superiorfrontal_thickavg', 'R_superiorparietal_thickavg', 'R_superiortemporal_thickavg', 'R_supramarginal_thickavg', 'R_frontalpole_thickavg', 'R_temporalpole_thickavg', 'R_transversetemporal_thickavg', 'R_insula_thickavg')

Total_StandVis <- lapply(BrainMetricList, function(i){
  lm(get(i) ~ Decision + age_scan + ICV + GENDER + IQ + AB21GCCS + scanner, data = Small_StandVis.POND, na.action = na.omit)
})

extract <- function(i, num){
  p_MRIQC <- lapply((i), function(f) Anova(f)$"Pr(>F)"[1])
  padj_MRIQC <- p.adjust(p_MRIQC, method="fdr", n=num)
  F_value <- lapply((i), function(f) Anova(f)$"F value"[1])
  merged <- as.data.frame(cbind(padj_MRIQC))
  return(merged)
}

num=length(Total_StandVis)
stats.StandVis <- extract(Total_StandVis, num) ##this will output the multiple correct p-values


#4.2. subcortical volume correction 
Small_StandVis.POND_SV <- POND_LenVis_Brain[, c('age_scan', 'ICV', 'GENDER', 'Decision', 'IQ', 'AB21GCCS', 'scanner', 'Lthal', 'Rthal', 'Lamyg', 'Ramyg', 'Lcaud', 'Rcaud', 'Lput', 'Rput', 'Lpal', 'Rpal', 'Lhippo', 'Rhippo', 'Laccumb', 'Raccumb')]

SubcorticalMetricList <- c('Lthal', 'Rthal', 'Lamyg', 'Ramyg', 'Lcaud', 'Rcaud', 'Lput', 'Rput', 'Lpal', 'Rpal', 'Lhippo', 'Rhippo', 'Laccumb', 'Raccumb')

Total_StandVis_SV <- lapply(SubcorticalMetricList, function(i){
  lm(get(i) ~ Decision + age_scan + ICV + GENDER + IQ + AB21GCCS + scanner, data = Small_StandVis.POND_SV, na.action = na.omit)
})

num=length(Total_StandVis_SV)
stats.StandVis_SV <- extract(Total_StandVis_SV, num)



#5. Performing the approach by decision interaction 

#5.1. Creating a "QC Approach" variable so I can perform the interaction 
POND_StandVis_Brain$Approach <- "Standard Visual"
POND_StandVisMet_Brain$Approach <- "Standard Visual Metric"
POND_StringVis_Brain$Approach <- "Stringent Visual"
POND_StringVisMet_Brain$Approach <- "Stringent Visual Metric"
POND_Auto_Brain$Approach <- "Automated"

ALLPOND_GROUPS <- rbind.fill(POND_StandVis_Brain, POND_StandVisMet_Brain, POND_StringVis_Brain, POND_StringVisMet_Brain, POND_Auto_Brain)

ALLPOND_GROUPS$Approach <- as.factor(ALLPOND_GROUPS$Approach)
ALLPOND_GROUPS$Decision <- as.factor(ALLPOND_GROUPS$Decision)

#5.2. Performing the ANOVA

CT_ALLPOND_GROUPS <- function(ALLPOND_GROUPS) {
  library(multcomp)
  library(emmeans)
  Sample.Data <- ALLPOND_GROUPS[, c('Approach', 'QCDecision', 'age_scan', 'GENDER', 'ICV', 'scanner', 'L_bankssts_thickavg',  'L_caudalanteriorcingulate_thickavg', 'L_caudalmiddlefrontal_thickavg', 'L_cuneus_thickavg', 'L_entorhinal_thickavg', 
                                    'L_fusiform_thickavg', 'L_inferiorparietal_thickavg', 'L_inferiortemporal_thickavg', 'L_isthmuscingulate_thickavg', 'L_lateraloccipital_thickavg', 'L_lateralorbitofrontal_thickavg', 
                                    'L_lingual_thickavg', 'L_medialorbitofrontal_thickavg', 'L_middletemporal_thickavg', 'L_parahippocampal_thickavg', 'L_paracentral_thickavg', 'L_parsopercularis_thickavg', 
                                    'L_parsorbitalis_thickavg', 'L_parstriangularis_thickavg', 'L_pericalcarine_thickavg', 'L_postcentral_thickavg', 'L_posteriorcingulate_thickavg', 'L_precentral_thickavg', 
                                    'L_precuneus_thickavg', 'L_rostralanteriorcingulate_thickavg', 'L_rostralmiddlefrontal_thickavg', 'L_superiorfrontal_thickavg', 'L_superiorparietal_thickavg', 'L_superiortemporal_thickavg', 
                                    'L_supramarginal_thickavg', 'L_frontalpole_thickavg', 'L_temporalpole_thickavg', 'L_transversetemporal_thickavg', 'L_insula_thickavg', 'R_bankssts_thickavg', 'R_caudalanteriorcingulate_thickavg', 
                                    'R_caudalmiddlefrontal_thickavg', 'R_entorhinal_thickavg', 'R_cuneus_thickavg', 'R_fusiform_thickavg', 'R_inferiorparietal_thickavg', 'R_inferiortemporal_thickavg', 'R_isthmuscingulate_thickavg', 
                                    'R_lateraloccipital_thickavg', 'R_lateralorbitofrontal_thickavg', 'R_lingual_thickavg', 'R_medialorbitofrontal_thickavg', 'R_middletemporal_thickavg', 'R_parahippocampal_thickavg', 'R_paracentral_thickavg', 
                                    'R_parsopercularis_thickavg', 'L_temporalpole_thickavg', 'R_parsorbitalis_thickavg', 'R_parstriangularis_thickavg', 'R_pericalcarine_thickavg', 'R_postcentral_thickavg', 'R_posteriorcingulate_thickavg', 
                                    'R_precentral_thickavg', 'R_precuneus_thickavg', 'R_rostralanteriorcingulate_thickavg', 'R_rostralmiddlefrontal_thickavg', 'R_superiorfrontal_thickavg', 'R_superiorparietal_thickavg', 'R_superiortemporal_thickavg', 
                                    'R_supramarginal_thickavg', 'R_frontalpole_thickavg', 'R_temporalpole_thickavg', 'R_transversetemporal_thickavg', 'R_insula_thickavg')]
  aov(Sample.Data[,7] ~ Approach*Decision + age_scan + GENDER + scanner + ICV, data = Sample.Data, na.action = na.omit, p.adjust = "bonferroni")
  Loop2 <- for (i in 7:length(Sample.Data)) {
    cat("Printing the pairwise t.test comparing cortical thickness measures across diagnosis without QC in", (colnames(Sample.Data)[i]), "\n")
    TT <- aov(Sample.Data[,i] ~ Approach*Decision + age_scan + GENDER + scanner + ICV, data = Sample.Data, na.action = na.omit, p.adjust = "bonferroni")
    print(summary(TT))
    emm <- emmeans(TT, ~ Approach*Decision)
    emm_contrast <- contrast(emm, "consec", simple = "each", combine = TRUE, adjust = "fdr")
    print(emm_contrast)
    contrast_plot <- emmip(TT, Approach ~ QCDecision)
    plot(contrast_plot)
  }
  return(Loop2)
}


#5.3. Multiple comparison correction 
Small_ALLPOND_GROUPS <- ALLPOND_GROUPS[, c('age_scan', 'ICV', 'GENDER', 'Decision', 'IQ', 'AB21GCCS', 'scanner', 'L_bankssts_thickavg', 'L_caudalanteriorcingulate_thickavg', 'L_caudalmiddlefrontal_thickavg', 'L_cuneus_thickavg', 'L_entorhinal_thickavg', 'L_fusiform_thickavg', 'L_inferiorparietal_thickavg', 'L_inferiortemporal_thickavg', 'L_isthmuscingulate_thickavg', 'L_lateraloccipital_thickavg', 'L_lateralorbitofrontal_thickavg', 'L_lingual_thickavg', 'L_medialorbitofrontal_thickavg', 'L_middletemporal_thickavg', 'L_parahippocampal_thickavg', 'L_paracentral_thickavg', 'L_parsopercularis_thickavg', 'L_parsorbitalis_thickavg', 'L_parstriangularis_thickavg', 'L_pericalcarine_thickavg', 'L_postcentral_thickavg', 'L_posteriorcingulate_thickavg', 'L_precentral_thickavg', 'L_precuneus_thickavg', 'L_rostralanteriorcingulate_thickavg', 'L_rostralmiddlefrontal_thickavg', 'L_superiorfrontal_thickavg', 'L_superiorparietal_thickavg', 'L_superiortemporal_thickavg', 'L_supramarginal_thickavg', 'L_frontalpole_thickavg', 'L_temporalpole_thickavg', 'L_transversetemporal_thickavg', 'L_insula_thickavg', 'R_bankssts_thickavg', 'R_caudalanteriorcingulate_thickavg', 'R_caudalmiddlefrontal_thickavg', 'R_entorhinal_thickavg', 'R_cuneus_thickavg', 'R_fusiform_thickavg', 'R_inferiorparietal_thickavg', 'R_inferiortemporal_thickavg', 'R_isthmuscingulate_thickavg', 'R_lateraloccipital_thickavg', 'R_lateralorbitofrontal_thickavg', 'R_lingual_thickavg', 'R_medialorbitofrontal_thickavg', 'R_middletemporal_thickavg', 'R_parahippocampal_thickavg', 'R_paracentral_thickavg', 'R_parsopercularis_thickavg', 'L_temporalpole_thickavg', 'R_parsorbitalis_thickavg', 'R_parstriangularis_thickavg', 'R_pericalcarine_thickavg', 'R_postcentral_thickavg', 'R_posteriorcingulate_thickavg', 'R_precentral_thickavg', 'R_precuneus_thickavg', 'R_rostralanteriorcingulate_thickavg', 'R_rostralmiddlefrontal_thickavg', 'R_superiorfrontal_thickavg', 'R_superiorparietal_thickavg', 'R_superiortemporal_thickavg', 'R_supramarginal_thickavg', 'R_frontalpole_thickavg', 'R_temporalpole_thickavg', 'R_transversetemporal_thickavg', 'R_insula_thickavg')]

Total_Appr_Dec <- lapply(BrainMetricList, function(i){
  lm(get(i) ~ Approach*Decision + age_scan + ICV + GENDER + scanner, data = Small_ALLPOND_GROUPS, na.action = na.omit)
})

num=length(Total_Appr_Dec)
stats.Appr_Dec <- extract(Total_Appr_Dec, num) ##this will output the multiple correct p-values












