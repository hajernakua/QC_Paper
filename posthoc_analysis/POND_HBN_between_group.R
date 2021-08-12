### this script will perform the post-hoc between-diagnoses analysis examining whether specific brain ROIs that have been reported to differ between diagnosis groups also differ in the POND sample and whether the results change depending on the QC approach implemented.
### the same script was used for POND and HBN data with different inputs from both (in this shared script, I only show the POND analysis)


##loading any packages
library(sjstats)

#1. Merging the data frames of the INCLUDED sample from each QC approach and the brain metrics 
StandardVisual_AllPOND_Brain <- merge(StandardVisual_AllPOND, BrainMeasures_AllPOND, by = "ID")
StringentVisual_AllPOND_Brain <- merge(StringentVisual_AllPOND, BrainMeasures_AllPOND, by = "ID")
StandardVisualMet_AllPOND_Brain <- merge(StandardVisualMet_AllPOND, BrainMeasures_AllPOND, by = "ID")
StringentVisualMet_AllPOND_Brain <- merge(StringentVisualMet_AllPOND, BrainMeasures_AllPOND, by = "ID")
Automated_AllPOND_Brain <- merge(AutomatedCohort_AllPOND, BrainMeasures_AllPOND, by = "ID")


#2. Create new DFs of participants with each diagnosis (ASD, ADHD, & OCD). Going to do this for each QC approach included group.

##Standard visual 
POND_StandVis_Brain_ASD <- StandardVisual_AllPOND_Brain$clin_diagnosis == "ASD"
POND_StandVis_Brain_ASD <- StandardVisual_AllPOND_Brain[POND_StandVis_Brain_ASD, ]
POND_StandVis_Brain_ASD <- POND_StandVis_Brain_ASD %>% drop_na(clin_diagnosis)

POND_StandVis_Brain_ADHD <- StandardVisual_AllPOND_Brain$clin_diagnosis == "ADHD"
POND_StandVis_Brain_ADHD <- StandardVisual_AllPOND_Brain[POND_StandVis_Brain_ADHD, ]
POND_StandVis_Brain_ADHD <- POND_StandVis_Brain_ADHD %>% drop_na(clin_diagnosis)

POND_StandVis_Brain_OCD <- StandardVisual_AllPOND_Brain$clin_diagnosis == "OCD"
POND_StandVis_Brain_OCD <- StandardVisual_AllPOND_Brain[POND_StandVis_Brain_OCD, ]
POND_StandVis_Brain_OCD <- POND_StandVis_Brain_OCD %>% drop_na(clin_diagnosis)

POND_StandVis_Brain_ASD_ADHD_OCD <- rbind(POND_StandVis_Brain_ASD, POND_StandVis_Brain_ADHD, POND_StandVis_Brain_OCD)


###stringent visual 
POND_StringVis_Brain_ASD <- StringentVisual_AllPOND_Brain$clin_diagnosis == "ASD"
POND_StringVis_Brain_ASD <- StringentVisual_AllPOND_Brain[POND_StringVis_Brain_ASD, ]
POND_StringVis_Brain_ASD <- POND_StringVis_Brain_ASD %>% drop_na(clin_diagnosis)

POND_StringVis_Brain_ADHD <- StringentVisual_AllPOND_Brain$clin_diagnosis == "ADHD"
POND_StringVis_Brain_ADHD <- StringentVisual_AllPOND_Brain[POND_StringVis_Brain_ADHD, ]
POND_StringVis_Brain_ADHD <- POND_StringVis_Brain_ADHD %>% drop_na(clin_diagnosis)

POND_StringVis_Brain_OCD <- StringentVisual_AllPOND_Brain$clin_diagnosis == "OCD"
POND_StringVis_Brain_OCD <- StringentVisual_AllPOND_Brain[POND_StringVis_Brain_OCD, ]
POND_StringVis_Brain_OCD <- POND_StringVis_Brain_OCD %>% drop_na(clin_diagnosis)

POND_StringVis_Brain_ASD_ADHD_OCD <- rbind(POND_StringVis_Brain_ASD, POND_StringVis_Brain_ADHD, POND_StringVis_Brain_OCD)


##Standard visual + metric
POND_StandVisMet_Brain_ASD <- StandardVisualMet_AllPOND_Brain$clin_diagnosis == "ASD"
POND_StandVisMet_Brain_ASD <- StandardVisualMet_AllPOND_Brain[POND_StandVisMet_Brain_ASD, ]
POND_StandVisMet_Brain_ASD <- POND_StandVisMet_Brain_ASD %>% drop_na(clin_diagnosis)

POND_StandVisMet_Brain_ADHD <- StandardVisualMet_AllPOND_Brain$clin_diagnosis == "ADHD"
POND_StandVisMet_Brain_ADHD <- StandardVisualMet_AllPOND_Brain[POND_StandVisMet_Brain_ADHD, ]
POND_StandVisMet_Brain_ADHD <- POND_StandVisMet_Brain_ADHD %>% drop_na(clin_diagnosis)

POND_StandVisMet_Brain_OCD <- StandardVisualMet_AllPOND_Brain$clin_diagnosis == "OCD"
POND_StandVisMet_Brain_OCD <- StandardVisualMet_AllPOND_Brain[POND_StandVisMet_Brain_OCD, ]
POND_StandVisMet_Brain_OCD <- POND_StandVisMet_Brain_OCD %>% drop_na(clin_diagnosis)

POND_StandVisMet_Brain_ASD_ADHD_OCD <- rbind(POND_StandVisMet_Brain_ASD, POND_StandVisMet_Brain_ADHD, POND_StandVisMet_Brain_OCD)


###stringent visual + metric
POND_StringVisMet_Brain_ASD <- StringentVisualMet_AllPOND_Brain$clin_diagnosis == "ASD"
POND_StringVisMet_Brain_ASD <- StringentVisualMet_AllPOND_Brain[POND_StringVisMet_Brain_ASD, ]
POND_StringVisMet_Brain_ASD <- POND_StringVisMet_Brain_ASD %>% drop_na(clin_diagnosis)

POND_StringVisMet_Brain_ADHD <- StringentVisualMet_AllPOND_Brain$clin_diagnosis == "ADHD"
POND_StringVisMet_Brain_ADHD <- StringentVisualMet_AllPOND_Brain[POND_StringVisMet_Brain_ADHD, ]
POND_StringVisMet_Brain_ADHD <- POND_StringVisMet_Brain_ADHD %>% drop_na(clin_diagnosis)

POND_StringVisMet_Brain_OCD <- StringentVisualMet_AllPOND_Brain$clin_diagnosis == "OCD"
POND_StringVisMet_Brain_OCD <- StringentVisualMet_AllPOND_Brain[POND_StringVisMet_Brain_OCD, ]
POND_StringVisMet_Brain_OCD <- POND_StringVisMet_Brain_OCD %>% drop_na(clin_diagnosis)

POND_StringVisMet_Brain_ASD_ADHD_OCD <- rbind(POND_StringVisMet_Brain_ASD, POND_StringVisMet_Brain_ADHD, POND_StringVisMet_Brain_OCD)


###automated
POND_Auto_Brain_ASD <- AutomatedCohort_AllPOND_Brain$clin_diagnosis == "ASD"
POND_Auto_Brain_ASD <- AutomatedCohort_AllPOND_Brain[POND_Auto_Brain_ASD, ]
POND_Auto_Brain_ASD <- POND_Auto_Brain_ASD %>% drop_na(clin_diagnosis)

POND_Auto_Brain_ADHD <- AutomatedCohort_AllPOND_Brain$clin_diagnosis == "ADHD"
POND_Auto_Brain_ADHD <- AutomatedCohort_AllPOND_Brain[POND_Auto_Brain_ADHD, ]
POND_Auto_Brain_ADHD <- POND_Auto_Brain_ADHD %>% drop_na(clin_diagnosis)

POND_Auto_Brain_OCD <- AutomatedCohort_AllPOND_Brain$clin_diagnosis == "OCD"
POND_Auto_Brain_OCD <- AutomatedCohort_AllPOND_Brain[POND_Auto_Brain_OCD, ]
POND_Auto_Brain_OCD <- POND_Auto_Brain_OCD %>% drop_na(clin_diagnosis)

POND_Auto_Brain_ASD_ADHD_OCD <- rbind(POND_Auto_Brain_ASD, POND_Auto_Brain_ADHD, POND_Auto_Brain_OCD)


#3. Performing the group-wise analysis - will be doing this analysis for each QC approach independently 

# Standard visual QC
HippoVol_StandVis_diag <- lm(Lhippo ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_StandVis_Brain_ASD_ADHD_OCD, na.action = na.omit)
HippoVol_StandVis_diag_An <- Anova(HippoVol_LenVis_diag)


RHippoVol_StandVis_diag <- lm(Rhippo ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_StandVis_Brain_ASD_ADHD_OCD, na.action = na.omit)
RHippoVol_StandVis_diag_An <- Anova(RHippoVol_StandVis_diag)


mOFC_StandVis_diag <- lm(L_medialorbitofrontal_thickavg ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_StandVis_Brain_ASD_ADHD_OCD, na.action = na.omit)
mOFC_StandVis_diag_An <- Anova(mOFC_StandVis_diag)

RmOFC_StandVis_diag <- lm(R_medialorbitofrontal_thickavg ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_StandVis_Brain_ASD_ADHD_OCD, na.action = na.omit)
RmOFC_StandVis_diag_An <- Anova(RmOFC_StandVis_diag)

lOFC_StandVis_diag <- lm(L_lateralorbitofrontal_thickavg ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_StandVis_Brain_ASD_ADHD_OCD, na.action = na.omit)
lOFC_StandVis_diag_An <- Anova(lOFC_StandVis_diag)

RlOFC_StandVis_diag <- lm(R_lateralorbitofrontal_thickavg ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_StandVis_Brain_ASD_ADHD_OCD, na.action = na.omit)
RlOFC_StandVis_diag_An <- Anova(RlOFC_StandVis_diag)

# Standard visual+ metric QC
HippoVol_StandVisMet_diag <- lm(Lhippo ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_StandVisMet_Brain_ASD_ADHD_OCD, na.action = na.omit)
HippoVol_StandVisMet_diag_An <- Anova(HippoVol_StandVisMet_diag)

RHippoVol_StandVisMet_diag <- lm(Rhippo ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_StandVisMet_Brain_ASD_ADHD_OCD, na.action = na.omit)
RHippoVol_StandVisMet_diag_An <- Anova(RHippoVol_StandVisMet_diag)


mOFC_StandVisMet_diag <- lm(L_medialorbitofrontal_thickavg ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_StandVisMet_Brain_ASD_ADHD_OCD, na.action = na.omit)
mOFC_StandVisMet_diag_An <- Anova(mOFC_StandVisMet_diag)

RmOFC_StandVisMet_diag <- lm(R_medialorbitofrontal_thickavg ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_StandVisMet_Brain_ASD_ADHD_OCD, na.action = na.omit)
RmOFC_StandVisMet_diag_An <- Anova(RmOFC_StandVisMet_diag)

lOFC_StandVisMet_diag <- lm(L_lateralorbitofrontal_thickavg ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_StandVisMet_Brain_ASD_ADHD_OCD, na.action = na.omit)
lOFC_StandVisMet_diag_An <- Anova(lOFC_StandVisMet_diag)

RlOFC_StandVisMet_diag <- lm(R_lateralorbitofrontal_thickavg ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_StandVisMet_Brain_ASD_ADHD_OCD, na.action = na.omit)
RlOFC_StandVisMet_diag_An <- Anova(RlOFC_StandVisMet_diag)


# stringent visual QC
HippoVol_StringVis_diag <- lm(Lhippo ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_StringVis_Brain_ASD_ADHD_OCD, na.action = na.omit)
HippoVol_StringVis_diag_An <- Anova(HippoVol_StringVis_diag)
HippoVol_StringVis_diag_An
summary(HippoVol_StringVis_diag_An)
cohens_f(HippoVol_StringVis_diag_An)


RHippoVol_StringVis_diag <- lm(Rhippo ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_StringVis_Brain_ASD_ADHD_OCD, na.action = na.omit)
RHippoVol_StringVis_diag_An <- Anova(RHippoVol_StringVis_diag)
RHippoVol_StringVis_diag_An
summary(RHippoVol_StringVis_diag_An)
cohens_f(RHippoVol_StringVis_diag_An)

mOFC_StringVis_diag <- lm(L_medialorbitofrontal_thickavg ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_StringVis_Brain_ASD_ADHD_OCD, na.action = na.omit)
mOFC_StringVis_diag_An <- Anova(mOFC_StringVis_diag)
mOFC_StringVis_diag_An
summary(mOFC_StringVis_diag_An)
cohens_f(mOFC_StringVis_diag_An)

RmOFC_StringVis_diag <- lm(R_medialorbitofrontal_thickavg ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_StringVis_Brain_ASD_ADHD_OCD, na.action = na.omit)
RmOFC_StringVis_diag_An <- Anova(RmOFC_StringVis_diag)
RmOFC_StringVis_diag_An
summary(RmOFC_StringVis_diag_An)
cohens_f(RmOFC_StringVis_diag_An)

lOFC_StringVis_diag <- lm(L_lateralorbitofrontal_thickavg ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_StringVis_Brain_ASD_ADHD_OCD, na.action = na.omit)
lOFC_StringVis_diag_An <- Anova(lOFC_StringVis_diag)
summary(lOFC_StringVis_diag)
cohens_f(lOFC_StringVis_diag)

RlOFC_StringVis_diag <- aov(R_lateralorbitofrontal_thickavg ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_StringVis_Brain_ASD_ADHD_OCD, na.action = na.omit)
summary(RlOFC_StringVis_diag)
cohens_f(RlOFC_StringVis_diag)
###no significant difference here


# stringent visual + metric QC
HippoVol_StringVisMet_diag <- lm(Lhippo ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_StringVisMet_Brain_ASD_ADHD_OCD, na.action = na.omit)
HippoVol_StringVisMet_diag_An <- Anova(HippoVol_StringVisMet_diag)
HippoVol_StringVisMet_diag_An
summary(HippoVol_StringVisMet_diag_An)

RHippoVol_StringVisMet_diag <- lm(Rhippo ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_StringVisMet_Brain_ASD_ADHD_OCD, na.action = na.omit)
RHippoVol_StringVisMet_diag_An <- Anova(RHippoVol_StringVisMet_diag)
RHippoVol_StringVisMet_diag_An
summary(RHippoVol_StringVisMet_diag_An)

mOFC_StringVisMet_diag <- lm(L_medialorbitofrontal_thickavg ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_StringVisMet_Brain_ASD_ADHD_OCD, na.action = na.omit)
mOFC_StringVisMet_diag_An <- Anova(mOFC_StringVisMet_diag)
mOFC_StringVisMet_diag_An
summary(mOFC_StringVisMet_diag_An)

RmOFC_StringVisMet_diag <- lm(R_medialorbitofrontal_thickavg ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_StringVisMet_Brain_ASD_ADHD_OCD, na.action = na.omit)
RmOFC_StringVisMet_diag_An <- Anova(RmOFC_StringVisMet_diag)
RmOFC_StringVisMet_diag_An
summary(RmOFC_StringVisMet_diag_An)

lOFC_StringVisMet_diag <- aov(L_lateralorbitofrontal_thickavg ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_StringVisMet_Brain_ASD_ADHD_OCD, na.action = na.omit)
summary(lOFC_StringVisMet_diag)

RlOFC_StringVisMet_diag <- aov(R_lateralorbitofrontal_thickavg ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_StringVisMet_Brain_ASD_ADHD_OCD, na.action = na.omit)
summary(RlOFC_StringVisMet_diag)
###no significant difference here


# automated QC
HippoVol_Auto_diag <- lm(Lhippo ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_Auto_Brain_ASD_ADHD_OCD, na.action = na.omit)
HippoVol_Auto_diag_An <- Anova(HippoVol_Auto_diag)
HippoVol_Auto_diag_An
summary(HippoVol_Auto_diag_An)
cohens_f(HippoVol_Auto_diag_An)

RHippoVol_Auto_diag <- lm(Rhippo ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_Auto_Brain_ASD_ADHD_OCD, na.action = na.omit)
RHippoVol_Auto_diag_An <- Anova(RHippoVol_Auto_diag)
RHippoVol_Auto_diag_An
summary(RHippoVol_Auto_diag_An)
cohens_f(RHippoVol_Auto_diag_An)

RmOFC_Auto_diag <- lm(R_medialorbitofrontal_thickavg ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_Auto_Brain_ASD_ADHD_OCD, na.action = na.omit)
RmOFC_Auto_diag_An <- Anova(RmOFC_Auto_diag)
RmOFC_Auto_diag_An
summary(RmOFC_Auto_diag_An)
cohens_f(RmOFC_Auto_diag_An)

mOFC_Auto_diag <- lm(L_medialorbitofrontal_thickavg ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_Auto_Brain_ASD_ADHD_OCD, na.action = na.omit)
mOFC_Auto_diag_An <- Anova(mOFC_Auto_diag)
mOFC_Auto_diag_An
summary(mOFC_Auto_diag_An)
cohens_f(mOFC_Auto_diag_An)

RlOFC_Auto_diag <- lm(R_lateralorbitofrontal_thickavg ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_Auto_Brain_ASD_ADHD_OCD, na.action = na.omit)
RlOFC_Auto_diag_An <- Anova(RlOFC_Auto_diag)
RlOFC_Auto_diag_An
summary(RlOFC_Auto_diag_An)
cohens_f(RlOFC_Auto_diag_An)

lOFC_Auto_diag <- lm(L_lateralorbitofrontal_thickavg ~ clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_Auto_Brain_ASD_ADHD_OCD, na.action = na.omit)
lOFC_Auto_diag_An <- Anova(lOFC_Auto_diag)
lOFC_Auto_diag_An
summary(lOFC_Auto_diag_An)
cohens_f(lOFC_Auto_diag_An)

#### setting up interaction between QC group and diagnosis analysis

#4. Setting up the approach variable for each of the QC data frames
POND_StandVis_Brain_ASD_ADHD_OCD$Approach <- "Standard Visual"
POND_StandVisMet_Brain_ASD_ADHD_OCD$Approach <- "Standard Visual + Metric"
POND_StringVis_Brain_ASD_ADHD_OCD$Approach <- "Stringent Visual"
POND_StringVisMet_Brain_ASD_ADHD_OCD$Approach <- "Stringent Visual + Metric"
POND_Auto_Brain_ASD_ADHD_OCD$Approach <- "Automated"

POND_Approach_Brain_ASD_ADHD_OCD <- rbind(POND_LenVis_Brain_ASD_ADHD_OCD, POND_LenVisMet_Brain_ASD_ADHD_OCD, POND_StringVis_Brain_ASD_ADHD_OCD, POND_StringVisMet_Brain_ASD_ADHD_OCD, POND_Auto_Brain_ASD_ADHD_OCD)


#5. Performing the interaction analysis
HippoVol_Approach_Diag <- lm(Lhippo ~ Approach*clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_Approach_Brain_ASD_ADHD_OCD, na.action = na.omit)
summary(HippoVol_Approach_Diag)
HippoVol_Approach_Diag_An <- Anova(HippoVol_Approach_Diag)
HippoVol_Approach_Diag_An
summary(HippoVol_Approach_Diag_An)
eta_sq(HippoVol_Approach_Diag_An)
cohens_f(HippoVol_Approach_Diag_An)


RHippoVol_Approach_Diag <- lm(Rhippo ~ Approach*clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_Approach_Brain_ASD_ADHD_OCD, na.action = na.omit)
summary(RHippoVol_Approach_Diag)
RHippoVol_Approach_Diag_An <- Anova(RHippoVol_Approach_Diag)
RHippoVol_Approach_Diag_An
summary(RHippoVol_Approach_Diag_An)
cohens_f(RHippoVol_Approach_Diag_An)

LmOFC_Approach_Diag <- lm(L_medialorbitofrontal_thickavg ~ Approach*clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_Approach_Brain_ASD_ADHD_OCD, na.action = na.omit)
summary(LmOFC_Approach_Diag)
LmOFC_Approach_Diag_An <- Anova(LmOFC_Approach_Diag)
LmOFC_Approach_Diag_An
cohens_f(LmOFC_Approach_Diag_An)


RmOFC_Approach_Diag <- lm(R_medialorbitofrontal_thickavg ~ Approach*clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_Approach_Brain_ASD_ADHD_OCD, na.action = na.omit)
summary(RmOFC_Approach_Diag)
RmOFC_Approach_Diag_An <- Anova(RmOFC_Approach_Diag)
RmOFC_Approach_Diag_An
cohens_f(RmOFC_Approach_Diag_An)


LlOFC_Approach_Diag <- lm(L_lateralorbitofrontal_thickavg ~ Approach*clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_Approach_Brain_ASD_ADHD_OCD, na.action = na.omit)
summary(LlOFC_Approach_Diag)
LlOFC_Approach_Diag_An <- Anova(LlOFC_Approach_Diag)
LlOFC_Approach_Diag_An
cohens_f(LlOFC_Approach_Diag_An)

RlOFC_Approach_Diag <- lm(R_lateralorbitofrontal_thickavg ~ Approach*clin_diagnosis + age_scan + ICV.x + GENDER + scanner, data = POND_Approach_Brain_ASD_ADHD_OCD, na.action = na.omit)
summary(RlOFC_Approach_Diag)
RlOFC_Approach_Diag_An <- Anova(RlOFC_Approach_Diag)
RlOFC_Approach_Diag_An
cohens_f(RlOFC_Approach_Diag_An)


#6. Plotting this interaction
LHippo_Approach_POND <- function(POND_Approach_Brain_ASD_ADHD_OCD) {
  library(ggplot2)
  library(ggpubr)
  library(ggsignif)
  LHippo_Approach_POND <- ggplot(data = POND_Approach_Brain_ASD_ADHD_OCD, aes(x = Approach, y = Lhippo, fill = clin_diagnosis), na.rm = TRUE) +
    stat_boxplot(geom = "errorbar") +
     #stat_compare_means(method = "anova", label.y = 40) +
    stat_compare_means(label = "p.signif", method = "t.test", ref.group = "NULL") +
    #stat_compare_means(comparisons = list(c("Standard Visual", "Standard Visual + Metric"), c("Standard Visual", "Stringent Visual"), c("Standard Visual", "Automated"), c("Standard Visual", "Stringent Visual + Metric"))) +
    geom_boxplot(alpha = 0.3, size = 0.2, outlier.color = 'red', outlier.shape = NA, aes(color = clin_diagnosis)) +
    stat_summary(fun.y = 'mean', geom = 'point', shape = 8, size = 0.5) +
    #geom_jitter(aes(color = clin_diagnosis), size = 0.05) +
    labs(x = "Quality Control Approach", y = "Left Hippocampal Volume") +
    theme_classic() +
    theme(text = element_text(size= 5))
  return(Boxplot)
}


tiff("LHippo_Approach_POND.tiff", width = 24, height = 20, units = "cm", res = 300)
print(LHippo_Approach_POND)
dev.off()






