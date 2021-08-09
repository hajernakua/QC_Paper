### this script will perform the post-hoc between-diagnoses analysis examining whether specific brain ROIs that have been reported to differ between diagnosis groups also differ in the POND sample and whether the results change depending on the QC approach implemented.

##loading any packages
library(sjstats)

#1. Merging the data frames of the INCLUDED sample from each QC approach and the brain metrics 
PNC_StandVis_Brain <- merge(PNC_StandVis, BrainMeasures_PNC, by = "ID")
PNC_StandVisMet_Brain <- merge(PNC_StandVisMet, BrainMeasures_PNC, by = "ID")
PNC_StringVis_Brain <- merge(PNC_StringVis, BrainMeasures_PNC, by = "ID")
PNC_StringVisMet_Brain <- merge(PNC_StringVisMet, BrainMeasures_PNC, by = "ID")
PNC_Automated_Brain <- merge(MRIQC.Cohort.PNC, BrainMeasures_PNC, by = "ID")

#2. Create new DFs of participants with each diagnosis (PSS & TDC). Going to do this for each QC approach included group.
##to get the TDC group, we need to remove all participants with any other diagnosis so this script does that for each group 

##Standard visual 
PNC_StandVis_Brain_PSS <- PNC_StandVis_Brain$psychosis == 1
PNC_StandVis_Brain_PSS <- PNC_StandVis_Brain[PNC_StandVis_Brain_PSS, ]
PNC_StandVis_Brain_PSS$clin_diagnosis <- "PSS"

##to get the TDC, I need to remove all participants with all other diagnoses 
PNC_StandVis_Brain_TDC <- PNC_StandVis_Brain[!(PNC_StandVis_Brain$PHOB == "yes" | 
                                             PNC_StandVis_Brain$PANDIS == "yes" | 
                                             PNC_StandVis_Brain$MDD == "yes" |
                                             PNC_StandVis_Brain$PTSD == "yes" |
                                             PNC_StandVis_Brain$OCD == "yes" |
                                             PNC_StandVis_Brain$SOC == "yes" |
                                             PNC_StandVis_Brain$AGR == "yes" |
                                             PNC_StandVis_Brain$GAD == "yes" |
                                             PNC_StandVis_Brain$SEP == "yes" |
                                             PNC_StandVis_Brain$CD == "yes" |
                                             PNC_StandVis_Brain$ODD == "yes" |
                                             PNC_StandVis_Brain$ADHD == "yes" |
                                             PNC_StandVis_Brain$psychosis == 1),]
PNC_StandVis_Brain_TDC$clin_diagnosis <- "TDC"

PNC_StandVis_Brain_PSS_TDC <- rbind(PNC_StandVis_Brain_PSS, PNC_StandVis_Brain_TDC)


##Standard visual + Metric
PNC_StandVisMet_Brain_PSS <- PNC_StandVisMet_Brain$psychosis == 1
PNC_StandVisMet_Brain_PSS <- PNC_StandVisMet_Brain[PNC_StandVisMet_Brain_PSS, ]
PNC_StandVisMet_Brain_PSS$clin_diagnosis <- "PSS"

##to get the TDC, I need to remove all participants with all other diagnoses 
PNC_StandVisMet_Brain_TDC <- PNC_StandVisMet_Brain[!(PNC_StandVisMet_Brain$PHOB == "yes" | 
                                             PNC_StandVisMet_Brain$PANDIS == "yes" | 
                                             PNC_StandVisMet_Brain$MDD == "yes" |
                                             PNC_StandVisMet_Brain$PTSD == "yes" |
                                             PNC_StandVisMet_Brain$OCD == "yes" |
                                             PNC_StandVisMet_Brain$SOC == "yes" |
                                             PNC_StandVisMet_Brain$AGR == "yes" |
                                             PNC_StandVisMet_Brain$GAD == "yes" |
                                             PNC_StandVisMet_Brain$SEP == "yes" |
                                             PNC_StandVisMet_Brain$CD == "yes" |
                                             PNC_StandVisMet_Brain$ODD == "yes" |
                                             PNC_StandVisMet_Brain$ADHD == "yes" |
                                             PNC_StandVisMet_Brain$psychosis == 1),]
PNC_StandVisMet_Brain_TDC$clin_diagnosis <- "TDC"

PNC_StandVisMet_Brain_PSS_TDC <- rbind(PNC_StandVisMet_Brain_PSS, PNC_StandVisMet_Brain_TDC)


##stringent visual 
PNC_StringVis_Brain_PSS <- PNC_StringVis_Brain$psychosis == 1
PNC_StringVis_Brain_PSS <- PNC_StringVis_Brain[PNC_StringVis_Brain_PSS, ]
PNC_StringVis_Brain_PSS$clin_diagnosis <- "PSS"

##to get the TDC, I need to remove all participants with all other diagnoses 
PNC_StringVis_Brain_TDC <- PNC_StringVis_Brain[!(PNC_StringVis_Brain$PHOB == "yes" | 
                                             PNC_StringVis_Brain$PANDIS == "yes" | 
                                             PNC_StringVis_Brain$MDD == "yes" |
                                             PNC_StringVis_Brain$PTSD == "yes" |
                                             PNC_StringVis_Brain$OCD == "yes" |
                                             PNC_StringVis_Brain$SOC == "yes" |
                                             PNC_StringVis_Brain$AGR == "yes" |
                                             PNC_StringVis_Brain$GAD == "yes" |
                                             PNC_StringVis_Brain$SEP == "yes" |
                                             PNC_StringVis_Brain$CD == "yes" |
                                             PNC_StringVis_Brain$ODD == "yes" |
                                             PNC_StringVis_Brain$ADHD == "yes" |
                                             PNC_StringVis_Brain$psychosis == 1),]
PNC_StringVis_Brain_TDC$clin_diagnosis <- "TDC"

PNC_StringVis_Brain_PSS_TDC <- rbind(PNC_StringVis_Brain_PSS, PNC_StringVis_Brain_TDC)


##stringent visual + metric 
PNC_StringVisMet_Brain_PSS <- PNC_StringVisMet_Brain$psychosis == 1
PNC_StringVisMet_Brain_PSS <- PNC_StringVisMet_Brain[PNC_StringVisMet_Brain_PSS, ]
PNC_StringVisMet_Brain_PSS$clin_diagnosis <- "PSS"

##to get the TDC, I need to remove all participants with all other diagnoses 
PNC_StringVisMet_Brain_TDC <- PNC_StringVisMet_Brain[!(PNC_StringVisMet_Brain$PHOB == "yes" | 
                                                   PNC_StringVisMet_Brain$PANDIS == "yes" | 
                                                   PNC_StringVisMet_Brain$MDD == "yes" |
                                                   PNC_StringVisMet_Brain$PTSD == "yes" |
                                                   PNC_StringVisMet_Brain$OCD == "yes" |
                                                   PNC_StringVisMet_Brain$SOC == "yes" |
                                                   PNC_StringVisMet_Brain$AGR == "yes" |
                                                   PNC_StringVisMet_Brain$GAD == "yes" |
                                                   PNC_StringVisMet_Brain$SEP == "yes" |
                                                   PNC_StringVisMet_Brain$CD == "yes" |
                                                   PNC_StringVisMet_Brain$ODD == "yes" |
                                                   PNC_StringVisMet_Brain$ADHD == "yes" |
                                                   PNC_StringVisMet_Brain$psychosis == 1),]
PNC_StringVisMet_Brain_TDC$clin_diagnosis <- "TDC"

PNC_StringVisMet_Brain_PSS_TDC <- rbind(PNC_StringVisMet_Brain_PSS, PNC_StringVisMet_Brain_TDC)



##automated QC
PNC_Automated_Brain_PSS <- PNC_Automated_Brain$psychosis == 1
PNC_Automated_Brain_PSS <- PNC_Automated_Brain[PNC_Automated_Brain_PSS, ]
PNC_Automated_Brain_PSS$clin_diagnosis <- "PSS"

##to get the TDC, I need to remove all participants with all other diagnoses 
PNC_Automated_Brain_TDC <- PNC_Automated_Brain[!(PNC_Automated_Brain$PHOB.x == "yes" | 
                                                      PNC_Automated_Brain$PANDIS == "yes" | 
                                                      PNC_Automated_Brain$MDD == "yes" |
                                                      PNC_Automated_Brain$PTSD == "yes" |
                                                      PNC_Automated_Brain$OCD == "yes" |
                                                      PNC_Automated_Brain$SOC == "yes" |
                                                      PNC_Automated_Brain$AGR == "yes" |
                                                      PNC_Automated_Brain$GAD == "yes" |
                                                      PNC_Automated_Brain$SEP == "yes" |
                                                      PNC_Automated_Brain$CD == "yes" |
                                                      PNC_Automated_Brain$ODD == "yes" |
                                                      PNC_Automated_Brain$ADHD == "yes" |
                                                      PNC_Automated_Brain$psychosis == 1),]
PNC_Automated_Brain_TDC$clin_diagnosis <- "TDC"

PNC_Automated_Brain_PSS_TDC <- rbind(PNC_Automated_Brain_PSS, PNC_Automated_Brain_TDC)


#3. Performing the group-wise analysis - will be doing this analysis for each QC approach independently 

# Standard visual QC
library(car)
ThalVol_StandVis_diag <- lm(Lthal ~ clin_diagnosis + age + ICV + Sex.x, data = PNC_StandVis_Brain_PSS_TDC, na.action = na.omit)
ThalVol_StandVis_diag_An <- Anova(ThalVol_StandVis_diag)
ThalVol_StandVis_diag_An
summary(ThalVol_LenVis_diag_An)


RThalVol_StandVis_diag <- lm(Rthal ~ clin_diagnosis + age + ICV + Sex.x, data = PNC_StandVis_Brain_PSS_TDC, na.action = na.omit)
RThalVol_StandVis_diag_An <- Anova(RThalVol_StandVis_diag)
RThalVol_StandVis_diag_An
summary(RThalVol_StandVis_diag_An)


# Standard visual + metric QC
ThalVol_StandVisMet_diag <- lm(Lthal ~ clin_diagnosis + age + ICV + Sex.x, data = PNC_StandVisMet_Brain_PSS_TDC, na.action = na.omit)
ThalVol_StandVisMet_diag_An <- Anova(ThalVol_StandVisMet_diag)
ThalVol_StandVisMet_diag_An
summary(ThalVol_LenVisMet_diag_An)


RThalVol_StandVisMet_diag <- lm(Rthal ~ clin_diagnosis + age + ICV + Sex.x, data = PNC_StandVisMet_Brain_PSS_TDC, na.action = na.omit)
RThalVol_StandVisMet_diag_An <- Anova(RThalVol_StandVisMet_diag)
RThalVol_StandVisMet_diag_An
summary(RThalVol_StandVisMet_diag_An)


# stringent visual QC
ThalVol_StringVis_diag <- lm(Lthal ~ clin_diagnosis + age + ICV + Sex.x, data = PNC_StringVis_Brain_PSS_TDC, na.action = na.omit)
ThalVol_StringVis_diag_An <- Anova(ThalVol_StringVis_diag)
ThalVol_StringVis_diag_An
summary(ThalVol_StringVis_diag_An)
cohens_f(ThalVol_StringVis_diag_An)

RThalVol_StringVis_diag <- lm(Rthal ~ clin_diagnosis + age + ICV + Sex.x, data = PNC_StringVis_Brain_PSS_TDC, na.action = na.omit)
RThalVol_StringVis_diag_An <- Anova(RThalVol_StringVis_diag)
RThalVol_StringVis_diag_An
summary(RThalVol_StringVis_diag_An)
cohens_f(RThalVol_StringVis_diag_An)


# stringent visual + metric QC
ThalVol_StringVisMet_diag <- lm(Lthal ~ clin_diagnosis + age + ICV + Sex.x, data = PNC_StringVisMet_Brain_PSS_TDC, na.action = na.omit)
ThalVol_StringVisMet_diag_An <- Anova(ThalVol_StringVisMet_diag)
ThalVol_StringVisMet_diag_An
summary(ThalVol_StringVisMet_diag_An)

RThalVol_StringVisMet_diag <- lm(Rthal ~ clin_diagnosis + age + ICV + Sex.x, data = PNC_StringVisMet_Brain_PSS_TDC, na.action = na.omit)
RThalVol_StringVisMet_diag_An <- Anova(RThalVol_StringVisMet_diag)
RThalVol_StringVisMet_diag_An
summary(RThalVol_StringVisMet_diag_An)


# automated QC
ThalVol_Auto_diag <- lm(Lthal ~ clin_diagnosis + age + ICV + Sex.x, data = PNC_Automated_Brain_PSS_TDC, na.action = na.omit)
ThalVol_Auto_diag_An <- Anova(ThalVol_StringVisMet_diag)
ThalVol_Auto_diag_An
summary(ThalVol_Auto_diag_An)
cohens_f(ThalVol_Auto_diag_An)

RThalVol_Auto_diag <- lm(Rthal ~ clin_diagnosis + age + ICV + Sex.x, data = PNC_Automated_Brain_PSS_TDC, na.action = na.omit)
RThalVol_Auto_diag_An <- Anova(RThalVol_StringVisMet_diag)
RThalVol_Auto_diag_An
summary(RThalVol_Auto_diag_An)
cohens_f(RThalVol_Auto_diag_An)


#### setting up interaction between QC group and diagnosis analysis

#4. Setting up the approach variable for each of the QC data frames
PNC_LenVis_Brain_PSS_TDC$Approach <- "Standard Visual"
PNC_LenVisMet_Brain_PSS_TDC$Approach <- "Standard Visual + Metric"
PNC_StringVis_Brain_PSS_TDC$Approach <- "Stringent Visual"
PNC_StringVisMet_Brain_PSS_TDC$Approach <- "Stringent Visual + Metric"
PNC_Automated_Brain_PSS_TDC$Approach <- "Automated"


PNC_Approach_Brain_PSS_TDC <- rbind(PNC_LenVis_Brain_PSS_TDC, PNC_LenVisMet_Brain_PSS_TDC, PNC_StringVis_Brain_PSS_TDC, PNC_StringVisMet_Brain_PSS_TDC, PNC_Automated_Brain_PSS_TDC)


#5. Performing the interaction analysis
LThalVol_Approach_Diag <- lm(Lthal ~ Approach*clin_diagnosis + age + ICV + Sex.x, data = PNC_Approach_Brain_PSS_TDC, na.action = na.omit)
summary(LThalVol_Approach_Diag)
LThalVol_Approach_Diag_An <- Anova(LThalVol_Approach_Diag)
LThalVol_Approach_Diag_An
summary(LThalVol_Approach_Diag_An)
cohens_f(LThalVol_Approach_Diag_An)



RThalVol_Approach_Diag <- lm(Rthal ~ Approach*clin_diagnosis + age + ICV + Sex.x, data = PNC_Approach_Brain_PSS_TDC, na.action = na.omit)
summary(RThalVol_Approach_Diag)
RThalVol_Approach_Diag_An <- Anova(RThalVol_Approach_Diag)
RThalVol_Approach_Diag_An
summary(RThalVol_Approach_Diag_An)
cohens_f(RThalVol_Approach_Diag_An)


#6. Plotting this interaction
Lthal_Approach_HBN <- function(PNC_Approach_Brain_PSS_TDC) {
  library(ggplot2)
  library(ggpubr)
  library(ggsignif)
  Lthal_Approach_PNC <- ggplot(data = PNC_Approach_Brain_PSS_TDC, aes(x = Approach, y = Lthal, fill = clin_diagnosis), na.rm = TRUE) +
    stat_boxplot(geom = "errorbar") +
    #stat_compare_means(method = "anova", label.y = 40) +
    stat_compare_means(label = "p.signif", method = "t.test", ref.group = "NULL") +
    #stat_compare_means(comparisons = list(c("Standard Visual", "Standard Visual + Metric"), c("Standard Visual", "Stringent Visual"), c("Standard Visual", "Automated"), c("Standard Visual", "Stringent Visual + Metric"))) +
    geom_boxplot(alpha = 0.3, size = 0.2, outlier.color = 'red', outlier.shape = NA, aes(color = clin_diagnosis)) +
    stat_summary(fun.y = 'mean', geom = 'point', shape = 8, size = 0.5) +
    #geom_jitter(aes(color = clin_diagnosis), size = 0.05) +
    labs(x = "Quality Control Approach", y = "Left Thalamic Volume") +
    theme_classic()  +
    theme(text = element_text(size= 20))
  return(Boxplot)
}

tiff("Lthal_Approach_PNC.tiff", width = 24, height = 20, units = "cm", res = 300)
print(Lthal_Approach_PNC)
dev.off()


Rthal_Approach_PNC <- ggplot(data = PNC_Approach_Brain_PSS_TDC, aes(x = Approach, y = Rthal, fill = clin_diagnosis), na.rm = TRUE) +
  stat_boxplot(geom = "errorbar") +
  #stat_compare_means(method = "anova", label.y = 40) +
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "NULL") +
  #stat_compare_means(comparisons = list(c("Standard Visual", "Standard Visual + Metric"), c("Standard Visual", "Stringent Visual"), c("Standard Visual", "Automated"), c("Standard Visual", "Stringent Visual + Metric"))) +
  geom_boxplot(alpha = 0.3, size = 0.2, outlier.color = 'red', outlier.shape = NA, aes(color = clin_diagnosis)) +
  stat_summary(fun.y = 'mean', geom = 'point', shape = 8, size = 0.5) +
  #geom_jitter(aes(color = clin_diagnosis), size = 0.05) +
  labs(x = "Quality Control Approach", y = "Right Thalamic Volume") +
  theme_classic()  +
  theme(text = element_text(size= 20))

tiff("Rthal_Approach_PNC.tiff", width = 24, height = 20, units = "cm", res = 300)
print(Rthal_Approach_PNC)
dev.off()











