### this script will perform the post-hoc between-diagnoses analysis examining whether specific brain ROIs that have been reported to differ between diagnosis groups also differ in the POND sample and whether the results change depending on the QC approach implemented.

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

#1. Standard visual QC
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

#2. Standard visual+ metric QC
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


#3. stringent visual QC
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


#4. stringent visual + metric QC
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


#5. automated QC
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















