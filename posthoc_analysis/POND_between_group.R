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












