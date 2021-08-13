###this script will perform the supplementary analyses to evaluate whether excluded participants would feature greater clinical impairments (supplementary measures) compared to included participants, across all QC approaches
### many aspects of this script will be shared across the different datasets, but there are some dataset specific commands


#1. Chi-square test to examine differences in gender and diagnosis across included and excluded groups for each QC approach 

#1.1. Standard Visual QC
  #there were 2 participants with NA values for gender so those participants were removed for this analysis 
  POND_StandVis_FullGen <- POND_StandVis %>% drop_na(GENDER)

Gen_Dec_StandVis <- chisq.test(POND_StandVis_FullGen$Decision, POND_StandVis_FullGen$GENDER)

Diagnosis_Dec_StandVis <- chisq.test(POND_StandVis$clin_diagnosis, POND_StandVis$Decision)

#1.2. Standard Visual + Metric QC
  POND_StandVisMet_FullGen <- POND_StandVisMet %>% drop_na(GENDER)

Gen_Dec_StandVisMet <- chisq.test(POND_StandVisMet_FullGen$Decision, POND_StandVisMet_FullGen$GENDER)

Diagnosis_Dec_StandVisMet <- chisq.test(POND_StandVisMet$clin_diagnosis, POND_StandVisMet$Decision)


#1.3. Stringent Visual QC
POND_StringVis_FullGen <- POND_StringVis %>% drop_na(GENDER)

Gen_Dec_StringVis <- chisq.test(POND_StringVis_FullGen$Decision, POND_StringVis_FullGen$GENDER)

Diagnosis_Dec_StringVis <- chisq.test(POND_StringVis$clin_diagnosis, POND_StringVis$Decision)


#1.4. Stringent Visual + Metric QC
POND_StringVisMet_FullGen <- POND_StringVisMet %>% drop_na(GENDER)

Gen_Dec_StringVisMet <- chisq.test(POND_StringVisMet_FullGen$Decision, POND_StringVisMet_FullGen$GENDER)

Diagnosis_Dec_StringVisMet <- chisq.test(POND_StringVisMet$clin_diagnosis, POND_StringVisMetn$Decision)


#1.5. Automated QC
POND_Auto_FullGen <- POND_Auto %>% drop_na(GENDER)

Gen_Dec_Auto <- chisq.test(POND_Auto_FullGen$Decision, POND_Auto_FullGen$GENDER)



#2. t-tests to examine differences in supplementary psychopathology constructs across included and excluded groups for each QC approach


#2.1. standard visual QC

POND.StandVis.TTest_Supp <- function(POND_StandVis) {
  Sample.Data <- POND_StandVis[, c('CB68EPTOT', 'CB68IPTOT', 'CB68TPTOT', 'CB684TOT',  'Decision')]
  t.test(Sample.Data[,1] ~ Decision, data = Sample.Data, na.action = na.omit, conf.level = 0.95, paired = FALSE)
  Loop <- for (i in 1:length(Sample.Data)) {
    cat("Printing the t.test comparing excluded and base group in", (colnames(Sample.Data)[i]), "\n")
    TT <- t.test(Sample.Data[,i] ~ Decision, data = Sample.Data, na.action = na.omit, conf.level = 0.95, paired = FALSE)
    print(TT)
  }
  return(Loop)
} 


#2.2. standard visual + metric QC

POND.StandVisMet.TTest_Supp <- function(POND_StandVisMet) {
  Sample.Data <- POND_StandVisMet[, c('CB68EPTOT', 'CB68IPTOT', 'CB68TPTOT', 'CB684TOT',  'Decision')]
  t.test(Sample.Data[,1] ~ Decision, data = Sample.Data, na.action = na.omit, conf.level = 0.95, paired = FALSE)
  Loop <- for (i in 1:length(Sample.Data)) {
    cat("Printing the t.test comparing excluded and base group in", (colnames(Sample.Data)[i]), "\n")
    TT <- t.test(Sample.Data[,i] ~ Decision, data = Sample.Data, na.action = na.omit, conf.level = 0.95, paired = FALSE)
    print(TT)
  }
  return(Loop)
} 


#2.3. stringent visual QC

POND.StringVis.TTest_Supp <- function(POND_StringVis) {
  Sample.Data <- POND_StringVis[, c('CB68EPTOT', 'CB68IPTOT', 'CB68TPTOT', 'CB684TOT',  'Decision')]
  t.test(Sample.Data[,1] ~ Decision, data = Sample.Data, na.action = na.omit, conf.level = 0.95, paired = FALSE)
  Loop <- for (i in 1:length(Sample.Data)) {
    cat("Printing the t.test comparing excluded and base group in", (colnames(Sample.Data)[i]), "\n")
    TT <- t.test(Sample.Data[,i] ~ Decision, data = Sample.Data, na.action = na.omit, conf.level = 0.95, paired = FALSE)
    print(TT)
  }
  return(Loop)
} 






