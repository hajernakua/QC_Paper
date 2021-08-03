###this script will perform a series of analyses to address the second aim of the paper: evaluate whether excluded participants would feature greater clinical impairment and altered brain metric characteristics compared to included participants, across all QC approaches
### this same script was used for all the datasets. The input was dataset-specific. 

#1. Creating a dataframe with included and excluded participants labelled so that we can carry out the analysis. This will be done for each QC approach.

StandVisual_QC$Decision <- "Standard Visual QC:Passed"
StandVisual_Exc$Decision <- "Standard Visual QC:Failed"
POND_StandVis <- rbind(StandVisual_QC, StandVisual_Exc)

StandVisualMet_QC$Decision <- "Standard Visual + Metric QC: Passed"
StandVisualMet_Exc$Decision <- "Standard Visual + Metric QC: Failed"
POND_StandVisMet <- rbind(StandVisualMet_QC, StandVisualMet_Exc)


StringVisual_QC$Decision <- "Stringent Visual QC: Passed"
StringVisual_Exc$Decision <- "Stringent Visual QC: Failed"
POND_StringVis <- rbind.fill(StringVisual_QC, StringVisual_Exc)


StringVisualMet_QC$Decision <- "Stringent Visual + Metric QC: Passed"
StringVisualMet_Exc$Decision <- "Stringent Visual + Metric QC: Failed"
POND_StringVisMet <- rbind.fill(StringVisualMet_QC, StringVisualMet_Exc)


Automated_QC$Decision <- "Automated QC: Passed"
Automated_Exc$Decision <- "Automated QC: Failed"
POND_Auto <- rbind.fill(Automated_QC, Automated_Exc)


#2. Performing the t-tests which will indicate whether there are significant differences in clinical characteristics between included and excluded samples 

POND.StandVisual.TTest <- function(POND_StandVis) {
  Sample.Data <- POND_StandVis[, c('age_scan', 'CB68TPTOT', 'AB21GCCS', 'IQ', 'Decision')]
  t.test(Sample.Data[,1] ~ Decision, data = Sample.Data, na.action = na.omit, conf.level = 0.95, paired = FALSE)
  Loop <- for (i in 1:length(Sample.Data)) {
    cat("Printing the t.test comparing excluded and base group in", (colnames(Sample.Data)[i]), "\n")
    TT <- t.test(Sample.Data[,i] ~ Decision, data = Sample.Data, na.action = na.omit, conf.level = 0.95, paired = FALSE)
    print(TT)
  }
  return(Loop)
} 


#correcting for multiple comparisons
pvalues <- c(8.325e-05, 0.5953, 2.127e-05, 7.465e-06)
p.adjust(pvalues, method="fdr", n=4)


POND.StandVisMet.TTest <- function(POND_StandVisMet) {
  Sample.Data <- POND_StandVisMet[, c('age_scan', 'CB68TPTOT', 'AB21GCCS', 'IQ', 'Decision')]
  t.test(Sample.Data[,1] ~ Decision, data = Sample.Data, na.action = na.omit, conf.level = 0.95, paired = FALSE)
  Loop <- for (i in 1:length(Sample.Data)) {
    cat("Printing the t.test comparing excluded and base group in", (colnames(Sample.Data)[i]), "\n")
    TT <- t.test(Sample.Data[,i] ~ Decision, data = Sample.Data, na.action = na.omit, conf.level = 0.95, paired = FALSE)
    print(TT)
  }
  return(Loop)
}

pvalues <- c(0.000175, 0.6206, 8.295e-05, 6.367e-06)
p.adjust(pvalues, method="fdr", n=4)



POND.StringVis.TTest <- function(POND_StringVis) {
  Sample.Data <- POND_StringVis[, c('age_scan', 'CB68TPTOT', 'AB21GCCS', 'IQ','Decision')]
  t.test(Sample.Data[,1] ~ Decision, data = Sample.Data, na.action = na.omit, conf.level = 0.95, paired = FALSE)
  Loop <- for (i in 1:length(Sample.Data)) {
    cat("Printing the t.test comparing excluded and base group in", (colnames(Sample.Data)[i]), "\n")
    TT <- t.test(Sample.Data[,i] ~ Decision, data = Sample.Data, na.action = na.omit, conf.level = 0.95, paired = FALSE)
    print(TT)
  }
  return(Loop)
}

pvalues <- c(5.628e-06, 0.7741, 0.0008345, 1.524e-06)
p.adjust(pvalues, method="fdr", n=4)




POND.StringVisMet.TTest <- function(POND_StringVisMet) {
  Sample.Data <- POND_StringVisMet[, c('age_scan', 'CB68TPTOT', 'AB21GCCS', 'IQ','Decision')]
  t.test(Sample.Data[,1] ~ Decision, data = Sample.Data, na.action = na.omit, conf.level = 0.95, paired = FALSE)
  Loop <- for (i in 1:length(Sample.Data)) {
    cat("Printing the t.test comparing excluded and base group in", (colnames(Sample.Data)[i]), "\n")
    TT <- t.test(Sample.Data[,i] ~ Decision, data = Sample.Data, na.action = na.omit, conf.level = 0.95, paired = FALSE)
    print(TT)
  }
  return(Loop)
}

pvalues <- c(9.479e-06, 0.8016, 0.001146, 9.654e-07)
p.adjust(pvalues, method="fdr", n=4)




POND.Auto.TTest <- function(POND_Auto) {
  Sample.Data <- POND_Auto[, c('age_scan', 'CB68TPTOT', 'AB21GCCS', 'IQ', 'Decision')]
  t.test(Sample.Data[,1] ~ Decision, data = Sample.Data, na.action = na.omit, conf.level = 0.95, paired = FALSE)
  Loop <- for (i in 1:length(Sample.Data)) {
    cat("Printing the t.test comparing excluded and base group in", (colnames(Sample.Data)[i]), "\n")
    TT <- t.test(Sample.Data[,i] ~ Decision, data = Sample.Data, na.action = na.omit, conf.level = 0.95, paired = FALSE)
    print(TT)
  }
  return(Loop)
}

pvalues <- c(0.1342, 0.7416, 0.00432, 0.0003657)
p.adjust(pvalues, method="fdr", n=4)



#3. getting the effect size the examined t-tests

#3.1. Standard visual tests
cohen.d(POND_StandVis$age_scan, POND_StandVis$Decision, na.rm = TRUE, conf.level = 0.95)
cohen.d(POND_StandVis$AB21GCCS, POND_StandVis$Decision, na.rm = TRUE, conf.level = 0.95)
cohen.d(POND_StandVis$IQ, POND_StandVis$Decision, na.rm = TRUE, conf.level = 0.95)
cohen.d(POND_StandVis$CB68TPTOT, POND_StandVis$Decision, na.rm = TRUE, conf.level = 0.95)

#3.2. Standard visual + metric tests
cohen.d(POND_StandVisMet$age_scan, POND_StandVisMet$Decision, na.rm = TRUE, conf.level = 0.95)
cohen.d(POND_StandVisMet$AB21GCCS, POND_StandVisMet$Decision, na.rm = TRUE, conf.level = 0.95)
cohen.d(POND_StandVisMet$IQ, POND_StandVisMet$Decision, na.rm = TRUE, conf.level = 0.95)
cohen.d(POND_StandVisMet$CB68TPTOT, POND_StandVisMet$Decision, na.rm = TRUE, conf.level = 0.95)

#3.3. Stringent visual tests
cohen.d(POND_StringVis$age_scan, POND_StringVis$Decision, na.rm = TRUE, conf.level = 0.95)
cohen.d(POND_StringVis$AB21GCCS, POND_StringVis$Decision, na.rm = TRUE, conf.level = 0.95)
cohen.d(POND_StringVis$IQ, POND_StringVis$Decision, na.rm = TRUE, conf.level = 0.95)
cohen.d(POND_StringVis$CB68TPTOT, POND_StringVis$Decision, na.rm = TRUE, conf.level = 0.95)


#3.4. Stringent visual + metric tests
cohen.d(POND_StringVisMet$age_scan, POND_StringVisMet$Decision, na.rm = TRUE, conf.level = 0.95)
cohen.d(POND_StringVisMet$AB21GCCS, POND_StringVisMet$Decision, na.rm = TRUE, conf.level = 0.95)
cohen.d(POND_StringVisMet$IQ, POND_StringVisMet$Decision, na.rm = TRUE, conf.level = 0.95)
cohen.d(POND_StringVisMet$CB68TPTOT, POND_StringVisMet$Decision, na.rm = TRUE, conf.level = 0.95)


#3.5. Automated tests
cohen.d(POND_Auto$age_scan, POND_Auto$Decision, na.rm = TRUE, conf.level = 0.95)
cohen.d(POND_Auto$AB21GCCS, POND_Auto$Decision, na.rm = TRUE, conf.level = 0.95)
cohen.d(POND_Auto$IQ, POND_Auto$Decision, na.rm = TRUE, conf.level = 0.95)
cohen.d(POND_Auto$CB68TPTOT, POND_Auto$Decision, na.rm = TRUE, conf.level = 0.95)

