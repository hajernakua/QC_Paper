###this script will perform the commands necessary to assess the main effect of QC approach on clinical metrics
###this script was used across all datasets with the input variables different (the example shared is from the POND dataset)


#1. Creating a data frame which has all 5 included groups labelled 
LenientVisual_AllPOND$QCApproach <- "Lenient Visual QC:Passed"
StandardCohort_AllPOND$QCApproach <- "Lenient Visual + Metric QC: Passed"
StringentVisual_AllPOND$QCApproach <- "Stringent Visual QC: Passed"
StringentCohort_AllPOND$QCApproach <- "Stringent Visual + Metric QC: Passed"
AutomatedCohort_AllPOND$QCApproach <- "Automated: Passed"

ALLPOND_INC <- rbind.fill(LenientVisual_AllPOND, StandardCohort_AllPOND, StringentVisual_AllPOND, StringentCohort_AllPOND, AutomatedCohort_AllPOND)


#2. performing the ANOVA to determine whether there is a main effect of QC approach on the clinical metrics examined in this study 

POND.IncGroups_ANOVA <- function(ALLPOND_INC) {
  library(car)
  Sample.Data <- ALLPOND_INC[, c('age_scan', 'CB68EPTOT', 'CB68IPTOT', 'CB68TPTOT', 'AB21GCCS', 'IQ', 'CB684TOT', 'QCApproach')]
  aov(Sample.Data[,1] ~ QCApproach, data = Sample.Data, na.action = na.omit)
  Loop <- for (i in 1:length(Sample.Data)) {
    cat("Printing the ANOVA comparing excluded groups based on", (colnames(Sample.Data)[i]), "\n")
    ANOVA <- aov(Sample.Data[,i] ~ QCApproach, data = Sample.Data, na.action = na.omit)
    print(summary(ANOVA))
  }
  return(Loop)
}


















