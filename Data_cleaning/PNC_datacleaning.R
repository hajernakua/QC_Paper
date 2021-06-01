
###This script will aggregate & clean all the different files needed for the analyses in the PNC dataset


##Step 0: loading necessary modules
library(ggplot2)
library(plotly)
library(sparklyr)
library(magrittr)
library(lubridate)
library(dplyr)
library(tidyverse)
library(plyr)


#Step 1: Loading and organizing required data files

##visual QC data
PNC_QC <- read.csv("path/to/csv")
PNC_QC$ID <- gsub('Sub-', '', PNC_QC$ID)
PNC_QC <- PNC_QC[!duplicated(PNC_QC$ID), ]
PNC_QC <- PNC_QC[!grepl("Error loading", PNC_QC$Rating),] #there were some people who had persistent processing errors and during the QC, the rater noted "Error Loading". These participants will be filtered out in this step 

###MRIQC data for the first batch of PNC participants (had to run it in 2 batches because there were too many participants for one run)
PNC_MRIQC1 <- read.csv("path/to/csv")
PNC_MRIQC1$ID <- sub("_T1w", "", PNC_MRIQC1[,1]) 
PNC_MRIQC1$ID <- gsub('sub-', '', PNC_MRIQC1$ID)

PNC_MRIQC2 <- read.csv("path/to/csv")
PNC_MRIQC2$ID <- sub("_T1w", "", PNC_MRIQC2[,1]) 
PNC_MRIQC2$ID <- gsub('sub-', '', PNC_MRIQC2$ID)

#merging/binding the MRIQC files
PNC_MRIQC <- rbind(PNC_MRIQC1, PNC_MRIQC2)

##making sure to remove any duplicates because some people were run on MRIQC twice 
PNC_MRIQC <- PNC_MRIQC[!duplicated(PNC_MRIQC$ID), ]

#merging the visual QC and MRIQC files
PNC_Data <- merge(PNC_QC, PNC_MRIQC, by = "ID")

##loading the automated QC data 
PNC_RFC_MRIQC1 <- read.csv("path/to/csv")
PNC_RFC_MRIQC2 <- read.csv("path/to/csv")

PNC_RFC_MRIQC <- rbind(PNC_RFC_MRIQC1, PNC_RFC_MRIQC2)
names(PNC_RFC_MRIQC)[names(PNC_RFC_MRIQC) == "subject_id"] <- "ID"

#merging/binding the automated + visual + MRIQC data
PNC_Data <- merge(PNC_Data, PNC_RFC_MRIQC, by = "ID")
PNC_Data <- PNC_Data[!duplicated(PNC_Data$ID), ]


#Step 2: Preparing the spreadsheets of clinical scores
CGAS_PNC <- read.csv("path/to/csv")
CGAS_PNC$ID <- gsub('sub-', '', CGAS_PNC$ID)
CGAS_PNC <- CGAS_PNC[!duplicated(CGAS_PNC$ID), ]

##making sure this column doesn't have spaces. If it does, then not all subjects may merge 
CGAS_PNC$ID <- trimws(CGAS_PNC$ID, which = c("both"))

Psychopathology_PNC <- read.csv("path/to/csv")
names(Psychopathology_PNC)[names(Psychopathology_PNC) == "SUBJID"] <- "ID"
Psychopathology_PNC$ID <- gsub('sub-', '', Psychopathology_PNC$ID)
Psychopathology_PNC <- Psychopathology_PNC[!duplicated(Psychopathology_PNC$ID), ]
Psychopathology_PNC$ID <- trimws(Psychopathology_PNC$ID, which = c("both"))

Demographics_PNC <- read.csv("path/to/csv")
Demographics_PNC$ID <- gsub('sub-', '', Demographics_PNC$ID)
Demographics_PNC <- Demographics_PNC[!duplicated(Demographics_PNC$ID), ]
Demographics_PNC$ID <- trimws(Demographics_PNC$ID, which = c("both"))

PNC_Clinical <- merge(Demographics_PNC, Psychopathology_PNC, by = "ID")
PNC_Clinical <- merge(PNC_Clinical, CGAS_PNC, by = "ID")
PNC_Clinical <- PNC_Clinical[!duplicated(PNC_Clinical$ID), ]

##merging clinical/demographic data with the QC PNC data
PNC_Data <- merge(PNC_Data, PNC_Clinical, by = "ID")
PNC_Data <- PNC_Data[!duplicated(PNC_Data$ID), ]

#Step 3: Creating a variable of participants who were excluded from the metric QC using IQMs from MRIQC
#CJV
summary(PNC_Data$cjv) #This provides us with a summary report on the sample's CJV
PNC_Data$cjv_std <- scale(PNC_Data$cjv) #This provides us with their STD so that we can see where they fit in a Gaussian distribution
bad_cjv_PNC <- PNC_Data[which(PNC_Data$cjv_std > 2),] #Anyone with STD of 2 or greater will be labelled to have a bad CJV

#CNR
summary(PNC_Data$cnr)
PNC_Data$cnr_std <- scale(PNC_Data$cnr) 
bad_cnr_PNC <- PNC_Data[which(PNC_Data$cnr_std < -2),] 

#SNR
summary(PNC_Data$snr_gm) 
PNC_Data$snr_gm_std <- scale(PNC_Data$snr_gm)
bad_snr_gm_PNC <- PNC_Data[which(PNC_Data$snr_gm_std < -2),] 

##aggregating all participants excluded from MRIQC's IQMs
MRIQC_exc_PNC <- rbind.fill(bad_cjv_PNC, bad_cnr_PNC, bad_snr_gm_PNC)



##Step 4:Creating a variable of participants who were excluded from the visual QC

#Standard Visual QC 
StandV.QC1.PNC <- PNC_Data$Rating == "Moderate/severe (4)" 
StandV.QC2.PNC <- PNC_Data$Rating == "Severe (5)"
ExcludedStandVisQC.PNC <- PNC_Data[StandV.QC1.PNC,]
ExcludedStandVisQC2.PNC <- PNC_Data[StandV.QC2.PNC,]
StandVis.ExcludedData.PNC <- data.frame(ExcludedStandVisQC.PNC) 
StandVis.ExcludedData2.PNC <- data.frame(ExcludedStandVisQC2.PNC)
StandVis.ExcludedData2.PNC <- rbind(ExcludedStandVisQC.PNC, ExcludedStandVisQC2.PNC) 

#Stringent visual QC
StringentV.QC1.PNC <- PNC_Data$Rating == "Moderate (3)" 
ExcludedStrinVisQC.PNC <- PNC_Data[StringentV.QC1.PNC,]
StrinVis.ExcludedData.PNC <- data.frame(ExcludedStrinVisQC.PNC)
StrinVis.ExcludedData.PNC <- rbind.fill(StrinVis.ExcludedData.PNC, LenVis.ExcludedData2.PNC)
StrinVis.ExcludedData.PNC <- StrinVis.ExcludedData.PNC[!duplicated(StrinVis.ExcludedData.PNC$ID), ]

#Standard Visual + Metric QC
StandVA.QC_PNC <- rbind.fill(MRIQC_exc_PNC, StandVis.ExcludedData2.PNC)
StandVA.QC_PNC <- StandVA.QC_PNC[!duplicated(StandVA.QC_PNC$ID), ]

#Stringent Visual + Metric QC
StringentVA.QC_PNC <- rbind.fill(MRIQC_exc_PNC, StrinVis.ExcludedData.PNC)
StringentVA.QC_PNC <- StringentVA.QC_PNC[!duplicated(StringentVA.QC_PNC$ID), ]

#Automated QC: Random Forest Classifier
RFC_Excluded_PNC <- PNC_Data$pred_y == "1"
ExcludedRFCQC_PNC <- PNC_Data[RFC_Excluded_PNC,]




#Step 5: Creating a variable of participants who were included from each of these QC approaches
PNC_StandVis <- PNC_Data[!PNC_Data$ID %in% StandVis.ExcludedData2.PNC$ID,]
PNC_StandVisMet <- PNC_Data[!PNC_Data$ID %in% StandVA.QC_PNC$ID,]
PNC_StringVis <- PNC_Data[!PNC_Data$ID %in% StrinVis.ExcludedData.PNC$ID,]
PNC_StringVisMet <- PNC_Data[!PNC_Data$ID %in% StringentVA.QC_PNC$ID,]
MRIQC.Cohort.PNC <- PNC_Data[!PNC_Data$ID %in% ExcludedRFCQC_PNC$ID,]



