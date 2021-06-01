###This script will aggregate & clean all the different files needed for the analyses in the HBN dataset


##loading necessary modules
library(ggplot2)
library(plotly)
library(sparklyr)
library(magrittr)
library(lubridate)
library(dplyr)
library(tidyverse)
library(plyr)



#Step 1: Loading + aggregating the necessary spreadsheets. The HBN dataset had 3 different sites so this script aggregates them together 
#Visual QC Spreadsheets
RU_HBN <- read.csv("/path/to/csv")
names(RU_HBN)[names(RU_HBN) == "SUBJECT"] <- "ID"
RU_HBN$ID <- as.character(RU_HBN$ID)

##want to make sure there are no spaces in the ID column
RU_HBN$ID <- trimws(RU_HBN$ID, which = c("both"))

CBIC_HBN <- read.csv("/path/to/csv")
names(CBIC_HBN)[names(CBIC_HBN) == "SUBJECT.ID"] <- "ID"
CBIC_HBN$ID <- as.character(CBIC_HBN$ID)
CBIC_HBN$ID <- trimws(CBIC_HBN$ID, which = c("both"))

SI_HBN <- read.csv(/path/to/csv")
SI_HBN$ID <- paste0('sub-', SI_HBN$ID)
SI_HBN$ID <- trimws(SI_HBN$ID, which = c("both"))

##making variable for site 
RU_HBN$Site <- "RU"
CBIC_HBN$Site <- "CBIC"
SI_HBN$Site <- "SI"

#Binding all spreadsheets 
HBN_VisualQC <- rbind.fill(RU_HBN, CBIC_HBN, SI_HBN)
HBN_VisualQC <- HBN_VisualQC[!duplicated(HBN_VisualQC$ID), ]


#Step 1B: loading in data from MRIQC Random Forest Classifier
MRIQC_RFC_HBN <- read.csv("/path/to/csv")
names(MRIQC_RFC_HBN)[names(MRIQC_RFC_HBN) == "subject_id"] <- "ID"
MRIQC_RFC_HBN$ID <- ifelse(grepl("N",MRIQC_RFC_HBN$ID), paste("sub-", MRIQC_RFC_HBN$ID, sep=""))
MRIQC_RFC_HBN$ID <- sub("_a.*", "", MRIQC_RFC_HBN[,1]) 

subs_im$subjectkey <- ifelse(grepl("sub-",subs_im$subjectkey), paste("", subs_im$subjectkey))

subs_im[,1] <- sub("sub-", "", subs_im[,1]) 
subs_im$subjectkey <- trimws(subs_im$subjectkey, which = c("both"))
subs_im$subjectkey <- gsub("NDAR","NDAR_",subs_im$subjectkey)
subs_im$subjectkey <- gsub("NDAR", "NDAR_", MRIQC_RFC_HBN[,1]) 

#Step 2: Getting CNR, CJV and SNR values from MRIQC
#Loading the MRIQC spreadsheet with the IQM info
MRIQC_IQMs_HBN <- read.csv("/path/to/csv")
names(MRIQC_IQMs_HBN)[names(MRIQC_IQMs_HBN) == "bids_name"] <- "ID"
MRIQC_IQMs_HBN$ID <- sub("_a.*", "", MRIQC_IQMs_HBN[,1]) #this removes anything after the _ which was present in the original spreadsheet
MRIQC_IQMs_HBN$ID <- trimws(MRIQC_IQMs_HBN$ID, which = c("both"))

##adding the MRIQC data from the SI site
MRIQC_IQM_SI <- read.csv("/path/to/csv")
MRIQC_IQM_SI$ID <- sub("_T1w", "", MRIQC_IQM_SI[,1])

#binding it with the main MRIQC dataframe
MRIQC_IQMs_HBN <- rbind.fill(MRIQC_IQM_SI, MRIQC_IQMs_HBN)

##removing the duplicates 
MRIQC_IQMs_HBN <- MRIQC_IQMs_HBN[!duplicated(MRIQC_IQMs_HBN$ID), ]


#Step 3: Merging Visual + Automated + Metric (IQMs) QC files
HBN_QC <- merge(HBN_VisualQC, MRIQC_IQMs_HBN, by = "ID") 
HBN_QC <- merge(MRIQC_RFC_HBN, HBN_QC, by = "ID") 
HBN_QC <- HBN_QC[!duplicated(HBN_QC$ID), ]

#Step 4: loading all the clinical spreadsheets
CBCL.HBN <- read.csv("/path/to/csv")
CBCL.HBN$ID <- paste0('sub-', CBCL.HBN$ID)
CBCL.HBN$ID <- trimws(CBCL.HBN$ID, which = c("both"))

Demographics.HBN <- read.csv("/path/to/csv")
Demographics.HBN$ID <- paste0('sub-', Demographics.HBN$ID)
Demographics.HBN$ID <- trimws(Demographics.HBN$ID, which = c("both"))

CGAS.HBN <- read.csv("/path/to/csv")
CGAS.HBN$ID <- paste0('sub-', CGAS.HBN$ID)
CGAS.HBN$ID <- sub(",a.*", "", CGAS.HBN[,1])
CGAS.HBN$ID <- trimws(CGAS.HBN$ID, which = c("both"))

IQ.HBN <- read.csv("/path/to/csv")
IQ.HBN$ID <- paste0('sub-', IQ.HBN$ID)
IQ.HBN$ID <- sub(",a.*", "",IQ.HBN[,1])
IQ.HBN$ID <- trimws(IQ.HBN$ID, which = c("both"))

##merging all the data to create one large clinical data spreadsheet 
ClinicalData.HBN <- merge(CBCL.HBN, Demographics.HBN, by = "ID")
ClinicalData.HBN <- merge(ClinicalData.HBN, IQ.HBN, by = "ID")
ClinicalData.HBN <- merge(ClinicalData.HBN, CGAS.HBN, by = "ID")

###merging clinical data with QC data 
HBN_QC <- merge(HBN_QC, ClinicalData.HBN, by = "ID") 
HBN_QC_NoDUPS <- HBN_QC[!duplicated(HBN_QC$ID), ]

###Step 5: Creating a variable of participants who were excluded from the metric QC using IQMs from MRIQC 
#CJV
summary(HBN_QC$cjv) #This provides us with a summary report on the sample's CJV
HBN_QC$cjv_std <- scale(HBN_QC$cjv) #This provides us with their STD so that we can see where they fit in a Gaussian distribution
bad_cjv_HBN <- HBN_QC[which(HBN_QC$cjv_std > 2),] #Anyone with STD of 2 or greater will be labelled to have a bad CJV

#CNR
summary(HBN_QC$cnr) 
HBN_QC$cnr_std <- scale(HBN_QC$cnr)
bad_cnr_HBN <- HBN_QC[which(HBN_QC$cnr_std < -2),] 

#SNR
summary(HBN_QC$snr_gm) 
HBN_QC$snr_gm_std <- scale(HBN_QC$snr_gm) 
bad_snr_gm_HBN <- HBN_QC[which(HBN_QC$snr_gm_std < -2),] 

##binding all the excluded participants from the IQMs
MRIQC_exc_HBN <- rbind.fill(bad_cjv_HBN, bad_cnr_HBN, bad_snr_gm_HBN)

###Step 6: Creating a variable of participants who were excluded from the visual QC

#Standard Visual QC  
StandV.QC1 <- HBN_QC$RATING == "Moderate/severe (4)" 
StandV.QC2 <- HBN_QC$RATING == "Severe (5)"
ExcludedStandVisQC <- HBN_QC[StandV.QC1,]
ExcludedStandVisQC2 <- HBN_QC[StandV.QC2,]
StandVis.ExcludedData_HBN <- data.frame(ExcludedStandVisQC) 
StandVis.ExcludedData2_HBN <- data.frame(ExcludedStandVisQC2)
StandVis.ExcludedData2_HBN <- rbind(StandVis.ExcludedData_HBN, StandVis.ExcludedData2_HBN) 
StandVis.ExcludedData2_HBN <- StandVis.ExcludedData2_HBN[!duplicated(StandVis.ExcludedData2_HBN$ID), ]


#Stringent Visual QC 
StringentV.QC1 <- HBN_QC$RATING == "Moderate (3)" 
ExcludedStrinVisQC <- HBN_QC[StringentV.QC1,]
StrinVis.ExcludedData1_HBN <- data.frame(ExcludedStrinVisQC)
StrinVis.ExcludedData2_HBN <- rbind.fill(StrinVis.ExcludedData1_HBN, StandVis.ExcludedData2_HBN)
StrinVis.ExcludedData2_HBN <- StrinVis.ExcludedData2_HBN[!duplicated(StrinVis.ExcludedData2_HBN$ID), ]

#Standard visual + metric QC
StandVA.QC_HBN <- rbind.fill(MRIQC_exc_HBN, StandVis.ExcludedData2_HBN)
StandVA.QC_HBN <- StandVA.QC_HBN[!duplicated(StandVA.QC_HBN$ID), ]

#Stringent visual + metric QC
StringentVA.QC <- rbind.fill(MRIQC_exc_HBN, StrinVis.ExcludedData2_HBN)
StringentVA.QC <- StringentVA.QC[!duplicated(StringentVA.QC$ID), ]

#Automated QC: Random Forest Classifier
RFC_Excluded_HBN <- HBN_QC$pred_y == "1"
ExcludedRFCQC_HBN <- HBN_QC[RFC_Excluded_HBN,]


#Step 7: Creating a variable of participants who were included from each of these QC approaches
StandVisMet.HBN <- HBN_QC[!HBN_QC$ID %in% StandVA.QC_HBN$ID,]
StandVis.HBN <- HBN_QC[!HBN_QC$ID %in% StandVis.ExcludedData2_HBN$ID,]
StringVisMet.HBN <- HBN_QC[!HBN_QC$ID %in% StringentVA.QC$ID,]
StringVis.HBN <- HBN_QC[!HBN_QC$ID %in% StrinVis.ExcludedData2_HBN$ID,]
Auto.HBN <- HBN_QC[!HBN_QC$ID %in% ExcludedRFCQC_HBN$ID,]


