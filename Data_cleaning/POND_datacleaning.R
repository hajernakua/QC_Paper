###This script will aggregate & clean all the different files needed for the analyses in the POND dataset


#Step 0: Loading necessary library packages
library(ggplot2)
library(plotly)
library(sparklyr)
library(magrittr)
library(lubridate)
library(dplyr)
library(tidyverse)
library(plyr)


#Step 1: There were two data downloads in the POND dataset, so the first step is to load in all the dataframes and aggregate the files
#DATA DOWNLOAD ONE - JUNE 2018 DOWNLOAD
Original_ClinicalData <- read.csv("/path/to/csv")
names(Original_ClinicalData)[names(Original_ClinicalData) == "SUBJECT"] <- "ID"
Original_ClinicalData$ID <- ifelse(grepl("^88",Original_ClinicalData$ID), paste("sub-0", Original_ClinicalData$ID, sep=""), paste("sub-", Original_ClinicalData$ID, sep=""))


#DATA DOWNLOAD TWO - JANUARY 2020 DOWNLOAD
New_ClinicalData <- read.csv("/path/to/csv")
names(New_ClinicalData)[names(New_ClinicalData) == "SUB_ID"] <- "ID"
New_ClinicalData$ID <- ifelse(grepl("^88",New_ClinicalData$ID), paste("sub-0", New_ClinicalData$ID, sep=""), paste("sub-", New_ClinicalData$ID, sep=""))
New_ClinicalData <- New_ClinicalData[!duplicated(New_ClinicalData$ID), ]

###the new data download had separated spreadsheets so I had to put them all together
CBCL_Sept2020 <- read.csv("/path/to/csv")
names(CBCL_Sept2020)[names(CBCL_Sept2020) == "Subject"] <- "ID"
CBCL_Sept2020$ID <- ifelse(grepl("^88",CBCL_Sept2020$ID), paste("sub-0", CBCL_Sept2020$ID, sep=""), paste("sub-", CBCL_Sept2020$ID, sep=""))

##extracting baseline (as opposed to some longitudinal) CBCL scores
CBCL_Sept2020_Baseline <- CBCL_Sept2020$FolderName == "Baseline CBCL (Ages 6 - 18 years)"
CBCL_Sept2020_Baseline <- CBCL_Sept2020[CBCL_Sept2020_Baseline,]

##making sure to remove accidental duplictates + drop any row with NA values
CBCL_Sept2020_Baseline <- CBCL_Sept2020_Baseline[!duplicated(CBCL_Sept2020_Baseline$ID), ]
CBCL_Sept2020_Baseline_NoNA <- CBCL_Sept2020_Baseline %>% drop_na(CB68IPTOT)

##some of the POND participants were younger and were administered the CBCL from ages 0-5
CBCL_0to5 <- read.csv("/path/to/csv")
names(CBCL_0to5)[names(CBCL_0to5) == "Subject"] <- "ID"
CBCL_0to5$ID <- ifelse(grepl("^88",CBCL_0to5$ID), paste("sub-0", CBCL_0to5$ID, sep=""), paste("sub-", CBCL_0to5$ID, sep=""))

##making sure to remove accidental duplictates + drop any row with NA values
CBCL_0to5 <- CBCL_0to5[!duplicated(CBCL_0to5$ID), ]
CBCL_0to5_NoNA <- CBCL_0to5 %>% drop_na(CBIPTOT)

##the column names for the 0-5 CBCL were different than the 6-18, so I just changed the column names so each psychopathology trait be in one column 
names(CBCL_0to5_NoNA)[names(CBCL_0to5_NoNA) == "CBEPTOT"] <- "CB68EPTOT"
names(CBCL_0to5_NoNA)[names(CBCL_0to5_NoNA) == "CBIPTOT"] <- "CB68IPTOT"
names(CBCL_0to5_NoNA)[names(CBCL_0to5_NoNA) == "CBTPTOT"] <- "CB68TPTOT"
names(CBCL_0to5_NoNA)[names(CBCL_0to5_NoNA) == "CBVITOT"] <- "CB68VITOT"
names(CBCL_0to5_NoNA)[names(CBCL_0to5_NoNA) == "CBVIITOT"] <- "CB68VIITOT"
names(CBCL_0to5_NoNA)[names(CBCL_0to5_NoNA) == "CBVIIITOT"] <- "CB68VIIITOT"
names(CBCL_0to5_NoNA)[names(CBCL_0to5_NoNA) == "CB4TOT"] <- "CB684TOT"
names(CBCL_0to5_NoNA)[names(CBCL_0to5_NoNA) == "CB5TOT"] <- "CB685TOT"
names(CBCL_0to5_NoNA)[names(CBCL_0to5_NoNA) == "CB6TOT"] <- "CB686TOT"
names(CBCL_0to5_NoNA)[names(CBCL_0to5_NoNA) == "CB1TOT"] <- "CB681TOT"
names(CBCL_0to5_NoNA)[names(CBCL_0to5_NoNA) == "CB2TOT"] <- "CB682TOT"

###rbinding the CBCL scores from the 0-5 and 6-18 data frames
Total_CBCL <- rbind.fill(CBCL_Sept2020_Baseline_NoNA, CBCL_0to5_NoNA)
Total_CBCL_NoDups <- Total_CBCL[!duplicated(Total_CBCL$ID), ]
Total_CBCL_NoDups_NoNA <- Total_CBCL_NoDups %>% drop_na(CB68EPTOT)

###merging the 2020 download clinical data + the CBCL aggregated data
New_fullClinicalData <- merge(New_ClinicalData, Total_CBCL_NoDups, by = "ID")
New_fullClinicalData <- New_fullClinicalData[!duplicated(New_fullClinicalData$ID), ]


#Step 1B: Reading the visual T1 QC file
OriginalPOND_QC <- read.csv("/path/to/csv")
NewPOND_QC <- read.csv("/path/to/csv")
names(NewPOND_QC)[names(NewPOND_QC) == "ï..ID"] <- "ID"
NewPOND_QC <- NewPOND_QC[!duplicated(NewPOND_QC$ID), ]

##merging QC files together 
AllPOND_QC <- rbind.fill(OriginalPOND_QC, NewPOND_QC)
AllPOND_Clin <- merge(AllPOND_QC, New_fullClinicalData)

##removing participants with missing ABAS (one of the clinical measures used) data
AllPOND_Clin_NoNA_ABAS <- AllPOND_Clin %>% drop_na(AB21GCCS)


#Step 1C: Reading the MRIQC report
MRIQCReport_Correct <- read.csv("/path/to/csv") #this version checks the runs and includes the run with the best data
MRIQCReport_Correct$ID <- sub("_s.*", "", MRIQCReport_Correct[,1]) 
MRIQCReport_Correct <- MRIQCReport_Correct[!duplicated(MRIQCReport_Correct$ID), ]


#Step 1D: Merging the clinical data frame and visual QC 
AllPOND_Clin_NoNA_ABAS_MRIQC <- merge(AllPOND_Clin_NoNA_ABAS, MRIQCReport_Correct, by = "ID") 

##Step 1E: Reading the automated QC data
##original pond
RFC <- read.csv("/path/to/csv")
names(RFC)[names(RFC) == "subject_id"] <- "ID"
RFC$ID <- ifelse(grepl("^88",RFC$ID), paste("sub-0", RFC$ID, sep=""), paste("sub-", RFC$ID, sep=""))
RFC <- RFC[!duplicated(RFC$ID), ]

##new pond
MRIQC_RFC_NewPOND1 <- read.csv("/path/to/csv")
MRIQC_RFC_NewPOND2 <- read.csv("/path/to/csv")

MRIQC_RFC_NewPOND <- rbind(MRIQC_RFC_NewPOND1, MRIQC_RFC_NewPOND2)
MRIQC_RFC_NewPOND <-MRIQC_RFC_NewPOND[!duplicated(MRIQC_RFC_NewPOND$ID), ]

MRIQC_RFC_NewPOND$ID <- ifelse(grepl("^88",MRIQC_RFC_NewPOND$ID), paste("sub-0", MRIQC_RFC_NewPOND$ID, sep=""), paste("sub-", MRIQC_RFC_NewPOND$ID, sep=""))
MRIQC_RFC_NewPOND$ID <- sub("_s.*", "", MRIQC_RFC_NewPOND[,1])

MRIQC_RFC_NewPOND <- MRIQC_RFC_NewPOND[!duplicated(MRIQC_RFC_NewPOND$ID), ]

##merging automated QC scores/data from both POND downloads
MRIQC_RFC <- rbind(RFC, MRIQC_RFC_NewPOND)

#merging the automated QC data with the clinical, visual + MRIQC data
AllPOND_Clin_NoNA_ABAS_MRIQC_RFC <- merge(AllPOND_Clin_NoNA_ABAS_MRIQC, MRIQC_RFC, by = "ID")

##getting age_scan + scanner type variable which is located in a different data frame 
Scanner <- read.csv("/path/to/csv")
names(Scanner)[names(Scanner) == "bids"] <- "ID"
Scanner$ID <- gsub('/ses-01', '', Scanner$ID)
Scanner$ID <- gsub('/ses-02', '', Scanner$ID)
Scanner$ID <- gsub('/ses-03', '', Scanner$ID)
Scanner$ID <- gsub('/ses-04', '', Scanner$ID)
Scanner$ID <- gsub('/ses-05', '', Scanner$ID)
Scanner$ID <- gsub('/ses-06', '', Scanner$ID)
Scanner <- Scanner[!duplicated(Scanner$ID), ]

AllPOND_Final <- merge(AllPOND_Clin_NoNA_ABAS_MRIQC_RFC, Scanner[, c('ID', 'age_scan', 'scanner')], by = "ID")
AllPOND_Final <- AllPOND_Final[!duplicated(AllPOND_Final$ID), ]

#Step 2A: Converting the numerical diagnosis to a character
AllPOND_Final$clin_diagnosis <- ifelse(AllPOND_Final$RESEARCH_CONFIRM_DIAG_STD == "1", "ASD", NA)
AllPOND_Final$clin_diagnosis <- ifelse(AllPOND_Final$RESEARCH_CONFIRM_DIAG_STD == "2", "ADHD", AllPOND_Final$clin_diagnosis)
AllPOND_Final$clin_diagnosis <- ifelse(AllPOND_Final$RESEARCH_CONFIRM_DIAG_STD == "3", "OCD", AllPOND_Final$clin_diagnosis)
AllPOND_Final$clin_diagnosis <- ifelse(AllPOND_Final$RESEARCH_CONFIRM_DIAG_STD == "6", "ADHD", AllPOND_Final$clin_diagnosis)
AllPOND_Final$clin_diagnosis <- ifelse(AllPOND_Final$RESEARCH_CONFIRM_DIAG_STD == "10", "CTRL", AllPOND_Final$clin_diagnosis)
AllPOND_Final$clin_diagnosis <- ifelse(AllPOND_Final$RESEARCH_CONFIRM_DIAG_STD == "15", "GAD", AllPOND_Final$clin_diagnosis)


##Step 2B: getting an IQ score from the WASI and WISQ  subscales 
AllPOND_Final$IQ <- AllPOND_Final$WASI_FSIQ_4
AllPOND_Final$IQ <- ifelse(is.na(AllPOND_Final$IQ), AllPOND_Final$WASI_II_FSIQ_4,AllPOND_Final$IQ)
AllPOND_Final$IQ <- ifelse(is.na(AllPOND_Final$IQ), AllPOND_Final$WISC_IV_FSIQ, AllPOND_Final$IQ)
AllPOND_Final$IQ <- ifelse(is.na(AllPOND_Final$IQ), AllPOND_Final$WISC_V_FSIQ, AllPOND_Final$IQ)
AllPOND_Final$IQ <- ifelse(is.na(AllPOND_Final$IQ), AllPOND_Final$WASI_II_FSIQ_2, AllPOND_Final$IQ)
AllPOND_Final$IQ <- ifelse(is.na(AllPOND_Final$IQ), AllPOND_Final$SBFULLIQ, AllPOND_Final$IQ)

       
#Step 3: Creating a variable of participants who were excluded from the visual QC

#3A: standard visual QC exclusion
StandVis_Exc <- AllPOND_Final$Decision == "E"
StandVis_Exc <- AllPOND_Final[StandVis_Exc,]
StandVis_ExcData <- data.frame(StandVis_Exc)

#Step 3.B: stringent visual QC exclusion
Doubtful <- AllPOND_Final$Decision == "Doubtful" ##these are participants who scored 3 on the 1-5 QC rating system system 
DoubtfulInfo <- AllPOND_Final[Doubtful,]
DoubtfulData <- data.frame(DoubtfulInfo) #this puts it in a dataframe so you can compare the demographic information

StringVis_ExcData <- rbind(ExcludedData, DoubtfulData)


#Step 4: Creating a variable of participants who were excluded from the metric QC using IQMs from MRIQC

#CJV
summary(AllPOND_Final$cjv) #This provides us with a summary report on the sample's CJV
AllPOND_Final$cjv_std <- scale(AllPOND_Final$cjv) #This provides us with their STD so that we can see where they fit in a Gaussian distribution
bad_cjv_POND <- AllPOND_Final[which(AllPOND_Final$cjv_std > 2),] #Anyone with STD of 2 or greater will be labelled to have a bad CJV + excluded

#CNR
summary(AllPOND_Final$cnr) 
AllPOND_Final$cnr_std <- scale(AllPOND_Final$cnr) 
bad_cnr_POND <- AllPOND_Final[which(AllPOND_Final$cnr_std < -2),] #Anyone with STD of -2 or less will be labelled to have a bad CNR

#SNR 
summary(AllPOND_Final$snr_gm) 
AllPOND_Final$snr_gm_std <- scale(AllPOND_Final$snr_gm) 
bad_snr_gm_POND <- AllPOND_Final[which(AllPOND_Final$snr_gm_std < -2),] #Anyone with STD of -2 or less will be labelled to have a bad SNR

##aggregating all participants excluded from MRIQC's IQMs
BAD_MRIQC <- rbind.fill(bad_cjv_POND, bad_cnr_POND, bad_snr_gm_POND)


#Step 5: Creating a variable of participants who were excluded from the both the visual + metric QC 

#excluded at the standard visual + metric level
Excluded_StandVisMet_POND <- rbind.fill(BAD_MRIQC, ExcludedData)

##excluded at stringent visual + metric level
Excluded_StringVisMet_POND <- rbind.fill(BAD_MRIQC, Excluded_Doubtful)

##removing duplicates because some participants were excluded from both 
Excluded_StandVisMet_POND <- Excluded_StandVisMet_POND[!duplicated(Excluded_StandVisMet_POND$ID), ]
Excluded_StringVisMet_POND <- Excluded_StringVisMet_POND[!duplicated(Excluded_StringVisMet_POND$ID), ]


#Step 5: Creating a variable of participants who were excluded from the automated QC
Excluded_Automated_POND <- AllPOND_Final$pred_y == "1"
Excluded_Automated_POND  <- AllPOND_Final[Excluded_Automated_POND ,]
Excluded_Automated_POND  <- data.frame(Excluded_Automated_POND)

#Step 6: Creating a variable of participants who were included from each of these QC approaches
StandardCohort_AllPOND <- AllPOND_Final[!AllPOND_Final$ID %in% Excluded_StandVisMet_POND$ID,]
StringentCohort_AllPOND <- AllPOND_Final[!AllPOND_Final$ID %in% Excluded_StringVisMet_POND$ID,]
StandardVisual_AllPOND <- AllPOND_Final[!AllPOND_Final$ID %in% StandVis_ExcData$ID,]
StringentVisual_AllPOND <- AllPOND_Final[!AllPOND_Final$ID %in% StringVis_ExcData$ID,]
AutomatedCohort_AllPOND <- AllPOND_Final[!AllPOND_Final$ID %in% Excluded_Automated_POND$ID,]






