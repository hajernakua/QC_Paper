###this script will perform a series of analyses to address whether the clinical characteristics would differ between included and excluded samples when implementing fMRI QC
### this same script was used for all the datasets. The input was dataset-specific. 

#1. loading in fMRI data
MRIQC_fMRI <- read.csv("/path/to/csv")

##a few POND-specific data cleaning steps 
MRIQC_fMRI$ID <- sub("_s.*", "", MRIQC_fMRI[,1]) 
MRIQC_fMRI <- MRIQC_fMRI[!duplicated(MRIQC_fMRI$ID), ]

MRIQC_fMRI_Clin <- merge(MRIQC_fMRI, AllPOND_Final, by = "ID")

#2. Excluding participants with a mean FD value >0.3mm

FD.Exclusion <- MRIQC_fMRI_Clin$fd_mean > 0.3 
sum(FD.Exclusion)
FDExclusSubjects <- MRIQC_fMRI_Clin[FD.Exclusion,]

QC_fMRI <- MRIQC_fMRI_Clin[ ! MRIQC_fMRI_Clin$ID %in% FDExclusSubjects$ID,]

#2.1. Data cleaning specific for POND. In addition to removing participants with mean FD >0.3mm, there were a few participants with incomplete scans so those participants were also removed
#Specifically, removing participants with TRs < 100 for Trio and <160 for Prisma 

#trio
QC_fMRI_Trio <- QC_fMRI$scanner == "Trio"
QC_fMRI_Trio <- QC_fMRI[QC_fMRI_Trio,]

TR_Exclusion <- QC_fMRI_Trio$size_t < 100
sum(TR_Exclusion)
QC_fMRI_Trio_Ex <- QC_fMRI_Trio[TR_Exclusion,]

#prisma
QC_fMRI_Prisma <- QC_fMRI$scanner == "Prisma"
QC_fMRI_Prisma <- QC_fMRI[QC_fMRI_Prisma,]

TR_Exclusion_prisma <- QC_fMRI_Prisma$size_t < 160
sum(TR_Exclusion_prisma)
QC_fMRI_Prisma_ex <- QC_fMRI_Prisma[TR_Exclusion_prisma,]

QC_fMRI <- QC_fMRI[ ! QC_fMRI$ID %in% QC_fMRI_Trio_Ex$ID,]
QC_fMRI <- QC_fMRI[ ! QC_fMRI$ID %in% QC_fMRI_Prisma_ex$ID,]

QC_fMRI_Exc <- rbind.fill(FDExclusSubjects, QC_fMRI_Prisma_ex, QC_fMRI_Trio_Ex)

#3. Labeling the included and excluded groups
QC_fMRI$Decision <- "Passed fMRI QC"
QC_fMRI_Exc$Decision <- "Failed fMRI QC"
QC_fMRI_threshold <- rbind(QC_fMRI, QC_fMRI_Exc)


#4. Performing the t-tests

QC_fMRI_ttests <- function(QC_fMRI_threshold) {
  Sample.Data <- QC_fMRI_threshold[, c('age_scan', 'CB68TPTOT', 'AB21GCCS', 'IQ', 'Decision')]
  t.test(Sample.Data[,1] ~ Decision, data = Sample.Data, na.action = na.omit, conf.level = 0.95, paired = FALSE)
  Loop <- for (i in 1:length(Sample.Data)) {
    cat("Printing the t.test comparing excluded and base group in", (colnames(Sample.Data)[i]), "\n")
    TT <- t.test(Sample.Data[,i] ~ Decision, data = Sample.Data, na.action = na.omit, conf.level = 0.95, paired = FALSE)
    print(TT)
  }
  return(Loop)
} 

pvalues <- c(7.331e-09, 0.00926, 5.651e-05, 8.596e-05)
p.adjust(pvalues, method="fdr", n=4)


#5. Getting the effect sizes 
cohen.d(QC_fMRI_threshold$age_scan, QC_fMRI_threshold$Decision, na.rm = TRUE, conf.level = 0.95)
cohen.d(QC_fMRI_threshold$AB21GCCS, QC_fMRI_threshold$Decision, na.rm = TRUE, conf.level = 0.95)
cohen.d(QC_fMRI_threshold$CB68VITOT, QC_fMRI_threshold$Decision, na.rm = TRUE, conf.level = 0.95)
cohen.d(QC_fMRI_threshold$CB68TPTOT, QC_fMRI_threshold$Decision, na.rm = TRUE, conf.level = 0.95)
cohen.d(QC_fMRI_threshold$IQ, QC_fMRI_threshold$Decision, na.rm = TRUE, conf.level = 0.95)
cohen.d(QC_fMRI_threshold$CB684TOT, QC_fMRI_threshold$Decision, na.rm = TRUE, conf.level = 0.95)







