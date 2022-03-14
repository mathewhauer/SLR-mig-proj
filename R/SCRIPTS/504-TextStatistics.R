source("./R/SCRIPTS/000-Libraries.R")

projsums<-read_csv("./R/DATA-PROCESSED/PROJECTIONS/projections_TOT_controlled_eae_MSP.csv")
projsums <- projsums2
## Miami
cnty <- "12086"
proj_cnty <- projsums[which(projsums$GEOID == cnty),]

### Inundation
amp_mid <-label_number_si(accuracy=0.1)(sum(abs(proj_cnty$SSP2_BASE[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p50")] - proj_cnty$SSP2_MIG[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p50")] )))
dis_mid <-label_number_si(accuracy=0.1)(sum(abs(proj_cnty$SSP2_BASE[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p50")] - proj_cnty$SSP2_inun[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p50")] )))

amp_low <- label_number_si(accuracy=0.1)(
  sum(abs(proj_cnty$Base_min[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p5")] - 
            proj_cnty$Mig_min[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p5")] )))
amp_hi <- label_number_si(accuracy=0.1)(
  sum(abs(proj_cnty$Base_max[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p95")] - 
            proj_cnty$Mig_max[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p95")] )))

dis_low <- label_number_si(accuracy=0.1)(
  sum(abs(proj_cnty$Base_min[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p5")] - 
            proj_cnty$Inundated_min[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p5")] )))
dis_hi <- label_number_si(accuracy=0.1)(
  sum(abs(proj_cnty$Base_max[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p95")] - 
            proj_cnty$Inundated_max[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p95")] )))

paste0(amp_mid, " [", amp_low, " - ", amp_hi, "]")
paste0(dis_mid, " [", dis_low, " - ", dis_hi, "]")

## Dare
cnty <- "37055"
proj_cnty <- projsums[which(projsums$GEOID == cnty),]

### Inundation
amp_mid <-label_number_si(accuracy=0.1)(sum(abs(proj_cnty$SSP2_BASE[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p50")] - proj_cnty$SSP2_MIG[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p50")] )))
dis_mid <-label_number_si(accuracy=0.1)(sum(abs(proj_cnty$SSP2_BASE[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p50")] - proj_cnty$SSP2_inun[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p50")] )))

amp_low <- label_number_si(accuracy=0.1)(
  sum(abs(proj_cnty$Base_min[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p5")] - 
            proj_cnty$Mig_min[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p5")] )))
amp_hi <- label_number_si(accuracy=0.1)(
  sum(abs(proj_cnty$Base_max[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p95")] - 
            proj_cnty$Mig_max[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p95")] )))

dis_low <- label_number_si(accuracy=0.1)(
  sum(abs(proj_cnty$Base_min[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p5")] - 
            proj_cnty$Inundated_min[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p5")] )))
dis_hi <- label_number_si(accuracy=0.1)(
  sum(abs(proj_cnty$Base_max[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p95")] - 
            proj_cnty$Inundated_max[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p95")] )))

paste0(amp_mid, " [", amp_low, " - ", amp_hi, "]")
paste0(dis_mid, " [", dis_low, " - ", dis_hi, "]")
## Rutherford TN
cnty <- "47149"
proj_cnty <- projsums[which(projsums$GEOID == cnty),]

### Inundation
amp_mid <-label_number_si(accuracy=0.1)(sum(abs(proj_cnty$SSP2_BASE[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p50")] - proj_cnty$SSP2_MIG[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p50")] )))
dis_mid <-label_number_si(accuracy=0.1)(sum(abs(proj_cnty$SSP2_BASE[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p50")] - proj_cnty$SSP2_inun[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p50")] )))

amp_low <- label_number_si(accuracy=0.1)(
  sum(abs(proj_cnty$Base_min[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p5")] - 
            proj_cnty$Mig_min[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p5")] )))
amp_hi <- label_number_si(accuracy=0.1)(
  sum(abs(proj_cnty$Base_max[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p95")] - 
            proj_cnty$Mig_max[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p95")] )))

dis_low <- label_number_si(accuracy=0.1)(
  sum(abs(proj_cnty$Base_min[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p5")] - 
            proj_cnty$Inundated_min[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p5")] )))
dis_hi <- label_number_si(accuracy=0.1)(
  sum(abs(proj_cnty$Base_max[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p95")] - 
            proj_cnty$Inundated_max[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p95")] )))

paste0(amp_mid, " [", amp_low, " - ", amp_hi, "]")
paste0(dis_mid, " [", dis_low, " - ", dis_hi, "]")

## Douglas CO
cnty <- "08035"
proj_cnty <- projsums[which(projsums$GEOID == cnty),]

### Inundation
amp_mid <-label_number_si(accuracy=0.1)(sum(abs(proj_cnty$SSP2_BASE[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p50")] - proj_cnty$SSP2_MIG[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p50")] )))
dis_mid <-label_number_si(accuracy=0.1)(sum(abs(proj_cnty$SSP2_BASE[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p50")] - proj_cnty$SSP2_inun[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p50")] )))

amp_low <- label_number_si(accuracy=0.1)(
  sum(abs(proj_cnty$Base_min[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p5")] - 
            proj_cnty$Mig_min[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p5")] )))
amp_hi <- label_number_si(accuracy=0.1)(
  sum(abs(proj_cnty$Base_max[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p95")] - 
            proj_cnty$Mig_max[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p95")] )))

dis_low <- label_number_si(accuracy=0.1)(
  sum(abs(proj_cnty$Base_min[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p5")] - 
            proj_cnty$Inundated_min[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p5")] )))
dis_hi <- label_number_si(accuracy=0.1)(
  sum(abs(proj_cnty$Base_max[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p95")] - 
            proj_cnty$Inundated_max[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p95")] )))

paste0(amp_mid, " [", amp_low, " - ", amp_hi, "]")
paste0(dis_mid, " [", dis_low, " - ", dis_hi, "]")

## Washington OR
cnty <- "41067"
proj_cnty <- projsums[which(projsums$GEOID == cnty),]

### Inundation
amp_mid <-label_number_si(accuracy=0.1)(sum(abs(proj_cnty$SSP2_BASE[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p50")] - proj_cnty$SSP2_MIG[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p50")] )))
dis_mid <-label_number_si(accuracy=0.1)(sum(abs(proj_cnty$SSP2_BASE[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p50")] - proj_cnty$SSP2_inun[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p50")] )))

amp_low <- label_number_si(accuracy=0.1)(
  sum(abs(proj_cnty$Base_min[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p5")] - 
            proj_cnty$Mig_min[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p5")] )))
amp_hi <- label_number_si(accuracy=0.1)(
  sum(abs(proj_cnty$Base_max[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p95")] - 
            proj_cnty$Mig_max[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p95")] )))

dis_low <- label_number_si(accuracy=0.1)(
  sum(abs(proj_cnty$Base_min[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p5")] - 
            proj_cnty$Inundated_min[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p5")] )))
dis_hi <- label_number_si(accuracy=0.1)(
  sum(abs(proj_cnty$Base_max[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p95")] - 
            proj_cnty$Inundated_max[which(proj_cnty$YEAR==2100 & proj_cnty$prob=="p95")] )))

paste0(amp_mid, " [", amp_low, " - ", amp_hi, "]")
paste0(dis_mid, " [", dis_low, " - ", dis_hi, "]")
## Overall

amp_mid <-label_number_si(accuracy=0.1)(sum(abs(projsums$SSP2_BASE[which(projsums$YEAR==2100 & projsums$prob=="p50")] - projsums$SSP2_MIG[which(projsums$YEAR==2100 & projsums$prob=="p50")] )))
dis_mid <-label_number_si(accuracy=0.1)(sum(abs(projsums$SSP2_BASE[which(projsums$YEAR==2100 & projsums$prob=="p50")] - projsums$SSP2_inun[which(projsums$YEAR==2100 & projsums$prob=="p50")] )))

amp_low <- label_number_si(accuracy=0.1)(
  sum(abs(projsums$Base_min[which(projsums$YEAR==2100 & projsums$prob=="p5")] - 
            projsums$Mig_min[which(projsums$YEAR==2100 & projsums$prob=="p5")] )))
amp_hi <- label_number_si(accuracy=0.1)(
  sum(abs(projsums$Base_max[which(projsums$YEAR==2100 & projsums$prob=="p95")] - 
            projsums$Mig_max[which(projsums$YEAR==2100 & projsums$prob=="p95")] )))

dis_low <- label_number_si(accuracy=0.1)(
  sum(abs(projsums$Base_min[which(projsums$YEAR==2100 & projsums$prob=="p5")] - 
            projsums$Inundated_min[which(projsums$YEAR==2100 & projsums$prob=="p5")] )))
dis_hi <- label_number_si(accuracy=0.1)(
  sum(abs(projsums$Base_max[which(projsums$YEAR==2100 & projsums$prob=="p95")] - 
            projsums$Inundated_max[which(projsums$YEAR==2100 & projsums$prob=="p95")] )))

paste0(amp_mid, " [", amp_low, " - ", amp_hi, "]")
paste0(dis_mid, " [", dis_low, " - ", dis_hi, "]")

