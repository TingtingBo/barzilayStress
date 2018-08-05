# Afgr Feb 2018

# This script will bbe used to find bin sizes for extreme tertile groups
# on the pathology factor scores in relation to the stress questionaire value

## Source Adon's library
source('/home/arosen/adroseHelperScripts/R/afgrHelpFunc.R')
install_load('ggplot2','mgcv','plyr')

## Load the data
# Start with the structural imaging data 
vol.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_jlfAntsCTIntersectionVol_20170412.csv')
ct.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_jlfAntsCTIntersectionCT_20170331.csv')
gmd.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_jlfAtroposIntersectionGMD_20170410.csv')
qa.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_t1QaData_20170306.csv')
t1.data <- merge(vol.data, ct.data)
t1.data <- merge(t1.data, gmd.data)
t1.data <- merge(t1.data, qa.data)

# Declare any functions we will need
binary.flip <- function(x)
{
    x*-1 + 1
}

averageLeftAndRight <- function(dataFrame){
  # Now get the right data
  dataFrame.right <- dataFrame[, grep('_R_', names(dataFrame))]
  dataFrame.tmp <- dataFrame[, -grep('_R_', names(dataFrame))]
  if(!identical(integer(0),grep('_right', names(dataFrame.tmp)))){
  dataFrame.right <- cbind(dataFrame.right, dataFrame.tmp[, grep('_right', names(dataFrame.tmp))])
  dataFrame.tmp <- dataFrame.tmp[,-grep('_right', names(dataFrame.tmp))]
  }
  if(!identical(integer(0),grep('_rh_', names(dataFrame.tmp)))){
  dataFrame.right <- cbind(dataFrame.right, dataFrame.tmp[, grep('_rh_', names(dataFrame.tmp))])
  dataFrame.tmp <- dataFrame.tmp[,-grep('_rh_', names(dataFrame.tmp))]
  }  
  if(dim(dataFrame.tmp)[2] == 0){
  dataFrame.tmp <- dataFrame
  dataFrame.right <- dataFrame.tmp[, grep('_rh_', names(dataFrame.tmp))]
  dataFrame.tmp <- dataFrame.tmp[,-grep('_rh_', names(dataFrame.tmp))]
  }  
  # First do the left data
  dataFrame.left <- dataFrame.tmp[, grep('_L_', names(dataFrame.tmp))]
  dataFrame.tmp <- dataFrame.tmp[, -grep('_L_', names(dataFrame.tmp))]
  if(!identical(integer(0),grep('_left', names(dataFrame.tmp)))){
  dataFrame.left <- cbind(dataFrame.left, dataFrame.tmp[, grep('_left', names(dataFrame.tmp))])
  dataFrame.tmp <- dataFrame.tmp[,-grep('_left', names(dataFrame.tmp))]
  }
  if(!identical(integer(0),grep('_lh_', names(dataFrame.tmp)))){
  dataFrame.left <- cbind(dataFrame.left, dataFrame.tmp[, grep('_lh_', names(dataFrame.tmp))])
  dataFrame.tmp <- dataFrame.tmp[,-grep('_lh_', names(dataFrame.tmp))]
  }
  if(dim(dataFrame.tmp)[2] == 0){
  dataFrame.tmp <- dataFrame
  dataFrame.left <- dataFrame.tmp[, grep('_lh', names(dataFrame.tmp))]
  dataFrame.tmp <- dataFrame.tmp[,-grep('_lh_', names(dataFrame.tmp))]
  dataFrame.tmp <- dataFrame.tmp[,-grep('_rh_', names(dataFrame.tmp))]
  } 
  # Now combine the data frames
  dataFrame.meaned <- (dataFrame.left + dataFrame.right)/2

  # Now remove the left and right indeices from the names of the meaned data frame
  colnames(dataFrame.meaned) <- gsub(pattern='_L_', replacement = '_', x = colnames(dataFrame.meaned), fixed = TRUE)
  colnames(dataFrame.meaned) <- gsub(pattern='_R_', replacement = '_', x = colnames(dataFrame.meaned), fixed = TRUE)
  colnames(dataFrame.meaned) <- gsub(pattern='_left', replacement = '', x = colnames(dataFrame.meaned), fixed = TRUE)
  colnames(dataFrame.meaned) <- gsub(pattern='_right', replacement = '', x = colnames(dataFrame.meaned), fixed = TRUE)
  colnames(dataFrame.meaned) <- gsub(pattern='_rh_', replacement = '_', x = colnames(dataFrame.meaned), fixed = TRUE)
  colnames(dataFrame.meaned) <- gsub(pattern='_lh_', replacement = '_', x = colnames(dataFrame.meaned), fixed = TRUE)
  # Now rm the left and right values and append the meaned values
  indexToRm <- grep('_L_', names(dataFrame))
  indexToRm <- append(indexToRm, grep('_R_', names(dataFrame)))
  indexToRm <- append(indexToRm, grep('_left', names(dataFrame)))
  indexToRm <- append(indexToRm, grep('_right', names(dataFrame)))
  indexToRm <- append(indexToRm, grep('_rh_', names(dataFrame)))
  indexToRm <- append(indexToRm, grep('_lh_', names(dataFrame)))
  # Now prep the output 
  output <- dataFrame[,-indexToRm]
  # Now combine our average values
  output <- cbind(output, dataFrame.meaned)
  # Now return the output
  return(output)
}

# Now load all of the imaging data in the mega csv to grab the summary metrics and lobular values
mega.csv <- read.csv('/data/jux/BBL/projects/barzilayStress/data/n1601_imagingDataDump_2018-04-04.csv')

# Now onto the perfusion data 
cbf.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/asl/n1601_jlfAntsCTIntersectionPcaslValues_20170403.csv')
cbf.wm.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/asl/n1601_jlfWMPcasl_20170412.csv')
qa.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/asl/n1601_PcaslQaData_20170403.csv')
cbf.data <- merge(cbf.data, cbf.wm.data)
cbf.data <- merge(cbf.data, qa.data)

# Now functional data 
reho.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/rest/n1601_jlfReHoValues_20170714.csv')
alff.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/rest/n1601_jlfALFFValues_20170714.csv')
qa.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/rest/n1601_RestQAData_20170714.csv')
rest.data <- merge(reho.data, alff.data)
rest.data <- merge(rest.data, qa.data)

# Now diffusion
tr.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/dti/n1601_jlfTRValues_20170411.csv')
qa.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/dti/n1601_dti_qa_20170301.csv')
dti.data <- merge(tr.data, qa.data)

# Now tract values
fa.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/dti/n1601_JHULabelsFA_20170321.csv')
fa.data <- merge(fa.data, qa.data)

# Now grab our stress value and merge this in
stress.data <- read.csv('/data/jux/BBL/projects/barzilayStress/data/pncstressdatasetforadon.csv')
stress.data$StressBin <- stress.data$Cummulative_Stress_Load_No_Rape
stress.data$StressBin[which(stress.data$StressBin>=3)] <- 3
demo.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/demographics/n1601_demographics_go1_20161212.csv')
healthExclude <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/health/n1601_health_20170421.csv", header=TRUE)
stress.data <- merge(stress.data, demo.data)
stress.data <- merge(stress.data, healthExclude)

t1.data <- merge(t1.data, stress.data)
cbf.data <- merge(cbf.data, stress.data)
rest.data <- merge(rest.data, stress.data)
dti.data <- merge(dti.data, stress.data)
fa.data <- merge(fa.data, stress.data)

# Now decalre a function which will bin the a continous varible into tertiles
calculateDeltaHiMeLo <- function(data, facScore) {
  colVal <- grep(paste('^', facScore, sep=''), names(data))
  
  quantiles <- quantile(data[,colVal], c(0,.3333,.6666,1), na.rm=T)
  
  data$PathGroup <- NA
  data$PathGroup[which(data[,colVal] < quantiles[2])] <- 'Low'
  data$PathGroup[which(data[,colVal] >= quantiles[2] &
                          data[,colVal] < quantiles[3])] <- 'Middle'
  data$PathGroup[which(data[,colVal] >= quantiles[3])] <- 'High'

  output <- data
  return(output)
}

# Now loop through and grab the pathological factor scores
pathVals <- names(t1.data)[c(522, 528, 529, 530, 531)]
pathVals <- 'Anxious_Misery_ar'

## Also declare the summary values
summaryMetrics <- c("mprage_jlf_ct_MeanCT", "pcasl_jlf_cbf_MeanGMCBF", "dti_jlf_tr_MeanTR", "mprage_jlf_vol_ICV", "mprage_antsCT_vol_GrayMatter", "mprage_jlf_vol_TBV", 'mprage_jlf_vol_ICV')
summaryMetrics <- unique(append(summaryMetrics, names(mega.csv)[c(471, 1540:1552)]))
## Test the significance for all of these effects
toAppend <- NULL
for(i in summaryMetrics){
  tmpMod <- as.formula(paste(i, "~ageAtScan1+sex+race2+Cummulative_Stress_Load_No_Rape+averageManualRating"))
  tmpDat <- calculateDeltaHiMeLo(t1.data, 'Anxious_Misery_ar')
  tmpDat <- tmpDat[-which(tmpDat$healthExcludev2==1),]
  tmpDat2 <- merge(tmpDat, mega.csv, by=c('bblid', 'scanid'), suffixes=c("", ".y"))
  tmpDat2$race2 <- factor(tmpDat2$race2)
  tmpDat2$race2 <- revalue(tmpDat2$race2, c('1'='1', '2'='2', '3'='2'))
  mod <- gam(tmpMod, data=tmpDat2)
  foo <- summary(mod)
  print(paste(i, foo$p.table['Cummulative_Stress_Load_No_Rape','Pr(>|t|)']))
  outputRow <- c(i, foo$p.table['Cummulative_Stress_Load_No_Rape','Pr(>|t|)'], NA)
  toAppend <- rbind(toAppend, outputRow)  
}

## So we know we have a volume and a ct effect
## So lest expand our search into the lobes for these modalities
## The GM lobes that is!
## First thing though, we need to create a subcortical volume variable for the left and right hemi's
mega.csv$mprage_jlfLobe_vol_R_DGM <- rowSums(mega.csv[,c(109,111,114,121,127,129,131)])
mega.csv$mprage_jlfLobe_vol_L_DGM <- rowSums(mega.csv[,c(110,112,115,122,128,130,132)])
summaryMetrics <- c('mprage_jlfLobe_vol_R_DGM', 'mprage_jlfLobe_vol_L_DGM',names(mega.csv)[c(1553:1576,234:245)])
summaryMetrics <- gsub(summaryMetrics, pattern='_R_', replacement='_')
summaryMetrics <- gsub(summaryMetrics, pattern='_L_', replacement='_')
summaryMetrics <- unique(summaryMetrics)
outputVals <- NULL
for(i in summaryMetrics){
  tmpMod <- as.formula(paste(i, "~s(ageAtScan1)+sex+race2+Cummulative_Stress_Load_No_Rape"))
  tmpDat <- calculateDeltaHiMeLo(t1.data, 'Anxious_Misery_ar')
  tmpDat <- tmpDat[-which(tmpDat$healthExcludev2==1),]
  tmpDat2 <- merge(tmpDat, mega.csv, by=c('bblid', 'scanid'), suffixes=c("", ".y"))
  tmpDat2 <- averageLeftAndRight(tmpDat2)
  tmpDat2$race2 <- factor(tmpDat2$race2)
  tmpDat2$race2 <- revalue(tmpDat2$race2, c('1'='1', '2'='2', '3'='2'))
  mod <- gam(tmpMod, data=tmpDat2)
  foo <- summary(mod)
  print(paste(i, foo$p.table['Cummulative_Stress_Load_No_Rape','Pr(>|t|)']))
  outputRow <- c(i, foo$p.table['Cummulative_Stress_Load_No_Rape','Pr(>|t|)'])
  outputVals <- rbind(outputVals, outputRow)
}
# Now apply FDR correction
rownames(outputVals) <- NULL
fdrPValue <- rep(NA, dim(outputVals)[1])
fdrPValue[grep("mprage_jlf_vol", outputVals[,1])] <- p.adjust(outputVals[grep("mprage_jlf_vol", outputVals[,1]),2], method='fdr')
fdrPValue[grep("mprage_jlfLobe_ct", outputVals[,1])] <- p.adjust(outputVals[grep("mprage_jlfLobe_ct", outputVals[,1]),2], method='fdr')
fdrPValue[grep("mprage_jlfLobe_vol", outputVals[,1])] <- p.adjust(outputVals[grep("mprage_jlfLobe_vol", outputVals[,1]),2], method='fdr')
outputVals <- cbind(outputVals, fdrPValue)

# Looks like only 1 lobular region corrected
# the region was the r temporal lobe for CT
# So now lets look at all of the regions w/in the right temporal lobe
summaryMetrics <- c('FuG', 'ITG', 'MTG', 'PP', 'PT', 'STG', 'TMP')
for(i in summaryMetrics){
  i <- paste("mprage_jlf_ct_R_", i, sep='')
  tmpMod <- as.formula(paste(i, "~s(ageAtScan1)+sex+envSES+Cummulative_Stress_Load_No_Rape"))
  tmpDat <- calculateDeltaHiMeLo(t1.data, 'Anxious_Misery_ar')
  tmpDat <- tmpDat[-which(tmpDat$healthExcludev2==1),]
  tmpDat2 <- merge(tmpDat, mega.csv, by=c('bblid', 'scanid'), suffixes=c("", ".y"))
  tmpDat2$race2 <- factor(tmpDat2$race2)
  tmpDat2$race2 <- revalue(tmpDat2$race2, c('1'='1', '2'='2', '3'='2'))
  mod <- gam(tmpMod, data=tmpDat2)
  foo <- summary(mod)
  print(paste(i, foo$p.table['Cummulative_Stress_Load_No_Rape','Pr(>|t|)']))
  outputRow <- c(i, foo$p.table['Cummulative_Stress_Load_No_Rape','Pr(>|t|)'], 'NA')
  outputVals <- rbind(outputVals, outputRow)
}
outputVals[grep('mprage_jlf_ct_R_', outputVals[,1]),3] <- p.adjust(outputVals[grep('mprage_jlf_ct_R_', outputVals[,1]),2], method='fdr')
outputVals <- rbind(toAppend, outputVals)
## Now do the volume regions
summaryMetrics <-c('R_Accumbens_Area','L_Accumbens_Area','R_Amygdala','L_Amygdala','R_Caudate','L_Caudate','R_Hippocampus','L_Hippocampus','R_Pallidum','L_Pallidum','R_Putamen','L_Putamen','R_Thalamus_Proper','L_Thalamus_Proper')
summaryMetrics <- names(tmpDat2)[grep("mprage_jlf_vol_", names(tmpDat2))][c(1:79, 140:145)]
summaryMetrics <- gsub(summaryMetrics, pattern='_R_', replacement='_')
summaryMetrics <- gsub(summaryMetrics, pattern='_L_', replacement='_')
summaryMetrics <- unique(summaryMetrics)
summaryMetrics <- summaryMetrics[-grep(".y", summaryMetrics)]
summaryMetrics <- summaryMetrics[-c(1,2,3,4,8,9,10,11,16,18,19)]
for(i in summaryMetrics){
  tmpMod <- as.formula(paste(i, "~s(ageAtScan1)+sex+race2+Cummulative_Stress_Load_No_Rape"))
  tmpDat <- calculateDeltaHiMeLo(t1.data, 'Anxious_Misery_ar')
  tmpDat <- tmpDat[-which(tmpDat$healthExcludev2==1),]
  tmpDat2 <- merge(tmpDat, mega.csv, by=c('bblid', 'scanid'), suffixes=c("", ".y"))
  tmpDat2 <- averageLeftAndRight(tmpDat2)
  tmpDat2$race2 <- factor(tmpDat2$race2)
  tmpDat2$race2 <- revalue(tmpDat2$race2, c('1'='1', '2'='2', '3'='2'))
  mod <- gam(tmpMod, data=tmpDat2)
  foo <- summary(mod)
  print(paste(i, foo$p.table['Cummulative_Stress_Load_No_Rape','Pr(>|t|)']))
  outputRow <- c(i, foo$p.table['Cummulative_Stress_Load_No_Rape','Pr(>|t|)'], 'NA')
  outputVals <- rbind(outputVals, outputRow)
}
outputVals[grep('mprage_jlf_vol_', outputVals[,1])[-c(1:4)],3] <- p.adjust(outputVals[grep('mprage_jlf_vol_', outputVals[,1])[-c(1:4)],2], method='fdr')
outputVals <- rbind(toAppend, outputVals)
rownames(outputVals) <- NULL
write.csv(outputVals, "~/telescopeMethodRanStressME.csv", quote=F, row.names=F)
## Now plot our strongest effect!
## Which is the cortical thickness in the planum temporale mprage_jlf_ct_R_PT
tmpMod <- as.formula(paste("mprage_jlf_vol_L_Thalamus_Proper~s(ageAtScan1)+sex+race2"))
mod <- gam(tmpMod, data=tmpDat2)
tmpDat2$tmp <- scale(residuals(mod))
outPlot <- ggplot(tmpDat2, aes(x=Cummulative_Stress_Load_No_Rape, y=tmp)) + 
  geom_point() +
  geom_smooth(method='lm') +
  ylab('mprage_jlf_vol_L_Thalamus_Proper')
# Now print this bad boy
pdf('~/telescopeStrongest.pdf')
outPlot
dev.off()

# Now perform the telescope in our interaction model
## Also declare the summary values
summaryMetrics <- c("mprage_jlf_ct_MeanCT", "pcasl_jlf_cbf_MeanGMCBF", "dti_jlf_tr_MeanTR", "mprage_jlf_vol_ICV", "mprage_antsCT_vol_GrayMatter", "mprage_jlf_vol_TBV", 'mprage_jlf_vol_ICV')
summaryMetrics <- unique(append(summaryMetrics, names(mega.csv)[c(471, 1540:1552)]))
## Test the significance for all of these effects
toAppend <- NULL
for(i in summaryMetrics){
  tmpMod <- as.formula(paste(i, "~s(ageAtScan1)+sex+race2+averageManualRating+Cummulative_Stress_Load_No_Rape*Anxious_Misery_ar"))
  tmpDat <- calculateDeltaHiMeLo(t1.data, 'Anxious_Misery_ar')
  tmpDat <- tmpDat[-which(tmpDat$healthExcludev2==1),]
  tmpDat <- tmpDat[-which(tmpDat$t1Exclude==1),]
  tmpDat2 <- merge(tmpDat, mega.csv, by=c('bblid', 'scanid'), suffixes=c("", ".y"))
  tmpDat2$race2 <- factor(tmpDat2$race2)
  tmpDat2$race2 <- revalue(tmpDat2$race2, c('1'='1', '2'='2', '3'='2'))
  mod <- gam(tmpMod, data=tmpDat2)
  foo <- summary(mod)
  print(paste(i, foo$p.table['Cummulative_Stress_Load_No_Rape:Anxious_Misery_ar','Pr(>|t|)']))
  outputRow <- c(i, foo$p.table['Cummulative_Stress_Load_No_Rape:Anxious_Misery_ar','Pr(>|t|)'], NA)
  toAppend <- rbind(toAppend, outputRow)  
}

## So we know we have a volume
## So lest expand our search into the lobes for these modalities
## The GM lobes that is!
summaryMetrics <- c('mprage_jlfLobe_vol_R_DGM', 'mprage_jlfLobe_vol_L_DGM',names(mega.csv)[c(1553:1576)])
summaryMetrics <- gsub(summaryMetrics, pattern='_R_', replacement='_')
summaryMetrics <- gsub(summaryMetrics, pattern='_L_', replacement='_')
summaryMetrics <- unique(summaryMetrics)
outputVals <- NULL
for(i in summaryMetrics){
  tmpMod <- as.formula(paste(i, "~s(ageAtScan1)+sex+race2+averageManualRating+Cummulative_Stress_Load_No_Rape*Anxious_Misery_ar"))
  tmpDat <- calculateDeltaHiMeLo(t1.data, 'Anxious_Misery_ar')
  tmpDat <- tmpDat[-which(tmpDat$healthExcludev2==1),]
  tmpDat <- tmpDat[-which(tmpDat$t1Exclude==1),]
  tmpDat2 <- merge(tmpDat, mega.csv, by=c('bblid', 'scanid'), suffixes=c("", ".y"))
  tmpDat2 <- averageLeftAndRight(tmpDat2)
  tmpDat2$race2 <- factor(tmpDat2$race2)
  tmpDat2$race2 <- revalue(tmpDat2$race2, c('1'='1', '2'='2', '3'='2'))
  mod <- gam(tmpMod, data=tmpDat2)
  foo <- summary(mod)
  print(paste(i, foo$p.table['Cummulative_Stress_Load_No_Rape:Anxious_Misery_ar','Pr(>|t|)']))
  outputRow <- c(i, foo$p.table['Cummulative_Stress_Load_No_Rape:Anxious_Misery_ar','Pr(>|t|)'])
  outputVals <- rbind(outputVals, outputRow)
}
# Now apply FDR correction
rownames(outputVals) <- NULL
fdrPValue <- rep(NA, dim(outputVals)[1])
fdrPValue[grep("mprage_jlfLobe_ct", outputVals[,1])] <- p.adjust(outputVals[grep("mprage_jlfLobe_ct", outputVals[,1]),2], method='fdr')
fdrPValue[grep("mprage_jlfLobe_vol", outputVals[,1])] <- p.adjust(outputVals[grep("mprage_jlfLobe_vol", outputVals[,1]),2], method='fdr')
outputVals <- cbind(outputVals, fdrPValue)

# Looks like both of our DGM regions corrected
# So now lets look at all of the regions w/in the this area
summaryMetrics <-c('R_Accumbens_Area','L_Accumbens_Area','R_Amygdala','L_Amygdala','R_Caudate','L_Caudate','R_Hippocampus','L_Hippocampus','R_Pallidum','L_Pallidum','R_Putamen','L_Putamen','R_Thalamus_Proper','L_Thalamus_Proper')
summaryMetrics <- names(tmpDat2)[grep("mprage_jlf_vol_", names(tmpDat2))][c(1:79, 140:145)]
summaryMetrics <- gsub(summaryMetrics, pattern='_R_', replacement='_')
summaryMetrics <- gsub(summaryMetrics, pattern='_L_', replacement='_')
summaryMetrics <- unique(summaryMetrics)
summaryMetrics <- summaryMetrics[-grep(".y", summaryMetrics)]
summaryMetrics <- summaryMetrics[-c(1:11,14,15,16,18,19)]
for(i in summaryMetrics){
  #i <- paste("mprage_jlf_vol_", i, sep='')

  tmpMod <- as.formula(paste(i, "~s(ageAtScan1)+sex+race2+averageManualRating+Cummulative_Stress_Load_No_Rape*Anxious_Misery_ar"))
  tmpDat <- calculateDeltaHiMeLo(t1.data, 'Anxious_Misery_ar')
  tmpDat <- tmpDat[-which(tmpDat$healthExcludev2==1),]
  tmpDat <- tmpDat[-which(tmpDat$t1Exclude==1),]
  tmpDat2 <- merge(tmpDat, mega.csv, by=c('bblid', 'scanid'), suffixes=c("", ".y"))
  tmpDat2 <- averageLeftAndRight(tmpDat2)
  tmpDat2$race2 <- factor(tmpDat2$race2)
  tmpDat2$race2 <- revalue(tmpDat2$race2, c('1'='1', '2'='2', '3'='2'))
  mod <- gam(tmpMod, data=tmpDat2)
  foo <- summary(mod)
  print(paste(i, foo$p.table['Cummulative_Stress_Load_No_Rape:Anxious_Misery_ar','Pr(>|t|)']))
  outputRow <- c(i, foo$p.table['Cummulative_Stress_Load_No_Rape:Anxious_Misery_ar','Pr(>|t|)'], 'NA')
  outputVals <- rbind(outputVals, outputRow)
}
outputVals[grep('mprage_jlf_vol_', outputVals[,1]),3] <- p.adjust(outputVals[grep('mprage_jlf_vol_', outputVals[,1]),2], method='fdr')
outputVals <- rbind(toAppend, outputVals)
rownames(outputVals) <- NULL
# Now write these values
write.csv(outputVals, "~/telescopeMethodRanStressIE.csv", quote=F, row.names=F)

# Now create an interaction plot for every ROI that corrects
summaryMetrics <- c("mprage_jlf_vol_Pallidum","mprage_jlf_vol_TBWM","mprage_jlf_vol_TBGM", "mprage_jlfLobe_vol_Parietal_Lobe", "mprage_jlfLobe_vol_Insular_Lobe")
# Now loop through and get bins in each
outDat <- list()
index <- 1
for(q in pathVals){
  tmpDat <- calculateDeltaHiMeLo(t1.data, q)
  tmpDat <- tmpDat[-which(tmpDat$healthExcludev2==1),]
  tmpDat <- tmpDat[-which(tmpDat$t1Exclude==1),]
  # Now create a line plot for all of these guys to 
  # make the interaction appearant
  pdf("interactionPlot.pdf")
  for(w in summaryMetrics){
    tmpDat2 <- merge(tmpDat, mega.csv, by=c('bblid', 'scanid'), suffixes=c(".y", ""))
    tmpDat2 <- averageLeftAndRight(tmpDat2)
    colVal <- grep(paste(w), names(tmpDat2))
    if(length(colVal)>1){
        colVal <- colVal[2]
    }
    tmpDat2 <- tmpDat2[complete.cases(tmpDat2$StressBin),]
    tmpDat2$StressBin <- factor(tmpDat2$StressBin, levels=c(0,1,2,3))
    tmpDat2$PathGroup <- factor(tmpDat2$PathGroup, levels=c("Low","Middle","High"))
    tmpDat2$race2 <- factor(tmpDat2$race2)
    outPlot <- ggplot(tmpDat2, aes(x=Anxious_Misery_ar, y=tmpDat2[,colVal], group=StressBin, col=StressBin)) + 
      geom_point() +
      geom_smooth(method='lm',level=0) + 
      ylab(gsub(x=w, pattern='.y', replacement=''))
    print(outPlot)
    outPlot2 <- ggplot(tmpDat2, aes(x=Cummulative_Stress_Load_No_Rape, y=tmpDat2[,colVal], group=PathGroup, col=PathGroup)) + 
      geom_point() +
      geom_smooth(method='lm',level=0) + 
      ylab(gsub(x=w, pattern='.y', replacement=''))
    print(outPlot2)
  }
  dev.off()
  # Now do the same but in the regressed values
  for(w in summaryMetrics){
      png(paste(w, ".png", sep=''), width=16, height=12, units='in', res=300)
      tmpDat2 <- merge(tmpDat, mega.csv, by=c('bblid', 'scanid'), suffixes=c(".y", ""))
      tmpDat2 <- averageLeftAndRight(tmpDat2)
      colVal <- grep(paste(w), names(tmpDat2))
      if(length(colVal)>1){
          colVal <- colVal[2]
      }
      tmpDat2 <- tmpDat2[complete.cases(tmpDat2$StressBin),]
      tmpMod <- gam(tmpDat2[,colVal] ~ s(ageAtScan1.y) + sex.y + averageManualRating + race2.y, data=tmpDat2)
      index <- complete.cases(tmpDat2[,w])
      tmpDat2$tmpVals <- NA
      tmpDat2$tmpVals[index] <- as.numeric(scale(residuals(tmpMod)))
      tmpDat2$StressBin <- factor(tmpDat2$StressBin, levels=c(0,1,2,3))
      tmpDat2$StressBin <- revalue(tmpDat2$StressBin, replace=c("0"="No TSE", "1"="1 TSE", "2"="2 TSE", "3"="3+ TSE"))
      tmpDat2$PathGroup <- factor(tmpDat2$PathGroup, levels=c("Low","Middle","High"))
      tmpDat2$race2 <- factor(tmpDat2$race2)
      outPlot <- ggplot(tmpDat2, aes(x=Anxious_Misery_ar, y=tmpVals, group=StressBin, col=StressBin)) +
      #geom_point() +
      geom_smooth(method='lm', size=3) +
      ylab('') +
      xlab('') +
      theme_bw() + 
      coord_cartesian(ylim=c(-1.5,1.5), xlim=c(-2.5,4)) +
      theme(legend.position="none") +
      theme(text = element_text(size = 34)) + 
      scale_color_brewer(palette='Reds')
      #print(outPlot)
      outPlot2 <- ggplot(tmpDat2, aes(x=Cummulative_Stress_Load_No_Rape, y=tmpVals, group=PathGroup, col=PathGroup)) +
      #geom_point() +
      geom_smooth(method='lm', size=3) +
      ylab('') + 
      xlab("Traumatic Stressful Events") +
     theme_bw() + 
      theme(legend.position="none") +
      coord_cartesian(ylim=c(-1.5,1.5), xlim=c(0,7)) +
      theme(text = element_text(size = 34)) + 
      scale_color_brewer(palette="Blues")
      png(paste(w, "PATH.png", sep=''), width=16, height=12, units='in', res=300)
      print(outPlot)
      dev.off()
      png(paste(w, "TSE.png", sep=''), width=16, height=12, units='in', res=300)
      print(outPlot2)
      dev.off()
  }
  tmpDat <- tmpDat[-which(tmpDat$PathGroup==2),]
  tmpDat <- tmpDat[-which(tmpDat$StressBin==1),]
  tmpDat <- tmpDat[-which(tmpDat$StressBin==2),]
  tmpDat <- tmpDat[which(is.na(tmpDat$StressBin)=="FALSE"),]
  outVal <- table(tmpDat$StressBin, tmpDat$PathGroup)
  outDat[[index]] <- outVal
  index <- index+1
  # Now grab the age values
  ageVals <- summarySE(data=tmpDat, measurevar='ageAtScan1', groupvars=c('StressBin','PathGroup'))
  outputCSV <- paste(q, ".csv", sep='')
  write.csv(ageVals, outputCSV, quote=F, row.names=F)
  ageVals <- summarySE(data=tmpDat, measurevar='envSES', groupvars=c('StressBin','PathGroup'))
  outputCSV <- paste(q, "SES.csv", sep='')
  write.csv(ageVals, outputCSV, quote=F, row.names=F)
  # Now create a combined factor
  tmpDat$combined <- paste(tmpDat$PathGroup, tmpDat$StressBin)
  tmpDat <- merge(tmpDat, mega.csv, by=c('bblid', 'scanid'))

  pdf("outPlots.pdf")  
  # Now do the summary metrics
  for(w in summaryMetrics){
    sumVal <- summarySE(data=tmpDat, measurevar=w, groupvars=c('StressBin', 'PathGroup'), na.rm=T)
    sumVal$combined <- paste(sumVal$StressBin,sumVal$PathGroup)
    minVal <- min(sumVal[,4])/2
    maxVal <- max(sumVal[,4])*2
    outPlot <- ggplot(sumVal, aes(x=combined, y=sumVal[,4], fill=PathGroup)) +
      geom_bar(stat='identity', position=position_dodge(), size=.1) + 
      labs(title='', x='Group (<1> = stress group <2> = path group)', y=gsub(x=w, pattern='.y', replacement='')) +
      theme_bw() +
      geom_errorbar(aes(ymin=sumVal[,4]-se, ymax=sumVal[,4]+se), 
                       width = .1, position=position_dodge(.9))
    print(outPlot)

    # Now write a csv with the values
    outValues <- summarySE(data=tmpDat, measurevar=w, groupvars=c('StressBin', 'PathGroup', 'sex.x'), na.rm=T)
    write.csv(outValues, paste(w, ".csv", sep=''), quote=F,)
      
  }
  dev.off()
}
