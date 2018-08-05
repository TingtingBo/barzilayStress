## AFGR May 2018

## This script will be used to create the color table used to visualize the ME of stress on our 
## PNC data across the entire sample

## Source any functions
source('/home/arosen/jlfVisualizer/scripts/Rfunction/makeITKSnapColorTable.R')
source('/home/arosen/adroseHelperScripts/R/afgrHelpFunc.R')
install_load('ggplot2','mgcv','plyr')
binary.flip <- function(x)
{
    x*-1 + 1
}

calculateDeltaHiMeLo <- function(data, facScore) {
  colVal <- grep(paste('^', facScore, sep=''), names(data))
  
  quantiles <- quantile(data[,colVal], c(0,.3333,.6666,1), na.rm=T)
  
  data$PathGroup <- NA
  data$PathGroup[which(data[,colVal] < quantiles[2])] <- 1
  data$PathGroup[which(data[,colVal] >= quantiles[2] &
                          data[,colVal] < quantiles[3])] <- 2
  data$PathGroup[which(data[,colVal] >= quantiles[3])] <- 3

  output <- data
  return(output)
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

## First thing we need to do is load the data
vol.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_jlfAntsCTIntersectionVol_20170412.csv')
vol.data <- averageLeftAndRight(vol.data)
ct.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_jlfAntsCTIntersectionCT_20170331.csv')
gmd.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_jlfAtroposIntersectionGMD_20170410.csv')
qa.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_t1QaData_20170306.csv')
t1.data <- merge(vol.data, ct.data)
t1.data <- merge(t1.data, gmd.data)
t1.data <- merge(t1.data, qa.data)

# Now load all of the imaging data in the mega csv to grab the summary metrics and lobular values
mega.csv <- read.csv('/data/jux/BBL/projects/barzilayStress/data/n1601_imagingDataDump_2018-04-04.csv')
mega.csv.orig <- mega.csv
mega.csv <- averageLeftAndRight(mega.csv)
# Now grab our stress value and merge this in
stress.data <- read.csv('/data/jux/BBL/projects/barzilayStress/data/pncstressdatasetforadon.csv')
stress.data$StressBin <- stress.data$Cummulative_Stress_Load_No_Rape
stress.data$StressBin[which(stress.data$StressBin>=3)] <- 3
demo.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/demographics/n1601_demographics_go1_20161212.csv')
healthExclude <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/health/n1601_health_20170421.csv", header=TRUE)
stress.data <- merge(stress.data, demo.data)
stress.data <- merge(stress.data, healthExclude)

t1.data <- merge(t1.data, stress.data)

## Test the significance for all of these effects
summaryMetrics <- names(mega.csv)[grep('mprage_jlf_vol_', names(mega.csv))][1:77]
summaryMetrics <- summaryMetrics[-c(1,2,3,4,8,9,10,11,18,19)]
toAppend <- NULL
for(i in summaryMetrics){
  tmpMod <- as.formula(paste(i, "~s(ageAtScan1)+sex+Cummulative_Stress_Load_No_Rape"))
  tmpDat <- calculateDeltaHiMeLo(t1.data, 'Anxious_Misery_ar')
  tmpDat <- tmpDat[-which(tmpDat$healthExcludev2==1),]
  tmpDat2 <- merge(tmpDat, mega.csv, by=c('bblid', 'scanid'), suffixes=c("", ".y"))
  mod <- gam(tmpMod, data=tmpDat2)
  foo <- summary(mod)
  outputRow <- c(i, foo$p.table['Cummulative_Stress_Load_No_Rape',c('t value','Pr(>|t|)')], NA)
  toAppend <- rbind(toAppend, outputRow)  
}
toAppend[,4] <- p.adjust(toAppend[,3], method='fdr')
toAppend <- toAppend[which(toAppend[,4]<.05),]
nameVals <- toAppend[,1]

## Now write the color table
writeColorTableandKey(inputData=toAppend, inputColumn=2, outName='stressME')

## Now do the same with race
toAppend <- NULL
for(i in summaryMetrics){
  tmpMod <- as.formula(paste(i, "~s(ageAtScan1)+sex+race2+Cummulative_Stress_Load_No_Rape"))
  tmpDat <- calculateDeltaHiMeLo(t1.data, 'Anxious_Misery_ar')
  tmpDat <- tmpDat[-which(tmpDat$healthExcludev2==1),]
  tmpDat2 <- merge(tmpDat, mega.csv, by=c('bblid', 'scanid'), suffixes=c("", ".y"))
  tmpDat2$race2 <- factor(tmpDat2$race2)
  tmpDat2$race2 <- revalue(tmpDat2$race2, c('1'='1', '2'='2', '3'='2'))
  mod <- gam(tmpMod, data=tmpDat2)
  foo <- summary(mod)
  outputRow <- c(i, foo$p.table['Cummulative_Stress_Load_No_Rape',c('t value','Pr(>|t|)')], NA)
  toAppend <- rbind(toAppend, outputRow)  
}
#toAppend[,4] <- p.adjust(toAppend[,3], method='fdr')
toAppend <- toAppend[which(toAppend[,1] %in% nameVals),]
toAppend[,4] <- p.adjust(toAppend[,3], method='fdr')

## Now write the color table
writeColorTableandKey(inputData=toAppend, inputColumn=2, outName='stressMEwithRace', minTmp=c(-4.8, 0), maxTmp=c(0, 1.4))

## NOw do the same in our white participants
toAppend <- NULL
for(i in summaryMetrics){
  tmpMod <- as.formula(paste(i, "~s(ageAtScan1)+sex+Cummulative_Stress_Load_No_Rape"))
  tmpDat <- calculateDeltaHiMeLo(t1.data, 'Anxious_Misery_ar')
  tmpDat <- tmpDat[-which(tmpDat$healthExcludev2==1),]
  tmpDat2 <- merge(tmpDat, mega.csv, by=c('bblid', 'scanid'), suffixes=c("", ".y"))
  tmpDat2$race2 <- factor(tmpDat2$race2)
  tmpDat2$race2 <- revalue(tmpDat2$race2, c('1'='1', '2'='2', '3'='2'))
  tmpDat2 <- tmpDat2[which(tmpDat2$race2=='1'),]
  mod <- gam(tmpMod, data=tmpDat2)
  foo <- summary(mod)
  outputRow <- c(i, foo$p.table['Cummulative_Stress_Load_No_Rape',c('t value','Pr(>|t|)')], NA)
  toAppend <- rbind(toAppend, outputRow)  
}
toAppend[,4] <- p.adjust(toAppend[,3], method='fdr')
toAppend <- toAppend[which(toAppend[,1] %in% nameVals),]

## Now write the color table
writeColorTableandKey(inputData=toAppend, inputColumn=2, outName='stressMEwithWhite', minTmp=c(-4.8, 0), maxTmp=c(0, 1.8))

## Now do the same in our black participants
toAppend <- NULL
for(i in summaryMetrics){
  tmpMod <- as.formula(paste(i, "~s(ageAtScan1)+sex+Cummulative_Stress_Load_No_Rape"))
  tmpDat <- calculateDeltaHiMeLo(t1.data, 'Anxious_Misery_ar')
  tmpDat <- tmpDat[-which(tmpDat$healthExcludev2==1),]
  tmpDat2 <- merge(tmpDat, mega.csv, by=c('bblid', 'scanid'), suffixes=c("", ".y"))
  tmpDat2$race2 <- factor(tmpDat2$race2)
  tmpDat2$race2 <- revalue(tmpDat2$race2, c('1'='1', '2'='2', '3'='2'))
  tmpDat2 <- tmpDat2[which(tmpDat2$race2=='2'),]
  mod <- gam(tmpMod, data=tmpDat2)
  foo <- summary(mod)
  outputRow <- c(i, foo$p.table['Cummulative_Stress_Load_No_Rape',c('t value','Pr(>|t|)')], NA)
  toAppend <- rbind(toAppend, outputRow)  
}
toAppend[,4] <- p.adjust(toAppend[,3], method='fdr')
toAppend <- toAppend[which(toAppend[,1] %in% nameVals),]

## Now write the color table
writeColorTableandKey(inputData=toAppend, inputColumn=2, outName='stressMEwithBlack', minTmp=c(-4.8, 0), maxTmp=c(0, 1.8))

## Now do our interactions
toAppend <- NULL
for(i in summaryMetrics){
  tmpMod <- as.formula(paste(i, "~s(ageAtScan1)+sex+Cummulative_Stress_Load_No_Rape*Anxious_Misery_ar"))
  tmpDat <- calculateDeltaHiMeLo(t1.data, 'Anxious_Misery_ar')
  tmpDat <- tmpDat[-which(tmpDat$healthExcludev2==1),]
  tmpDat2 <- merge(tmpDat, mega.csv, by=c('bblid', 'scanid'), suffixes=c("", ".y"))
  mod <- gam(tmpMod, data=tmpDat2)
  foo <- summary(mod)
  outputRow <- c(i, foo$p.table['Cummulative_Stress_Load_No_Rape:Anxious_Misery_ar',c('t value','Pr(>|t|)')], NA)
  toAppend <- rbind(toAppend, outputRow)  
}
toAppend[,4] <- p.adjust(toAppend[,3], method='fdr')
toAppend <- toAppend[which(toAppend[,4]<.05),]
nameVals <- toAppend[,1]

## Now write the color table
writeColorTableandKey(inputData=toAppend, inputColumn=2, outName='stressInteraction', minTmp=c(-4, 0), maxTmp=c(0, 3.5))

## Now do our interactions with race
toAppend <- NULL
for(i in summaryMetrics){
  tmpMod <- as.formula(paste(i, "~s(ageAtScan1)+race2+sex+Cummulative_Stress_Load_No_Rape*Anxious_Misery_ar"))
  tmpDat <- calculateDeltaHiMeLo(t1.data, 'Anxious_Misery_ar')
  tmpDat <- tmpDat[-which(tmpDat$healthExcludev2==1),]
  tmpDat2 <- merge(tmpDat, mega.csv, by=c('bblid', 'scanid'), suffixes=c("", ".y"))
  tmpDat2$race2 <- factor(tmpDat2$race2)
  tmpDat2$race2 <- revalue(tmpDat2$race2, c('1'='1', '2'='2', '3'='2'))
  mod <- gam(tmpMod, data=tmpDat2)
  foo <- summary(mod)
  outputRow <- c(i, foo$p.table['Cummulative_Stress_Load_No_Rape:Anxious_Misery_ar',c('t value','Pr(>|t|)')], NA)
  toAppend <- rbind(toAppend, outputRow)  
}
toAppend[,4] <- p.adjust(toAppend[,3], method='fdr')
toAppend <- toAppend[which(toAppend[,1] %in% nameVals),]

## Now write the color table
writeColorTableandKey(inputData=toAppend, inputColumn=2, outName='stressInteractionRace', minTmp=c(-4, 0), maxTmp=c(0, 3.5))
