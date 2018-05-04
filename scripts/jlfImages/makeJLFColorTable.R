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

## First thing we need to do is load the data
vol.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_jlfAntsCTIntersectionVol_20170412.csv')
ct.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_jlfAntsCTIntersectionCT_20170331.csv')
gmd.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_jlfAtroposIntersectionGMD_20170410.csv')
qa.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_t1QaData_20170306.csv')
t1.data <- merge(vol.data, ct.data)
t1.data <- merge(t1.data, gmd.data)
t1.data <- merge(t1.data, qa.data)

# Now load all of the imaging data in the mega csv to grab the summary metrics and lobular values
mega.csv <- read.csv('/data/jux/BBL/projects/barzilayStress/data/n1601_imagingDataDump_2018-04-04.csv')

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
summaryMetrics <- names(mega.csv)[grep('mprage_jlf_vol_', names(mega.csv))][1:139]
summaryMetrics <- summaryMetrics[-c(1,2,14,17,18,19,20)]
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
toAppend[,4] <- p.adjust(toAppend[,3], method='fdr')
toAppend <- toAppend[which(toAppend[,1] %in% nameVals),]

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
toAppend <- toAppend[which(toAppend[,4]<.055),]
nameVals <- toAppend[,1]

## Now write the color table
writeColorTableandKey(inputData=toAppend, inputColumn=2, outName='stressInteraction', minTmp=c(-4.8, 0), maxTmp=c(2.8, 3.7))

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
writeColorTableandKey(inputData=toAppend, inputColumn=2, outName='stressInteractionRace', minTmp=c(-4.8, 0), maxTmp=c(2.8, 3.7))
