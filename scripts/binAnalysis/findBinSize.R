# Afgr Feb 2018

# This script will bbe used to find bin sizes for extreme tertile groups
# on the pathology factor scores in relation to the stress questionaire value

## Source Adon's library
source('/home/arosen/adroseHelperScripts/R/afgrHelpFunc.R')
library('ggplot2', 'mgcv')

## Load the data
# Start with the structural imaging data 
vol.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_jlfAntsCTIntersectionVol_20170412.csv')
ct.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_jlfAntsCTIntersectionCT_20170331.csv')
gmd.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_jlfAtroposIntersectionGMD_20170410.csv')
qa.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/n1601_t1QaData_20170306.csv')
t1.data <- merge(vol.data, ct.data)
t1.data <- merge(t1.data, gmd.data)
t1.data <- merge(t1.data, qa.data)

binary.flip <- function(x)
{
    x*-1 + 1
}

# Now load all of the imaging data in the mega csv to grab the summary metrics and lobular values
mega.csv <- read.csv('../../data/n1601_imagingDataDump_2018-04-04.csv')

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
stress.data <- read.csv('/data/joy/BBL/projects/barzilayStress/data/pncstressdatasetforadon.csv')
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
  data$PathGroup[which(data[,colVal] < quantiles[2])] <- 1
  data$PathGroup[which(data[,colVal] >= quantiles[2] &
                          data[,colVal] < quantiles[3])] <- 2
  data$PathGroup[which(data[,colVal] >= quantiles[3])] <- 3

  output <- data
  return(output)
}

# Now loop through and grab the pathological factor scores
pathVals <- names(t1.data)[c(522, 528, 529, 530, 531)]
pathVals <- names(t1.data)[c(528)]

## Also declare the summary values
summaryMetrics <- c("mprage_jlf_ct_MeanCT", "pcasl_jlf_cbf_MeanGMCBF", "dti_jlf_tr_MeanTR", "mprage_jlf_vol_ICV", "mprage_antsCT_vol_GrayMatter", "mprage_jlf_vol_L_Pallidum.y","mprage_jlf_vol_R_Pallidum.y", "mprage_jlf_vol_TBV", 'mprage_jlf_vol_ICV')
summaryMetrics <- unique(append(summaryMetrics, names(tmpDat2)[c(1050, 2119:2131)]))

## Test the significance for all of these effects
toAppend <- NULL
for(i in summaryMetrics){
  tmpMod <- as.formula(paste(i, "~s(ageAtScan1)+sex+race2+Cummulative_Stress_Load_No_Rape"))
  tmpDat <- calculateDeltaHiMeLo(t1.data, 'Anxious_Misery_ar')
  tmpDat <- tmpDat[-which(tmpDat$healthExcludev2==1),]
  tmpDat2 <- merge(tmpDat, mega.csv, by=c('bblid', 'scanid'), suffixes=c("", ".y"))
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
mega.csv$mprage_jlf_vol_R_DGM <- rowSums(mega.csv[,c(109,111,114,121,127,129,131)])
mega.csv$mprage_jlf_vol_L_DGM <- rowSums(mega.csv[,c(110,112,115,122,128,130,132)])
summaryMetrics <- c('mprage_jlf_vol_R_DGM', 'mprage_jlf_vol_L_DGM',names(tmpDat2)[2132:2155])
outputVals <- NULL
for(i in summaryMetrics){
  tmpMod <- as.formula(paste(i, "~s(ageAtScan1)+sex+race2+Cummulative_Stress_Load_No_Rape"))
  tmpDat <- calculateDeltaHiMeLo(t1.data, 'Anxious_Misery_ar')
  tmpDat <- tmpDat[-which(tmpDat$healthExcludev2==1),]
  tmpDat2 <- merge(tmpDat, mega.csv, by=c('bblid', 'scanid'), suffixes=c("", ".y"))
  mod <- gam(tmpMod, data=tmpDat2)
  foo <- summary(mod)
  print(paste(i, foo$p.table['Cummulative_Stress_Load_No_Rape','Pr(>|t|)']))
  outputRow <- c(i, foo$p.table['Cummulative_Stress_Load_No_Rape','Pr(>|t|)'])
  outputVals <- rbind(outputVals, outputRow)
}
# Now apply FDR correction
rownames(outputVals) <- NULL
fdrPValue <- rep(NA, dim(outputVals)[1])
fdrPValue[1:14] <- p.adjust(outputVals[1:14,2], method='fdr')
fdrPValue[15:26] <- p.adjust(outputVals[15:26,2], method='fdr')
outputVals <- cbind(outputVals, fdrPValue)
# Looks like only 1 lobular region corrected
# the region was the r temporal lobe for CT
# So now lets look at all of the regions w/in the right temporal lobe
summaryMetrics <- c('FuG', 'ITG', 'MTG', 'PP', 'PT', 'STG', 'TMP')
for(i in summaryMetrics){
  i <- paste("mprage_jlf_ct_R_", i, sep='')
  tmpMod <- as.formula(paste(i, "~s(ageAtScan1)+sex+race2+Cummulative_Stress_Load_No_Rape"))
  tmpDat <- calculateDeltaHiMeLo(t1.data, 'Anxious_Misery_ar')
  tmpDat <- tmpDat[-which(tmpDat$healthExcludev2==1),]
  tmpDat2 <- merge(tmpDat, mega.csv, by=c('bblid', 'scanid'), suffixes=c("", ".y"))
  mod <- gam(tmpMod, data=tmpDat2)
  foo <- summary(mod)
  print(paste(i, foo$p.table['Cummulative_Stress_Load_No_Rape','Pr(>|t|)']))
  outputRow <- c(i, foo$p.table['Cummulative_Stress_Load_No_Rape','Pr(>|t|)'], 'NA')
  outputVals <- rbind(outputVals, outputRow)
}
outputVals[grep('mprage_jlf_ct_R_', outputVals[,1]),3] <- p.adjust(outputVals[grep('mprage_jlf_ct_R_', outputVals[,1]),2], method='fdr')
outputVals <- rbind(toAppend, outputVals)
## Now do the volume regions
summaryMetrics <- c('R_MCgG','R_ACgG','R_PCgG','R_PHG','R_Ent','R_Accumbens_Area','L_Accumbens_Area','R_Amygdala','L_Amygdala','R_Caudate','L_Caudate','R_Hippocampus','L_Hippocampus','R_Pallidum','L_Pallidum','R_Putamen','L_Putamen','R_Thalamus_Proper','L_Thalamus_Proper')
for(i in summaryMetrics){
  i <- paste("mprage_jlf_vol_", i, sep='')
  tmpMod <- as.formula(paste(i, "~s(ageAtScan1)+sex+race2+Cummulative_Stress_Load_No_Rape"))
  tmpDat <- calculateDeltaHiMeLo(t1.data, 'Anxious_Misery_ar')
  tmpDat <- tmpDat[-which(tmpDat$healthExcludev2==1),]
  tmpDat2 <- merge(tmpDat, mega.csv, by=c('bblid', 'scanid'), suffixes=c("", ".y"))
  mod <- gam(tmpMod, data=tmpDat2)
  foo <- summary(mod)
  print(paste(i, foo$p.table['Cummulative_Stress_Load_No_Rape','Pr(>|t|)']))
  outputRow <- c(i, foo$p.table['Cummulative_Stress_Load_No_Rape','Pr(>|t|)'], 'NA')
  outputVals <- rbind(outputVals, outputRow)
}
outputVals[grep('mprage_jlf_ct_R_', outputVals[,1]),3] <- p.adjust(outputVals[grep('mprage_jlf_ct_R_', outputVals[,1]),2], method='fdr')
outputVals <- rbind(toAppend, outputVals)
rownames(outputVals) <- NULL
write.csv(outputVals, "~/telescopeMethodRanStressME.csv", quote=F, row.names=F)
## Now plot our strongest effect!
## Which is the cortical thickness in the planum temporale mprage_jlf_ct_R_PT
tmpMod <- as.formula(paste("mprage_jlf_ct_R_PT~s(ageAtScan1)+sex+race2"))
mod <- gam(tmpMod, data=tmpDat2)
tmpDat2$tmp <- scale(residuals(mod))
outPlot <- ggplot(tmpDat2, aes(x=Cummulative_Stress_Load_No_Rape, y=tmp)) + 
  geom_point() +
  geom_smooth(method='lm') +
  ylab('mprage_jlf_ct_R_PT')
# Now print this bad boy
pdf('~/telescopeStrongest.pdf')
outPlot
dev.off()

# Now perform the telescope in our interaction model
## Also declare the summary values
summaryMetrics <- c("mprage_jlf_ct_MeanCT", "pcasl_jlf_cbf_MeanGMCBF", "dti_jlf_tr_MeanTR", "mprage_jlf_vol_ICV", "mprage_antsCT_vol_GrayMatter", "mprage_jlf_vol_L_Pallidum.y","mprage_jlf_vol_R_Pallidum.y", "mprage_jlf_vol_TBV", 'mprage_jlf_vol_ICV')
summaryMetrics <- unique(append(summaryMetrics, names(tmpDat2)[c(1050, 2119:2131)]))

## Test the significance for all of these effects
toAppend <- NULL
for(i in summaryMetrics){
  tmpMod <- as.formula(paste(i, "~s(ageAtScan1)+sex+race2+Cummulative_Stress_Load_No_Rape*Anxious_Misery_ar"))
  tmpDat <- calculateDeltaHiMeLo(t1.data, 'Anxious_Misery_ar')
  tmpDat <- tmpDat[-which(tmpDat$healthExcludev2==1),]
  tmpDat2 <- merge(tmpDat, mega.csv, by=c('bblid', 'scanid'), suffixes=c("", ".y"))
  mod <- gam(tmpMod, data=tmpDat2)
  foo <- summary(mod)
  print(paste(i, foo$p.table['Cummulative_Stress_Load_No_Rape:Anxious_Misery_ar','Pr(>|t|)']))
  outputRow <- c(i, foo$p.table['Cummulative_Stress_Load_No_Rape:Anxious_Misery_ar','Pr(>|t|)'], NA)
  toAppend <- rbind(toAppend, outputRow)  
}

## So we know we have a volume and a ct effect
## So lest expand our search into the lobes for these modalities
## The GM lobes that is!
summaryMetrics <- c('mprage_jlf_vol_R_DGM', 'mprage_jlf_vol_L_DGM',names(tmpDat2)[2132:2227])
outputVals <- NULL
for(i in summaryMetrics){
  tmpMod <- as.formula(paste(i, "~s(ageAtScan1)+sex+race2+Cummulative_Stress_Load_No_Rape*Anxious_Misery_ar"))
  tmpDat <- calculateDeltaHiMeLo(t1.data, 'Anxious_Misery_ar')
  tmpDat <- tmpDat[-which(tmpDat$healthExcludev2==1),]
  tmpDat2 <- merge(tmpDat, mega.csv, by=c('bblid', 'scanid'), suffixes=c("", ".y"))
  mod <- gam(tmpMod, data=tmpDat2)
  foo <- summary(mod)
  print(paste(i, foo$p.table['Cummulative_Stress_Load_No_Rape:Anxious_Misery_ar','Pr(>|t|)']))
  outputRow <- c(i, foo$p.table['Cummulative_Stress_Load_No_Rape:Anxious_Misery_ar','Pr(>|t|)'])
  outputVals <- rbind(outputVals, outputRow)
}
# Now apply FDR correction
rownames(outputVals) <- NULL
fdrPValue <- rep(NA, dim(outputVals)[1])
fdrPValue[1:12] <- p.adjust(outputVals[1:12,2], method='fdr')
outputVals <- cbind(outputVals, fdrPValue)
# Looks like only 1 lobular region corrected
# the region was the r temporal lobe for CT
# So now lets look at all of the regions w/in the right temporal lobe
summaryMetrics <- c('FuG', 'ITG', 'MTG', 'PP', 'PT', 'STG', 'TMP')
for(i in summaryMetrics){
  i <- paste("mprage_jlf_ct_R_", i, sep='')
  tmpMod <- as.formula(paste(i, "~s(ageAtScan1)+sex+race2+Cummulative_Stress_Load_No_Rape*Anxious_Misery_ar"))
  tmpDat <- calculateDeltaHiMeLo(t1.data, 'Anxious_Misery_ar')
  tmpDat <- tmpDat[-which(tmpDat$healthExcludev2==1),]
  tmpDat2 <- merge(tmpDat, mega.csv, by=c('bblid', 'scanid'), suffixes=c("", ".y"))
  mod <- gam(tmpMod, data=tmpDat2)
  foo <- summary(mod)
  print(paste(i, foo$p.table['Cummulative_Stress_Load_No_Rape:Anxious_Misery_ar','Pr(>|t|)']))
  outputRow <- c(i, foo$p.table['Cummulative_Stress_Load_No_Rape:Anxious_Misery_ar','Pr(>|t|)'], 'NA')
  outputVals <- rbind(outputVals, outputRow)
}
outputVals[grep('mprage_jlf_ct_R_', outputVals[,1]),3] <- p.adjust(outputVals[grep('mprage_jlf_ct_R_', outputVals[,1]),2], method='fdr')
outputVals <- rbind(toAppend, outputVals)
rownames(outputVals) <- NULL

# Now loop through and get bins in each
outDat <- list()
index <- 1
for(q in pathVals){
  tmpDat <- calculateDeltaHiMeLo(t1.data, q)
  tmpDat <- tmpDat[-which(tmpDat$healthExcludev2==1),]
  # Now create a line plot for all of these guys to 
  # make the interaction appearant
  pdf("interactionPlot.pdf")
  for(w in summaryMetrics){
    tmpDat2 <- merge(tmpDat, mega.csv, by=c('bblid', 'scanid'), suffixes=c("", ".y"))
    colVal <- grep(paste(w), names(tmpDat2))
    tmpDat2 <- tmpDat2[complete.cases(tmpDat2$StressBin),]
    tmpDat2$StressBin <- factor(tmpDat2$StressBin, levels=c(0,1,2,3))
    tmpDat2$PathGroup <- factor(tmpDat2$PathGroup, levels=c(1,2,3))
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


