# AFGR December 6th 2017

# This script wil be used to prepare all of the rds files for the mass univariate analysis for 
# the stress project led by Ran Brazley

# Load all the data
match.subjects <- read.csv('/data/jux/BBL/projects/barzilayStress/data/matchSubjects.csv')
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
demo.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/demographics/n1601_demographics_go1_20161212.csv')
stress.data <- merge(stress.data, demo.data)
# Now add the health excludes
healthExclude <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/health/n1601_health_20170421.csv", header=TRUE)
stress.data <- merge(stress.data, healthExclude)

t1.data <- merge(t1.data, stress.data)
cbf.data <- merge(cbf.data, stress.data)
rest.data <- merge(rest.data, stress.data)
dti.data <- merge(dti.data, stress.data)
fa.data <- merge(fa.data, stress.data)

demoOfInterest <- c('bblid', 'scanid', 'ageAtScan1', 'sex', 'averageManualRating', 't1Exclude', 'Cummulative_Stress_Load_No_Rape', 'race2', 'Overall_Psychopathology_ar', 'envSES', 'Anxious_Misery_ar','incidentalFindingExclude')

# Now prepare t1 coviarte info
t1.cov <- t1.data[,demoOfInterest]
t1.cov <- t1.cov[complete.cases(t1.cov),]
t1.cov$sex <- as.factor(t1.cov$sex)
t1.cov$race2 <- as.factor(t1.cov$race2)
#t1.cov$averageManualRating <- ordered(t1.cov$averageManualRating)
t1.cov$t1Exclude <- binary.flip(t1.cov$t1Exclude)
t1.cov$t1Exclude[t1.cov$incidentalFindingExclude==1] <- 0
t1.cov$t1Exclude2 <- 0
t1.cov$t1Exclude2[t1.cov$bblid %in% match.subjects$bblid] <- 1

saveRDS(t1.cov, "/data/jux/BBL/projects/barzilayStress/data/t1Cov.RDS")

# Now do the cbf data
demoOfInterest <- c('bblid', 'scanid', 'ageAtScan1', 'sex', 'pcaslTSNR', 'pcaslExclude', 'Cummulative_Stress_Load_No_Rape', 'race2', 'Overall_Psychopathology_ar', 'envSES', 'Anxious_Misery_ar', 'incidentalFindingExclude')
cbf.cov <- cbf.data[,demoOfInterest]
cbf.cov <- cbf.cov[complete.cases(cbf.cov),]
cbf.cov$sex <- as.factor(cbf.cov$sex)
cbf.cov$race2 <- as.factor(cbf.cov$race2)
cbf.cov$pcaslExclude <- binary.flip(cbf.cov$pcaslExclude)
cbf.cov$cbfExclude[cbf.cov$incidentalFindingExclude==1] <- 0
cbf.cov$cbfExclude2 <- 0
cbf.cov$cbfExclude2[cbf.cov$bblid %in% match.subjects$bblid] <- 1
saveRDS(cbf.cov, "/data/jux/BBL/projects/barzilayStress/data/cbfCov.RDS")

# Now do reho 
demoOfInterest <- c('bblid', 'scanid', 'ageAtScan1', 'sex', 'restRelMeanRMSMotion', 'restExclude', 'Cummulative_Stress_Load_No_Rape', 'race2', 'Overall_Psychopathology_ar', 'envSES', 'Anxious_Misery_ar', 'incidentalFindingExclude')
rest.cov <- rest.data[,demoOfInterest]
rest.cov <- rest.cov[complete.cases(rest.cov),]
rest.cov$sex <- as.factor(rest.cov$sex)
rest.cov$race2 <- as.factor(rest.cov$race2)
rest.cov$restExclude <- binary.flip(rest.cov$restExclude)
rest.cov$restExclude[rest.cov$incidentalFindingExclude==1] <- 0
rest.cov$restExclude2 <- 0
rest.cov$restExclude2[rest.cov$bblid %in% match.subjects$bblid] <- 1
saveRDS(rest.cov, "/data/jux/BBL/projects/barzilayStress/data/restCov.RDS")

# Now do DTI
demoOfInterest <- c('bblid', 'scanid', 'ageAtScan1', 'sex', 'dti64Tsnr', 'dti64Exclude', 'Cummulative_Stress_Load_No_Rape', 'race2', 'Overall_Psychopathology_ar', 'envSES', 'Anxious_Misery_ar', 'incidentalFindingExclude')
dti.cov <- dti.data[,demoOfInterest]
dti.cov <- dti.cov[complete.cases(dti.cov),]
dti.cov$sex <- as.factor(dti.cov$sex)
dti.cov$race2 <- as.factor(dti.cov$race2)
dti.cov$dti64Exclude <- binary.flip(dti.cov$dti64Exclude)
dti.cov$t1Exclude[dti.cov$incidentalFindingExclude==1] <- 0
dti.cov$dtiExclude2 <- 0
dti.cov$dtiExclude2[dti.cov$bblid %in% match.subjects$blid] <- 1
saveRDS(dti.cov, "/data/jux/BBL/projects/barzilayStress/data/dtiCov.RDS")

dti.cov <- fa.data[,demoOfInterest]
dti.cov <- dti.cov[complete.cases(dti.cov),]
dti.cov$sex <- as.factor(dti.cov$sex)
dti.cov$race2 <- as.factor(dti.cov$race2)
dti.cov$dti64Exclude <- binary.flip(dti.cov$dti64Exclude)
dti.cov$t1Exclude[dti.cov$incidentalFindingExclude==1] <- 0
dti.cov$dtiExclude2 <- 0
dti.cov$dtiExclude2[dti.cov$bblid %in% match.subjects$blid] <- 1
saveRDS(dti.cov, "/data/jux/BBL/projects/barzilayStress/data/faCov.RDS")

# Now prepare the demographics for each of these modalities
source("~/adroseHelperScripts/R/afgrHelpFunc.R")
t1.sum <- summarySE(data=t1.data[which(t1.data$averageManualRating!=0),], groupvars=c('sex','Cummulative_Stress_Load_No_Rape') , measurevar='ageAtScan1')
cbf.sum <- summarySE(data=cbf.data[which(cbf.data$pcaslExclude==0),], groupvars=c('sex','Cummulative_Stress_Load_No_Rape'), measurevar='ageAtScan1')
rest.sum <- summarySE(data=rest.data[which(rest.data$restExclude==0),],groupvars=c('sex','Cummulative_Stress_Load_No_Rape'), measurevar='ageAtScan1') 

## Now we need to plot why our medical exclude kills our effect in the volume of the SCA
install_load('mgcv', 'ggplot2')
t1.data <- merge(t1.data, stress.data)
mod <- gam(mprage_jlf_vol_R_SCA ~ s(ageAtScan1) + race2 + sex + averageManualRating, data=t1.data)
t1.data$tmp <- scale(residuals(mod)) 
outPlot <- ggplot(t1.data, aes(x=Cummulative_Stress_Load_No_Rape, y=tmp, col=factor(healthExcludev2))) +
  geom_point() +
  geom_smooth(data=t1.data[which(t1.data$healthExclude==0),], method=lm, aes(x=Cummulative_Stress_Load_No_Rape, y=tmp), color='red') +
  geom_smooth(method=lm, color='blue') +
  ylab("mprage_jlf_vol_R_SCA")
# Now do the same for our tbv metric

