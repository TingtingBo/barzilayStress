## AFGR April 2018
## This script will be used to train a svm to classify our resilient folks
## from our non resilient folks
## Resilience is defined as those that have experinced a high load of stressful events but have not
## gone on to develop high psychiatric pathological loading

## Source Adon's library
source('/home/arosen/adroseHelperScripts/R/afgrHelpFunc.R')
source('./functions.R')
install_load('ggplot2','mgcv')

## Load data
# Now load all of the imaging data in the mega csv to grab the summary metrics and lobular values
mega.csv <- read.csv('/data/jux/BBL/projects/barzilayStress/data/n1601_imagingDataDump_2018-04-04.csv')

# now load the stress data
stress.data <- read.csv('/data/jux/BBL/projects/barzilayStress/data/pncstressdatasetforadon.csv')
stress.data$StressBin <- stress.data$Cummulative_Stress_Load_No_Rape
stress.data$StressBin[which(stress.data$StressBin>=3)] <- 3
demo.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/demographics/n1601_demographics_go1_20161212.csv')
healthExclude <- read.csv("/data/joy/BBL/studies/pnc/n1601_dataFreeze/health/n1601_health_20170421.csv", header=TRUE)
stress.data <- merge(stress.data, demo.data)
stress.data <- merge(stress.data, healthExclude)

# now merge em
all.data <- merge(mega.csv, stress.data, by=c('bblid', 'scanid'), suffixes=c("", ".y"))

# Now isolate the subjects we want to use
all.data.tu <- all.data[which(all.data$averageManualRating!=0 & all.data$healthExcludev2==0 & all.data$StressBin>=2),]

# Now create our path bin value
all.data.tu <- calculateDeltaHiMeLo(all.data.tu, "Anxious_Misery_ar")

# Now remove our middle path group
all.data.tu <- all.data.tu[-which(all.data.tu$PathGroup==2),]

# Now create our binary outcome
all.data.tu$resilientOutcome <- 0
all.data.tu$resilientOutcome[which(all.data.tu$PathGroup==3)] <- 1

# Now find out which subjects don't have a sex, or imaging data we want
all.data.tu <- all.data.tu[complete.cases(all.data.tu$sex),]
all.data.tu <- all.data.tu[complete.cases(all.data.tu[,grep('mprage_jlf_vol_', names(all.data.tu))]),]

## Now age and sex regress the volume data - that which we are interested in
volume.data <- all.data.tu[,grep('mprage_jlf_vol_', names(all.data.tu))]
volume.data <- apply(volume.data, 2, function(x) regressOutAge(x, all.data.tu$ageAtScan1, all.data.tu$sex))

## Now attach our outcome
volume.data <- cbind(all.data.tu$resilientOutcome, volume.data)

## Now write this csv for TPOT
write.csv(volume.data, "~/forTpot.csv", quote=F, row.names=F)

## This isn't run on chead but here is the TPOT call:
## tpot forTpot.csv -is , -target y -mode classification -scoring auc_roc -v 2
## I believe the best class method will be an svm... will report back later

