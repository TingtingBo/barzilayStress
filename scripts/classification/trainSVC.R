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
all.data.tu <-
