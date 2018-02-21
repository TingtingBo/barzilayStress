# Afgr Feb 2018

# This script will bbe used to find bin sizes for extreme tertile groups
# on the pathology factor scores in relation to the stress questionaire value

## Load the data
# Load all the data
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
stress.data <- read.csv('/data/joy/BBL/projects/barzilayStress/data/pncstressdatasetforadon.csv')
stress.data$StressBin <- stress.data$Cummulative_Stress_Load_No_Rape
stress.data$StressBin[which(stress.data$StressBin>=3)] <- 3
demo.data <- read.csv('/data/joy/BBL/studies/pnc/n1601_dataFreeze/demographics/n1601_demographics_go1_20161212.csv')
stress.data <- merge(stress.data, demo.data)

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
# Now loop through and get bins in each
outDat <- list()
index <- 1
for(q in pathVals){
  tmpDat <- calculateDeltaHiMeLo(t1.data, q)
  tmpDat <- tmpDat[-which(tmpDat$PathGroup==2),]
  tmpDat <- tmpDat[-which(tmpDat$StressBin==1),]
  tmpDat <- tmpDat[-which(tmpDat$StressBin==2),]
  tmpDat <- tmpDat[which(is.na(tmpDat$StressBin)=="FALSE"),]
  outVal <- table(tmpDat$StressBin, tmpDat$PathGroup)
  outDat[[index]] <- outVal
  index <- index+1
  # Now grab the age values
  ageVals <- summarySE(data=tmpDat, measurevar='ageAtScan1', groupvars=c('StressBin','PathGroup', 'sex'))
  outputCSV <- paste(q, ".csv", sep='')
  write.csv(ageVals, outputCSV, quote=F, row.names=F) 
}


