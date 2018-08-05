## AFGR April 2018
## This script will be used to train a svm to classify our resilient folks
## from our non resilient folks
## Resilience is defined as those that have experinced a high load of stressful events but have not
## gone on to develop high psychiatric pathological loading

## Source Adon's library
source('/home/arosen/adroseHelperScripts/R/afgrHelpFunc.R')
source('/home/arosen/jlfVisualizer/scripts/Rfunction/makeITKSnapColorTable.R')
source('./functions.R')
install_load('ggplot2','mgcv', 'pROC', 'caret', 'plyr')

## Source a function
robustscale <- function (data, dim = 2, center = TRUE, scale = TRUE)
{
    medians = NULL
    if (center) {
        medians <- apply(data, dim, median, na.rm = TRUE)
        data = sweep(data, dim, medians, "-")
    }
    mads = NULL
    if (scale) {
        mads <- apply(data, dim, mad, na.rm = TRUE)
        data = (sweep(data, dim, mads, "/"))
    }
    return(list(data = data, medians = medians, mads = mads))
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
all.data <- calculateDeltaHiMeLo(all.data, "Anxious_Misery_ar")
## Now create a histogram of these path values
tmpPlot <- ggplot(all.data, aes(x=all.data$Anxious_Misery_ar, fill=factor(PathGroup))) +
  geom_histogram(bins=200)
# Now create a histogram for those subjects that have 3+ TSE
tmpPlot2 <- ggplot(all.data[which(all.data$StressBin==3),], aes(x=Anxious_Misery_ar, fill=factor(PathGroup))) +
  geom_histogram(bins=50)


# Now find out which subjects don't have a sex, or imaging data we want
all.data <- all.data[complete.cases(all.data$sex),]
all.data <- all.data[complete.cases(all.data[,grep('mprage_jlf_vol_', names(all.data))]),]
all.data[,grep('mprage_jlf_vol_', names(all.data))] <- apply(all.data[,grep('mprage_jlf_vol_', names(all.data))], 2, function(x) regressOutAge(x, all.data$ageAtScan1, all.data$sex, all.data$race2))

# Now isolate the subjects we want to use
all.data.tu <- all.data[which(all.data$t1Exclude==0 & all.data$healthExcludev2==0 & all.data$StressBin>=3),]
#all.data.tu <- averageLeftAndRight(all.data.tu)
all.data.tu2 <- all.data
all.data.tu2$collapseVar <- paste(all.data.tu2$StressBin, all.data.tu2$PathGroup)
all.data.tu2 <- all.data.tu2[which(all.data.tu2$averageManualRating!=0 & all.data.tu2$healthExcludev2==0),]
all.data.tu3 <- all.data.tu2[which(all.data.tu2$collapseVar=="0 1" | all.data.tu2$collapseVar=="3 1" | all.data.tu2$collapseVar=="3 2" | all.data.tu2$collapseVar=="0 2"),]
all.data.tu2 <- all.data.tu2[which(all.data.tu2$collapseVar=="0 1" | all.data.tu2$collapseVar=="3 1"),]

# Now remove our middle path group
all.data.tu <- all.data.tu[-which(all.data.tu$PathGroup==2),]

# Now create our binary outcomes
all.data.tu$resilientOutcome <- 0
all.data.tu$resilientOutcome[which(all.data.tu$PathGroup==3)] <- 1
all.data.tu2$resilientOutcome <- 0 
all.data.tu2$resilientOutcome[which(all.data.tu2$StressBin!=0)] <- 1
all.data.tu3$outcome <- factor(all.data.tu3$collapseVar)

# Now grab the volume values
volume.data <- all.data.tu[,grep('mprage_jlf_vol_', names(all.data.tu))]
volume.data2 <- all.data.tu2[,grep('mprage_jlf_vol_', names(all.data.tu2))]
volume.data3 <- all.data.tu3[,grep('mprage_jlf_vol_', names(all.data.tu3))]

## Now attach our outcome
volume.data <- cbind(all.data.tu$resilientOutcome, volume.data)
volume.data2 <- cbind(all.data.tu2$resilientOutcome, volume.data2)
volume.data3 <- cbind(as.numeric(all.data.tu3$outcome), volume.data3)

## Now write this csv for TPOT
freeze <- volume.data
write.csv(volume.data, "~/forTpot.csv", quote=F, row.names=F)
write.csv(volume.data2, "~/forTpot2.csv", quote=F, row.names=F)
write.csv(volume.data3, "~/forTpot3.csv", quote=F, row.names=F)

## This isn't run on chead but here is the TPOT call:
## tpot forTpot.csv -is , -target y -mode classification -scoring auc_roc -v 2
## I believe the best class method will be an svm... will report back later

## First grab AUC from an roc curve for each of the individual regions
## SOme of these are pretty impressive
## L MFC is >.75!
## Largest AUC is .77 for the R PoG!
allAUC <- apply(volume.data[,-1], 2, function(x) auc(roc(volume.data[,1] ~ x)))

# Now make a color table for these univariate AUC values
allAUC <- cbind(names(allAUC), unname(allAUC))
writeColorTableandKey(inputData=allAUC, inputColumn=2, outName='rawAUC', maxTmp=c(.43,.78))
## Here is the shell call to create the image
# /home/arosen/hiLo/scripts/05_BrainRankFigure/scripts/makeZScoreJLFPNCTemplateImage.sh rawAUC-KEY.csv 4 1

## Now plot a histogram for our two groups and there vaolume values
colnames(volume.data)[1] <- 'outcome'
tmpDat1 <- volume.data[which(volume.data$outcome==0),]
tmpDat2 <- volume.data[which(volume.data$outcome==1),]
outPlot1 <- ggplot(volume.data) +
  geom_histogram(data=tmpDat2, aes(x=mprage_jlf_vol_R_PoG, fill='red')) +
  geom_histogram(data=tmpDat1, aes(x=mprage_jlf_vol_R_PoG, fill='blue')) +
  theme(legend.position="none") +
  xlab("Right Post Central Gyrus") +
  xlim(c(-3500,3500))
outPlot2 <- rocplot.single(pred=volume.data$mprage_jlf_vol_R_PoG, grp=as.numeric(volume.data$outcome), title="R PoG ROC")
pdf('univariatePred.pdf')
outPlot1
outPlot2
dev.off()

## 


## Now I am going to train a SVC to classify our resilient and non resilient groups
## I am going to do this in a 5 fold cv fashion?
## First thing I need to to do is declare my folds
volume.data <- freeze
folds <- createFolds(factor(volume.data[,1]), k=5)
outPred <- rep(NA, length(volume.data[,1]))
colnames(volume.data)[1] <- 'outcome'
volume.data$outcome <- factor(volume.data$outcome)
volume.data$outcome <- revalue(volume.data$outcome,c('0'='Path', '1'='NoPath'))
volume.data[,2:144] <- scale(volume.data[,2:144])
cost<-10^(-1:2)
gamma<- c(.5,1,2)
fitGrid <- expand.grid(cost)
colnames(fitGrid) <- 'C'
for(i in 1:length(folds)){
    index <- folds[[i]]
    training <- volume.data[-index,]
    testing <- volume.data[index,]
    trctrl <- trainControl(method="repeatedcv",number=10,repeats=3,classProbs=TRUE, summaryFunction=twoClassSummary)    
    #svm_Linear <- train(outcome~., data = training, method = "svmLinear",
    #  trControl=trctrl,metric="ROC",
    #  tuneLength = 10,tuneGrid = fitGrid)
    # Now use lasso to train a regression model
    optLam <- cv.glmnet(y=as.vector(training$outcome), x=as.matrix(training[,-1]), alpha=0.0001, family="binomial", parallel=F)
    lasModel1 <- glmnet(y=as.vector(training$outcome), x=as.matrix(training[,-1]), alpha=0.0001, lambda=optLam$lambda.min, family="binomial")
    outPred[index] <- predict(lasModel1, newx=as.matrix(testing[,-1]), type='response')
}
roc(volume.data$outcome ~ outPred)
rocplot.single(pred=outPred, grp=binary.flip(as.numeric(volume.data$outcome)), title="Resilience Prediction")

# Now create  summary svm to see how different ROI's get weighted
sumSVM <- train(outcome~.,data=volume.data, method="svmLinear",trControl=trctrl,metric="ROC",tuneLength=10, tuneGrid=fitGrid)
