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

# Now find out which subjects don't have a sex, or imaging data we want
all.data <- all.data[complete.cases(all.data$sex),]
all.data <- all.data[complete.cases(all.data[,grep('mprage_jlf_vol_', names(all.data))]),]
all.data[,grep('mprage_jlf_vol_', names(all.data))] <- apply(all.data[,grep('mprage_jlf_vol_', names(all.data))], 2, function(x) regressOutAge(x, all.data$ageAtScan1, all.data$sex, all.data$race2))

# Now isolate the subjects we want to use
all.data.tu <- all.data[which(all.data$averageManualRating!=0 & all.data$healthExcludev2==0 & all.data$StressBin>=3),]

# Now remove our middle path group
all.data.tu <- all.data.tu[-which(all.data.tu$PathGroup==2),]

# Now create our binary outcome
all.data.tu$resilientOutcome <- 0
all.data.tu$resilientOutcome[which(all.data.tu$PathGroup==3)] <- 1

# Now grab the volume values
volume.data <- all.data.tu[,grep('mprage_jlf_vol_', names(all.data.tu))]

## Now attach our outcome
volume.data <- cbind(all.data.tu$resilientOutcome, volume.data)

## Now write this csv for TPOT
freeze <- volume.data
write.csv(volume.data, "~/forTpot.csv", quote=F, row.names=F)

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
folds <- createFolds(factor(volume.data[,1]), k=13)
outPred <- rep(NA, length(volume.data[,1]))
colnames(volume.data)[1] <- 'outcome'
volume.data$outcome <- factor(volume.data$outcome)
volume.data$outcome <- revalue(volume.data$outcome,c('0'='Path', '1'='NoPath'))
volume.data[,2:144] <- robustscale(volume.data[,2:144])$data
cost<-10^(-1:2)
gamma<- c(.5,1,2)
fitGrid <- expand.grid(cost)
colnames(fitGrid) <- 'C'
for(i in 1:length(folds)){
    index <- folds[[i]]
    training <- volume.data[-index,]
    testing <- volume.data[index,]
    trctrl <- trainControl(method="repeatedcv",number=10,repeats=3,classProbs=TRUE, summaryFunction=twoClassSummary)
    
    svm_Linear <- train(outcome~., data = training, method = "svmLinear",
      trControl=trctrl,metric="ROC",
      tuneLength = 10,tuneGrid = fitGrid)
    # Now see if we can get probabilities for these in the testing data set
    outPred[index] <- predict(svm_Linear, newdata=testing, type='prob')[,1]
}
roc(volume.data$outcome ~ outPred)
rocplot.single(pred=outPred, grp=binary.flip(as.numeric(volume.data$outcome)), title="Resilience Prediction")

# Now create  summary svm to see how different ROI's get weighted
sumSVM <- train(outcome~.,data=volume.data, method="svmLinear",trControl=trctrl,metric="ROC",tuneLength=10, tuneGrid=fitGrid)
