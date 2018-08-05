## AFGR May 2018

## This script will be used to prepare the color values for the classification for Ran Barzilay's
## resilience project
## It requires the output from the plotCVPlots.py script
## Also this sciprt is to be run on the local MBP -_-

## Load the functions
source('functions.R')

## Load the data
row.name <- read.csv('forTpot.csv')
select.percent <- read.csv('betaWeightsAllFolds.csv', header=F)
select.strength <- read.csv('betaWeightsFinal.csv', header=F)

## Prep the data
rownames(select.percent) <- colnames(row.name)[-1]
colnames(select.strength) <- colnames(row.name)[-1]

## Now prepare selection percentages
select.percent[select.percent!=0] <- 1
select.percent <- apply(select.percent, 1, sum)/500
outvals1 <- cbind(names(select.percent), select.percent)

## Now prepare the beta weight values
name.vals <- colnames(row.name)[-1][index]
index <- select.strength[1,]!=0
select.strength <- select.strength[1,select.strength[1,]!=0]
select.strength <- as.numeric(scale(t(select.strength)))
outvals2 <- cbind(name.vals, select.strength)

## Now export the color scales and keys
writeColorTableandKey(inputData=outvals1, inputColumn=2, outName='selectPercent', minTmp=c(-1, 0), maxTmp=c(0, 1))
writeColorTableandKey(inputData=outvals2, inputColumn=2, outName='selectStrength', minTmp=c(-3.5, 0), maxTmp=c(0, 2.6))
