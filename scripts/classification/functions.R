# Now I need to make a function which will regress out age from a column
regressOutAge <- function(valuesToBeRegressed, ageColumn, sexColumn, raceColumn){
    age <- scale(ageColumn)[,1]
    ageSqu <- scale(ageColumn)[,1]^2
    ageCub <- scale(ageColumn)[,1]^3
    newValues <- lm(valuesToBeRegressed ~ age + ageSqu + ageCub + sexColumn + factor(raceColumn))$residuals
    return(newValues)
}

## Now build a function which will return the CV predicted SVM prob
## for existsing in s apsecific class


## Decalre a function which will return our path group based on tertiles
calculateDeltaHiMeLo <- function(data, facScore) {
    colVal <- grep(paste('^', facScore, sep=''), names(data))
    
    quantiles <- quantile(data[,colVal], c(0,.34,.666,1), na.rm=T)
    
    data$PathGroup <- NA
    data$PathGroup[which(data[,colVal] < quantiles[2])] <- 1
    data$PathGroup[which(data[,colVal] >= quantiles[2] &
    data[,colVal] < quantiles[3])] <- 2
    data$PathGroup[which(data[,colVal] > quantiles[3])] <- 3
    
    output <- data
    return(output)
}

## Now create some ROC plot functions
rocdata <- function(grp, pred){
    # Produces x and y co-ordinates for ROC curve plot
    # Arguments: grp - labels classifying subject status
    #            pred - values of each observation
    # Output: List with 2 components:
    #         roc = data.frame with x and y co-ordinates of plot
    #         stats = data.frame containing: area under ROC curve, p value, upper and lower 95% confidence interval
    
    grp <- as.factor(grp)
    if (length(pred) != length(grp)) {
        stop("The number of classifiers must match the number of data points")
    }
    
    if (length(levels(grp)) != 2) {
        stop("There must only be 2 values for the classifier")
    }
    
    cut <- unique(pred)
    tp <- sapply(cut, function(x) length(which(pred > x & grp == levels(grp)[2])))
    fn <- sapply(cut, function(x) length(which(pred < x & grp == levels(grp)[2])))
    fp <- sapply(cut, function(x) length(which(pred > x & grp == levels(grp)[1])))
    tn <- sapply(cut, function(x) length(which(pred < x & grp == levels(grp)[1])))
    tpr <- tp / (tp + fn)
    fpr <- fp / (fp + tn)
    roc = data.frame(x = fpr, y = tpr)
    roc <- roc[order(roc$x, roc$y),]
    
    i <- 2:nrow(roc)
    auc <- (roc$x[i] - roc$x[i - 1]) %*% (roc$y[i] + roc$y[i - 1])/2
    
    pos <- pred[grp == levels(grp)[2]]
    neg <- pred[grp == levels(grp)[1]]
    q1 <- auc/(2-auc)
    q2 <- (2*auc^2)/(1+auc)
    se.auc <- sqrt(((auc * (1 - auc)) + ((length(pos) -1)*(q1 - auc^2)) + ((length(neg) -1)*(q2 - auc^2)))/(length(pos)*length(neg)))
    ci.upper <- auc + (se.auc * 0.96)
    ci.lower <- auc - (se.auc * 0.96)
    
    se.auc.null <- sqrt((1 + length(pos) + length(neg))/(12*length(pos)*length(neg)))
    z <- (auc - 0.5)/se.auc.null
    p <- 2*pnorm(-abs(z))
    
    stats <- data.frame (auc = auc,
    p.value = p,
    ci.upper = ci.upper,
    ci.lower = ci.lower
    )
    
    return (list(roc = roc, stats = stats))
}

rocplot.single <- function(grp, pred, title = "ROC Plot", p.value = FALSE){
    require(ggplot2)
    plotdata <- rocdata(grp, pred)
    
    p <- ggplot(plotdata$roc, aes(x = x, y = y)) +
    geom_line(aes(colour = ""), size=3) +
    geom_abline (intercept = 0, slope = 1) +
    theme_bw() +
    scale_x_continuous("1-Specificity") +
    scale_y_continuous("Sensitivity") +
    scale_colour_manual(values = "#000000") +
    ggtitle(title) +
    theme_bw() +
    theme(legend.position=c(1,0)) +
    theme(legend.justification=c(1,0)) +
    theme(legend.title=element_blank(),
    text = element_text(size=30)) + theme(legend.position="none")
    
    return(p)
}


binary.flip <- function(x)
{
    x*-1 + 1
}

## Now add the JLF visualizer functions down here
returnHeatMapITKSnapVals <- function(inputZScores, lowColor='blue', hiColor='red', minValue=NULL, maxValue=NULL){
    # Create some functions this function will call... yeesh
    range01 <- function(x, minVal=NULL, maxVal=NULL){
        # Now make sure we have some standard deviation
        if(is.na(sd(x))){
            output <- rep(1, length(x))
            return(output)
        }
        if (sd(x)==0 ){
            output <- rep(1, length(x))
            return(output)
        }
        # Now see if we are provided a max and min val
        diffFlag <- 1
        bothCheck <- 2
        if(identical(NULL, minVal)){
            minVal <- min(x)
            bothCheck <- bothCheck - 1
            diffFlag <- 0
        }
        if(identical(NULL, maxVal)){
            print('foo')
            maxVal <- max(x)
            bothCheck <- bothCheck - 1
            diffFlag <- 0
        }
        if(diffFlag==0 & bothCheck!=0){
            print(diffFlag)
            print(bothCheck)
            warning("Only one min or max value provided use caution!!")
        }
        (x-minVal)/diff(c(minVal, maxVal))
    }
    cRamp <- function(x){
        cols <- colorRamp(c(lowColor, hiColor))(range01(as.numeric(x), minVal=minValue, maxVal=maxValue))
    }
    # Output values
    outputValues <- matrix(0, nrow=(length(inputZScores)+1), ncol=8)
    
    # Now cretae our rgb values
    redValues <- round(cRamp(inputZScores)[,1], digits=0)
    greenValues <- round(cRamp(inputZScores)[,2], digits=0)
    blueValues <- round(cRamp(inputZScores)[,3], digits=0)
    
    # Now create our index column
    outputValues[,1] <- seq(0, length(inputZScores))
    
    # Now put the proper values in the correct place
    outputValues[2:(length(inputZScores)+1),2] <- redValues
    outputValues[2:(length(inputZScores)+1),3] <- greenValues
    outputValues[2:(length(inputZScores)+1),4] <- blueValues
    
    # Now we need to do the Transperancy column
    outputValues[,5] <- c(0, rep(1, length(inputZScores)))
    
    # Now the visibility column
    outputValues[,6] <- c(0, rep(1, length(inputZScores)))
    
    # Now the mesh visibility
    outputValues[,7] <- c(0, rep(1, length(inputZScores)))
    
    # Now the label indicies
    labelIndexNames <- c('Clear Label', paste('Label ', inputZScores, sep=''))
    labelIndexNames <- paste('"', labelIndexNames, '"', sep='')
    outputValues[,8] <- labelIndexNames
    
    # Now return our output
    return(outputValues)
}

returnPosNegAndNeuColorScale <- function(outputZScores, colorScaleNeg=c('blue', 'light blue'), colorScalePos=c('red', 'yellow'), colorScaleNeu=c('gray'), sigThreshold=.05, minimum=NULL, maximum=NULL){
    # MAKE SURE WE ARE DEALING WITH NUMERICS!!!!
    outputZScores <- as.numeric(as.character(outputZScores))
    
    # First convert our sig threshold into a z score to find our cut off value
    cutOff <- abs(qnorm(sigThreshold))
    
    # Now we need to make our seperate our data into neutral, positive, and negative values
    # We are going to order these just so it is easier to match the labesl to the output ROI
    # when working with the ouput of this function
    negativeValues <- outputZScores[which(outputZScores < 0)]
    negativeValues <- negativeValues[order(negativeValues)]
    if(identical(NULL, minimum)){
        minimum <- range(negativeValues)
    }
    positiveValues <- outputZScores[which(outputZScores >= 0)]
    positiveValues <- positiveValues[order(positiveValues)]
    if(identical(NULL, maximum)){
        maximum <- range(positiveValues)
    }
    # Create our blank label row first
    values <- rep(0, 7)
    blankRow <- append(values, paste('"', 'Clear Label' ,'"', sep=''))
    
    # Now we need to create our individual color scales
    #startPoint <- NULL
    output <- blankRow
    if(length(negativeValues) > 0 ){
        negativeColors <- returnHeatMapITKSnapVals(negativeValues, lowColor=colorScaleNeg, hiColor='dark blue', minValue=minimum[1], maxValue=minimum[2])[2:(length(negativeValues)+1),]
        #negIndex <- max(as.numeric(as.character(negativeColors[,1])))
        #startPoint <- cbind(startPoint, negIndex)
        output <- rbind(output, negativeColors)
    }
    if(length(positiveValues) > 0 ){
        positiveColors <- returnHeatMapITKSnapVals(positiveValues, lowColor='red', hiColor=colorScalePos, minVal=maximum[1], maxVal=maximum[2])[2:(length(positiveValues)+1),]
        #posIndex <- max(as.numeric(as.character(positiveColors[,1])))
        #startPoint <- cbind(startPoint, posIndex)
        output <- rbind(output, positiveColors)
    }
    # Now I need to make sure that the index column doesn't have any repeats
    # This will be done by running an an index thorugh the first column
    output[,1] <- seq(0, length(outputZScores))
    
    # Now we are all set! just need to return our output
    return(output)
}

# Declare a function to write the table and key
writeColorTableandKey <- function(inputData, inputColumn, outName, minTmp=NULL, maxTmp=NULL){
    # First create the color table
    tmpColorTable <- returnPosNegAndNeuColorScale(inputData[complete.cases(inputData[,inputColumn]),inputColumn], colorScaleNeg=c('light blue'), colorScalePos=c('orange', 'yellow', 'white'), sigThreshold=1, minimum=minTmp, maximum=maxTmp)
    valuesToBind <- c('1616', '190', '190', '190', '0.40', '1', '1', '"Label Nonsense"')
    
    # Now produce the output key
    tmpOutputKey <- matrix(NA, nrow=dim(tmpColorTable)[1]-1, ncol=3)
    tmpOutputKey[,1] <- as.character(inputData[complete.cases(inputData[,inputColumn]),1])
    tmpOutputKey[,2] <- as.character(inputData[complete.cases(inputData[,inputColumn]),inputColumn])
    tmpOutputKey <- tmpOutputKey[order(as.numeric(tmpOutputKey[,2])),]
    tmpOutputKey[,3] <- rev(seq(dim(tmpColorTable)[1]-1, 1, -1))
    tmpOutputKeyFlip <- tmpOutputKey
    
    # Now write the tables
    outCTName <- paste(outName, '-ColorTable.txt', sep='')
    outKeyName <- paste(outName, '-KEY.csv', sep='')
    tmpColorTable <- rbind(tmpColorTable, valuesToBind)
    write.table(tmpColorTable, file=outCTName, sep="\t", quote=F, row.names=F, col.names=F)
    tmpOutputKey <- rbind(tmpOutputKey, tmpOutputKeyFlip)
    tmpOutputKey[,1] <- rmLatVal(tmpOutputKey[,1])
    tmpOutputKey <- tmpOutputKey[!duplicated(tmpOutputKey[,1]),]
    write.csv(tmpOutputKey, file=outKeyName, quote=F)
}


# Now create a function to remove non lateral ROI's lateral key
rmLatVal <- function(inputROIName){
    nonLateralROIVals <- c("3rd_Ventricle", "4th_Ventricle", "Brain_Stem", "CSF", "Cerebellar_Vermal_Lobules")
    # Now find and rm the R_ and L_ identifiers from the provided list of ROI's
    indexToMod <- NULL
    for(i in nonLateralROIVals){
        val <- grep(i, inputROIName)
        indexToMod <- append(indexToMod, val)
    }
    
    # Now modify the values
    tmpVals <- inputROIName[indexToMod]
    vals <- gsub(x=tmpVals, pattern="R_", replacement="")
    vals <- gsub(x=vals, pattern="L_", replacement="")
    output <- inputROIName
    output[indexToMod] <- vals
    return(output)
}
