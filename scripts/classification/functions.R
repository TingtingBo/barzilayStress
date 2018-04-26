# Now I need to make a function which will regress out age from a column
regressOutAge <- function(valuesToBeRegressed, ageColumn, sexColumn){
    age <- scale(ageColumn)[,1]
    ageSqu <- scale(ageColumn)[,1]^2
    ageCub <- scale(ageColumn)[,1]^3
    newValues <- lm(valuesToBeRegressed ~ age + ageSqu + ageCub + sexColumn)$residuals
    return(newValues)
}

## Now build a function which will return the CV predicted SVM prob
## for existsing in s apsecific class


## Decalre a function which will return our path group based on tertiles
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
