## The function reads the outcome-of-care-measures.csv file and returns
## a 2-column data frame containing the hospital in each state that has 
## the ranking specified in num.
## For readability a lot of the statements are broken into multiple lines

rankall <- function(outcome, num = "best") {
	## Initialize parameters we want to be sure are dataframes
	inputData <- data.frame()
	stateData <- data.frame()
	orderedData <- data.frame()
	outputData <- data.frame(matrix(0, ncol = 2, nrow = 54))
	colnames(outputData) <- c("hospital", "state")
	
	rank<-0
	numhosp<-0

	## Read the input data
	inputData <- read.csv("outcome-of-care-measures.csv", 
		na.strings = c("Not Available"), colClasses = "character")

	## Check that outcome are valid
	
	if (outcome == "heart attack")
		colnum<-11
	else if (outcome == "heart failure")
		colnum<-17
	else if (outcome == "pneumonia")
		colnum<-23
	else 
		stop("invalid outcome")

	## Create vector of states
	stateVec<-unique(inputData[, 7])

	## order the statevec alphabetically and assign to outputData 
	stateVec <- sort(stateVec)
	outputData$state <- stateVec

	## Use a for loop to create dataframe containing state and hospital
	## chosen from outcome and rank

	## initialize index into output dataframe outputData
	rownum<-1 

	for (i in stateVec) {
	
		## Subset data according to input parameter state
		stateData <- subset(inputData, inputData[,7]==i)	

		## Now stateData needs to be ordered by rank after mortality rates
		## but first mortality rates must be converted to numerics
		stateData[,colnum] <- as.numeric(stateData[,colnum])

		## Order the rankedData after rate and hospitalname so that we can return the
		## hospital with the name that comes first alphabetically
		orderedData <- stateData[order(stateData[,colnum],stateData[,2]),]

		## get the number of hospitals in state
		numhosp <- length(orderedData[,2])

		if (num == "best"){
			outputData$hospital[rownum] <- orderedData[1,2]
			outputData$state[rownum] <- orderedData[1,7]
		}
		else if (num == "worst"){
			orderedData <- stateData[order(stateData[,colnum], stateData[,2], decreasing = TRUE),]
			outputData$hospital[rownum] <- orderedData[1,2]
			outputData$state[rownum] <- orderedData[1,7]
		}
		else if ((num > 0) & (num <= numhosp)){
			outputData$hospital[rownum] <- orderedData[num,2]
			outputData$state[rownum] <- orderedData[num,7]
		}
		else { 
			outputData$hospital[rownum] <- NA
			outputData$state[rownum] <- i
		}
		rownum<-rownum+1
	}	
	outputData
}