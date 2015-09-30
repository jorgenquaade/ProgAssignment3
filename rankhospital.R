## Rank hospitals according to deathrate and return specific rank
## For readability a lot of the statements are broken into multiple lines

rankhospital <- function(state, outcome, num = "best") {
	## Initialize parameters we want to be sure are dataframes
	inputData <- data.frame()
	stateData <- data.frame()
	rankedData <- data.frame()
	outputdata<-data.frame()

	rank<-0
	numhosp<-0

	## Read the input data
	inputData <- read.csv("outcome-of-care-measures.csv", 
		na.strings = c("Not Available"), colClasses = "character")

	## Check that state and outcome are valid
	if (!state %in% inputData[,7]) stop("invalid state")

	if (outcome == "heart attack")
		colnum<-11
	else if (outcome == "heart failure")
		colnum<-17
	else if (outcome == "pneumonia")
		colnum<-23
	else 
		stop("invalid outcome")

	## get the number of hospitals in state
	
	

	## Subset data according to input parameter state
	stateData <- subset(inputData, inputData[,7]==state)	

	## Now stateData needs to be ordered by rank after mortality rates
	## but first mortality rates must be converted to numerics and NA's removed
	## Using <=100 is to cheat subset into accepting a logical statement since
	## rate cannot exceed 100 percent	
	stateData[,colnum] <- as.numeric(stateData[,colnum])

	## Order the rankedData after rate and hospitalname so that we can return the
	## hospital with the name that comes first alphabetically
	outputData <- stateData[order(stateData[,colnum],stateData[,2]),]

	numhosp <- length(outputData[,2])

	if (num == "best"){
		outputData <- outputData[1,2]
	}
	else if (num == "worst"){
		outputData <- stateData[order(stateData[,colnum], stateData[,2], decreasing = TRUE),]
		outputData <- outputData[1,2]
	}
	else if ( num > 0 & num <= numhosp){
		rank <- num
		outputData<-outputData[rank,2]
	}
	else 
		outputData<-NA
	
	outputData
}