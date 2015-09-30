## Finding the best hospital in US using open data
## For readability a lot of the statements are broken into multiple lines

best <- function(state, outcome) {
	## Initialize parameters we want to be sure are dataframes
	inputData <- data.frame()
	stateData <- data.frame()
	minRateData <- data.frame()

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

	
	## Subset data according to input parameter state
	stateData <- subset(inputData, inputData[,7]==state)	

	## Now StateData is subsetted with rows (in case there is more than one) containing
	## the minimum value for death rate, but first I'll get the minimum as a numeric.
	minDeathRate <- min(as.numeric(stateData[,colnum]), na.rm=TRUE)
	minRateData <- subset(stateData, as.numeric(stateData[,colnum]) == minDeathRate)

	## Order the minRateData after hospitalname so that we can return the
	## hospital with the name that comes first alphabetically
	minRateData[order(minRateData[,2])]

	## Now the first row contains the hospital name in that state with lowest 30-day death
	## rate and if there are more than one, the first in the alphabet
	minRateData[,2]
}