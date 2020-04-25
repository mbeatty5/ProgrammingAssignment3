setwd("C:/Users/mbeatty/Desktop/HARD DRIVE/Other/Coursera/R Programming/Programming Assignment 3")

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")




rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        setwd("C:/Users/mbeatty/Desktop/HARD DRIVE/Other/Coursera/R Programming/Programming Assignment 3")
        outcome.data <- read.csv("outcome-of-care-measures.csv", 
                                 colClasses = "character")
        
        ## changing outcome to all lowercase to allow both cases
        outcome <- tolower(outcome)
        
        ## column name is same as variable, so changing it
        selected.state <- state
        
        ## Check that state and outcome are valid
        if(!selected.state %in% unique(outcome.data[["State"]])) {
                stop("invalid state")
        }
        
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                stop("invalid outcome")
        }
        
        
        ## sets column index vector
        column.index <- numeric()
        
        ## Return hospital name in that state with the given rank
        ## selects column index based on outcome
        if(outcome == "heart attack") {
                column.index <- 11
        }
        if(outcome == "heart failure") {
                column.index <- 17
        }
        if(outcome == "pneumonia") {
                column.index <- 23
        }
        
        ## If tied, sort hospitals alphabetically and select first off that
        outcome.data <- outcome.data[order(outcome.data$Hospital.Name, 
                                           na.last = TRUE, 
                                           decreasing = FALSE), ]
        ## Sets the desired column to be numeric vector, coercing NAs
        suppressWarnings(outcome.data[, column.index] <- as.numeric(outcome.data[, column.index]))
        ## Removes any NAs from final hospital rankings
        outcome.data[complete.cases(outcome.data[, column.index]),]
        

        ## subsets the outcome data to selected state
        outcome.data <- outcome.data[outcome.data$State == state,]
        ## orders the subsetted data based on selected outcome and state
        outcome.data <- outcome.data[order(outcome.data[, column.index]),]
        
        ## Return hospital name in that state with given rank
        if(num == "best") {
                return(outcome.data$Hospital.Name[1])
        }
        if(num == "worst") {
                return(outcome.data$Hospital.Name[nrow(outcome.data)])
        }
        if(num != "best" & num != "worst" & num <= nrow(outcome.data)) {
                return(outcome.data$Hospital.Name[num])
        }
        if(num > nrow(outcome.data)) {
                return("NA")
        }
        
        
}





