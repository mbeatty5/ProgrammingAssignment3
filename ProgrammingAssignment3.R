setwd("C:/Users/mbeatty/Desktop/HARD DRIVE/Other/Coursera/R Programming/Programming Assignment 3")

outcome <- read.csv("outcome-of-care-measures.csv", 
                    colClasses = "character")
str(outcome)
View(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])
outcome <- "heart failure"
outcome.data <- read.csv("outcome-of-care-measures.csv", 
                         colClasses = "character")
column.index <- numeric()
if(outcome == "heart failure") {
        column.index <- 11
}
if(outcome == "heart attack") {
        column.index <- 17
}
if(outcome == "pneumonia") {
        column.index <- 23
}
if(outcome != "heart failure" &
   outcome != "heart attack" &
   outcome != "pneumonia") {
        stop("invalid outcome")
}
column.index
outcome.data2 <- outcome.data[order(outcome.data$Hospital.Name, 
                                    na.last = TRUE, 
                                    decreasing = FALSE), 
                              ]
View(outcome.data2)
outcome.data2[, 
              column.index] <- as.numeric(outcome.data2[, column.index])
outcome.data3 <- outcome.data2[complete.cases(outcome.data2[, column.index]),]
View(outcome.data3)

state.abb
str(state.abb)
state <- "CO"
if(state %in% state.abb) {
        state <- state
} else {
        stop("invalid state")
}

View(outcome.data3)
outcome.data4 <- outcome.data3[outcome.data3$State == state,]
outcome.data5 <- outcome.data4[order(outcome.data4[, column.index]),]
View(outcome.data5)
outcome.data5$Hospital.Name[1]



best <- function(state, outcome) {
        ## Set working drive
        setwd("C:/Users/mbeatty/Desktop/HARD DRIVE/Other/Coursera/R Programming/Programming Assignment 3")
        ## Read outcome data
        outcome.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        column.index <- numeric() ## sets column index vector
        
        if(!state %in% state.abb) {
                stop("invalid state")
        }
        
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                stop("invalid outcome")
        }
        
        ## selects column index based on outcome
        ## throws error message if outcome is invalid
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
        outcome.data <- outcome.data[order(outcome.data$Hospital.Name, na.last = TRUE, decreasing = FALSE), ]
        ## Sets the desired column to be numeric vector, coercing NAs
        suppressWarnings(outcome.data[, column.index] <- as.numeric(outcome.data[, column.index]))
        ## Removes any NAs from final hospital rankings
        outcome.data[complete.cases(outcome.data[, column.index]),]
        
        
        ## Check that state is valid
        ## Throw errors via the stop function with exact message
        
        ## Return hospital name in that state with lowest 30day death rate
        ## subsets the outcome data to selected state
        outcome.data <- outcome.data[outcome.data$State == state,]
        ## orders the subsetted data based on selected outcome and state
        outcome.data <- outcome.data[order(outcome.data[, column.index]),]
        outcome.data$Hospital.Name[1]
}
        




best.other.submission <- function(state, outcome) {
        
        # Read outcome data
        out_dt <- data.table::fread('outcome-of-care-measures.csv')
        
        outcome <- tolower(outcome)
        
        # Column name is same as variable so changing it 
        chosen_state <- state 
        
        # Check that state and outcome are valid
        if (!chosen_state %in% unique(out_dt[["State"]])) {
                stop('invalid state')
        }
        
        if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                stop('invalid outcome')
        }
        
        # Renaming Columns to be less verbose and lowercase
        setnames(out_dt, 
                 tolower(sapply(colnames(out_dt), 
                                gsub, 
                                pattern = "^Hospital 30-Day Death \\(Mortality\\) Rates from ", 
                                replacement = "" ))
        )
        
        #Filter by state
        out_dt <- out_dt[state == chosen_state]
        
        # Columns indices to keep
        col_indices <- grep(paste0("hospital name|state|^",outcome), 
                            colnames(out_dt))
        
        # Filtering out unnessecary data 
        out_dt <- out_dt[, .SD ,.SDcols = col_indices]
        
        # Find out what class each column is 
        # sapply(out_dt,class)
        out_dt[, outcome] <- out_dt[,  as.numeric(get(outcome))]
        
        
        # Removing Missing Values for numerical datatype (outcome column)
        out_dt <- out_dt[complete.cases(out_dt),]
        
        # Order Column to Top 
        out_dt <- out_dt[order(get(outcome), `hospital name`)]
        
        return(out_dt[, "hospital name"][1])
        
}
