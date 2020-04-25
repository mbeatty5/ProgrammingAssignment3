rankall <- function(outcome, num = "best") {
        ## Read outcome data
        setwd("C:/Users/mbeatty/Desktop/HARD DRIVE/Other/Coursera/R Programming/Programming Assignment 3")
        outcome.data <- read.csv("outcome-of-care-measures.csv", 
                                 colClasses = "character")
        
        ## changing outcome to all lowercase to allow both cases
        outcome <- tolower(outcome)
        
        ## Check that outcome is valid
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

        
        ## Creates new, smaller data frame
        out.dat <- as.data.frame(cbind(outcome.data$Hospital.Name, 
                                       outcome.data$State, 
                                       outcome.data[, column.index]))
        colnames(out.dat) <- c("hospital", 
                               "state", 
                               "mortalityrate")
        suppressWarnings(out.dat$mortalityrate <- as.numeric(as.character((out.dat$mortalityrate))))
        
        ## Splits the data frame by state
        out.dat.s <- split(out.dat, out.dat$state)
        ## Creates function to rank
        
        staterank <- function(out.dat) {
                #Make list of positions
                rank.list <- order(out.dat$mortalityrate, out.dat$hospital)
                
                #Check validity of num argument and assign numeric value
                if (num == "best")
                        num <- 1
                else if (num == "worst")
                        num <- length(rank.list)
                else if (!is.numeric(num))
                        stop("Unrecognized num argument")
                
                out.dat[rank.list[num], 1]
        }
        
        
        ranked.states <- data.frame(sapply(out.dat.s, staterank))
        ranked.states <- data.frame(ranked.states, 
                                    row.names(ranked.states))
        names(ranked.states) <- c("hospital", 
                                  "state")
        ranked.states
        
        
        ## For each state, find the hospital of the given rank
        
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        
}

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)




## TESTING

setwd("C:/Users/mbeatty/Desktop/HARD DRIVE/Other/Coursera/R Programming/Programming Assignment 3")
outcome.data <- read.csv("outcome-of-care-measures.csv", 
                         colClasses = "character")
## sets column index vector
column.index <- numeric()

outcome <- "heart attack"
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
outcome.data <- outcome.data[complete.cases(outcome.data[, column.index]),]
View(outcome.data)
## Creates new, smaller data frame
out.dat <- as.data.frame(cbind(outcome.data$Hospital.Name, 
                               outcome.data$State, 
                               outcome.data[, column.index]))
colnames(out.dat) <- c("hospital", 
                       "state", 
                       "mortalityrate")
suppressWarnings(out.dat$mortalityrate <- as.numeric(as.character((out.dat$mortalityrate))))
View(out.dat)

## Set empty vector for final result
final.ranking <- setNames(data.frame(matrix(ncol = 2, 
                                            nrow = 0)), 
                          c("hospital", "state"))

## Make the data frame with the selected ranking
num = "best"
if (num == "best"){
        return(out.dat[order(state, get(outcome), `hospital`)
                      , .(hospital = head(`hospital`, 1))
                      , by = state, ])
}


out.dat.s <- split(out.dat, out.dat$state)
View(out.dat.s)


staterank <- function(out.dat) {
        #Make list of positions
        rank.list <- order(out.dat$mortalityrate, out.dat$hospital)
        
        #Check validity of num argument and assign numeric value
        if (num == "best")
                num <- 1
        else if (num == "worst")
                num <- length(rank.list)
        else if (!is.numeric(num))
                stop("Unrecognized num argument")
        
        out.dat[rank.list[num], hospital]
}


ranked.states <- data.frame(sapply(out.dat.s, staterank))
ranked.states <- data.frame(ranked.states, row.names(ranked.states))
names(ranked.states) <- c("hospital", "state")
ranked.states