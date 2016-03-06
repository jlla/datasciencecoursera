#JOÃO AMORIM
#R PROGRAMMING (2016)
#Assignment 3

# Write a function called rankhospital that takes three arguments: the 
# 2-character abbreviated name of a state (state), an outcome (outcome), and the
# ranking of a hospital in that state for that outcome (num). The function reads
# the outcome-of-care-measures.csv file and returns a character vector with the
# name of the hospital that has the ranking specified by the num argument.

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv")

        ## Check that state and outcome are valid (equal to function best)
        states <- levels(data[, 7])[data[, 7]]
        state_flag <- FALSE
        for (i in 1:length(states)) {
                if (state == states[i]) {
                        state_flag <- TRUE
                }
        }
        if (!state_flag) {
                stop ("invalid state")
        } 
        if (!((outcome == "heart attack") | (outcome == "heart failure")
              | (outcome == "pneumonia"))) {
                stop ("invalid outcome")
        }

        ## Return hospital name in that state with the given rank 30-day death 
        ## rate
        col <- if (outcome == "heart attack") {
                11
        } else if (outcome == "heart failure") {
                17
        } else {
                23
        }
        
        data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
        data[, 2] <- as.character(data[, 2])
        data_state <- data[grep(state, data$State), ]
        orderdata <- data_state[order(data_state[, col], data_state[, 2], na.last = NA), ]
        if(num == "best") {
                orderdata[1, 2]
        } else if(num == "worst") {
                orderdata[nrow(orderdata), 2]
        } else{
                orderdata[num, 2]
        }
}
