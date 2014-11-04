rankhospital <- function(state, outcome, num = "best") {

text <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.string = c("No data are available from the hospital for this measure", "Not Available"))

outcome_sample <- c("heart attack", "heart failure", "pneumonia")
num_sample <- c("best", "worst")

if (state %in% text$State == FALSE) stop("invalid state")
if (outcome %in% outcome_sample == FALSE) stop("invalid outcome")

        if (outcome == "heart attack")
        cols <- c(2, 11)
        if (outcome == "heart failure")
        cols <- c(2, 17) 
        if (outcome == "pneumonia")
        cols <- c(2, 23)

        sub <- subset(text, State == state, cols)
        substr <- subset(sub, subset=(sub[, 2] != "NA"))
        z <- order(as.numeric(substr[, 2]), substr[, 1])
        substr <- substr[z, ]
        
        if (num > nrow(substr)) {
           "NA"}
        if (num == "best") {
           num <- 1    
           substr[num, 1] }                     
        if (num == "worst") {
           num <- nrow(substr)
           substr[num, 1] } 
        else
        substr[num, 1]                

}
