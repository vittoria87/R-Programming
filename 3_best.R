best <- function(state, outcome) {
        text <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.string = c("No data are available from the hospital for this measure", "Not Available"))
        sample <- c("heart attack", "heart failure", "pneumonia")

        if (state %in% text$State == FALSE) stop("invalid state")
        if (outcome %in% sample == FALSE) stop("invalid outcome")
 
        if (outcome == "heart attack")
        cols <- c(2, 11)
        if (outcome == "heart failure")
        cols <- c(2, 17) 
        if (outcome == "pneumonia")
        cols <- c(2, 23)

        sub <- data.frame()
        sub <- subset(text, State == state, cols) 
        min_result <- min(as.numeric(sub[, 2]), na.rm = TRUE)
        result <- sub[which(as.numeric(sub[, 2]) == min_result), 1]

        if (length(result) > 1) sort(result)[1]
        else result
        }
