rankall <- function(outcome, num = "best") {
text <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.string = c("No data are available from the hospital for this measure", "Not Available"))
outcome_sample <- c("heart attack", "heart failure", "pneumonia")
if (outcome %in% outcome_sample == FALSE) {stop("invalid outcome")}

        if (outcome == "heart attack")
        cols <- c(2, 7, 11)
        if (outcome == "heart failure")
        cols <- c(2, 7, 17) 
        if (outcome == "pneumonia")
        cols <- c(2, 7, 23)

        sub <- subset(text, , cols)
        substr <- subset(sub, subset=(sub[, 3] != "NA"))
        list <- split(substr, substr$State)
        result <- data.frame()

        for (i in (1:length(list))) {
            df <- data.frame(list[[i]][1], list[[i]][2], list[[i]][3])
            z <- order(as.numeric(df[, 3]), df[, 1])
            df <- df[z, ]

            if (is.numeric(num) == FALSE) {             
               if (num == "best") {
                  r <- data.frame(df[1, 1], df[1, 2])}
               if (num == "worst") {
                  r <- data.frame(df[nrow(df), 1], df[nrow(df), 2])} 
               }
            else {
                 if (num > nrow(df)) {r <- data.frame("NA", list[[i]][2][1,])}
                 else {r <- data.frame(df[num, 1], df[num, 2])}
                 }

            result <- rbind(result, r)
            }
        
        names(result) = c("hospital", "state")
        result
}
