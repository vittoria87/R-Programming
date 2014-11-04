corr <- function(directory, threshold = 0) {
  files_list <- list.files(directory, full.names = TRUE)
  result <- numeric()
  cors <- numeric()
  dat <- data.frame()
  
  for (i in 1:332) {
    dat <- read.csv(files_list[i])
    cc <- complete.cases(dat)
    nobs <- sum(cc)
    if (nobs > threshold) {
      cors <- cor(dat$nitrate, dat$sulfate, use = "pairwise.complete.obs")
    }
    else cors <- c()
    result <- c(result, cors)
  }
  result             
}
