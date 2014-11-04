complete <- function(directory, id = 1:332) {
  
  files_list <- list.files(directory, full.names = TRUE)
  storage <- data.frame() 
  dat <- c() 
  nobs <- 0 
  out <- data.frame()
  j <- 1
  
  for(i in id) {
    storage <- read.csv(files_list[i])  
    nobs <- sum(complete.cases(storage))
    dat <- c(id[j], nobs)
    out <- rbind(out, dat)
    j <- j + 1
  }
  names(out) <- c("id", "nobs")
  out
}
