corr <- function(data_dir, threshold = 0) {
  
  ## "directory" is a character vector of length 1 indicating 
  ## the location of the CSV files

  ## "threshold" is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## notrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the results

  ## create filename extension pattern based upon passed parameter
  patt_filenames <- character()
  for (i in 1:332) {
    patt_filenames <- c(patt_filenames, sprintf("%s/%03d.csv", data_dir, i))
  }

  ## Read files into a list of data frames
  myfiles <- lapply(patt_filenames, read.csv)
  
  # Calculate correlation and store within return vector
  sn_cor <- numeric()
  for (i in 1:332) {
    # Remove rows with NA on the data frame
    myfiles[[i]] <- myfiles[[i]][rowSums(is.na(myfiles[[i]])) == 0,]
    if (nrow(myfiles[[i]]) >= threshold) {
      sn_cor[i] <- round(cor(myfiles[[i]][,2], myfiles[[i]][,3]), 5)
    } else {
      sn_cor[i] <- 0
    }
  }

  return(sn_cor)

}

# cr <- corr("~/R/Coursera examples/Week 2/rprog_data_specdata/Specdata", 150)
# head(cr)
# summary(cr)
# cr <- corr("~/R/Coursera examples/Week 2/rprog_data_specdata/Specdata", 400)
# head(cr)
# summary(cr)
# cr <- corr("~/R/Coursera examples/Week 2/rprog_data_specdata/Specdata", 5000)
# summary(cr)
# length(cr)
# cr <- corr("~/R/Coursera examples/Week 2/rprog_data_specdata/Specdata")
# summary(cr)
# length(cr)

cr <- corr("~/R/Coursera examples/Week 2/rprog_data_specdata/Specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)
