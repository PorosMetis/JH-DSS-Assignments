complete <- function(data_dir, id = 1:332) {
  
  ## "directory" is a character vector of length 1 indicating 
  ## the location of the CSV files

  ## "id" is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id   nobs
  ## 1    117
  ## 2    1041
  ## . . .
  ## where 'id' is the monitor ID number and nobs is the
  ## number of complete cases

  ## create filename extension pattern based upon passed parameter
  patt_filenames <- character()
  for (i in id) {patt_filenames <- c(patt_filenames, sprintf("%s/%03d.csv", data_dir, i))}
  
  ## Create filenames list and read into single data frame
  myfiles <- do.call(rbind, lapply(patt_filenames, read.csv))

  ## Calculate number of complete observations
  myfiles_nona <- myfiles[complete.cases(myfiles),]
  myfn_agg <- aggregate(myfiles_nona, by = list(myfiles_nona$ID), FUN = length)

  ## Print final results
  names(myfn_agg) <- c("ID", "Date", "sulfate", "nitrate", "nobs")
  myfn_agg[,c(1,5)]
}

# complete("~/R/Coursera examples/Week 2/rprog_data_specdata/Specdata", c(2, 4, 8, 10, 12))
# complete("~/R/Coursera examples/Week 2/rprog_data_specdata/Specdata", 30:25)
# complete("~/R/Coursera examples/Week 2/rprog_data_specdata/Specdata", 3)

set.seed(42)
cc <- complete("~/R/Coursera examples/Week 2/rprog_data_specdata/Specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])
