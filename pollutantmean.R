pollutantmean <- function(data_dir, pollutant, id = 1:332) {
  
  ## "directory" is a character vector of length 1 indicating 
  ## the location of the CSV files
  
  ## "pollutant" is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulphate" or "nitrate"
  
  ## "id" is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the "id" vector (ignoring NA values)
  ## NOTE: Do not round the result!

  ## create filename extension pattern based upon passed parameter
  patt_filenames <- character()
  for (i in id) {patt_filenames <- c(patt_filenames, sprintf("%s/%03d.csv", data_dir, i))}
  
  ## Create filenames list and read into single data frame
  myfiles <- do.call(rbind, lapply(patt_filenames, read.csv))
  
  ## Calculate mean and print
  pollutant_subset_mean <- mean(myfiles[[pollutant]], na.rm = TRUE)
  format(round(pollutant_subset_mean, 3), nsmall = 3)
}

pollutantmean("~/R/Coursera examples/Week 2/rprog_data_specdata/Specdata", "sulfate", 1:10)
pollutantmean("~/R/Coursera examples/Week 2/rprog_data_specdata/Specdata", "nitrate", 70:72)
pollutantmean("~/R/Coursera examples/Week 2/rprog_data_specdata/Specdata", "nitrate")