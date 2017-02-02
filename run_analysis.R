
# library()

HAR.Config.url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
HAR.Config.data_path = "./data/"
HAR.Config.file_zipped <- 'dataset.zip'

setwd(".")

#' Download e unzip data files
#' @param config Configuration settings
#'
HAR.download <-function(Config){
  file = paste0(Config.data_path, Config.file_zipped)
  if(!file.exists(file)){
    if(!dir.exists(Config.data_path)){
      dir.create(Config.data_path)
    }
    download.file(Config.url, file)
  }
  unzip(file, exdir = Config.data_path)
  file.rename(paste0(Config.data_path,'UCI HAR Dataset'), paste0(Config.data_path,'dataset'))
}


#' Load dataset
#' @param config Configuration settings
#' @param dataset train|test
#'
HAR.load <- function(Config, dataset = "train"){
  x <- read.table(paste0(Config.data_path, 'dataset/', dataset , '/X_' , dataset , '.txt'))
  y <- read.table(paste0(Config.data_path, 'dataset/', dataset , '/y_' , dataset , '.txt'), col.names=c("activity"))
  sub <- read.table(paste0(Config.data_path, 'dataset/', dataset , '/subject_' , dataset , '.txt'), col.names=c("subject"))
  data <- cbind(sub, x, y)
  data
}

#' 1. Merges the training and the test sets to create one data set.
#' 

HAR.loadAndMerge <- function(Config){
  train_data <- HAR.load(Config, 'train')
  test_data <- HAR.load(Config, 'test')
  data <- rbind(train_data, test_data)
  data
}


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# 3. Uses descriptive activity names to name the activities in the data set

# 4. Appropriately labels the data set with descriptive variable names.

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.