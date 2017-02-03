
# library()

# Set the default configuration settings
HAR.Config <- c()
HAR.Config$url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
HAR.Config$data_path <- "./data/"
HAR.Config$file_zipped <- 'dataset.zip'

#' Download e unzip data files
#' @param config Configuration settings
#' @example HAR.download() 
#'
HAR.download <-function(Config = HAR.Config){
  file = paste0(Config$data_path, Config$file_zipped)
  if(!file.exists(file)){
    if(!dir.exists(Config$data_path)){
      dir.create(Config$data_path)
    }
    download.file(Config$url, file)
  }
  unzip(file, exdir = Config$data_path)
  file.rename(paste0(Config$data_path,'UCI HAR Dataset'), paste0(Config$data_path,'dataset'))
}


#' Load dataset
#' @param config Configuration settings
#' @param dataset train|test
#' @example HAR.load(dataset="test")
#' 
HAR.load <- function(dataset = "train", Config = HAR.Config){
  if(!file.exists(paste0(Config$data_path, 'dataset/features.txt'))){
    HAR.download(Config)
  }
  feature_list <- read.table(paste0(Config$data_path, 'dataset/features.txt'), col.names = c("id", "name"))
  activity_labels <- read.table(paste0(Config$data_path, 'dataset/activity_labels.txt'), col.names = c("activity_id", "activity"))
  
  # clean features names
  feature_labels <-feature_list[,"name"]
  feature_labels <- gsub('\\(\\)', '',  feature_labels)
  feature_labels <- gsub('\\)', '',  feature_labels)
  feature_labels <- gsub(',', '-',  feature_labels)
  feature_labels <- gsub('\\(', '-',  feature_labels)
  
  # 4. Appropriately labels the data set with descriptive variable names.
  x <- read.table(paste0(Config$data_path, 'dataset/', dataset , '/X_' , dataset , '.txt'), col.names=feature_labels)
  y <- read.table(paste0(Config$data_path, 'dataset/', dataset , '/y_' , dataset , '.txt'), col.names=c("activity_id"))
  
  # 3. Uses descriptive activity names to name the activities in the data set
  y <- merge(y, activity_labels, by.x = "activity_id", by.y ="activity_id", all.x=TRUE)
  
  sub <- read.table(paste0(Config$data_path, 'dataset/', dataset , '/subject_' , dataset , '.txt'), col.names=c("subject"))
  
  data <- cbind(x, sub, y)
  data
}

#' 1. Merges the training and the test sets to create one data set.
#' @param Config configuration settings
#' @example HAR.loadAndMerge()
#' 
HAR.loadAndMerge <- function(Config = HAR.Config){
  train_data <- HAR.load(dataset = 'train', Config = Config)
  test_data <- HAR.load(dataset = 'test', Config = Config)
  data <- rbind(train_data, test_data)
  data
}


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
HAR.extractMeanAndStdFields <- function(datas){
  data[,grepl('mean|std|subject|activity', names(data))]
}

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
HAR.calcAvg <- function(data){
  
}