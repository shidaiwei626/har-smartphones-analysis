
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(plotly))

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
  
  if(!dir.exists(paste0(Config$data_path,'dataset'))){
    unzip(file, exdir = Config$data_path)
    file.rename(paste0(Config$data_path,'UCI HAR Dataset'), paste0(Config$data_path,'dataset'))
  } 
  paste0('Dataset downloaded and unzipped in folder: ', Config$data_path,'dataset')
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
  
  feature_labels <-feature_list[,"name"]
  feature_labels <- HAR.describeFeatures(feature_labels)
  
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


#' 2. Extracts only the measurements on the mean and standard deviation for each measurement.
#' @param data dataset used to extract the fields
HAR.extractMeanAndStdFields <- function(data){
  data[,grepl('mean|std|subject|activity', names(data))]
}

#' 4. Appropriately labels the data set with descriptive variable names.
#' @param feature_labes the names of the dataframes to be describe
HAR.describeFeatures <- function(feature_labels){
  feature_labels <- gsub('\\(\\)', '',  feature_labels)
  feature_labels <- gsub('\\)', '',  feature_labels)
  feature_labels <- gsub(',', '-',  feature_labels)
  feature_labels <- gsub('\\(', '-',  feature_labels)
  feature_labels <- gsub("^(t)","time-",feature_labels)
  feature_labels <- gsub("^(f)","freq-",feature_labels)
  #feature_labels <- gsub("([Gg]ravity)","gravity",feature_labels)
  #feature_labels <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","body",feature_labels)
  #feature_labels <- gsub("[Gg]yro","gyro",feature_labels)
  #feature_labels <- gsub("AccMag","acc_magnitude",feature_labels)
  #feature_labels <- gsub("([Bb]odyaccjerkmag)","body_acc_jerk_magnitude",feature_labels)
  #feature_labels <- gsub("JerkMag","jerk_magnitude",feature_labels)
  #feature_labels <- gsub("GyroMag","gyro_magnitude",feature_labels)
  #feature_labels <- gsub('-', '_',  feature_labels)
  feature_labels
}

#' 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#' @param data the dataset to be summarized
HAR.calcAvg <- function(data){
  tbl_df(data) %>%
    group_by(activity, subject) %>%
    summarise_each(funs(mean), -subject, -activity, -activity_id) %>%
    gather(measurement, mean, -subject, -activity)
}

#' Save the data into the file
#' @param data the dataset to be saved
#' @param filename the filename where the dataset to be ssaved
HAR.saveData <- function(data, filename="tidy_data_avgs.txt"){
  write.table(data, file=filename, row.name=FALSE)
}

#' plot the averages dataset
#' @param data the dataset to be ploted
HAR.plotAvgs <- function(data){
  # data <- as.data.frame(avgs_dataset)
  p <- ggplot(data, aes(x=measurement, y=mean)) +
    geom_point(col=data$subject, alpha=0.2, size=1) +
    facet_grid(data$activity ~ .) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  p
  # ggplotly(p)
}