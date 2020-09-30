library(dplyr)

run_analysis <- function() {
  #download dataset resources
  download_dataset()
  
  #get all train information
  train_measures <- as_tibble(read.table(unzip("dataset.zip", "UCI HAR Dataset/train/X_train.txt")))
  train_activity <- as_tibble(read.table(unzip("dataset.zip", "UCI HAR Dataset/train/y_train.txt")))
  train_subject <- as_tibble(read.table(unzip("dataset.zip", "UCI HAR Dataset/train/subject_train.txt")))
  
  #get all test information
  test_measures <- as_tibble(read.table(unzip("dataset.zip", "UCI HAR Dataset/test/X_test.txt")))
  test_activity <- as_tibble(read.table(unzip("dataset.zip", "UCI HAR Dataset/test/y_test.txt")))
  test_subject <- as_tibble(read.table(unzip("dataset.zip", "UCI HAR Dataset/test/subject_test.txt")))
  
  #get features names
  features_names <- read.table(unzip("dataset.zip", "UCI HAR Dataset/features.txt"))
  features_names <- as.vector(features_names$V2)
  
  #merge test and train data frames
  complete_df <- bind_rows(train_measures, test_measures)
  
  #add colname using the content of the file "features.txt" and select only mean and standard deviation measures 
  complete_df <- complete_df %>% `colnames<-`(features_names) %>% select(grep(pattern="mean\\()|std", features_names))

  #renaming columns 
  current_names <- colnames(complete_df)
  current_names <- gsub("\\()", "", current_names)
  current_names <- sub(pattern = "^t", "time-", current_names)
  current_names <- sub(pattern = "^f", "frequency-", current_names)
  current_names <- sub(pattern = "std", "standard-deviation", current_names)
  current_names <- sub(pattern = "Acc", "-acceleration", current_names)
  current_names <- sub(pattern = "Gyro", "-gyroscope", current_names)
  current_names <- sub(pattern = "Jerk", "-jerk", current_names)
  current_names <- sub(pattern = "Mag", "-magnitude", current_names)
  current_names <- tolower(current_names)  
  colnames(complete_df) <- current_names
  
  #merge activity informations
  activity <- bind_rows(train_activity, test_activity)
  activity <-activity %>% mutate(V1 = case_when(V1 == 1 ~ "Walking",
                         V1 == 2 ~ "Walking up",
                         V1 == 3 ~ "Walking down",
                         V1 == 4 ~ "Sitting",
                         V1 == 5 ~ "Standing",
                         V1 == 6 ~ "Laying"))
  colnames(activity) <- "activities"

  #merge subjects
  subject <- bind_rows(train_subject, test_subject)
  colnames(subject) <- "subject"

  #merge activities and measures
  complete_df <- bind_cols(subject, activity, complete_df)
  
  average_dataframe(complete_df)
}

download_dataset <- function() {
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, destfile = "dataset.zip", method = "curl")
}

average_dataframe <- function(df) {
  #the average of each variable for each activity and each subject.
  new_df <- df %>%
    group_by(subject, activities) %>%
    summarize(across(.fns=mean))
  
  #save the new data frame into .txt file
  file_path <- file.path(getwd(), "/output.txt")
  write.table(new_df, file_path, col.names = TRUE )
}

read_output <- function() {
  #read the output file and show it in a nice way
  file_path <- file.path(getwd(), "/output.txt")
  data <- read.table(file_path, header = TRUE) 
  View(data)
}