##Installed tidyverse which includes dplyr, readr and lubridate as well as other useful packages nd markdowm to create the codebook 


install.packages("tidyverse")
library(tidyverse)

install.packages("rmarkdown")
library(rmarkdown)

# DI have downloaded the .zip file to a temp file to avoid cluttering my working dorectory. 

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destfile <- tempfile(fileext = ".zip")

# Once downloaded  and unzipped the files are ready for the working directory. 

download.file(url, destfile, mode = "wb")
unzip(destfile, exdir = "UCI_HAR_Dataset")

# A list of the the files enables me to know what I need to upload. 

list.files("UCI_HAR_Dataset", recursive = TRUE)

# I am loading the metadata to help get a better understanding of the data and what it contains and also use the data to label the variables in the data tables.  
features <- read.table("UCI_HAR_Dataset/UCI HAR Dataset/features.txt", col.names = c("index", "feature"))
activities <- read.table("UCI_HAR_Dataset/UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

# Training data
x_train <- read.table("UCI_HAR_Dataset/UCI HAR Dataset/train/X_train.txt", col.names = features$feature)
y_train <- read.table("UCI_HAR_Dataset/UCI HAR Dataset/train/y_train.txt", col.names = "activity_code")
subject_train <- read.table("UCI_HAR_Dataset/UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

# Test data
x_test <- read.table("UCI_HAR_Dataset/UCI HAR Dataset/test/X_test.txt", col.names = features$feature)
y_test <- read.table("UCI_HAR_Dataset/UCI HAR Dataset/test/y_test.txt", col.names = "activity_code")
subject_test <- read.table("UCI_HAR_Dataset/UCI HAR Dataset/test/subject_test.txt", col.names = "subject")

# Pre-merge cleaning:  Check column names to ensure  names are valid R column names (e.g., no duplicates or illegal characters).
make.names(features$feature, unique = TRUE)

# Confirmed  row counts to ensure they will bind. 
stopifnot(nrow(x_train) == nrow(y_train), nrow(y_train) == nrow(subject_train))

# Combined the  train and test sets
x_all <- rbind(x_train, x_test)
y_all <- rbind(y_train, y_test)
subject_all <- rbind(subject_train, subject_test)

# Merge everything
data_all <- cbind(subject_all, y_all, x_all)

# Used the  descriptive activity names in the meta data  to name the activities in the data set
data_all$activity <- factor(data_all$activity_code, levels = activities$code, labels = activities$activity)

#Fixed column names, some feature names contain parentheses, dashes, or commas. 
names(data_all) <- make.names(names(data_all), unique = TRUE)

View(data_all)

colnames(data_all)

# Moved `activity` up to be the third column (after `subject` and `activity_code`) to make it easier to read.

data_all <- data_all %>%
  relocate(activity, .after = activity_code)

# Changed the activity names to lower case also to make it easier for everyone to read.  

data_all$activity <- tolower(data_all$activity)

# Extracted only the measurements on the mean and standard deviation for each measurement. 
# Uses matches()  to grab any column names that contain "mean" or "std" (standard deviation)
# which aligned with the original feature naming convention from features.txt.

tidy_data <- data_all %>%
  select(subject, activity, matches("mean\\.|std\\."))

View(tidy_data)

# Created a second data set that
# grouped the data by each subject and each activity - group_by(subject, activity)
# calculates the average of each variable (i.e., each sensor feature) within those groups - summarise(across(everything(), mean)
# ungrouped the result, giving a clean, independent dataset - groups = "drop"

summary_data <- tidy_data %>%
  group_by(subject, activity) %>%
  summarise(across(everything(), mean), .groups = "drop")

#  Saved the new data sets in the  project folder

write.table(tidy_data, file = "tidy_data.txt", row.names = FALSE)
write.table(summary_data, file = "summary_data.txt", row.names = FALSE)



