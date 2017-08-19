### Generates a tidy data set for the final project of Getting and Cleaning Data 
### course

## Load dplyr package
library(dplyr)

## 1. 
## Read files for the test data set
subject <- read.table("./UCI HAR Dataset/test/subject_test.txt")
y <- read.table("./UCI HAR Dataset/test/y_test.txt")
x <- read.table("./UCI HAR Dataset/test/X_test.txt")
## Build test dataset
test <- cbind("subject" = subject$V1, "activity" = y$V1, x)


## Read files for the training data set
subject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
y <- read.table("./UCI HAR Dataset/train/y_train.txt")
x <- read.table("./UCI HAR Dataset/train/X_train.txt")
## Build train dataset
train <- cbind("subject" = subject$V1, "activity" = y$V1, x)

## Merge (join) test and train data set
fullData <- rbind(test, train)


## 2.
## Read the file with variables (columns) names
varNames <- read.table("./UCI HAR Dataset/features.txt", stringsAsFactors = FALSE, row.names = 1)[,1]
## Select ids of those that contain "mean()" or "std()"
cids <- grep("mean\\(\\)|std\\(\\)", varNames)

## Select the specified columns from the fullData set
Data <- select(fullData, subject, activity, cids + 2)


## 3.
## Rename activity rows
actNames <- read.table("./UCI HAR Dataset/activity_labels.txt", stringsAsFactors = FALSE, row.names = 1)[,1]
Data$activity <- sapply(Data$activity, function(x) actNames[x])


## 4.
## Generate names for columns
cNames <- varNames[cids] %>% sapply(function(x) gsub("\\(\\)|-", "", x)) %>% 
        sapply(function(x) gsub("mean", "Mean", x)) %>%
        sapply(function(x) gsub("std", "Std", x))
## Rename the columns
colnames(Data) <- c("subject", "activity", cNames)


## 5.
## Build the requested tidy data set
tidyData <- Data %>% group_by(subject, activity) %>% summarise_all(mean)


## Store tidyData into file
write.table(tidyData, "./UCI HAR Dataset/tidyData.txt", row.name = FALSE)