## ---------------------------
##
## Script name: run_analysis.R
##
## Purpose of script: Collect, work with, and clean a dataset.
##
## Author: Shelbournator
##
## Date Created: 2020-03-22
##
## ---------------------------
##
## Notes: This script was created for the final assignment of the Getting and
##   Cleaning Data course on Coursera.
##
## ---------------------------

# Function
run_analysis <- function(x,y) {
        
        #### --------------
        # 0. Download and read in data.
        #### --------------
        
        ## Download file from websource.
        
        # if(!file.exists("./data")){dir.create("./data")}
        # fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        # download.file(fileURL, destfile="./data/samsung.csv", method="curl")
        
        # Read in subject IDs, activity labels and variable labels (column names).
        
        test.subjects <- read.table("data/UCI HAR Dataset/test/subject_test.txt", col.names="ID")
        train.subjects <- read.table("data/UCI HAR Dataset/train/subject_train.txt", col.names="ID")
        test.activity <- read.table("data/UCI HAR Dataset/test/y_test.txt", col.names="activity")
        train.activity <- read.table("data/UCI HAR Dataset/train/y_train.txt", col.names="activity")
        colnames <- read.table("data/UCI HAR Dataset/features.txt", colClasses=c("NULL", NA))
        activitynames <- read.table("data/UCI HAR Dataset/activity_labels.txt")
        
        
        # Read in data sets
        
        test <- read.table("data/UCI HAR Dataset/test/X_test.txt", col.names=colnames[,1])
        train <- read.table("data/UCI HAR Dataset/train/X_train.txt", col.names=colnames[,1])
        
        #### --------------
        # 1. Merge the training and the test sets to create one data set.
        #### --------------
        
        ## Add subject IDs, activity labels and dataset of origin.
        
        test <- cbind(test.subjects, test.activity, "test", test)
        train <- cbind(train.subjects, train.activity, "train", train)
        
        ## Rename dataset of origin variable.
        
        colnames(test)[3] <- "dataset"; colnames(train)[3] <- "dataset"
        
        ## Use cbind to combine test and training data (dataset variable) 
        
        data <- rbind(test,train)
        
        #### -------------- 
        # 2. Extracts only the measurements on the mean and standard deviation for each measurement.
        #### --------------
        
        require(dplyr)
        label.columns <- c("ID", "activity", "dataset")
        save.columns <- grep("mean|std", names(data), value=TRUE)
        save.columns <- append(label.columns, save.columns)
        data <- select(data, save.columns)
        
        #### --------------
        # 3. Uses descriptive activity names to name the activities in the data set
        #### --------------
        
        # 1 - Walking
        data$activity <- sub("1", "Walking", data$activity)
        
        # 2 - Walking Upstairs
        data$activity <- sub("2", "Walking_Upstairs", data$activity)
        
        # 3 - Walking downstairs
        data$activity <- sub("3", "Walking_Downstairs", data$activity)
        
        # 4 - Sitting
        data$activity <- sub("4", "Sitting", data$activity)
        
        # 5 - Standing
        data$activity <- sub("5", "Standing", data$activity)
        
        # 6 - Laying
        data$activity <- sub("6", "Laying", data$activity)
        
        #### --------------
        # 4. Appropriately labels the data set with descriptive variable names.
        #### --------------
        
        # See README.txt.
        
        # The first data set is now finished as data.
        
        write.table(data, "./dataset.txt")
        
        #### --------------
        # 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
        #### --------------
        
        require(reshape2)
        
        dataMelt <- melt(data, id=names(data)[1:3], measure.vars=names(data)[4:82])
        
        data2 <- aggregate( value ~ variable + activity + ID, dataMelt, mean )
        
        names(data2)[4] <- "mean"
        
        # The second data set is now finished as data2.
        
        write.table(data2, "./dataset2.txt")
        
}