



# download file
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile = "dataset.zip")

# unzip file
unzip("dataset.zip")

# read the data 
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)
features <- read.table("UCI HAR Dataset/features.txt", header = FALSE)

# make column names 
colnames(activityLabels) <- c("activityid", "activitytype")
colnames(features) <- c("featureid", "featurename")

xtest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)
colnames(xtest) <- features$featurename

ytest <- read.table("UCI HAR Dataset/test/Y_test.txt", header = FALSE)
colnames(ytest) <- "activityid"

subjecttest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
colnames(subjecttest) <- "subjectid"

testdata <- cbind(ytest, subjecttest, xtest)

xtrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
colnames(xtrain) <- features$featurename

ytrain <- read.table("./UCI HAR Dataset/train/Y_train.txt",header = FALSE)
colnames(ytrain) <- "activityid"

subjecttrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
colnames(subjecttrain) <- "subjectid"

traindata <- cbind(ytrain, subjecttrain, xtrain)

# Merges the training and the test sets to create one data set.
alldata <- rbind(testdata, traindata)

# Extracts only the measurements on the mean and standard deviation for each measurement
meanandstd <-alldata[,grepl("mean|std|subject|activityid",colnames(alldata))]

# Uses descriptive activity names to name the activities in the data set
meanandstd$activityid[meanandstd$activityid == "1"] <- "walking"
meanandstd$activityid[meanandstd$activityid == "2"] <- "walking_upstairs"
meanandstd$activityid[meanandstd$activityid == "3"] <- "walking_downstairs"
meanandstd$activityid[meanandstd$activityid == "4"] <- "sitting"
meanandstd$activityid[meanandstd$activityid == "5"] <- "standing"
meanandstd$activityid[meanandstd$activityid == "6"] <- "laying"

# Appropriately labels the data set with descriptive variable names
names(meanandstd)<- gsub("[()]", "", names(meanandstd))


# creates a second, independent tidy data set with the average of each variable for each activity and each subject
library(tidyr)
library(dplyr)

avgdata <- (gather(meanandstd, measurementkey, measurementvalue, -c(activityid, subjectid)) 
                %>% group_by(activityid, subjectid, measurementkey)
                %>% summarize(measurementvalue = mean(measurementvalue))
                %>% spread(measurementkey, measurementvalue))


# write to file        
write.table(avgdata, file = "avgdata.txt", row.name=FALSE)
