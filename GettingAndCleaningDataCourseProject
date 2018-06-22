#MAJOR GOAL: Merges the training and test sets to create one data set

#MINOR GOAL: Read Data 
zipLoc <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipname <- "UCIHAR.zip"

if(!file.exists(zipname)) {
  download.file(zipLoc, zipname)
}

unzip(zipname)

trainActivity <- read.table(file.path("UCI HAR Dataset", "train", "y_train.txt"))
trainValues <- read.table(file.path("UCI HAR Dataset", "train", "X_train.txt"))
trainSubjects <- read.table(file.path("UCI HAR Dataset", "train", "subject_train.txt"))

training <- rbind(cbind(trainActivity, trainValues, trainSubjects))

testActivity <- read.table(file.path("UCI HAR Dataset", "test", "y_test.txt"))
testValues <- read.table(file.path("UCI HAR Dataset", "test", "X_test.txt"))
testSubjects <- read.table(file.path("UCI HAR Dataset", "test", "subject_test.txt"))

test <- rbind(cbind(testActivity, testValues, testSubjects))

features <- read.table(file.path("UCI HAR Dataset", "features.txt"), as.is = TRUE)

activities <- read.table(file.path("UCI HAR Dataset", "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

#MINOR GOAL: Combine training and test scores into one frame = AllSubjects
AllSubjects <- rbind(training, test)
colnames(AllSubjects) <- c("Subject", features [, 2], "activity")

#MAJOR GOAL: Extracts only the measurements on the mean and standard deviation for each 

#MINOR GOAL: Keep only columns that have those
AllSubjectsColumns <- grepl("subject|activity|mean|std", colnames(AllSubjects))

AllsubjectsMini <- AllSubjects[ , AllSubjectsColumns]

#MAJOR GOAL: Uses descriptive activity names to name the activities in the data set

AllsubjectsMini$activity <- factor(AllsubjectsMini$activity, levels = activities[, 1], labels = activities[, 2])

#MAJOR GOAL: Appropriately labels the data set with descriptive variable names

Cols <- colnames(AllsubjectsMini)

Cols <- gsub("^f", "Frequency ", Cols)
Cols <- gsub("^t", "Time ", Cols)
Cols <- gsub("Acc", "Accelerometer ", Cols)
Cols <- gsub("Gyro", "Gyroscope ", Cols)
Cols <- gsub("Mag", "Magnitude ", Cols)
Cols <- gsub("mean", "Mean ", Cols)
Cols <- gsub("std", "StandardDeviation ", Cols)
Cols <- gsub("BodyBody", "Body", Cols)

colnames(AllsubjectsMini) <- Cols

# MAJOR GOAL: Create second data set with average of each variable

AllsubjectsMiniMeans <- AllsubjectsMini %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

write.table(AllsubjectsMiniMeans, "tidy_data.txt", row.names = FALSE, quote = FALSE)

