library(plyr)

# setwd("J:/Coursera/Getting and Cleaning Data/Course Project/UCI HAR Dataset")

# STEP 01: Merging training and test sets
# =======================================
# Read and merge activity data, set name to variables
activity_data_training <- read.table("train/Y_train.txt")
activity_data_test <- read.table("test/Y_test.txt")
activity_data_all <- rbind(activity_data_training, activity_data_test)
names(activity_data_all) <- c("activity")
# Read and merge subject files, set name to variables
subject_data_training <- read.table("train/subject_train.txt")
subject_data_test <- read.table("test/subject_test.txt")
subject_data_all <- rbind(subject_data_training, subject_data_test)
names(subject_data_all) <- c("subject")
# Read and merge feature files, set name to variables
feature_data_training <- read.table("train/X_train.txt")
feature_data_test <- read.table("test/X_test.txt")
feature_data_all <- rbind(feature_data_training, feature_data_test)
feature_names <- read.table("features.txt")
names(feature_data_all) <- feature_names$V2
# Merge data into one data frame
data_all <- cbind(subject_data_all, activity_data_all, feature_data_all)

# STEP 02: Extracts only the measurements on the mean and standard deviation for each measurement.
mean_and_std_features <- feature_names[grep("(mean|std)\\(\\)", feature_names[,2]), 2]
selected_columns <- c("subject", "activity", as.character(mean_and_std_features))
data_all_step02 <- subset(data_all, select = selected_columns)

# STEP 03: Uses descriptive activity names to name the activities in the data set
data_all_step03 <- data_all_step02
activity_names <- read.table("activity_labels.txt")
data_all_step03[,2] <- activity_names[data_all_step03[,2],2]

# STEP 04: Appropriately labels the data set with descriptive variable names. 
data_all_step04 <- data_all_step03
names(data_all_step04) <- gsub("^t", "time", names(data_all_step04))
names(data_all_step04) <- gsub("^f", "freq", names(data_all_step04))
names(data_all_step04) <- gsub("Acc", "Accelerometer", names(data_all_step04))
names(data_all_step04) <- gsub("Gyro", "Gyroscope", names(data_all_step04))
names(data_all_step04) <- gsub("Mag", "Magnitude", names(data_all_step04))
names(data_all_step04) <- gsub("BodyBody", "Body", names(data_all_step04))

# STEP 05: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_average_data <- ddply(data_all_step04, .(subject, activity), function(x) colMeans(x[,3:68]))
write.table(tidy_average_data, file="tidy_average_data.txt", row.name=FALSE)