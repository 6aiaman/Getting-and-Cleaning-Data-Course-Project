# 1. Merges the training and the test sets to create one data set.

# merges X_train.txt and X_test.txt into x.data
x.train <- read.table("train/X_train.txt")
x.test <- read.table("test/X_test.txt")
x.data <- rbind(x.train, x.test)

# merges y_train.txt and y_test.txt into y.data
y.train <- read.table("train/y_train.txt")
y.test <- read.table("test/y_test.txt")
y.data <- rbind(y.train, y.test)

# merges subject_t_train.txt and subject_t_test.txt into subject_t.data
subject.train <- read.table("train/subject_train.txt")
subject.test <- read.table("test/subject_test.txt")
subject.data <- rbind(subject.train, subject.test)



# 2. Extracts only the measurements on the mean and standard 
#       deviation for each measurement.

# all features from features.txt
features <- read.table("features.txt")

# column ids for a subset of features which have "[Mm]ean" or "[Ss]td" in their names
features.mean.std <- grep(".*[Mm]ean.*|.*[Ss]td.*", features[,2])

# selects columns from x.data, which have "mean" or "std" in their names
x.data <- x.data[, features.mean.std]

# assigns feature's names to x.data.mean.std's columns
names(x.data) <- features[features.mean.std, 2]

# Cleaning the names of the columns in x.data.mean.std
names(x.data) <- gsub('-std', 'Std', names(x.data))
names(x.data) <- gsub('-mean', 'Mean', names(x.data))
names(x.data) <- gsub('[-()]', '', names(x.data))



# 3. Uses descriptive activity names to name the activities in the data set

activityNames <- read.table("activity_labels.txt")

# change the number representations into descriptive representations from activity_labels.txt
y.data[, 1] <- activityNames[y.data[, 1], 2]



# 4. Appropriately labels the data set with descriptive variable names.

# merges subject.data and y.data
all.data <- cbind(subject.data, y.data)

# gives descriptive names to subject.data and y.data columns
colnames(all.data) <- c("subject","activity")

# merges subject.data and y.data with all.data
all.data <- cbind(all.data, x.data)


# 5. From the data set in step 4, creates a second, independent tidy data 
#       set with the average of each variable for each activity and each subject.

all.data$activity <- as.factor(all.data$activity)
all.data$subject <- as.factor(all.data$subject)
tidy = aggregate(all.data, by=list(activity = all.data$activity, subject=all.data$subject), mean)
write.table(tidy, "tidy_data.txt", row.name=FALSE)
