# 1. Merge the training and the test sets to create one data set.
# Step 0.  Set the working directory to "UCI HAR Dataset":
# 1.1 Load the relevant files into R.
subject_test        <- read.table("./test/subject_test.txt")
X_test              <- read.table("./test/X_test.txt")
y_test              <- read.table("./test/y_test.txt")

subject_train       <- read.table("./train/subject_train.txt")
X_train             <- read.table("./train/X_train.txt")
y_train             <- read.table("./train/y_train.txt")


# 1.2 Name the variables from the appropriate files.
names(subject_test) <- "subject"
names(subject_train) <- "subject"
names(y_test) <- "activity"
names(y_train) <- "activity"
features <- read.table("features.txt")
features <- features$V2
names(X_test) <- features
names(X_train) <- features

# 1.3 The "type" variable (test or train) is created and the test and train sub-dataframes are created.
type <- rep("test", length(subject_test$subject))
test <- cbind(subject_test, y_test, X_test, type)
type <- rep("train", length(subject_train$subject))
train <- cbind(subject_train, y_train, X_train, type)

# 1.4 The data frame is fully assembled.
df <- rbind(test, train)

# 2. Extract only the measurements on the mean and standard deviation for each measurement.
# 2.1 (I'm assuming we should still keep the subject and activity columns.)

mean_std <- grep("mean|std", names(df))
df_temp <- df[mean_std]
df_mean_std <- cbind(df[, 1:2], df_temp)


# 3. Use descriptive actvity names to name the activities in the data set
# (Compare to the accompanying 'activity_labels.txt' file.)

df_mean_std$activity <- gsub("1", "walking", df_mean_std$activity)
df_mean_std$activity <- gsub("2", "walking_upstairs", df_mean_std$activity)
df_mean_std$activity <- gsub("3", "walking_downstairs", df_mean_std$activity)
df_mean_std$activity <- gsub("4", "sitting", df_mean_std$activity)
df_mean_std$activity <- gsub("5", "standing", df_mean_std$activity)
df_mean_std$activity <- gsub("6", "laying", df_mean_std$activity)


# 4. Appropriately label the data set with descriptive variable names.
# (Done in step 1.2 in the aid of assembling the data frame from the beginning.)

# 5. From the data set in step 4, create a second, independent tidy data set with the average of each head
# variable for each activity and each subject.
# 5.1 Create the skeleton of the data frame with every unique combination of subjects and activities.
tidy_data <- data.frame(subject = rep(c(1:30), each = 6), activity = rep(unique(df_mean_std$activity), 30))

# 5.2 For each variable (looping through the columns), compute the mean for each subject-activity pair
# (nested looping through the rows), done by
# first adding a new empty column (30 subjects times 6 activities is 180 observation means),
# second subsetting out the subset corresponding to the relevant portion of df_mean_std, and
# finally adding each mean to the appropriate place in the tidy_data data frame.
for(col_index in 3:ncol(df_mean_std)){
    tidy_data[,col_index] <- numeric(180)
    for(row_index in 1:nrow(tidy_data)){
        relevant_subset <- df_mean_std[df_mean_std$subject == tidy_data[row_index,1] & df_mean_std$activity == tidy_data[row_index,2], col_index]
        tidy_data[row_index, col_index] <- mean(relevant_subset)
    }
}
# 5.3 Let the column names match those of the df_mean_std.
names(tidy_data) <- names(df_mean_std)
# 5.4 Save the data frame to the directory containing "UCI HAR Dataset".
setwd("..")
write.csv(tidy_data, "Motion_Means.csv")
