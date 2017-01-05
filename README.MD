This repo serves as Rob Creel's submission for the final project in Coursera's course in Cleaning and Tidying Data.  The run_analysis.R file performs the following procedure to the files in the "UCI HAR Dataset" directory, downloadable from "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip".

1. Merge the training and the test sets to create one data set.
Step 0.  Set the working directory to "UCI HAR Dataset":
1.1 Load the relevant files into R.
1.2 Name the variables from the appropriate files.
1.3 The "type" variable (test or train) is created and the test and train sub-dataframes are created.
1.4 The data frame is fully assembled.

2. Extract only the measurements on the mean and standard deviation for each measurement.
2.1 (I'm assuming we should still keep the subject and activity columns.)

3. Use descriptive actvity names to name the activities in the data set
(Compare to the accompanying 'activity_labels.txt' file.)

4. Appropriately label the data set with descriptive variable names.
(Done in step 1.2 in the aid of assembling the data frame from the beginning.)

5. From the data set in step 4, create a second, independent tidy data set with the average of each head
variable for each activity and each subject.
5.1 Create the skeleton of the data frame with every unique combination of subjects and activities.
5.2 For each variable (looping through the columns), compute the mean for each subject-activity pair
(nested looping through the rows), done by
first adding a new empty column (30 subjects times 6 activities is 180 observation means),
second subsetting out the subset corresponding to the relevant portion of df_mean_std, and
finally adding each mean to the appropriate place in the tidy_data data frame.
5.3 Let the column names match those of the df_mean_std.
5.4 Save the data frame to the directory containing "UCI HAR Dataset".
