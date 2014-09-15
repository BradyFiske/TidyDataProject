## Activity Recognition by Subject Summary
## Completed September, 2014

## This R script assumes that the zipped file from the data source 
## (https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip) has already been downloaded and unzipped
## into a working directory.  If you haven't already downloaded and unzipped the file, do so now.  Make sure this script is in the same working directory as 
## the data files that have been unzipped from the download. See http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
## for details on the data collection process.

## See the README.rm file in this github repository () for detailed information on the data manipulation process.  In brief, this
## script reassembles a series of data sets derived from experiments in tracking smartphone accelerometer and gyroscope sensor readings
## for human subjects engaged in a series of defined activities.  The script then extracts a group of variables related to measurement means
## and standard deviations for the sensor measurements, renames the variables, and summarizes the data by deriving an overall mean 
## for each variable by subject by activity.




#### Read raw data from the unzipped files into R format

## Read testing dataset
x_testing_dataset <- read.table("~/Desktop/Coursera/UCI HAR Dataset/test/X_test.txt")

## Read training dataset
x_training_dataset <- read.table("~/Desktop/Coursera/UCI HAR Dataset/train/X_train.txt")

## Read variable names
col_var_names <- read.table("~/Desktop/Coursera/UCI HAR Dataset/features.txt")

## Read subject ID assigned to training row identifiers
subject_training_data <- read.table("~/Desktop/Coursera/UCI HAR Dataset/train/subject_train.txt")

##Add desriptive column header for subject testing data
colnames(subject_training_data) <- "SubjectID"

## Read subject ID assigned to testing row identifiers
subject_testing_data <- read.table("~/Desktop/Coursera/UCI HAR Dataset/test/subject_test.txt")

##Add descriptive column header for subject testing data
colnames(subject_testing_data) <- "SubjectID"

## Read activity ID assigned to testing row identifiers
activity_testing_data <- read.table("~/Desktop/Coursera/UCI HAR Dataset/test/y_test.txt")

##Add descriptive column header for activity testing data
colnames(activity_testing_data) <- "Activity_Type"

## Read activity ID assigned to training row identifiers
activity_training_data <- read.table("~/Desktop/Coursera/UCI HAR Dataset/train/y_train.txt")

##Add descriptive column header for activity testing data
colnames(activity_training_data) <- "Activity_Type"

## Read activity code descriptions
activity_description <- read.table("~/Desktop/Coursera/UCI HAR Dataset/activity_labels.txt")

## Add descriptive column headers for activity description
colnames(activity_description) <- c("Code", "Activity_Description")

## Assign column names to testing file
for (i in 1:561){
    colnames(x_testing_dataset) <- col_var_names$V2
    i = i+1
}

## Assign column names to training file
for (i in 1:561){
  colnames(x_training_dataset) <- col_var_names$V2
  i = i+1
}

#### Combine and subset the data

## Combine Activity ID and Subject ID columns with testing dataset
x_test <- cbind(activity_testing_data, x_testing_dataset)
x_test <- cbind(subject_testing_data, x_test)

## Combine Activity ID and Subject ID columns with training dataset
x_training <- cbind(activity_training_data, x_training_dataset)
x_training <- cbind(subject_training_data, x_training)

## Created unified dataset by combining testing and training datasets
master_data <- rbind(x_test, x_training)

## Extract subsets of variables related to means and standard deviations, making sure that Subject ID and Activity ID come along as well
means_subset <- subset(master_data, select=grep("mean()", names(master_data)))
std_subset <- subset(master_data, select=grep("std()", names(master_data)))
subjects <- subset(master_data, select=grep("SubjectID", names(master_data)))
activities <- subset(master_data, select=grep("Activity_Type", names(master_data)))

## Combine extracted subsets into a master subset with only mean and standard deviation related variables
means_stds <- cbind(means_subset, std_subset)
means_stds <- cbind(activities, means_stds)
means_stds <- cbind(subjects, means_stds)

## Clarify variable names
names(means_stds) <- gsub("-std", "Std_Dev_", names(means_stds))
names(means_stds) <- gsub("-mean", "Mean_", names(means_stds))
names(means_stds) <- gsub("tBody", "Body_", names(means_stds))
names(means_stds) <- gsub("fBody", "FFT_Body_", names(means_stds))
names(means_stds) <- gsub("tGravity", "Gravity_", names(means_stds))
names(means_stds) <- gsub("\\(\\)", "", names(means_stds))
names(means_stds) <- gsub("Gyro", "Angular_Velocity_", names(means_stds))
names(means_stds) <- gsub("Acc", "Linear_Acceleration_", names(means_stds))
names(means_stds) <- gsub("-X", "X_Axis", names(means_stds))
names(means_stds) <- gsub("-Y", "Y_Axis", names(means_stds))
names(means_stds) <- gsub("-Z", "Z_Axis", names(means_stds))
names(means_stds) <- gsub("Jerk", "Over_Time_", names(means_stds))
names(means_stds) <- gsub("Mag", "Magnitude_", names(means_stds))
names(means_stds) <- gsub("_$", "", names(means_stds))

#### Manipulate the dataset

## Sort the dataset by Activity Type Code
means_stds <- means_stds[order(means_stds$Activity_Type),]

## Turn Activity Type variable into a factor variable
means_stds$Activity_Type <- as.factor(means_stds$Activity_Type)

## Use levels and defined string to recode activity codes with activity descriptions
levels(means_stds$Activity_Type) = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")

## Reorder the dataset by Subject by Activity Type
means_stds1 <- means_stds[order(means_stds$SubjectID, means_stds$Activity_Type),]

## Need to install and activate the plyr package to use ddply function

## Generalized check and install function for R packages    
packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

packages(plyr)

## Create summary of subject and activity measurements
activity_by_subject <- ddply(means_stds1[,3:81], .(means_stds1$SubjectID, means_stds1$Activity_Type), .fun=colMeans)

## Fix header names in summary dataset with cleaner names
names(activity_by_subject)[1] <- "SubjectID"
names(activity_by_subject)[2] <- "Activity_ID"

#### Output the result

## Output summary to a text file
write.table(activity_by_subject, file="activity_by_subject.txt", row.name=FALSE)
