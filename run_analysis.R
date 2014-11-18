run_analysis <- function(){
# course project for Getting and Cleaning Data
#
# Implement functions:
# 1.  Merges the training and the test sets to create one data set.
# 2.  Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.  Uses descriptive activity names to name the activities in the data set
# 4.  Appropriately labels the data set with descriptive variable names. 
# 5.  From the data set in step 4, creates a second, independent tidy data set 
#     with the average of each variable for each activity and each subject.
#####################

# 1.  Merges the training and the test sets to create one data set.
features <- read.table("C:\\Users\\Rui\\Coursera\\getdata\\UCI HAR Dataset\\features.txt")

X_train <- read.table("C:\\Users\\Rui\\Coursera\\getdata\\UCI HAR Dataset\\train\\X_train.txt")
y_train <- read.table("C:\\Users\\Rui\\Coursera\\getdata\\UCI HAR Dataset\\train\\y_train.txt")
subject_train <- read.table("C:\\Users\\Rui\\Coursera\\getdata\\UCI HAR Dataset\\train\\subject_train.txt")

X_test <- read.table("C:\\Users\\Rui\\Coursera\\getdata\\UCI HAR Dataset\\test\\X_test.txt")
y_test <- read.table("C:\\Users\\Rui\\Coursera\\getdata\\UCI HAR Dataset\\test\\y_test.txt")
subject_test <- read.table("C:\\Users\\Rui\\Coursera\\getdata\\UCI HAR Dataset\\test\\subject_test.txt")

X_train_plus_test <- rbind(X_train, X_test)
y_train_plus_test <- rbind(y_train, y_test)
subject_train_plus_test <- rbind(subject_train, subject_test)

### all_data combines all subjects, activity_label, and train_plus_test data sets
all_data <- cbind(subject_train_plus_test, y_train_plus_test, X_train_plus_test)

# 4.  Appropriately labels the data set with descriptive variable names. 
f<-features[,2]
name <-c ("subject", "activity", as.character(f))
names(all_data)=name


# 2.  Extracts only the measurements on the mean and standard deviation for each measurement. 
dim_all_data <- dim(all_data)
mean_each_measures <- colMeans(all_data[,3:dim_all_data[2]])
std_each_measures <- apply(all_data[,3:dim_all_data[2]], 2, sd)


# 3.  Uses descriptive activity names to name the activities in the data set

for (i in 1:dim_all_data[1]){
  if(all_data[i,2] =="1")
    all_data[i,2]<-"WALKING"
  if(all_data[i,2]=="2")
    all_data[i,2]<-"WALKING_UPSTAIRS"
  if(all_data[i,2]=="3")
    all_data[i,2]<-"WALKING_DOWNSTAIRS"
  if(all_data[i,2]=="4")
    all_data[i,2]<-"SITTING"
  if(all_data[i,2]=="5")
    all_data[i,2]<-"STANDING"
  if(all_data[i,2]=="6")
    all_data[i,2]<-"LAYING"
}

library(plyr)
data <- arrange(all_data, all_data$subject, all_data$activity)

#5.  From the data set in step 4, creates a second, independent tidy data set 
#     with the average of each variable for each activity and each subject.
library(data.table)
dt.data <- data.table(data)
dt.tidy_data <- dt.data[, lapply(.SD,mean), by=c("subject", "activity")]
tidy_data <- data.frame(dt.tidy_data)

# output tidy data set to a *.txt file
write.table(tidy_data, "C:\\Users\\Rui\\Coursera\\getdata\\UCI HAR Dataset\\tidy_data.txt", sep="\t", row.names=FALSE)

}