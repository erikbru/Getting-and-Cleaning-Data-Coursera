#_______________________________________________________________
#
# Getting and cleaning data: Peer assignment
# Script Erik Bruin
#_______________________________________________________________

rm(list=ls()) #clear workspace
library(dplyr)

if (!file.exists("./peergetting")) {dir.create("./peergetting")}  #create a directory specifically for this project
setwd("./peergetting")

#Download and unzip the dataset
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  download.file(fileURL, 'getdata_dataset.zip', mode='wb')
  unzip('getdata_dataset.zip')
  setwd('./UCI HAR Dataset')

#reading the features and activityType data
features=     read.table('./features.txt',header=FALSE); #imports the feature vector with time and frequency domain variables
activityLabels= read.table('./activity_labels.txt',header=FALSE); #imports the 6 activities with descriptions

#reading the training data
subjectTrain= read.table('./train/subject_train.txt',header=FALSE); #identifies the subject (the 30 volunteers) for each observation
xTrain =      read.table('./train/x_train.txt',header=FALSE); #imports the training data
yTrain =      read.table('./train/y_train.txt',header=FALSE); #imports the activity id for each observation

#Give descriptive names of colums in data frames
colnames(activityLabels)  = c('activityId','activityLabel')
colnames(subjectTrain)  = "subjectId"
colnames(xTrain)        = features[,2] #each column actually is a feature in the feature vector (561 features)
colnames(yTrain)        = "activityId"

#Merge the 3 data frames that compose the training data into 1 dataframe
training=cbind(yTrain, subjectTrain, xTrain)

#repeat steps for test set (reading data, descriptive column names, merge)
subjectTest=  read.table('./test/subject_test.txt',header=FALSE)
xTest =      read.table('./test/x_test.txt',header=FALSE)
yTest =      read.table('./test/y_test.txt',header=FALSE)
colnames(subjectTest)  = "subjectId"
colnames(xTest)        = features[,2]
colnames(yTest)        = "activityId"
test=cbind(yTest, subjectTest, xTest)

#Merging training and test set into 1
AllData=rbind(training, test)

#Extract only the measurements on the mean and standard deviation for each measurement
IndexColumnsKeep= grep("activityId|subjectId|mean\\(\\)|std", names(AllData)) #assuming meanFreq()'s are not wanted
AllDataSub=AllData[ , IndexColumnsKeep]

#Use descriptive activity names to name the activities in the data set
SubWithLabels=merge(AllDataSub, activityLabels, by='activityId')
SubWithLabels=select(SubWithLabels, activityId, activityLabel, subjectId, everything()) #activityLabel already is a Factor as it should be
SubWithLabels=SubWithLabels[,-1] #removing activityId as it is not needed anymore

#Label the data set with descriptive variable names
colnames(SubWithLabels)=sub('^t', 'time', colnames(SubWithLabels)) #They should be descriptive, but also not become too long.
colnames(SubWithLabels)=sub('^f', 'frequency', colnames(SubWithLabels)) # I can do is for instance spell out time instead of t, and frequency instead of f.

#Creates a second, independent tidy data set with the average of each variable for each activity and each subject
FinalDf= SubWithLabels %>%
  group_by(activityLabel, subjectId) %>%
  summarize_all(mean)

#Writing to txt file
write.table(FinalDf, "tidy_data_set.txt", row.names = FALSE)



