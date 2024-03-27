#Peer-Graded Assignment: Getting and Cleaning Data 

##The purpose of this project is to demonstrate your ability to collect, work
##with, and clean a data set. The goal is to prepare tidy data that can be used 
##for later analysis. You will be graded by your peers on a series of yes/no 
##questions related to the project. You will be required to submit: 1) a tidy 
##data set as described below, 2) a link to a Github repository with your script 
##for performing the analysis, and 3) a code book that describes the variables, 
##the data, and any transformations or work that you performed to clean up the 
##data called CodeBook.md. You should also include a README.md in the repo with 
##your scripts. This repo explains how all of the scripts work and how they are 
##connected.

#Instructions
##You should create one R script called run_analysis.R that does the following: 

## 1. Merges the training and the test sets to create one data set.

## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

## 3. Uses descriptive activity names to name the activities in the data set

## 4. Appropriately labels the data set with descriptive variable names. 

## 5. From the data set in step 4, creates a second, independent tidy data set
## with the average of each variable for each activity and each subject.

#Preparation Stage
library(tidyverse)

## Setting the working directory so my commands are short and sweet.
setwd("C:/Users/sgras/OneDrive/Documents/School/WNTR 2024/Getting and Cleaning Data/Week 4/Assignment")


#-------------------------------------------------------------------------------
# Part 1
#-------------------------------------------------------------------------------

# 1.0 Import Data
## 1.1.1 Test Data 
xtest <- read.table("UCI HAR Dataset/test/X_test.txt")
ytest <- read.table("UCI HAR Dataset/test/Y_test.txt")
stest <- read.table("UCI HAR Dataset/test/subject_test.txt")

## 1.1.2 Training Data
xtrain <- read.table("UCI HAR Dataset/train/X_train.txt")
ytrain <- read.table("UCI HAR Dataset/train/Y_train.txt")
strain <- read.table("UCI HAR Dataset/train/subject_train.txt")

## 1.1.3 Activity Labels Data
activitylabels <- read.table("UCI HAR Dataset/activity_labels.txt")

## 1.1.4 Features Data
features <- read.table("UCI HAR Dataset/features.txt")

# 1.2 Renaming the Columns Correctly: 
## 1.2.1 Test
colnames(xtest) <- features[,2] 
#### Taking the second column from "features" and assigning that to the columns for ytrain.
colnames(ytest) <- "activityId"
#### Setting the ytest activity ID's
colnames(stest) <- "subjectId"
#### Setting the subject ID's

## 1.2.2 Train 
colnames(xtrain) <- features[,2]
#### Taking the second column from "features" and assigning that to the columns for xtrain.
colnames(ytrain) <-"activityId"
#### Setting the ytrain activity ID's
colnames(strain) <- "subjectId"
#### Setting the subject ID's

colnames(activitylabels) <- c('activityId','activityType')

# 1.3 Merging all data in one set:

mrg_train <- cbind(ytrain, strain, xtrain)
mrg_test <- cbind(ytest, stest, xtest)
setAllInOne <- rbind(mrg_train, mrg_test)

### dim(setAllInOne)
### [1] 10299   563

#-------------------------------------------------------------------------------
# Part 2
#-------------------------------------------------------------------------------
# 2.0 Read The Current Columns 
colNames <- colnames(setAllInOne)
colNames

# 2.1 Use grepl to search for "activityID", "SubjectID" "mean" and "std

mean_and_std <- (grepl("activityId" , colNames) | 
                         grepl("subjectId" , colNames) | 
                         grepl("mean.." , colNames) | #This finds any case with "mean"
                         grepl("std.." , colNames) #This finds any case with "std"
)

# 2.3 Making subset from setAllInOne with only mean and std for each measuremnt:

setForMeanAndStd <- setAllInOne[ , mean_and_std == TRUE]
setForMeanAndStd

#-------------------------------------------------------------------------------
# Part 3
#-------------------------------------------------------------------------------

setWithActivityNames <- merge(setForMeanAndStd, activitylabels,
                              by="activityId",
                              all.x=TRUE)

#-------------------------------------------------------------------------------
# Part 4 
#-------------------------------------------------------------------------------

# I would hope I already did this as I worked.

#-------------------------------------------------------------------------------
# Part 5 
#-------------------------------------------------------------------------------

# 5.1 Making a second tidy data set
## 5.1.1 Selecting just mean 
mean <- (grepl("activityId" , colNames) | 
                         grepl("subjectId" , colNames) | 
                         grepl("mean.." , colNames) #This finds any case with "mean"
                 )

## 5.1.2 Set for just means
setForMean <- setAllInOne[ , mean == TRUE]
setForMean

## 5.1.3 Set with activity names with just the means this time
Tidyset_with_Means <- merge(setForMean, activitylabels,
                              by="activityId",
                              all.x=TRUE)

## 5.1.4 Write the .txt file!

write.table(Tidyset_with_Means, "C:/Users/sgras/OneDrive/Documents/School/WNTR 2024/Getting and Cleaning Data/Week 4/Assignment\\secTidySet.txt", sep = " ")

#-------------------------------------------------------------------------------
# Codebook
#-------------------------------------------------------------------------------

#Setup
library(haven)
library(sjlabelled)
library(tidyverse)
library(psych)
simple_codebook <- enframe(get_label(setWithActivityNames))

# use more informative column names
colnames(simple_codebook) <- c("variable_id", "item_text")

descriptives <- setWithActivityNames %>% describe() %>% as_tibble() %>% select("n","min","max","mean")
# add stats to codebook 
simple_codebook <- cbind(simple_codebook,descriptives)

write.csv(simple_codebook, "C:/Users/sgras/OneDrive/Documents/School/WNTR 2024/Getting and Cleaning Data/Week 4/Assignment\\simple_codebook.csv", row.names = TRUE)
")









