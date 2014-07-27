## General description for run_analysis.R
1. Extracts only the measurements on the mean and standard deviation for each parameter from the test and training datasets
2. Uses descriptive activity names to name the activities in the data set
3. Appropriately labels the data set with descriptive variable names
4. Merges the extracted variables from test and training datasets in to a single new dataset
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject
 
## Reading parameter names from 'features.txt'
varNames<-read.table("./UCI HAR Dataset/features.txt",header=F,sep="")

## Reading activity description labels from 'activity_labels.txt'
actNames<-read.table("./UCI HAR Dataset/activity_labels.txt",header=F, sep="")

## Function that extracts only measurements on mean and standard devition for each paramaeter from the datasets
meanStd<-function(framed,a="mean()",b="std()"){
  selected <- c()
  for (name in names(framed)){
    if (grepl(a,name, fixed=T)|grepl(b,name,fixed=T)){
      selected<-c(selected,name)
    } 
  }
  selected
}

## Reading the test data
### Read subject ids from file 'subject_test.txt'
subTest<-read.table("./UCI HAR Dataset/test/subject_test.txt",header=F, sep="",col.names="SubjectID")
subTest$SubjectID<-as.factor(subTest$SubjectID)
### Read activity description numbers from 'y_test.txt'
yTest<-read.table("./UCI HAR Dataset/test/y_test.txt",header=F, sep="",col.names="ActivityType")
yTest$ActivityType<-as.factor(yTest$ActivityType)
### Name activity levels in yTest with description labels
levels(yTest$ActivityType)<-actNames[,2]
### Combine subject id and activity levels for test data
subyTest<-cbind(subTest,yTest)
### Read all parameters in file 'X_test.txt' and insert parameter names
xTest<-read.table("./UCI HAR Dataset/test/X_test.txt",header=F,sep="")
colnames(xTest)<-varNames[,2]
### Extract only mean and standard deviation for all parameters from test data using the function meanStd
extractTest<-xTest[,meanStd(xTest)]
### Combine extracted test data with subject id and activity labels
testExtract<-cbind(subyTest,extractTest)
### Insert a variable SubjectType (label "test") to indicate test data
testExtract$SubjectType<-as.factor(rep("test",length(testExtract[,1])))

## Reading the training data
### Read subject ids from file 'subject_train.txt'
subTrain<-read.table("./UCI HAR Dataset/train/subject_train.txt", header=F,sep="",col.names="SubjectID")
subTrain$SubjectID<-as.factor(subTrain$SubjectID)
### Read activity description numbers from 'y_train.txt'
yTrain<-read.table("./UCI HAR Dataset/train/y_train.txt", header=F,sep="",col.names="ActivityType")
yTrain$ActivityType<-as.factor(yTrain$ActivityType)
### Name activity levels in yTrain with description labels
levels(yTrain$ActivityType)<-actNames[,2]
### Combine subject id and activity levels for training data
subyTrain<-cbind(subTrain,yTrain)
### Read all parameters in file 'X_train.txt' and insert parameter names
xTrain<-read.table("./UCI HAR Dataset/train/X_train.txt",header=F,sep="")
colnames(xTrain)<-varNames[,2]
### Extract only mean and standard deviation for all parameters from training data using the function meanStd
extractTrain<-xTrain[,meanStd(xTrain)]
### Combine extracted training data with subject id and activity labels
trainExtract<-cbind(subyTrain,extractTrain)
### Insert a variable SubjectType (label "train") to indicate training data 
trainExtract$SubjectType<-as.factor(rep("train",length(trainExtract[,1])))

## Combine extracted data from test and training experiments 
fulExtract<-rbind(testExtract,trainExtract)
fullExtract<-fulExtract[,c(69,1:68)]

## Create a tidy data set containing mean and standard deviation measurements of all parameters in test and training datasets averaged by subject ID and activity type and write this tidy data set in to a text file 'tidySet.txt'
library(plyr)
tidy<- function(a=fullExtract){
  matr<-fullExtract[,4:69]
  nameExt<-aggregate(matr, by = list(fullExtract$SubjectID,fullExtract$ActivityType),FUN="mean")
  colnames(nameExt)[1]<- "SubjectID"
  colnames(nameExt)[2]<- "ActivityType"
  write.table(nameExt,"./tidySet.txt", sep="\t",row.names=F,eol="\r\n")
}