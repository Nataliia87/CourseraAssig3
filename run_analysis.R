## You have downloaded Sumsung Data set from "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip".
## You unzip the file, so you have the folder"UCI HAR Dataset".
## Set it as a Home directory.
## For the assignments of this project,the files in the "Inertial Signals" folders are not used.
## We will need some packages:
library(data.table)
library(plyr)
### Reading files
## Read the subject files:
testsubject<-fread("test/subject_test.txt")
trainsubject<-fread("train/subject_train.txt")
## Read the data files:
testdata<-fread("test/X_test.txt")
traindata<-fread("train/X_train.txt")
## Read the activity files:
testact<-fread("test/y_test.txt")
trainact<-fread("train/y_train.txt")

### 1 Merging the training and the test sets to create one data set.
## Now we have 6 files, and we need to combine them.
## 
subject<-rbind(testsubject,trainsubject)
## Make a normal name of our column 
setnames(subject,"V1","Subject")
##
act<-rbind(testact,trainact)
setnames(act,"V1","Activity")
##
data<-rbind(testdata,traindata)
## Combine them
subject<-cbind(subject,act)
data<-cbind(subject,data)
## Setting a key
setkey(data, Subject, Activity)

### 2 Extract only the measurements on the mean and standard deviation for each measurement.
## We have features.txt file in folder. Need to read it.
features<- fread("features.txt")
features<-setnames(features,names(features),c("Number","Name"))
features <- features[grepl("mean\\(\\)|std\\(\\)", Name)]
## Create a new column that will show us number of the feature that we need and will correspond with our main data set.
features$fCode <- features[, paste0("V", Number)]

select <- c(key(data), features$fCode)
data<-data[,select,with = FALSE]
##Will be data set that we need.

### 3 Use descriptive activity names to name the activities in the data set.
## We have activity_labels.txt file in our folder. Read it.
activities<-fread("activity_labels.txt")
setnames(activities, names(activities), c("Activity", "actName"))
## Can use this file on the next steps.

### 4 Appropriately label the data set with descriptive variable names.
## Merge two tables into data 
data<- merge(data,activities,by="Activity",all.x=TRUE)
## Our last column will show activity.
## Now we do not need the first column of our data. Can remove it:
data<-data[,-1]

## Now we need to make beautiful names for our columns- features. 
## Take names of features in vector.
colnames<-features$Name
## Make them readable
colnames <- gsub("Acc", "Accelerometr", colnames)
colnames <- gsub("Gyro", "Gyroscope", colnames)
colnames <- gsub("^t", "Time", colnames)
colnames <- gsub("^f", "Frequency", colnames)
colnames<- gsub("BodyBody", "Body", colnames)
colnames <- gsub("mean", "Mean", colnames)
colnames <- gsub("std", "Std", colnames)
colnames <- gsub("Mag", "Magnitude", colnames)
colnames <- gsub("\\(|\\)", "", colnames, perl  = TRUE)
## Now it's a vector of readable names.
## Take names of col in our data. We need to leave inly features col. So we just remove another col.
old<-names(data)
old1<-old[-68]
old2<-old1[-1]
## Now we change our unredable col names on readable.
setnames(data,old=old2,new=colnames)

### 5 From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.

Tidydata<-ddply(data,c("Subject","actName"),numcolwise(mean))
write.table(Tidydata,file="Tidydata.txt")

## Making a codebook

codebook <- data.frame(variable.name=c(names(Tidydata)),
    Description=c("ID the subject who performed the activity for each window sample. Its range is from 1 to 30.",
               "Activity name",
               "Average from mean of time variable for body signal in X direction using Accelerometr",
               "Average from mean of time variable for body signal in Y direction using Accelerometr",
               "Average from mean of time variable for body signal in Z direction using Accelerometr",
               "Average from standard deviation of time variable for body signal in X direction using Accelerometr",
               "Average from standard deviation of time variable for body signal in Y direction using Accelerometr",
               "Average from standard deviation of time variable for body signal in Z direction using Accelerometr",
               "Average from mean of gravity variable for body signal in X direction using Accelerometr",
               "Average from mean of gravity variable for body signal in Y direction using Accelerometr",
               "Average from mean of gravity variable for body signal in Z direction using Accelerometr",
               "Average from standard deviation of gravity variable for body signal in X direction using Accelerometr",
               "Average from standard deviation of gravity variable for body signal in Y direction using Accelerometr",
               "Average from standard deviation of gravity variable for body signal in Z direction using Accelerometr",
               "Average from mean of time variable for body Jerk signal in X direction using Accelerometr",
               "Average from mean of time variable for body Jerk signal in Y direction using Accelerometr",
               "Average from mean of time variable for body Jerk signal in Z direction using Accelerometr",
               "Average from standard deviation of time variable for body Jerk signal in X direction using Accelerometr",
               "Average from standard deviation of time variable for body Jerk signal in Y direction using Accelerometr",
               "Average from standard deviation of time variable for body Jerk signal in Z direction using Accelerometr",
               "Average from mean of time variable for body signal in X direction using Gyroscope",
               "Average from mean of time variable for body signal in Y direction using Gyroscope",
               "Average from mean of time variable for body signal in Z direction using Gyroscope",
               "Average from standard deviation of time variable for body signal in X direction using Gyroscope",
               "Average from standard deviation of time variable for body signal in Y direction using Gyroscope",
               "Average from standard deviation of time variable for body signal in Z direction using Gyroscope",
               "Average from mean of time variable for body Jerk signal in X direction using Gyroscope",
               "Average from mean of time variable for body Jerk signal in Y direction using Gyroscope",
               "Average from mean of time variable for body Jerk signal in Z direction using Gyroscope",
               "Average from standard deviation of time variable for body Jerk signal in X direction using Gyroscope",
               "Average from standard deviation of time variable for body Jerk signal in Y direction using Gyroscope",
               "Average from standard deviation of time variable for body Jerk signal in Z direction using Gyroscope",
               "Average from mean of time variable for body signal magnitude using Accelerometr",
               "Average from standard deviation of time variable for body signal magnitude using Accelerometr",
               "Average from mean of time variable for gravity signal magnitude using Accelerometr",
               "Average from standard deviation of time variable for graviry signal magnitude using Accelerometr",
               "Average from mean of time variable for body Jerk signal magnitude using Accelerometr",
               "Average from standard deviation of time variable for body Jerk signal magnitude using Accelerometr",
               "Average from mean of time variable for body signal magnitude using Gyroscope",
               "Average from standard deviation of time variable for body signal magnitude using Gyroscope",
               "Average from mean of time variable for body Jerk signal magnitude using Gyroscope",
               "Average from standard deviation of time variable for body Jerk signal magnitude using Gyroscope",
               "Average from mean of frequency variable for body signal in X direction using Accelerometr",
               "Average from mean of frequency variable for body signal in Y direction using Accelerometr",
               "Average from mean of frequency variable for body signal in Z direction using Accelerometr",
               "Average from standard deviation of frequency variable for body signal in X direction using Accelerometr",
               "Average from standard deviation of frequency variable for body signal in Y direction using Accelerometr",
               "Average from standard deviation of frequency variable for body signal in Z direction using Accelerometr",
               "Average from mean of frequency variable for body Jerk signal in X direction using Accelerometr",
               "Average from mean of frequency variable for body Jerk signal in Y direction using Accelerometr",
               "Average from mean of frequency variable for body Jerk signal in Z direction using Accelerometr",
               "Average from standard deviation of frequency variable for body Jerk signal in X direction using Accelerometr",
               "Average from standard deviation of frequency variable for body Jerk signal in Y direction using Accelerometr",
               "Average from standard deviation of frequency variable for body Jerk signal in Z direction using Accelerometr",
               "Average from mean of frequency variable for body signal in X direction using Gyroscope",
               "Average from mean of frequency variable for body signal in Y direction using Gyroscope",
               "Average from mean of frequency variable for body signal in Z direction using Gyroscope",
               "Average from standard deviation of frequency variable for body signal in X direction using Gyroscope",
               "Average from standard deviation of frequency variable for body signal in Y direction using Gyroscope",
               "Average from standard deviation of frequency variable for body signal in Z direction using Gyroscope",
               "Average from mean of frequency variable for body signal magnitude using Accelerometr",
               "Average from standard deviation of frequency variable for body signal magnitude using Accelerometr",
               "Average from mean of frequency variable for body Jerk signal magnitude using Accelerometr",
               "Average from standard deviation of frequency variable for body Jerk signal magnitude using Accelerometr",
               "Average from mean of frequency variable for body signal magnitude using Gyroscope",
               "Average from standard deviation of frequency variable for body signal magnitude using Gyroscope",
               "Average from mean of frequency variable for body Jerk signal magnitude using Gyroscope",
               "Average from standard deviation of frequency variable for body Jerk signal magnitude using Gyroscope"
                              ),
   source=rep("CourseraAssig3",length(Tidydata)),
   filename = rep("CodeBook.md",length(Tidydata))
   
)
## Saving CodeBook in MD file.
write.table(codebook,file="CodeBook.md")
### Finished.
