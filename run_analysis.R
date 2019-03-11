library(dplyr)
directory<-"C:/Users/hp/Desktop/Coursera/GettingAndCleaningData/UCI HAR Dataset/test"
setwd(directory)

convertFileToCSV<- function(directory){
  
  files<-list.files(directory, pattern=".txt", full.names=TRUE)
  for(i in 1:length(files)){
    df<- read.table(files[i], header = FALSE)
    x<- strsplit(files[i],"/")
    newfile<- paste(gsub("\\..*","",gsub("\\..*","",x[[c(length(x[1]),length(x[[1]]))]])),"csv",sep=".")
    write.csv(df, newfile, row.names = FALSE)
    
  }

}

##1. Merging the training and the test sets to create one data set

convertFileToCSV(directory)
dataX_test<-read.csv("X_test.csv",header=TRUE)
dataY_test<-read.csv("y_test.csv",header=TRUE)
dataSubject_test<-read.csv("subject_test.csv",header=TRUE)

varlistY_test<-colnames(dataY_test)
varlistSubject_test<-colnames(dataSubject_test)


dataY_test<-rename(dataY_test, ActivityLabel=varlistY_test[1])
dataSubject_test<-rename(dataSubject_test, SubjectLabel=varlistSubject_test[1])


testData<-cbind(dataX_test,dataY_test,dataSubject_test)

directory<-"C:/Users/hp/Desktop/Coursera/GettingAndCleaningData/UCI HAR Dataset/train"
setwd(directory)
convertFileToCSV(directory)

dataX_train<-read.csv("X_train.csv",header=TRUE)
dataY_train<-read.csv("y_train.csv",header=TRUE)
dataSubject_train<-read.csv("subject_train.csv",header=TRUE)

varlistY_train<-colnames(dataY_train)
varlistSubject_train<-colnames(dataSubject_train)


dataY_train<-rename(dataY_train, ActivityLabel=varlistY_train[1])
dataSubject_train<-rename(dataSubject_train, SubjectLabel=varlistSubject_train[1])


trainData<-cbind(dataX_train,dataY_train,dataSubject_train)

directory<-"C:/Users/hp/Desktop/Coursera/GettingAndCleaningData/UCI HAR Dataset"
setwd(directory)

activity<-read.table("activity_labels.txt",header=FALSE)
data<-rbind(testData,trainData)
write.csv(data, "MergedData.csv", row.names = FALSE)


## 2.Extracting only the measurements on the mean and standard deviation for each measurement
directory<-"C:/Users/hp/Desktop/Coursera/GettingAndCleaningData/UCI HAR Dataset"
setwd(directory)
features<-read.table("features.txt",header=FALSE)
features<-mutate(features,V3=paste0("V",as.character(V1)))
write.csv(features, "features.csv", row.names = FALSE)

std<-grepl(pattern = "std", features$V2, ignore.case = TRUE)
m<-grepl(pattern = "mean", features$V2, ignore.case = TRUE)
stdMean<-features[std | m,]$V3

MergedData<-read.csv("MergedData.csv",header = T)
subsetMergedData1<- select(MergedData, stdMean)
c<-colnames(subsetMergedData1)

##the data set is labelled with descriptive variable names

renameColumns<-function(data,newcolnames){
  if(length(colnames(data))==length(newcolnames)){
    for (i in 1:length(colnames(data))){
      colnames(data)[i]<-as.character(newcolnames[i])
    }
  }
  else print("length of columns do not match")
  
  data
}

subsetMergedData1<-renameColumns(subsetMergedData1,features[std | m,]$V2)

subsetMergedData2<- select(MergedData, ActivityLabel:SubjectLabel)
subsetMergedData<-cbind(subsetMergedData1,subsetMergedData2)

write.csv(subsetMergedData, "subsetMergedData.csv", row.names = FALSE)


##descriptive activity names to name the activities in the data set

subsetMergedData$ActivityLabel <- factor(subsetMergedData$ActivityLabel, 
                                         levels = activity[,1], labels = activity[,2])


## a second, independent tidy data set is created with the average of each variable 
## for each activity and each subject.
by=list(activity = allData$activity, subject=allData$subject)

tidydata<- aggregate(subsetMergedData, by=list(activity = subsetMergedData$ActivityLabel, subject=subsetMergedData$SubjectLabel), FUN = mean)

tidydata[,90] = NULL
tidydata[,89] = NULL
write.table(tidydata, "tidy.txt", sep="\t")


