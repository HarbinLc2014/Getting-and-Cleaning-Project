
                                                                         ## Load the file and package

zipfilename<-"getdata-projectfiles-UCI HAR Dataset.zip"
foldername<-"getdata-projectfiles-UCI HAR Dataset"
library(dplyr)
if(!file.exists(foldername))
{
    if(!file.exists(zipfilename))
      {
           stop("File Not Found")
       }
    unzip(zipfilename,junkpath=TRUE)
}
features<-read.table("features.txt",sep="",stringsAsFactors=FALSE)
xtest<-read.table("X_test.txt",sep="",colClasses=(rep("numeric",561)))
ytest<-read.table("y_test.txt",sep="")
subjecttest<-read.table("subject_test.txt",sep="",colClasses="integer")

xtrain<-read.table("X_train.txt",sep="",colClasses=(rep("numeric",561)))
ytrain<-read.table("y_train.txt",sep="")
subjecttrain<-read.table("subject_train.txt",sep="")

##Set column names for Subject and Activity data set

colnames(subjecttest)[1]="Subject"
colnames(subjecttrain)[1]="Subject"
colnames(ytrain)[1]="Activity"
colnames(ytest)[1]="Activity"

                                                                         ##Transform numeric values to activity labels in train data set

j<-1
for(i in ytrain$V1){
if(i == 1){
ytrain[j,1]<-"WALKING"
}
if(i == 2){
ytrain[j,1]<-"WALKING_UPSTAIRS"
}
if(i == 3){
ytrain[j,1]<-"WALKING_DOWNSTAIRS"
}
if(i == 4){
ytrain[j,1]<-"SITTING"
}
if(i == 5){
ytrain[j,1]<-"STANDING"
}
if(i == 6){
ytrain[j,1]<-"LAYING"
}
j<-j+1
}

##Reset value for parameter j

j<-1


                                                                         ##Transform numeric values to activity labels in test data set

for(i in ytest$V1){
if(i == 1){
ytest[j,1]<-"WALKING"
}
if(i == 2){
ytest[j,1]<-"WALKING_UPSTAIRS"
}
if(i == 3){
ytest[j,1]<-"WALKING_DOWNSTAIRS"
}
if(i == 4){
ytest[j,1]<-"SITTING"
}
if(i == 5){
ytest[j,1]<-"STANDING"
}
if(i == 6){
ytest[j,1]<-"LAYING"
}
j<-j+1
}

##Merge test dataset and train dataset

test<-cbind(xtest,subjecttest,ytest)
train<-cbind(xtrain,subjecttrain,ytrain)
label1<-features$V2
label2<-"Subject"
label3<-"Activity"
label<-c(label2,label3,label1)
dataset<-rbind(test,train)

                                                                         ##Create a new dataset groupby Subject and Activity

result<-group_by(dataset,Subject,Activity)

                                                                         ##Use summarise_each to get mean value

final<-summarise_each_(result,funs(mean),colnames(result)[1:561])
colNames<-label
colnames(final)<-label

                                                                         ##Get mean and standard deviation colunms from features and other labels

p = (grepl("Mean..",colNames) | grepl("Activity",colNames) | grepl("Subject",colNames) | grepl("-mean()..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean()..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))
final<-final[p==TRUE]
colNames<-colnames(final)
i<-1

                                                                         ##Make appropriate colunm names for the new dataset

for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("-mean()","Mean",colNames[i])
  colNames[i] = gsub("-std()","Std",colNames[i])
  colNames[i] = gsub("^(t)","Time",colNames[i])
  colNames[i] = gsub("^(f)","Freq",colNames[i])
  colNames[i] = gsub("gravity","Gravity",colNames[i])
  colNames[i] = gsub("angle","Angle",colNames[i])
  colNames[i] = gsub("tBody","TimeBody",colNames[i])
  colNames[i] = gsub("BodyBody","Body",colNames[i])
}


colnames(final)<-colNames

                                                                         ##Create textfile and write dataset to file

if(!file.exists("data.txt")){
file.create("data.txt")
}
write.table(final,"data.txt",row.name=FALSE,sep='\t')

                                                                         ##Work finished

print("The work has been saved in file data.txt ")


