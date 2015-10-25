getwd()
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","NIKE.zip")
unzip("NIKE.zip")
list.files("./UCI HAR Dataset")

# Read test data
subjectTest = read.table('./UCI HAR Dataset/test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest =read.table('./UCI HAR Dataset/test/X_test.txt',header=FALSE); #imports x_test.txt
yTest = read.table('./UCI HAR Dataset/test/y_test.txt',header=FALSE); #imports y_test.txt

# Read in the data from files
subjectTrain = read.table('./UCI HAR Dataset/train/subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain       = read.table('./UCI HAR Dataset/train/X_train.txt',header=FALSE); #imports x_train.txt
yTrain       = read.table('./UCI HAR Dataset/train/y_train.txt',header=FALSE); #imports y_train.txt

#Read column names data
features     = read.table('./UCI HAR Dataset/features.txt',header=FALSE); #imports features.txt
activityType = read.table('./UCI HAR Dataset/activity_labels.txt',header=FALSE); #imports activity_labels.txt
View(subjectTest)
# Assign column names to the data imported above
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";
colnames(subjectTest) = "subjectId";
colnames(xTest) = features[,2];
colnames(yTest) = "activityId";

#Create training data by combining xtrain, ytrain and subjecttrain 
trainingData = cbind(subjectTrain,xTrain,yTrain);

testData = cbind(subjectTest,xTest,yTest);

# 1 create final data set by combining test and training data
combinedData = rbind(trainingData,testData);
colNames = colnames(combinedData)

# 2 stores mean and standard deviation measures and sets it to true
meanStdMsrs = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

#Get a subset of final data for mean and std deviation measures
combinedData <- combinedData[meanStdMsrs == TRUE]
View(combinedData)

# 3 Get activity names
combinedData <- merge(combinedData,activityType,by.x="activityId",by.y="activityId")
View(combinedData)

# 4 update column names to cleaner ones
colNames <- colnames(combinedData)
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};
View(combinedData)
colnames(combinedData) = colNames

# 5 create a new data set with average of each variable
combinedDataNoActivityType <- combinedData[,names(combinedData)!="activityType"]
newDataSet <- aggregate(combinedDataNoActivityType[,names(combinedDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=combinedDataNoActivityType$activityId,subjectId = combinedDataNoActivityType$subjectId),mean);
write.table(newDataSet,'./UCI HAR Dataset/tidy dataset.txt',row.names = FALSE)
