require(plyr)

addSuffix <- function(x, suffix) {
        if (!(x %in% c("Subject","Activity"))) {
                paste(x,suffix, sep = "")
        }
        else{
                x
        }
}

#Set the path
base_path <- file.path(getwd(),"UCI HAR Dataset")
test_path <- file.path(base_path, "test")
train_path <- file.path(base_path, "train")

#Get the TEST data
xtest <- read.table(file.path(test_path,"X_test.txt"))
ytest <- read.table(file.path(test_path,"Y_test.txt"))
subjecttest <- read.table(file.path(test_path,"subject_test.txt"))

#Get the TRAIN data
xtrain <- read.table(file.path(train_path,"X_train.txt"))
ytrain <- read.table(file.path(train_path,"Y_train.txt"))
subjecttrain <-
        read.table(file.path(train_path,"subject_train.txt"))

#Get activity labels
activitylabels <-
        read.table(file.path(base_path, "activity_labels.txt"),
                   col.names = c("Id", "Activity"))

#Get features labels
featurelabels <- read.table(file.path(base_path, "features.txt"),
                            colClasses = c("character"))

#1.Merges the training and the test sets to create one data set.
traindata <- cbind(cbind(xtrain, subjecttrain), ytrain)
testdata <- cbind(cbind(xtest, subjecttest), ytest)
sensordata <- rbind(traindata, testdata)
sensorlabels <-
        rbind(rbind(featurelabels, c(562, "Subject")), c(563, "Id"))[,2]
names(sensordata) <- sensorlabels

#2. Extracts only the measurements on the mean and standard deviation for each measurement.
sensordatameanstd <-
        sensordata[,grepl("mean\\(\\)|std\\(\\)|Subject|Id", names(sensordata))]

#3. Uses descriptive activity names to name the activities in the data set.
sensordatameanstd <-
        join(sensordatameanstd, activitylabels, by = "Id", match = "first")
sensordatameanstd <- sensordatameanstd[,-1]

#4. Appropriately labels the data set with descriptive names.
names(sensordatameanstd) <-
        gsub("([()])","",names(sensordatameanstd))
names(sensordatameanstd) <- make.names(names(sensordatameanstd))

#5. From the data set in step 4, creates a second, independent tidy data set
#   with the average of each variable for each activity and each subject.
finaldata <-
        ddply(sensordatameanstd, c("Subject","Activity"), numcolwise(mean))
finaldataheaders <- names(finaldata)
finaldataheaders <- sapply(finaldataheaders, addSuffix, ".mean")
names(finaldata) <- finaldataheaders

#FINAL STEP:
#A tidy data set as a txt file created with write.table() using row.name=FALSE
write.table(finaldata, file = "tidy_data_set_by_tamas_izsak.txt", row.name = FALSE)