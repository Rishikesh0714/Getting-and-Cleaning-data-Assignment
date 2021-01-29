#Setting appropriate Working Directory
setwd(
  "D:/Rishikesh_Data_Science/R_proggrame_coursera/Getting and cleaning data/ASSIGNMENT"
)
fileURL <-
  "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

#Check if file exists and download file
if (!file.exists("Data"))
{
  dir.create("Data")
}

#Download Data
download.file(fileURL, destfile = "./Data/Raw_Dataset.zip")
dateDownloaded <- date()

#date:- "Thu Jan 28 20:08:06 2021"

library(dplyr)

#read training data
train_x <-
  read.table("./Data/Raw_Dataset/UCI HAR Dataset/train/X_train.txt", )
train_y <-
  read.table("./Data/Raw_Dataset/UCI HAR Dataset/train/y_train.txt", )
sub_train <-
  read.table("./Data/Raw_Dataset/UCI HAR Dataset/train/subject_train.txt",
  )

#read test data
test_x <-
  read.table("./Data/Raw_Dataset/UCI HAR Dataset/test/X_test.txt", )
test_y <-
  read.table("./Data/Raw_Dataset/UCI HAR Dataset/test/y_test.txt", )
sub_test <-
  read.table("./Data/Raw_Dataset/UCI HAR Dataset/test/subject_test.txt", )


##Q.1. Merges the training and the test sets to create one data set.
#Column Binding train(x,y),test(x,y),sub_train and sub_test.Then Row binding together

HumanActivity <-
  rbind(cbind(sub_train, train_x, train_y),
        cbind(sub_test, test_x, test_y))

#read features and activity label
features <-
  read.table("./Data/Raw_Dataset/UCI HAR Dataset/features.txt", )

#Changing column names
colnames(HumanActivity) <- c("Subjects", features[, 2], "Activity")

#remove base table as we dont require it anymore besides it take up our memory
rm(train_x, train_y, sub_train, test_x, test_y, sub_test)


## Q.2. Extracts only the measurements on the mean and standard deviation for each measurement
ColIntrested <-
  grepl("Subject|mean|std|Activity", colnames(HumanActivity)) #Logical output so we know which coloumn have mean std and activity(wich is y)
HumanActivity <- HumanActivity[, ColIntrested]


## Q.3. Uses descriptive activity names to name the activities in the data set

#Read Activity Table
activity_labels <-
  read.table("./Data/Raw_Dataset/UCI HAR Dataset/activity_labels.txt")

HumanActivity$Activity <-
  factor(HumanActivity$Activity,
         levels = activity_labels[, 1],
         labels = activity_labels$V2)


## Q.4. Appropriately labels the data set with descriptive variable names

ColumnNames <- colnames(HumanActivity) #listing column names

#removing metacharacters "()" and "-"
ColumnNames <- gsub("-", "", ColumnNames)
ColumnNames <- gsub("\\()", "", ColumnNames)

#Using Full form in the variables from features_info.txt
ColumnNames <- sub("^t", "TimeDomain", ColumnNames)
ColumnNames <- sub("^f", "FrequencyDomain", ColumnNames)
ColumnNames <- gsub("Acc", "Accelerometer", ColumnNames)
ColumnNames <- gsub("Gyro", "Gyrometer", ColumnNames)
ColumnNames <-
  gsub("[Ff]req", "Frequency", ColumnNames, ignore.case = "FrequencyDomain")
ColumnNames <- gsub("Mag", "Magnitude", ColumnNames)
ColumnNames <- gsub("mean", "Mean", ColumnNames)
ColumnNames <- gsub("std", "StandardDeviation", ColumnNames)

# removing Repeating "body"
ColumnNames <- gsub("([Bb][o][d][y])+", "Body", ColumnNames)

# Replacing column names
colnames(HumanActivity) <- ColumnNames


## Q.5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
HumanActivityMean <-
  HumanActivity %>% group_by(Subjects, Activity) %>% summarise_each(funs = mean)


# Writing tidy data
write.table(HumanActivity, "tidy_HumanActivity_data.txt")
write.table(HumanActivityMean, "tidy_HumanActivityMean_data.txt")
