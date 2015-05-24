require(data.table)
require(reshape2)

path <- getwd()
pathIn <- file.path(path, "UCI HAR Dataset")

url <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if (!file.exists(path)) {dir.create(path)}
download.file(url, file.path(path, f)) 

dtSubjectTraining <- fread(file.path(pathIn, "train", "subject_train.txt"))
dtSubjectTest  <- fread(file.path(pathIn, "test" , "subject_test.txt" ))
dtActivityTraining <- fread(file.path(pathIn, "train", "Y_train.txt"))
dtActivityTest  <- fread(file.path(pathIn, "test" , "Y_test.txt" ))

fileToDataTable <- function (f) {
+     df <- read.table(f)
+     dt <- data.table(df)
+ }

# Merges the training and the test sets to create one data set.

dtTraining <- fileToDataTable(file.path(pathIn, "train", "X_train.txt"))
dtTest  <- fileToDataTable(file.path(pathIn, "test" , "X_test.txt" ))
dtSubject <- rbind(dtSubjectTraining, dtSubjectTest)
setnames(dtSubject, "V1", "subject")

dtActivity <- rbind(dtActivityTraining, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")

dt <- rbind(dtTrain, dtTest)
dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)

# Extracts only the measurements on the mean and standard deviation for each measurement.
 
setkey(dt, subject, activityNum)
dtFeatures <- fread(file.path(pathIn, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]
dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with=FALSE]

# Uses descriptive activity names to name the activities in the data set

dtActivityNames <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

# Appropriately labels the data set with descriptive variable names. 

dt <- merge(dt, dtActivityNames, by="activityNum", all.x=TRUE)
setkey(dt, subject, activityNum, activityName)

dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))
dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)
dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)
grepthis <- function (regex) {
+     grepl(regex, dt$feature)
+ }

cat <- 2
y <- matrix(seq(1, cat), nrow=n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
dt$featDom <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
dt$featInst <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
dt$featAcc <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
dt$featVar <- factor(x %*% y, labels=c("Mean", "SD"))
dt$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
dt$featMag <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))
cat <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

setkey(dt, subject, activity, featDom, featAcc, featInst, featJerk, featMag, featVar, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]

# Write the File

f <- file.path(path, "DatasetHumanActivityRecognitionUsingSmartphones.txt")
write.table(dtTidy, f, quote=FALSE, sep="\t", row.names=FALSE)
