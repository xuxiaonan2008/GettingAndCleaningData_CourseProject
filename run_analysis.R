library(dplyr)
# at the very beginning I would like to talk about my own trying for the first task. 
# The following shows that I cbind the six txt.files from the file folders "test" and 
# "train" together in "combined_df" and then read "features.txt". What I would do is 
# to give the "combined_df" the features name using "colnames" but I got problems: 
# I was not able to add "subject" and "acitivity" as factors in the data.frame "features".
# If I used "append" or "rbind", the elements in "features" would change to be 
# integer-like and I did not know how to change it back. So I decided to change 
# the "features.txt" by hand, that is, to add two rows in the txt. so that I can 
# read the second column of "features.txt" including "subject" and "activity". It succeed!
# Congratulations to me! But then I got a next problem for the task 2. For the code 
# I got an error:`data` must be uniquely named but has duplicate columns. Yes it is true 
# because there are duplicate names in the "features.txt"! But why it shows me that is an
# error...I guess my code for the task 1 is something wrong. I spent too much time on it 
# but still I am not very clear about it. So I decided to see other's answer for the task 1, 
# and then beginn from it. Thanks for "https://rpubs.com/ninjazzle/DS-JHU-3-4-Final" for 
# the task 1.

## this is my try for the task 1:
# path_test = "/Users/macbookpro/Desktop/GettingAndCleaningData_CourseProject/UCI HAR Dataset/test"
# setwd(path_test)
# ls_test = list.files(path=path_test, pattern="*.txt") 
# files_df_test <- lapply(ls_test, function(x) {read.table(file = x)})
# df_test <- do.call("cbind", lapply(files_df_test, as.data.frame))
# 
# path_train = "/Users/macbookpro/Desktop/GettingAndCleaningData_CourseProject/UCI HAR Dataset/train"
# setwd(path_train)
# ls_train = list.files(path=path_train, pattern="*.txt") 
# files_df_train <- lapply(ls_train, function(x) {read.table(file = x)})
# df_train <- do.call("cbind", lapply(files_df_train, as.data.frame))
# combined_df <- rbind(df_test, df_train)
# 
# variables_features <- read.table("/Users/macbookpro/Desktop/GettingAndCleaningData_CourseProject/UCI HAR Dataset/features.txt")
# dim(variables_features) # the result is 563 2
# colnames(combined_df) <- variables_features$V2
# dim(combined_df) # the result is 10299   563

## this is my try for the task 2. But I guess there are something wrong with my code for
## the task 1 and as a result I could not continue with task 2 smoothly:
#combined_df <- data.frame(combined_df, check.names = FALSE)
#df_measure <- combined_df %>% select(subject, activity, contains("mean"), contains("std"))

## here I got the Error: `data` must be uniquely named but has duplicate columns.


# from here are the answers:
# task 1:

setwd("/Users/macbookpro/Desktop/GettingAndCleaningData_CourseProject/")

# read the "features.txt" and give column names. The second column will be used for the tidy data later:
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
head(features)
# read the "activity_labels.txt" and give column names. The "code" numbers will be changed into "activity" names in the tidy data later:
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
# read the six txt and give col.name:
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

# combine all the tables into a Merged_Data
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject, Y, X)
dim(Merged_Data) # 10299 563



# task 2:
TidyData <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))
dim(TidyData) #10299    88



# task 3: 

name.cng <- data.frame(orig = c("1", "2", "3","4","5","6"), 
                       spaced=c("WALKING", "WALKING_UPSTAIRS","WALKING_DOWNSTAIRS",
                                "SITTING", "STANDING", "LAYING") )
for (i in 1:NROW(name.cng)){ 
        TidyData$code <- sub(name.cng[i,1], name.cng[i,2], TidyData$code) }



# task 4: change variables for 13 times excpect for skewnuss(), kurtosis(), bandsEnergy()
# and angle(), because these four names are for me appropriately enough.

names(TidyData) <- gsub(x = names(TidyData), pattern = "mean()", replacement = "Mean")  
names(TidyData) <- gsub(x = names(TidyData), pattern = "std()", replacement = "Standard-deviation")
names(TidyData) <- gsub(x = names(TidyData), pattern = "mad()", replacement = "Median-absolute-deviation")
names(TidyData) <- gsub(x = names(TidyData), pattern = "max()", replacement = "Largest-value")
names(TidyData) <- gsub(x = names(TidyData), pattern = "min()", replacement = "Smallest-value")
names(TidyData) <- gsub(x = names(TidyData), pattern = "sma()", replacement = "Signal-magnitude-area")
names(TidyData) <- gsub(x = names(TidyData), pattern = "energy()", replacement = "Energy-measure")
names(TidyData) <- gsub(x = names(TidyData), pattern = "iqr()", replacement = "Interquartile-range")
names(TidyData) <- gsub(x = names(TidyData), pattern = "entropy()", replacement = "Signal-entropy")
names(TidyData) <- gsub(x = names(TidyData), pattern = "arCoeff()", replacement = "Autorregresion-coefficients")
names(TidyData) <- gsub(x = names(TidyData), pattern = "correlation()", replacement = "Correlation-coefficient")
names(TidyData) <- gsub(x = names(TidyData), pattern = "maxInds()", replacement = "Index-frequency-component")
names(TidyData) <- gsub(x = names(TidyData), pattern = "meanFreq()", replacement = "Mean-frequency")

# write the result into "TityData_task1-4.csv":
write.csv(TidyData, file = "/Users/macbookpro/Desktop/GettingAndCleaningData_CourseProject/UCI HAR Dataset/TidyData_task1-4.csv")


# task 5:

TidyData2 <- TidyData %>% group_by(subject, code) %>% summarise_all(mean)

# write the result into "TityData_task5.csv":
write.csv(TidyData2, file = "/Users/macbookpro/Desktop/GettingAndCleaningData_CourseProject/UCI HAR Dataset/TidyData_task5.csv")

