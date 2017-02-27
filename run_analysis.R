library(dplyr)
##################################################################################################

##########   Getting and Cleaning the Assignment ################################

#this part is for download and unzip the file folder
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","temp.zip")
data <- unzip("temp.zip",files = NULL, list = FALSE, overwrite = TRUE, junkpaths = FALSE, exdir = ".", unzip = "internal",setTimes = FALSE)

#read all necesary files to proccess
activity_labels<- read.table("./UCI HAR Dataset/activity_labels.txt")
features<-read.table("./UCI HAR Dataset/features.txt")
x_test<-read.table("./UCI HAR Dataset/test/X_test.txt")
y_test<-read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")
x_train<- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train<- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train<- read.table("./UCI HAR Dataset/train/subject_train.txt")

##################################################################################################
#1 Merges the training and the test sets to create one data set.

# merge all train data in xy_train
xy_train<-cbind(subject_train,y_train,x_train)

# merge all test data in xy_test
xy_test<- cbind(subject_test, y_test, x_test)

# merge with r bind the test and train data
xy_test_train<-rbind(xy_test,xy_train)


# to know the origin of the data (test or train) we need add a columm that specified
# the data frame origin for each observacion

# created a variable with the dim's of xy_test
xy_test_dim<-dim(xy_test)

# created a variable with the dim's of xy_train
xy_train_dim<-dim(xy_train)

# created a vector of factors "test, train" as many values of train as the 
# number of observatios in xy_train and the same to test.
db_source<-as.factor(c(rep("test", xy_test_dim[1]),rep("train", xy_train_dim[1] )))

#finnaly we join all data (xy_test_train and the db_source) in the data frame all_test_train.
all_test_train<-cbind(db_source, xy_test_train)
# chec the dim of the new data frame
dim(all_test_train)

# now we have to set the columns names (features) of the new data frame
# paste the name and the number of the variable name included in the feature data frame
features_proc<- paste(features$V2, features$V1, sep="_")

# we need remember that early we added 3 columns to original estructure of the x_train and x_test.
#this colums are: db_source (data frame source: train or test), the subject 
#that performed the activity (train and test) and the class labels activity. Now
# created a vector with the names of the new's 3 columns and all features proccesed
columns_names<-c("df_source", "subject", "activity" , features_proc)

# now set the colnames to the all_test_train data frame
colnames(all_test_train)<-columns_names






##################################################################################################
#2 Extracts only the measurements on the mean and standard deviation for each measurement.

#use the grep function to obtain the number of columns where appears "mean" or "std"
features_fm1<-grep("subject|activity|mean|std" ,columns_names)

#subset all_test_train to obtain the desire data frame
subs_all<- all_test_train[,features_fm1]



##################################################################################################
#3 Uses descriptive activity names to name the activities in the data set
# replace activity values in all_test_train using factor levels
all_test_train$activity <- factor(all_test_train$activity, 
                                 levels = activity_labels[, 1], labels = activity_labels[, 2])






##################################################################################################
#4Appropriately labels the data set with descriptive variable names.
#edit some text name of the variables to make R undestandable and make clear the variable names

features_proc<-gsub("X", "_on_the_x_axis", features_proc)
features_proc<-gsub("Y", "_on_the_y_axis", features_proc)
features_proc<-gsub("Z", "_on_the_z_axis", features_proc)


#convert the uppercases to lower
features_proc<-tolower(features_proc)


features_proc<-gsub( "^f|\\(t", "frequency_domain_",features_proc )
features_proc<-gsub("^t", "time_domain_", features_proc)
features_proc<-gsub("gyro", "_gyroscope", features_proc)
features_proc<-gsub("acc", "_accelerometer", features_proc)
features_proc<-gsub("-","_", features_proc)
features_proc<-gsub("\\)","_", features_proc)
features_proc<-gsub("\\(","_", features_proc)
features_proc<-gsub("\\,","_", features_proc)
features_proc<-gsub("std","_standard_deviation", features_proc)
features_proc<-gsub("mad", "_median_absolute_deviation", features_proc)
features_proc<-gsub("max","_largest_value_in_array",features_proc )
features_proc<-gsub("min", "_smallest_value_in_array",features_proc )
features_proc<-gsub("sma","_signal_magnitude_area",features_proc )
features_proc<-gsub("iqr", "_interquartile_range", features_proc)
features_proc<-gsub("entropy", "_signal_entropy", features_proc)
features_proc<-gsub("arcoeff","_autorregresion_coefficients", features_proc)
features_proc<-gsub("maxinds", "_index_of_the_frequency_component_with_largest_magnitude",features_proc )
features_proc<-gsub("meanfreq", "_weighted_average_of_the_frequency_components" ,features_proc )
features_proc<-gsub("skewness", "_skewness_of_the_frequency_domain_signal",features_proc )
features_proc<-gsub("bandsEnergy", "_energy_of_a_frequency_interval", features_proc) 
features_proc<-gsub("angle","angle_between_to_vectors_", features_proc)
features_proc<-gsub("bodybody","body", features_proc)
features_proc<-gsub("mean","_mean", features_proc)

#regenerate the vector of the columns names
columns_names<-c("df_source", "subject", "activity" , features_proc)

# now set the colnames again to the all_test_train data frame
colnames(all_test_train)<-columns_names


##################################################################################################
#5From the data set in step 4, creates a second, independent tidy data set 

#with the average of each variable for each activity and each subject.
# by practical purposes with gruop by db_source (train or test)

all_test_trains_means <- all_test_train %>% 
    group_by(df_source,subject, activity) %>%
    summarise_each(funs(mean))
dim(all_test_trains_means)

# write a new file "tidy_data.txt"
write.table(all_test_trains_means, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
