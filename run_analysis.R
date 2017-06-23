run_analysis<-function(){
        
        ## Loading train data including the Training set,
        ## Training labels, and Subject IDs
        trainData<-read.table("./train/X_train.txt")
        trainLabels<-read.table("./train/y_train.txt")
        trainSubj<-read.table("./train/subject_train.txt")
        
        ## Loading test data including the Test set, Test labels, and Subject IDs
        testData<-read.table("./test/X_test.txt")
        testLabels<-read.table("./test/y_test.txt")
        testSubj<-read.table("./test/subject_test.txt")
        
        ## Loading Activity names data
        actLabels<-read.table("activity_labels.txt")
        
        ## Loading list of all features
        features<-read.table("features.txt")
        
        ## Merges training and test data, 
        ##Activity labels associated with the data,
        ##and Subject IDs associated with the data 
        mergeData<-rbind(trainData,testData)
        mergeLabels<-rbind(trainLabels,testLabels)
        mergeSubject<-rbind(trainSubj,testSubj)
        
        ## Names the data set and creates a tidy data set with
        ## measurements of the mean and standard deviation
        names(mergeData)<-features$V2
        choosen_columns<-grep(".*[Mm]ean|.*[Ss]td",names(mergeData))
        mean_and_std<-mergeData[,choosen_columns]
        mean_and_std$activity<-actLabels$V2[mergeLabels$V1]
        mean_and_std$subject<-mergeSubject$V1
        
        ## Creates a second tidy data set with average of each variable for
        ## each activity and each subject
        ave_mean_and_std<-group_by(mean_and_std,subject,activity)%>%summarise_all(funs(mean))
        
        ## Writes a tidy data set into a file
        write.table(ave_mean_and_std, file="ave_mean_and_std.txt",row.names = FALSE)
}
        