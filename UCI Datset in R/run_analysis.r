run_analysis <- function(){
    library(data.table)
    library(dplyr)
    library(reshape2)
    
    meas.train = read.table("./UCI HAR Dataset/train/X_train.txt")
    meas.test = read.table("./UCI HAR Dataset/test/X_test.txt")
    MEASURE = rbind(meas.train,meas.test)

    act.train = read.table("./UCI HAR Dataset/train/y_train.txt")
    act.test = read.table("./UCI HAR Dataset/test/y_test.txt")
    ACT = rbind(act.train,act.test)
    ACT = rename(ACT, Activity_Label = V1)
    for(i in 1:nrow(ACT)){
        if (ACT[i,1]== 1){
            ACT[i,1]= "walking"
        }
        if (ACT[i,1]==2){
            ACT[i,1] = "walking upstairs"
        }
        if (ACT[i,1]==3){
            ACT[i,1] = "walking downstairs"
        }
        if (ACT[i,1]==4){
            ACT[i,1] = "sitting"
        }
        if (ACT[i,1]==5){
            ACT[i,1] = "standing"
        }
        if (ACT[i,1]==6){
            ACT[i,1] = "laying"
        }
    }
    
    
    subj.train = read.table("./UCI HAR Dataset/train/subject_train.txt")
    subj.test = read.table("./UCI HAR Dataset/test/subject_test.txt")
    SUBJECT = rbind(subj.train,subj.test)
    SUBJECT = rename(SUBJECT, Subject_ID = V1)
    
    meas.name = read.table("./UCI HAR Dataset/features.txt")
    meas.spec.name = meas.name[[2]]
    meas.spec.name = as.character(meas.spec)
    meas.w.name = setNames(MEASURE,meas.spec.name)
    find = grep(".[Mm]ean\\(\\)|.std\\(\\)",meas.spec.name)
    filter.meas = meas.w.name[,find]
    comb.data = cbind(SUBJECT,filter.meas,ACT)
    write.table(comb.data,"./3_GandCData/dataset.txt",row.names=FALSE)
    
    list <- as.character(names(filter.meas))
    melt.data <- melt(comb.data,id = c("Subject_ID","Activity_Label"),measure.vars = list)
    subj.data <- dcast(melt.data,Subject_ID ~ variable,mean)
    act.data <- dcast (melt.data,Activity_Label ~ variable,mean)
    write.table(subj.data,"./3_GandCData/Subject_ID_mean.txt",row.names=FALSE)
    write.table(act.data,"./3_GandCData/Activity_ID_mean.txt",row.names=FALSE)
}
