# Peer-Assesement-for-Getting-and-Cleaning-data
#The run_analysis.R is the script to get a tidy data set.

#reading the train and test data sets
setwd("test");
testx<-read.table("X_test.txt");
testy<-read.table("y_test.txt");
tests<-read.table("subject_test.txt");
setwd("../");
setwd("train");
trainx<-read.table("X_train.txt");
trainy<-read.table("y_train.txt");
trains<-read.table("subject_train.txt");

#reading the names from features.txt
setwd("../");
names<-read.table("features.txt");

#Binding the train and test datasets for x,y and subjects. 
datax<-rbind(trainx,testx);
names(datax)<-names[,2];
datas<-rbind(trains,tests);
datay<-rbind(trainy,testy);
datas$subject<-as.character(datas[,1]);
datay$activity<-as.character(datay[,1]);

#Giving values for 1 to 6 in the datay data.
datay[,2]<-gsub("1","WALKING",datay[,2]);
datay[,2]<-gsub("2","WALKING_UPSTAIRS",datay[,2]);
datay[,2]<-gsub("3","WALKING_DOWNSTAIRS",datay[,2]);
datay[,2]<-gsub("4","SITTING",datay[,2]);
datay[,2]<-gsub("5","STANDING",datay[,2]);
datay[,2]<-gsub("6","LAY",datay[,2]);

making the three datasets, namely the datax,datay ans datas to be one single dataset
datall<-cbind(datax,datas[,2],datay[,2]);

#naming the 562 and 563 column as subject and activity respectively
names(datall)[562]<-"subject";
names(datall)[563]<-"activity";
names<-names(datall);

#Naming the second tidy data set as mean and std dv
mindex<-grep("mean()",names);
mnames<-names[mindex];
sindex<-grep("std()",names);
snames<-names[sindex];
meandata<-datall[,mnames];
stddata<-datall[,snames];

#Binding the meandata, stddata , subject and activity from datall
fdata<-cbind(meandata,stddata,datall$subject,datall$activity);
names(fdata)[80]<-"subject";
names(fdata)[81]<-"activity";
fdata$subject<-as.character(fdata$subject);
fdata$subject<-as.numeric(fdata$subject);
fdata$activity<-as.character(fdata$activity);
index<-order(fdata$subject,fdata$activity);
fdata<-fdata[index,];
fdata$activity<-as.factor(fdata$activity);
names(fdata)<-gsub("-","",names(fdata));
names(fdata)<-gsub("\\()","",names(fdata));
s<-split(fdata,list(fdata$subject,fdata$activity));

#naming the fdata
snames<-names(fdata);
snames<-snames[1:79];
ff<-sapply(s,function(x) colSums(x[,snames])/dim(x)[1]);
t<-t(ff);
rownames<-rownames(t);
subject<-substr(rownames,1,2);
subject<-gsub("\\.","",subject);
activity<-substr(rownames,3,length(rownames));
activity<-gsub("\\.","",activity);
dataset<-data.frame(t,row.names=NULL);
names<-names(dataset);
names<-paste("Avg",names,sep="");
names(dataset)<-names;
dataset$subject<-as.factor(subject);
dataset$activity<-as.factor(activity);
finaldata<-data.frame(dataset[,80:81],dataset[,1:79]);
write.table(finaldata,file="finaldata1.txt");
