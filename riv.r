# Library
library(reshape2)
library(plyr)

# Load files
setwd("~/Desktop/Element451/RIV/Server")
riv<-read.csv("application material.csv",header=T, stringsAsFactors = T)
student<-read.csv("RIV Data Feb 15 2019.csv",header=T, stringsAsFactors = T)
requirement<-read.csv("Requirements.csv",header=T, stringsAsFactors = F)
student<-subset(student,student$population!="OTHER")

# Label Type
student$type[grepl('Nursing',student$Major.Name)&student$population=="PS"]<-"ps_nursing"
student$type[!grepl('Nursing',student$Major.Name)&student$population=="PS"]<-"ps_notnursing"
student$type[student$Citizenship.Status!="US"&student$Citizenship.Status!="PR"&student$population=="GRAD"]<-"grad_international"
student$type[grepl('Nursing',student$Major.Name)&student$Citizenship.Status %in% c("US","PR")&student$population=="GRAD"]<-"grad_nursing"
student$type[!grepl('Nursing',student$Major.Name)&student$Citizenship.Status %in% c("US","PR")&student$population=="GRAD"]<-"grad_notnursing"
student$type[student$population=="Doctorate"]<-"doctorate"
student$type[grepl('Nursing',student$Major.Name)&student$population=="UDAY"&student$Transfer!="transfer"]<-"day_nursing_firsttime"
student$type[!grepl('Nursing',student$Major.Name)&student$population=="UDAY"&student$Transfer!="transfer"]<-"day_notnursing_firsttime"
student$type[grepl('Nursing',student$Major.Name)&student$population=="UDAY"&student$Transfer=="transfer"]<-"day_nursing_transfer"
student$type[!grepl('Nursing',student$Major.Name)&student$population=="UDAY"&student$Transfer=="transfer"]<-"day_notnursing_transfer"
table(student$type)


student_doc<-merge(student,riv,by.x = "Element.Id", by.y="user_id",all.x = T)
student_doc_select<-student_doc[,c("Element.Id","document_name")]
student_doc_select$count<-1
student_doc_byid<-dcast(student_doc_select,Element.Id~document_name,fun.aggregate = sum,value.var="count")
student_doc_merge<-merge(student_doc_byid,student[,c("Element.Id","type")],by="Element.Id")
missing_doc<-data.frame()
for(i in 1:nrow(student_doc_merge)){
  y<-requirement[requirement$program==student_doc_merge$type[i],!requirement[requirement$program==student_doc_merge$type[i],] %in% names(student_doc_merge[,which(student_doc_merge[i,]!=0)])]
  missing_doc<-rbind.fill(missing_doc,y)
}
missing_doc_id<-cbind(student_doc_merge[,"Element.Id"],missing_doc)
colnames(missing_doc_id)[1]<-"Element.Id"
missing_doc_all<-merge(student,missing_doc_id,by = "Element.Id")
write.csv(missing_doc_all,"student_missing_doc.csv")


