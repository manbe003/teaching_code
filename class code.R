#librarys
library(tidyr)
library(tidyverse)
library(here)


#randomly assign roles
x<-c("pink","orange", "green", "purple")
y<-sample(x,4)
y<-as.data.frame(y)
rownames(y)<-c("disc_leader", "note_taker","reporter","referencer")


#randomly pick groups to call on
x1<-1:25
y<-sample(x1,11)
y

#script for turning polls into gradebook

#first import the poll and the gradebook
gradebook<-read.csv(file='2021-06-10T1443_Grades-PSYCH_250_A.csv')
gradebook<-gradebook[,1:5]
May12<-read.csv(file='94591718196_2021-05-12_PollReport-2.csv', header=FALSE, stringsAsFactors = FALSE)
May17<-read.csv(file='94591718196_2021-05-17_PollReport.csv', header=FALSE, stringsAsFactors = FALSE)
May19<-read.csv(file='94591718196_2021-05-19_PollReport.csv', header=FALSE, stringsAsFactors = FALSE)
May24<-read.csv(file='94591718196_2021-05-24_PollReport.csv', header=FALSE, stringsAsFactors = FALSE)
May26<-read.csv(file='94591718196_2021-05-26_PollReport.csv', header=FALSE, stringsAsFactors = FALSE)
June2<-read.csv(file='94591718196_2021-06-02_PollReport.csv', header=FALSE, stringsAsFactors = FALSE)
colnames(May12)<-c(1,2,3,4,5,"Splitloginid")



#what if I make a function where all I have to do is put in the right answer and it autoruns?
grade_output<-function(correct_answer, df, colnum){
  df[df==correct_answer]<-2
  df[,colnum][df[,colnum]!=2]<-1  
  Splitloginid<-strsplit(as.character(df$V3),"@")
  Splitloginid<-do.call(rbind, Splitloginid)
  Splitloginid<-as.data.frame(Splitloginid, stringsAsFactors=FALSE)
  return(as.data.frame(cbind(Splitloginid$V1, df[,colnum]))) 
  }
  
May12_graded<-(grade_output("The same kinds of behavioral differences have characterized schizophrenia since the diagnosis was discovered", May12, 6))
colnames(May12_graded)<-c("SIS.Login.ID", "5_12_polleverywhere")

#now join
gradebook<-left_join(gradebook, May12_graded)
gradebook$`5_12_polleverywhere`[is.na(gradebook$`5_12_polleverywhere`)]<-0

May5_graded<-(grade_output("Multiracial individuals tend to have experiences of identity uncertainty", May5, 6))
colnames(May5_graded)<-c("SIS.Login.ID", "5_5_polleverywhere")

gradebook<-left_join(gradebook, May10_graded)
gradebook$`5_10_polleverywhere`[is.na(gradebook$`5_10_polleverywhere.y`)]<-0

May10_graded<-(grade_output("It often includes voices that are not heard elsewhere", May10, 8))
colnames(May10_graded)<-c("SIS.Login.ID", "5_10_polleverywhere")


#function for no wrong answer
grade_output_noright<-function(df, colnum){
  df[,colnum][!is.na(df[,colnum])]<-2
  Splitloginid<-strsplit(as.character(df$V3),"@")
  Splitloginid<-do.call(rbind, Splitloginid)
  Splitloginid<-as.data.frame(Splitloginid, stringsAsFactors=FALSE)
  return(as.data.frame(cbind(Splitloginid[,1], df[,colnum]))) 
}

June2_graded<-grade_output_noright(June2,6)
colnames(June2_graded)<-c("SIS.Login.ID", "6_2_polleverywhere")

#now join
gradebook<-left_join(gradebook, (as.numeric(May17_graded)
gradebook$`6_2_polleverywhere`[is.na(gradebook$`6_2_polleverywhere`
)]<-0

#now save it
write.csv(gradebooknew,"~/Desktop/teaching related/gradebookfinalpolls.csv",row.names = TRUE)

c<-read.csv("office hours attendance tracking - Sheet1.csv")
