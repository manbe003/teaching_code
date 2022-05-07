library(dplyr)
library(tidyr)
library(stringr)


#here I'm trying to combine polls with the gradebook
gradebook<-read.csv("2022-05-06T0940_Grades-PSYCH_250_A.csv")
gradebookorig<-gradebook
gradebook<-gradebook[3:102,]
poll4_5<-read.csv("why-is-critical-thinking-important-to-results.csv")
poll4_7<-read.csv("do-you-like-the-idea-of-having-some-results.csv", skip=11)
poll4_12<-read.csv("would-you-prefer-to-results.csv",skip=11)
poll4_14<-read.csv("please-pick-your-favorite-option-results.csv", skip=11)
poll4_19<-read.csv("who-won-results.csv", skip=10)
poll4_21<-read.csv("do-you-have-any-examples-of-witnessing-results.csv")
poll4_26<-read.csv("would-you-prefer-results.csv", skip=11)
poll4_28<-read.csv("do-you-prefer-results.csv", skip=10)
poll5_3<-read.csv("is-there-a-time-that-s-best-for-you-to-results.csv",skip=17)
poll5_5<-read.csv("how-does-this-clip-relate-to-the-results.csv")



#split student names
split<-as.data.frame(str_split_fixed(gradebook$Student, ", ", 2))
split<-as.data.frame(str_split_fixed(split$V2, " ", 2))

gradebook$name<-split$V1

gradebook$name<-tolower(gradebook$name)

for (i in 1:length(gradebook$Student)){
  if (gradebook$Student[i] == "Yang, Grace Si-Ping"){
    gradebook$name[i] <- "gracey"
    
  }
}

for (i in 1:length(gradebook$Student)){
  if (gradebook$Student[i] == "Rees, Jake"){
    gradebook$name[i] <- "jaker"
    
  }
}

for (i in 1:length(gradebook$Student)){
  if (gradebook$Student[i] == "An, Jasmine"){
    gradebook$name[i] <- "jasminea"
    
  }
}

for (i in 1:length(gradebook$Student)){
  if (gradebook$Student[i] == "Wang, Joanna"){
    gradebook$name[i] <- "joannaw"
    
  }
}

for (i in 1:length(gradebook$Student)){
  if (gradebook$Student[i] == "Dinh, Michael"){
    gradebook$name[i] <- "michaeld"
    
  }
}



for (i in 1:length(gradebook$Student)){
  if (gradebook$Student[i] == "Eychaner, Natalie"){
    gradebook$name[i] <- "nataliee"
    
  }
}

for (i in 1:length(gradebook$Student)){
  if (gradebook$Student[i] == "Ba, Andrew"){
    gradebook$name[i] <- "andrewb"
    
  }
}




#make a function that will do this for the poll results
y<-NA
x<-function(poll){
splitpoll<-as.data.frame(str_split_fixed(poll$Screen.name, " ", 2))
poll$name<-splitpoll$V1

#need to lower cases on both name variables
poll$name<-tolower(poll$name)


#correct for names when there are two with same first name
for (i in 1:length(poll$name)){
  if (poll$Screen.name[i] == "Grace Si-Ping Y."){
    poll$name[i] <- "gracey"
    
  }
}



for (i in 1:length(poll$name)){
  if (poll$Screen.name[i] == "Jake R."){
    poll$name[i] <- "jaker"
    
  }
}


for (i in 1:length(poll$name)){
  if (poll$Screen.name[i] == "Jasmine An"){
    poll$name[i] <- "jasminea"
    
  }
}



for (i in 1:length(poll$name)){
  if (poll$Screen.name[i] == "Michael D."){
    poll$name[i] <- "michaeld"
    
  }
}



for (i in 1:length(poll$name)){
  if (poll$Screen.name[i] == "Natalie E."){
    poll$name[i] <- "nataliee"
    
  }
}

for (i in 1:length(poll$name)){
  if (poll$Screen.name[i] == "Andrew B."){
    poll$name[i] <- "andrewb"
    
  }
}


for (i in 1:length(poll$name)){
  if (poll$name[i] == "mel"){
    poll$name[i] <- "melany"
    
  }
}

for (i in 1:length(poll$name)){
  if (poll$name[i] == "madi"){
    poll$name[i] <- "madison"
    
  }
}

for (i in 1:length(poll$name)){
  if (poll$Screen.name[i] == "Joanna W."){
    poll$name[i] <- "joannaw"
    
  }
}

for (i in 1:length(poll$name)){
  if (poll$name[i] == "guest524"){
    poll$name[i] <- "jasmine"
    
  }
}

for (i in 1:length(poll$name)){
  if (poll$name[i] == "guest703"){
    poll$name[i] <- "claire"
    
  }
}

poll<-distinct(poll, poll$Screen.name, .keep_all=TRUE)
y<-merge(gradebook, poll, by = "name", all.x = TRUE, no.dups = TRUE)
y<-distinct(y, y$Student, .keep_all=TRUE)


#okay now I can fill it in
#first have a yes/no column for whether they completed the poll

y$poll<-NA
for (i in 1:length(y$name)){
  if (!is.na(y$Response[i])){
    y$poll[i]<-2
    
  }
  else{
    y$poll[i]<-0
  }
}
return(y)
}




fill<-function(col){
for(i in 1:length(col)){
  if(is.na(col[i])){
  col[i]<-z$poll[i]
  }
}
  return(col)
}

z<-x(poll4_5)
z$X4.5.poll..7175607.<-fill(z$X4.5.poll..7175607.)
gradebook<-z[,1:117]
colnames(gradebook)<-c("name",colnames(gradebookorig))

z<-NA
z<-x(poll4_7)
z$X4_7.poll..7175608.<-fill(z$X4_7.poll..7175608.)
gradebook<-z[,1:117]
colnames(gradebook)<-c("name",colnames(gradebookorig))

z<-NA
z<-x(poll4_12)
z$X4.12.poll..7175601.<-fill(z$X4.12.poll..7175601.)
gradebook<-z[,1:117]
colnames(gradebook)<-c("name",colnames(gradebookorig))

z<-NA
z<-x(poll4_14)
z$X4.14.poll..7175638.<-fill(z$X4.14.poll..7175638.)
gradebook<-z[,1:117]
colnames(gradebook)<-c("name",colnames(gradebookorig))

z<-NA
z<-x(poll4_19)
z$X4.19.poll..7175603.<-fill(z$X4.19.poll..7175603.)
gradebook<-z[,1:117]
colnames(gradebook)<-c("name",colnames(gradebookorig))

z<-NA
z<-x(poll4_21)
z$X4.21.poll..7175604.<-fill(z$X4.21.poll..7175604.)
gradebook<-z[,1:117]
colnames(gradebook)<-c("name",colnames(gradebookorig))

z<-NA
z<-x(poll4_26)
z$X4.26.poll...7175605.<-fill(z$X4.26.poll...7175605.)
gradebook<-z[,1:117]
colnames(gradebook)<-c("name",colnames(gradebookorig))

z<-NA
z<-x(poll4_28)
z$X4.28.poll..7175606.<-fill(z$X4.28.poll..7175606.)
gradebook<-z[,1:117]
colnames(gradebook)<-c("name",colnames(gradebookorig))

z<-NA
z<-x(poll5_3)
z$X5_3.poll...7175612.<-fill(z$X5_3.poll...7175612.)
gradebook<-z[,1:117]
colnames(gradebook)<-c("name",colnames(gradebookorig))

z<-NA
z<-x(poll5_5)
z$X5_5.poll..7175613.<-fill(z$X5_5.poll..7175613.)
gradebook<-z[,1:117]
colnames(gradebook)<-c("name",colnames(gradebookorig))

gradebook$X5_5.poll..7175613.[19]<-1
gradebook$X5_5.poll..7175613.[22]<-1
gradebook$X5_5.poll..7175613.[32]<-1










#fill in full points for everyone 3-29 and 3-31
gradebook$X3.29.poll..7175637.<-2
gradebook$X3.31.poll..7175600.<-2


gradebook<-gradebook[,2:117]
gradebook<-rbind(gradebookorig[1:2,], gradebook[1:100,])
write.csv(gradebook, "forupload.csv")

