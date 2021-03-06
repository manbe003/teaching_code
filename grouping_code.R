library(dplyr)
library(tidyverse)
library(numbers)

setwd("~/Desktop/teaching related - spring 22")
#don't forget to add an "enter" at the end of the file first by clicking and viewing file
dataset<-read.csv("results.csv", skip=18)
dataset<-as.data.frame(cbind(dataset$Response,dataset$Registered.participant))
colnames(dataset)<-c("race", "name")










#populate sample data
#dataset<-as.data.frame(sample(c("Latine", "East Asian", "East Asian", "White", "Black/African American", "Middle Eastern", "East Asian", "South Asian", "Native/Indigenous American", "White"), 99, replace=T))
#dataset<-cbind(dataset, as.data.frame(sample(c("Kat", "Sue", "Ger", "Natash", "Ti", "Re", "Zizi", "Yaya", "Lebanon"), 99, replace=T)))
#colnames(dataset)<-c("race", "name")

#I want to tell it to make groups of 4, without replacement, that are NOT the same race
#copying Dick's code 

classsize = nrow(dataset)
groupcount = 26

#remainder
#rejects=classsize%%4

#if (rejects>0){
#  groupcount=groupcount+1
#}

#add White
dataset$groupnumber = 0
group = 1
for (i in 1:nrow(dataset)){
  if (group == groupcount){
    group = 1
  }
  if (dataset$race[i] %in% 'White'){
    dataset$groupnumber[i] = group
    group = group+1
  }
}

table(dataset$groupnumber)




#add Latine
group = 1
for (i in 1:nrow(dataset)){
  if (group == groupcount){
    group = 1
  }
  if (dataset$race[i] %in% c('Latine')){
    dataset$groupnumber[i] = group
    group = group + 1
  }
}
table(dataset$groupnumber)

#add biggest East Asian group
group = 18
for (i in 1:nrow(dataset)){
  if (group == groupcount){
    group = 1
  }
  if (dataset$race[i] %in% c('Chinese')){
    dataset$groupnumber[i] = group
    group = group + 1
  }
}

table(dataset$groupnumber)

#add South Asian
group = 5
for (i in 1:nrow(dataset)){
  if (group == groupcount){
    group = 1
  }
  if (dataset$race[i] %in% c('South Asian')){
    dataset$groupnumber[i] = group
    group = group + 1
  }
}

table(dataset$groupnumber)



#add Black
group = 1
for (i in 1:nrow(dataset)){
  if (group == groupcount){
    group = 1
  }
  if (dataset$race[i] %in% c('Black/African American')){
    dataset$groupnumber[i] = group
    group = group + 1
  }
}

table(dataset$groupnumber)


#add vietnamese
group = 10
for (i in 1:nrow(dataset)){
  if (group == groupcount){
    group = 1
  }
  if (dataset$race[i] %in% c('Vietnamese')){
    dataset$groupnumber[i] = group
    group = group + 1
  }
}

table(dataset$groupnumber)

#add Japanese 
group = 12
for (i in 1:nrow(dataset)){
  if (group == groupcount){
    group = 1
  }
  if (dataset$race[i] %in% c('Japanese')){
    dataset$groupnumber[i] = group
    group = group + 1
  }
}

#add Middle Eastern
group = 18
for (i in 1:nrow(dataset)){
  if (group == groupcount){
    group = 1
  }
  if (dataset$race[i] %in% c('Middle Eastern')){
    dataset$groupnumber[i] = group
    group = group + 1
  }
}

#add Korean
group = 19
for (i in 1:nrow(dataset)){
  if (group == groupcount){
    group = 1
  }
  if (dataset$race[i] %in% c('Korean')){
    dataset$groupnumber[i] = group
    group = group + 1
  }
}

group = 21
for (i in 1:nrow(dataset)){
  if (group == groupcount){
    group = 1
  }
  if (dataset$race[i] %in% c("Other East Asian")){
    dataset$groupnumber[i] = group
    group = group + 1
  }
}

####cap groups at 4####
grouptable = table(dataset$groupnumber)

group = 1

while (grouptable[[group+1]] >= 4){
  group = group+1
}

for (i in 1:nrow(dataset)){
  if (dataset$race[i] %in% c('Vietnamese', 'Japanese', 'Korean', 'Other East Asian' "Middle Eastern", "Native/Indigenous American")){
  dataset$groupnumber[i] = group
  group = group + 1
  }
  if (group>groupcount-1) break
}

data<-rbind(dataset, c("Black/African American", "Nia Jones", "15"), c("Latine", "Adriana Vasquez", "18"))

##disperse remaining students
grouptable = table(dataset$groupnumber)

#runs until group count minus 2
while (grouptable[[groupcount]]<4){
  #find where to start adding
  group = 1
  while (grouptable[[group+1]]>=4){
    group = group + 1
  }

#run one loop of add up to minus 1 of group count
for (i in 1:nrow(dataset)){
  if (dataset$groupnumber[i]==0){
    dataset$groupnumber[i] = group
    group = group + 1
  }
  if (group == groupcount) break
}
  table(dataset$groupnumber)
  grouptable = table(dataset$groupnumber)
}

table(dataset$groupnumber)

dataset$groupnumber[dataset$groupnumber==0]<-25


groups<-(cbind(dataset$name, dataset$groupnumber))
colnames(groups)<-c("name", "group number")

