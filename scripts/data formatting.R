#setwd("C:\\Users\\callu\\Desktop\\nest_box_cleaning\\data")
#pitdata<-read.csv("alldata.csv")

#ADD 2 TO ALL READERS IN "NEIGHBOUR" ENCLOSURES TO DISTINGUISH THEM EASILY
pitdata$reader<-ifelse(pitdata$type=="S",pitdata$reader,pitdata$reader+2)
str(pitdata)


## CORRECT TIME SO ALL READERS ARE RUNNING ON THE SAME CLOCK
## THIS IS VERY IMPORTANT LATER
## This data is taken from the start time of pit reader files and compared to the exact time of my watch when it was started
test.data<-read.csv("C:/Users/callu/OneDrive/University/Liverpool/Experiments/Out group experiments/Data/time test.csv")
test.data$reader<-as.character(test.data$reader)
test.data$trial<-as.character(test.data$trial)
test.data$encl<-as.character(test.data$encl)
test.data$test.no<-as.character(test.data$test.no)
test.data$start<-as.POSIXct(as.character(test.data$start),format="%d/%m/%Y %H:%M:%OS", tz="GMT")
test.data$file.start<-as.POSIXct(as.character(test.data$file.start),format="%d/%m/%Y %H:%M:%OS", tz="GMT")
test.data$diff<-test.data$start-test.data$file.start
test.data.agg<-aggregate(diff~trial*reader*encl,data=test.data,mean)

pitdata$reader<-as.character(pitdata$reader)
pitdata$trial<-as.character(pitdata$trial)
pitdata$diff<-NA

for(j in 1:NROW(test.data)){
  k<-which(pitdata$trial==test.data.agg$trial[j] & pitdata$reader==test.data.agg$reader[j])
  pitdata$diff[k]<-test.data.agg$diff[j]
}

pitdata$new.date<-pitdata$date+pitdata$diff


##################################
#### Format data within trial ####
##################################


trials<-unique(pitdata$trial)
i=2
#for(i in 1:length(trials))
trial.df<-as.data.frame(pitdata[pitdata$trial==trials[i],])
unique(trial.df$pit)
trial.df.ordered<-trial.df[order(trial.df$new.date),]
table(as.character(trial.df.ordered$pit))
##Correct IDs with 1 character different##
groupdata<-read.csv("C:/Users/callu/OneDrive/University/Liverpool/Experiments/Out group experiments/Data/Groups.csv")
groupdata$type<-as.character(groupdata$type)
idlist<-as.character(groupdata[groupdata$trial==i & groupdata$type=="os" |
                                 groupdata$trial==i & groupdata$type=="ys" |
                                 groupdata$trial==i & groupdata$type=="ms",]$pit)

neighbourlist<-as.character(groupdata[groupdata$trial==i & groupdata$type=="n",]$pit)

trial.df.ordered$neighbour<-ifelse(trial.df.ordered$pit %in% neighbourlist,TRUE,FALSE)
trial.df.ordered<-trial.df.ordered[!trial.df.ordered$neighbour,]
table(trial.df.ordered$pit)
trial.df.ordered$pit.correct<-ifelse(trial.df.ordered$pit %in% idlist,TRUE,FALSE)
#trial.df.wrong<-trial.df.ordered[!trial.df.ordered$pit.correct,]
str(trial.df.ordered)
trial.df.ordered$value <- NA
trial.df.ordered$low.thresh <- NA
sums<-logical(5)
for(i in 1:NROW(trial.df.wrong)){
  
  
  for(j in 1:length(idlist)){
    if(sum(str_split_fixed(trial.df.wrong$pit[i],"",10)==str_split_fixed(idlist[j],"",10))==9)
    {trial.df.wrong$value[i]<-idlist[j]}
    
    if(is.na(trial.df.wrong$value[i]))
    {sums[j]<-sum(str_split_fixed(trial.df.wrong$pit[i],"",10)==str_split_fixed(idlist[j],"",10))==8}
    ifelse(sum(sums)==1,trial.df.wrong$low.thresh[i]<-idlist[which(sums==1)],NA)
  }
}
j=1

for(i in 1:NROW(trial.df.ordered[!trial.df.ordered$pit.correct,])){
  
  
  for(j in 1:length(idlist)){
    if(sum(str_split_fixed(trial.df.ordered[!trial.df.ordered$pit.correct,]$pit[i],"",10)==str_split_fixed(idlist[j],"",10))==9)
    {trial.df.ordered[!trial.df.ordered$pit.correct,]$value[i]<-idlist[j]}
    
    if(is.na(trial.df.ordered[!trial.df.ordered$pit.correct,]$value[i]))
    {sums[j]<-sum(str_split_fixed(trial.df.ordered[!trial.df.ordered$pit.correct,]$pit[i],"",10)==str_split_fixed(idlist[j],"",10))==8}
    ifelse(sum(sums)==1,trial.df.ordered[!trial.df.ordered$pit.correct,]$low.thresh[i]<-idlist[which(sums==1)],NA)
  }
}

table(trial.df.ordered$pit)

str(trial.df.wrong)
str(trial.df.ordered)
trial.new<-merge(trial.df.ordered,trial.df.wrong,by=c("pit","date","new.date","reader","trial","type","neighbour","julian","save.date","pit.correct","diff"),all=T)
trial.new$new_pit<-trial.new$pit
str(trial.new)
trial.new[!trial.new$pit.correct,]$new_pit<-trial.new[!trial.new$pit.correct,]$value

trial.new$new_pit<-droplevels(trial.new$new_pit)
trial.new$pit<-droplevels(trial.new$pit)
trial.new$new_pit<-as.character(trial.new$new_pit)
table(trial.new$new_pit)
unique(trial.new$new_pit)
sum(is.na(trial.new$new_pit))


