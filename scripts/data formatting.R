setwd("C:\\Users\\callu\\Desktop\\nest_box_cleaning\\data")
library(tidyverse)
pitdata<-read_csv("alldata.csv")

#ADD 2 TO ALL READERS IN "NEIGHBOUR" ENCLOSURES TO DISTINGUISH THEM EASILY
pitdata$reader<-ifelse(pitdata$type=="S",pitdata$reader,pitdata$reader+2)

#format data types
pitdata$date<-as.POSIXct(pitdata$date,format="%Y-%m-%d %H:%M:%OS", tz="GMT")
pitdata$reader<-as.character(pitdata$reader)
pitdata$trial<-as.character(pitdata$trial)
pitdata$X1<-NULL

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
#Subset to individual trials
trial.df<-as.data.frame(pitdata[pitdata$trial==trials[i],])
#order trials by date and time (this changed when all enclosures were standardised to the same clock)
trial.df.ordered<-trial.df[order(trial.df$new.date),]
#since order has changed, rownumbers are no longer meaningful, so rename to the current order
rownames(trial.df.ordered) <- seq(length=nrow(trial.df.ordered))

##Correct IDs that have 1 (high threshold) or 2 (low threshold) character difference##
#read in gata for each group to pull out actual IDs
groupdata<-read.csv("C:/Users/callu/OneDrive/University/Liverpool/Experiments/Out group experiments/Data/Groups.csv")
groupdata$type<-as.character(groupdata$type)
#create an object containing only the actual pit tag numbers for subjects
idlist<-as.character(groupdata[groupdata$trial==i & groupdata$type=="os" |
                                 groupdata$trial==i & groupdata$type=="ys" |
                                 groupdata$trial==i & groupdata$type=="ms",]$pit)

#create an object containing only the actual pit tag numbers for neighbours
neighbourlist<-as.character(groupdata[groupdata$trial==i & groupdata$type=="n",]$pit)

#establish which lines contain neighbour pit IDs and remove
trial.df.ordered$neighbour<-ifelse(trial.df.ordered$pit %in% neighbourlist,TRUE,FALSE)
trial.df.ordered<-trial.df.ordered[!trial.df.ordered$neighbour,]

#Establish which lines contain subject pit numbers and create a subset df of the tags that arent correct
trial.df.ordered$pit.correct<-ifelse(trial.df.ordered$pit %in% idlist,TRUE,FALSE)
trial.df.wrong<-trial.df.ordered[!trial.df.ordered$pit.correct,]

#read each recorded pit ID and test it againt actual pit IDs to see how many characters off it is
trial.df.wrong$high.thresh <-character(NROW(trial.df.wrong))
trial.df.wrong$low.thresh <-character(NROW(trial.df.wrong))
sums.high<-logical(5)
sums.low<-logical(5)

#Loop through all data with wrong pit read
for(i in 1:NROW(trial.df.wrong)){
  
  #loop through all actual pit tag numbers to test against each wrong read
  for(j in 1:length(idlist)){
    
    #sum the number of characters that match in both the wrong reads and the actual pit numbers.
    #if 9 are equal return true, otherwise, false.
    #since there are 5 actual pit tags, we will have a logical object with a length of 5
    sums.high[j]<-sum(str_split_fixed(trial.df.wrong$pit[i],"",10)==str_split_fixed(idlist[j],"",10))==9
    # since logical is either 1/0 (true/false) we can sum.
    # If sum of these is 0, none match with just 1 character different.
    # If sum is 1, only 1 actual pit has 1 character difference to the miss-read so its likely that pit tag
    # If sum is 2 or more, then more than 1 pit tag could be assigned to this miss-read, so we cant assign any
    # So if sum =1
    ifelse(sum(sums.high)==1,
          # which pit tag does it differ to by 1 character? assign it
           trial.df.wrong$high.thresh[i]<-idlist[which(sums.high==1)],
          # if its not equal to 1, assign NA
           trial.df.wrong$high.thresh[i]<-NA)
    
    # If at this point there is still an NA for the pit tag
    if(is.na(trial.df.wrong$high.thresh[i]))
    # do the same as before but allowing for a 2 character difference
    {sums.low[j]<-sum(str_split_fixed(trial.df.wrong$pit[i],"",10)==str_split_fixed(idlist[j],"",10))==8}
    ifelse(sum(sums.low)==1,
           trial.df.wrong$low.thresh[i]<-idlist[which(sums.low==1)],
           trial.df.wrong$low.thresh[i]<-NA)
  }
}

#The pit tags we have corrected for are in a different data frame, 
#so merge this data frame back into the original one
trial.new = { 
  dt1 <- data.table(trial.df.ordered, key = c("pit","new.date","reader","date","julian","trial","type","save.date","diff","neighbour","pit.correct"))
  dt2 <- data.table(trial.df.wrong, key = c("pit","new.date","reader","date","julian","trial","type","save.date","diff","neighbour","pit.correct"))
  data.frame( dt2[dt1] )
}

# create a new column that either has the original pit tag if it was correct
# or the newly assigned pit tag if it was not
trial.new$new_pit<-ifelse(trial.new$pit.correct, trial.new$pit, trial.new$value)
