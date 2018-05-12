setwd("C:\\Users\\callu\\Desktop\\nest_box_cleaning\\data")
#install.packages("overlap")
library(stringr)
library(plyr)
library(tidyr)
library(dplyr)
library(data.table)
library(lubridate)

#To print fractional seconds
#options(digits = 5)
options(digits.secs = 3)
#increase max.print
options(max.print=1000000)


#### READ IN TXT FILES AS A LIST OF DATA FRAMES

files <- list.files("C:\\Users\\callu\\Desktop\\nest_box_cleaning\\data", recursive=TRUE, pattern="\\.TXT$")
length(files)
df<-list()
i<-1
start_time <- Sys.time()
for(i in 1:length(files)){
  print(paste(files[i]))
	#DETERMINE THE NUMBER OF LINES TO READ FROM THE TEXT
	#THE LAST 38 LINES ARE NOT NEEDED
	n.rows<-length(readLines(paste(files[i]))) - 38
	#READ IN LINES BUT SKIP FIRST 26 SINCE THEY ARENT NEEDED
  data <- read.table(paste(files[i]),skip=26,header=FALSE,nrow=n.rows)
  
  #Add column with file name
	data$file<-files[i]
  df[[i]]<-data
  print(paste("END" ,i, "of" ,length(files)))

}
#MAKE ALL DATA FRAMES WITHIN LIST 1 DATA FRAME
pitdata<-ldply(df,data.frame)
str(pitdata)

#TIME IS RECORDED AS 3 SEPERATE COLUMNS SO COMBINE
pitdata<-unite(pitdata, col="time1", c("V6","V5","V4"), sep = ":", remove = TRUE)
pitdata<-unite(pitdata, col="time", c("time1","V3"), sep = ".", remove = TRUE)
pitdata$time<- gsub(" ", 0, pitdata$time)

#DATE IS RECORDED AS 3 SEPERATE COLUMNS SO COMBINE
pitdata<-unite(pitdata, col="date1", c("V9","V8","V7"), sep = "/", remove = TRUE)

#MAKE DATE AND TIME ONE COLUMN AND FORMAT AS DATE TIME VARIABLE
pitdata<-unite(pitdata, col="date", c("date1","time"), sep = " ", remove = TRUE)
pitdata$date<-as.POSIXct(pitdata$date,format="%y/%m/%d %H:%M:%OS", tz="GMT")
	
#PIT TAGS ARE RECORDED AS 4 SEPERATE COLUMNS SO COMBINE.
pitdata<-unite(pitdata, col="pit", c("V11","V12","V13","V14"), sep = "", remove = TRUE)
pitdata$pit<- as.factor(paste0("04",pitdata$pit))
str(pitdata)

#RENAME COLUMNS and remove unwanted columns
names(pitdata)[names(pitdata) == 'V10'] <- "julian"
names(pitdata)[names(pitdata) == 'V2'] <- "reader"
pitdata$V1<-NULL

#SPLIT "file" TO ADD 3 VARIABLES and rename
pitdata<-separate(pitdata, col="file", sep=" ", into=c("trial", "type", "save.date"))

end_time <- Sys.time()
start_time-end_time
str(pitdata)

#write.csv(pitdata,"alldata.csv")
