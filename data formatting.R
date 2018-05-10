setwd("C:\\Users\\callu\\Desktop\\nest_box_cleaning\\data")
#install.packages("overlap")
library(stringr)
library(plyr)
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
system.time({
for(i in 1:length(files)){
  

	#DETERMINE THE NUMBER OF LINES TO READ FROM THE TEXT
	#THE LAST 38 LINES ARE NOT NEEDED
	n.rows<-length(readLines(paste(files[i]))) - 38
	#READ IN LINES BUT SKIP FIRST 26 SINCE THEY ARENT NEEDED
	data <- read.table(paste(files[i]),skip=26,header=FALSE,nrow=n.rows)
	
	#JUST TO KEEP TRACK OF WHERE IT IS UP TO. IT TAKES A WHILE....
	print(paste(files[i]))
	
	#TIME IS RECORDED AS 3 SEPERATE COLUMNS SO COMBINE
	cols.time1<- c("V6","V5","V4")
	data$time1<- apply(data[,cols.time1],1,paste,collapse=":")
	cols.time2<- c("time1","V3")
	data$time<- apply(data[,cols.time2],1,paste,collapse=".")
	data$time<- gsub(" ", 0, data$time)

	#DATE IS RECORDED AS 3 SEPERATE COLUMNS SO COMBINE
	cols.date<- c("V9","V8","V7")
	data$date1<- apply(data[,cols.date],1,paste,collapse="/")

	#MAKE DATE AND TIME ONE COLUMN AND FORMAT AS DATE TIME VARIABLE
	data$date2<- apply(data[,c("date1","time")],1,paste,collapse=" ")	
	data$date<-as.POSIXct(data$date2,format="%y/%m/%d %H:%M:%OS", tz="GMT")
	
	#PIT TAGS ARE RECORDED AS 4 SEPERATE COLUMNS SO COMBINE.
	pit<-c("V11","V12","V13","V14")
	#ALSO ADD "04" TO THE BEGINNING SINCE IT ISNT RECORDED
	data$pit<- as.factor(paste0("04",apply(data[,pit],1,paste,collapse="")))

	#RENAME COLUMNS
	names(data)[names(data) == 'V10'] <- "julian"
	names(data)[names(data) == 'V2'] <- "reader"

	#ONLY KEEP THE COLUMNS STATED
	keeps <- c("pit","reader","date","julian")
	data1<-data[keeps]

	#ADD COLUMN WITH FILE NAME AND THEN SPLIT TO ADD 3 VARIABLES
	data1$file<-files[i]
	trial<-data.frame(str_split_fixed(data1$file, " ", 3))
	data2<-cbind(data1,trial)
	df[[i]]<-plyr::rename(data2, c("X1"="trial", "X2"="type", "X3"="save.date"))

	#AGAIN, JUST TO KEEP TRACK OF WHERE ITS AT WHILST RUNNING
	print(paste("END" ,i, "of" ,length(files)))
	}
})
end_time <- Sys.time()
start_time-end_time
head(df[[4]])
str
#MAKE ALL DATA FRAMES WITHIN LIST 1 DATA FRAME
pitdata<-ldply(df,data.frame)