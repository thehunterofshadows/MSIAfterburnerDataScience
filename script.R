rm(list = ls())
library(data.table)
library(lubridate)
library(dplyr)
library(corrplot)

mydata <- fread("HardwareMonitoring.hml",sep=",",header=TRUE, skip=2, fill=TRUE, na.strings="NA")
                # colClasses = c("integer", "string", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
mydata<-data.table(mydata)


#Tidy up the data
names(mydata)[1]<-"Type"
names(mydata)[2]<-"Date"
names(mydata)[3]<-"gpuTemp"
names(mydata)[4]<-"gpuLoad"
names(mydata)[8]<-"gpuMemUsage"
names(mydata)[9]<-"gpuClock"
names(mydata)[10]<-"gpuMemClock"
names(mydata)[12]<-"gpuFanSpeed1"
names(mydata)[13]<-"gpuFanSpeed2"




myNames<-names(mydata)[3:128]
for (i in myNames){
  mydata[[i]]<-as.numeric(mydata[[i]])
}

mydata<-filter(mydata, Type=="80")
mydata[[2]]<-dmy_hms(mydata[[2]])
mydata<-mydata[,-c(124:128)]
mydata<-mydata[,-c(1:2)]
#myNA<-mydata[is.na(mydata[gpuClock]),]
mydata<-mydata[!is.na(mydata['CPU clock']),]
mydata<-mydata[!is.na(mydata['Framerate']),]
mydata<-mydata[!(mydata['Frametime']>1000),]
mydata<-mydata[(mydata['Framerate']<200),]
mydata<-mydata[,-5]

#myNA<-mydata[is.na(mydata['Framerate']),]


#with framerate and frame time - it's showing those two are corrlrated with all of these
#corData<-mydata[,c(119,120,1,2,5,6,7,8,9,10,41,66,91,117)]

#without framerate and frametime to see if there is corrlation between other variables that we should consider.
corData<-mydata[,c(1,2,5,6,7,8,9,10,41,66,91,117)]




plotCor<-function(){
  myCor <- cor(corData, use="complete.obs")
  corrplot(myCor, order = "FPC", method = "color", type = "lower", tl.cex = 0.8, tl.col = rgb(0, 0, 0))
}

#looking at the NA's in CPU clock and trying to figure out why they are there, and what should be done about them.


#use full
# apply(mydata,2,function(x) any(is.na(x)))
# summary(mydata$`CPU clock`)

plot1<-function(){
  plot(mydata$gpuClock, mydata$Framerate)
  abline(lm(Framerate ~ gpuClock, mydata))
}

plot2<-function(){
  mydata<-mydata[,c("gpuClock","gpuTemp")]
  mydata<-mydata[mydata['gpuClock']>1600,]
  plot(mydata$gpuClock, mydata$gpuTemp)
  abline(lm(gpuTemp ~ gpuClock, mydata))
}