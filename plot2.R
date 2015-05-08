#Exploratory Data Analysis - Course Project 1
#Plot 2
#
library(plyr)
library(lubridate)

#dataset: Electric Power Consumption
#Read first n rows - for inspection
#epc<-read.table("../household_power_consumption.txt",header=TRUE,sep=";",nrows=100)

              
#classes<-c("character","character",rep("numeric",7))

#Read the entire dataset
epc<-read.table("../household_power_consumption.txt",header=TRUE,
                sep=";")

#Convert date/time data
epc$Date<-as.Date(epc$Date,"%d/%m/%Y")# careful with the format of Year

epc$Time<-as.character(epc$Time)
epc$Time<-strptime(epc$Time,"%H:%M:%S")


#filter only for the required dates

epcsub<-subset(epc,ymd(Date)%in%c(ymd("20070201"),ymd("20070202")))
rm(epc)

##Plot 1##
viewgraph<-function(){
     
     y<-as.numeric(epcsub$Global_active_power)
     y<-y/500
     dt<- paste(epcsub$Date,format(epcsub$Time,"%H:%M:%S"))
     dt<- strptime(dt,"%Y-%m-%d %H:%M:%S")
     plot(dt,y,type="l",xlab="",ylab="Global Active Power (kilowatts)")
     
}

## Output to png file

##Alternative 1 see plot on screen first then copy to png
#viewgraph()
#dev.copy(png,"plot2.png")
#dev.off()

#Alternative 2 - open png device and plot to it - no output on screen
png("plot2.png",width=480,height=480)
viewgraph()
dev.off()

