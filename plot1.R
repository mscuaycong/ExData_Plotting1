#Exploratory Data Analysis - Course Project 1
#Plot 1
#
library(plyr)
library(lubridate)

#dataset: Electric Power Consumption
#Read first n rows - for inspection
epc<-read.table("../household_power_consumption.txt",header=TRUE,sep=";",nrows=100)

              
#classes<-c("character","character",rep("numeric",7))

#Read the entire dataset
epc<-read.table("../household_power_consumption.txt",header=TRUE,
                sep=";")

#Convert date/time data
epc$Date<-as.Date(epc$Date,"%d/%m/%Y")# careful with the format of Year
epc$Time<-strptime(as.character(epc$Time),"%T")


epcsub<-select(epc,-Time)

#filter only for the required dates

epcsub<-subset(epc,ymd(Date)%in%c(ymd("20070201"),ymd("20070202")))
rm(epc)

##Plot 1##
viewgraph<-function(){
     x<-as.numeric(epcsub$Global_active_power)
     x<-x/500
     hist(x,col="red",xlab="Global Active Power (kilowatts)",
     ylab="Frequency",main="Global Active Power")
     axis(2,c(200,400,600,800,1000,1200))
}

## Output to png file

##Alternative 1 see plot on screen first then copy to png
#viewgraph()
#dev.copy(png,"plot1.png")
#dev.off()

#Alternative 2 - open png device and plot to it - no output on screen
png("plot1.png",width=480,height=480)
viewgraph()
dev.off()

