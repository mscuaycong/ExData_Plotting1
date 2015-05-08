#Exploratory Data Analysis - Course Project 1
#Plot 3
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

##Plot 3##
viewgraph<-function(){
     
     y1<-as.numeric(epcsub$Sub_metering_1)
     y2<-as.numeric(epcsub$Sub_metering_2)
     y3<-as.numeric(epcsub$Sub_metering_3)
     dt<- paste(epcsub$Date,format(epcsub$Time,"%H:%M:%S"))
     dt<- strptime(dt,"%Y-%m-%d %H:%M:%S")
     
     plot(dt,y1,type="l",xlab="",ylab="Energy sub metering",yaxt="n",ylim=c(0,35))
          
     y4<-y2/7
     points(dt,y4,type="l",col="red")
     points(dt,y3,type="l",col="blue")
     
     legend( x="topright", 
          legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
          col=c("black","red","blue"), lwd=1, merge=FALSE )
     axis(2,c(0,10,20,30))
}

## Output to png file

##Alternative 1 see plot on screen first then copy to png
#viewgraph()
#dev.copy(png,"plot3.png")
#dev.off()

#Alternative 2 - open png device and plot to it - no output on screen
png("plot3.png",width=480,height=480)
viewgraph()
dev.off()

