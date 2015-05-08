#Exploratory Data Analysis - Course Project 1
#Plot 4
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


##Plot 1 of 4 ##
viewgraph1 <-function(){
     
     y<-as.numeric(epcsub$Global_active_power)
     y<-y/500
     dt<- paste(epcsub$Date,format(epcsub$Time,"%H:%M:%S"))
     dt<- strptime(dt,"%Y-%m-%d %H:%M:%S")
     plot(dt,y,type="l",xlab="",ylab="Global Active Power")
     
}



##Plot 2 of 4##
viewgraph2 <-function(){
        
     
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
          col=c("black","red","blue"), lwd=1, merge=FALSE,bty="n" )
     axis(2,c(0,10,20,30))
}

##Plot 3 of 4
viewgraph3 <-function(){
      
     y<-as.numeric(epcsub$Voltage)
     dt<- paste(epcsub$Date,format(epcsub$Time,"%H:%M:%S"))
     dt<- strptime(dt,"%Y-%m-%d %H:%M:%S")
     
     plot(dt,y,type="l",xlab="datetime",ylab="Voltage",yaxt="n")
     axis(2,at=seq(800,2200,200),labels=c("","234","","238","","242","","246"))
          
}

##Plot 4 of 4
viewgraph4 <-function(){
     
     
     y<-as.numeric(epcsub$Global_reactive_power)
     dt<- paste(epcsub$Date,format(epcsub$Time,"%H:%M:%S"))
     dt<- strptime(dt,"%Y-%m-%d %H:%M:%S")
     
     plot(dt,y,type="l",xlab="datetime",ylab="Global_reactive_power",yaxt="n") 
     
     axis(2,at=seq(0,250,50),
          labels=c("0.0","0.1","0.2","0.3","0.4","0.5"),
          ylim=c(0,1000))
       
}




## Output to png file

##Alternative 1 see plot on screen first then copy to png
##Set-up graph parameters
# par(mfcol=c(2,2),mar=c(4,4,2,2),oma=c(1,1,1,1))
# viewgraph1()
# viewgraph2()
# viewgraph3()
# viewgraph4()
# dev.copy(png,"plot4.png")
# dev.off()

#Alternative 2 - open png device and plot to it - no output on screen
png("plot4.png",width=480,height=480)
##Set-up graph parameters
par(mfcol=c(2,2),mar=c(4,4,2,2),oma=c(1,1,1,1))
viewgraph1()
viewgraph2()
viewgraph3()
viewgraph4()
dev.off()

