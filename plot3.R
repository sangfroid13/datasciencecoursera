directory<-"C:/Users/hp/Desktop/Coursera/ExploratoryDataAnalysis"
filename<-"household_power_consumption.txt"

setwd(directory)
initial<- read.table(filename, sep = ";", header = TRUE ,nrows=100)
classes<-sapply(initial, class)
varlist<- colnames(initial)
##classes

newclasses <- c(Date="character", Time="character", Voltage="numeric", Global_active_power="numeric",Global_intensity="numeric",Sub_metering_1="numeric",Sub_metering_2="numeric",Sub_metering_3="numeric",Global_active_power="numeric",Global_reactive_power="numeric")

dataAll<-read.table(filename,sep = ";", header = TRUE, colClasses = newclasses, dec = ".", na.strings ="?")
dataReq<- filter(dataAll,dataAll[varlist[1]]=="1/2/2007" | dataAll[varlist[1]]=="2/2/2007")

dataReq$DateNew <- as.Date(dataReq$Date, format="%d/%m/%Y")
##dataReq$Ti<-strptime(dataReq[varlist[2]], "%H:%M:%S")
dataReq$dt <- as.POSIXct(paste(dataReq$DateNew, dataReq$Time))


myplot3<-function(dataReq){
  png(filename = "plot3.png", width = 480, height = 480)
  with(dataReq, {
    plot(Sub_metering_1~dt, type="l",
         ylab="Energy sub metering", xlab="")
    lines(Sub_metering_2~dt,col='Red')
    lines(Sub_metering_3~dt,col='Blue')
  })
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, 
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  dev.off()
}

myplot3(dataReq)

