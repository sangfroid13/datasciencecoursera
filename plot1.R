directory<-"C:/Users/hp/Desktop/Coursera/ExploratoryDataAnalysis"
##change directory as per requirement
filename<-"household_power_consumption.txt"

setwd(directory)
initial<- read.table(filename, sep = ";", header = TRUE ,nrows=100)
classes<-sapply(initial, class)
varlist<- colnames(initial)
##classes and varlist give an indication of the structure of the dataset

newclasses <- c(Date="character", Time="character", Voltage="numeric", Global_active_power="numeric",Global_intensity="numeric",Sub_metering_1="numeric",Sub_metering_2="numeric",Sub_metering_3="numeric",Global_active_power="numeric",Global_reactive_power="numeric")

dataAll<-read.table(filename,sep = ";", header = TRUE, colClasses = newclasses, dec = ".", na.strings ="?")
dataReq<- filter(dataAll,dataAll[varlist[1]]=="1/2/2007" | dataAll[varlist[1]]=="2/2/2007")

dataReq$DateNew <- as.Date(dataReq$Date, format="%d/%m/%Y")

dataReq$dt <- as.POSIXct(paste(dataReq$DateNew, dataReq$Time))

myplot1<- function(dataReq){
  png(filename = "plot1.png", width = 480, height = 480)
  hist(dataReq$Global_active_power,col="Red",xlab="Global Active Power (kilowatts)",ylab="Frequency", main = "Global Active Power")
  dev.off()
  }

myplot1(dataReq)

