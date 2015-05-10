Plot4 <- function(){
  ## Plot4.R
  
  ##Load the data.
  power.Consumption <- read.table(file = 'household_power_consumption.txt',
                                  sep=';',header=TRUE,stringsAsFactors = FALSE,colClasses=c("character","character",rep("numeric",7)),na.strings = "?")
  ##select the dates we are going to look at
  power.Consumption <- power.Consumption[power.Consumption$Date=='1/2/2007' | power.Consumption$Date=='2/2/2007',]
  ##Convert date values to a datetime
  power.Consumption$Date <- as.character(as.Date(x = power.Consumption$Date,format = '%d/%m/%Y'))
  power.Consumption$datetime <- as.POSIXct(paste(power.Consumption$Date, power.Consumption$Time), format="%Y-%m-%d %H:%M:%S")
  
  ##set up plot
  png('plot4.png',width = 480, height = 480, units = "px")
  par(mfcol = c(2, 2))
  #First Plot - global active power
  
  plot(Global_active_power ~ datetime,data=power.Consumption,type="n"
       ,xaxt='n',main='',xlab='',ylab="Global Active Power (Kilowatts)")
  lines(Global_active_power ~ datetime,data=power.Consumption)
  axis.POSIXct(1, power.Consumption$datetime, format="%a")
  
  #second plot- sub metering
  plot(Sub_metering_1 ~ datetime,data=power.Consumption,type="n"
       ,xaxt='n',main='',xlab='',ylab="Energy sub metering")
  lines(Sub_metering_1 ~ datetime,data=power.Consumption,col='black')
  lines(Sub_metering_2 ~ datetime,data=power.Consumption,col='red')
  lines(Sub_metering_3 ~ datetime,data=power.Consumption,col='blue')
  legend(x = "topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
         col=c("black","red","blue"),lty=1,bty='n')
  axis.POSIXct(1, power.Consumption$datetime, format="%a")
  
  #Third plot - voltage
  plot(Voltage ~ datetime,data=power.Consumption,type="n"
       ,xaxt='n',main='',xlab='datetime',ylab="Voltage")
  lines(Voltage ~ datetime,data=power.Consumption,col='black')
  axis.POSIXct(1, power.Consumption$datetime, format="%a")
  
  #Fourth Plot - global reactive power
  
  plot(Global_reactive_power ~ datetime,data=power.Consumption,type="n"
       ,xaxt='n',main='',xlab='datetime',ylab="Global_reactive_power")
  lines(Global_reactive_power ~ datetime,data=power.Consumption,col='black')
  axis.POSIXct(1, power.Consumption$datetime, format="%a")

  
  dev.off()
  
}