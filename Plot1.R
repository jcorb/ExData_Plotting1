Plot1 <- function(){
  ## Plot1.R
  
  ##Load the data.
  power.Consumption <- read.table(file = 'household_power_consumption.txt',
                                  sep=';',header=TRUE,stringsAsFactors = FALSE,colClasses=c("character","character",rep("numeric",7)),na.strings = "?")
  ##select the dates we are going to look at
  power.Consumption <- power.Consumption[power.Consumption$Date=='1/2/2007' | power.Consumption$Date=='2/2/2007',]
  ##Convert date values to a timstamp
  power.Consumption$Date <- as.character(as.Date(x = power.Consumption$Date,format = '%d/%m/%Y'))
  power.Consumption$datetime <- as.POSIXct(paste(power.Consumption$Date, power.Consumption$Time), format="%Y-%m-%d %H:%M:%S")
 
  png('plot1.png',width = 480, height = 480, units = "px")
  hist(power.Consumption$Global_active_power,col="red",
       main="Global Active Power",xlab="Global Active Power (Kilowatts)")
  dev.off()
  
}