
## Plot the given data and save this to a PNG file.
plotfast4 <- function(data) {
     png('plot4.png')
     par(bg=NA)
     par(mfrow=c(2,2))
     #top left
     with(data,plot(DateTime,Global_active_power,
                    xlab=NA, type='n',
                    ylab="Global Active Power (kilowatts)"))
     with(data,lines(DateTime,Global_active_power,xlab=NA))
     #top right
     with(data,plot(DateTime,Voltage,type='n',
                    xlab="datetime",ylab="Voltage"))
     with(data,lines(DateTime,Voltage))
     #bottom left
     with(data,plot(DateTime,Sub_metering_1,
                    xlab=NA,type='n',
                    ylab="Energy sub metering"))
     with(data,lines(DateTime,Sub_metering_1,
                     xlab=NA))
     with(data,lines(DateTime,Sub_metering_2,
                     xlab=NA,col="red"))
     with(data,lines(DateTime,Sub_metering_3,
                     xlab=NA,col="blue"))
     legend("topright",c("Sub_metering_1",
                         "Sub_metering_2",
                         "Sub_metering_3"),
            lty=c(1,1,1),
            col=c("black","red","blue"),
            bty = "n")
     #bottom right
     with(data,plot(DateTime,Global_reactive_power,
                    xlab="datetime",type='n'))
     with(data,lines(DateTime,Global_reactive_power))
     dev.off()
}


## Call downloadAndReadData to get the data,
## then call plotfast to plot the given data as desired,
## also creating a PNG file of the plot.
plot4 <- function() {
     data <- downloadAndReadData() 
     plotfast4(data)
}


## Downloads the data file (if not already present),
## then reads the relevant two dates into R.
## Date and Time columns are combined to form
## a DateTime column, which is then parsed.
## Missing values, originally "?", are replaced by NAs.
downloadAndReadData <- function() {
     library(sqldf)
     fn = "household_power_consumption.txt"
     query = 'select * from file where Date = "1/2/2007" or Date = "2/2/2007"'
     if(file.exists(fn)) {
          data <- read.csv.sql(fn, sql = query,sep=";")
     }
     else {
          temp <- tempfile()
          download.file(paste("https://d396qusza40orc.",
                              "cloudfront.net/",
                              "exdata%2Fdata%2",
                              "Fhousehold_power_consumption.zip",
                              sep = ""), temp)
          data <- read.csv.sql(unz(temp, fn), sql = query,sep=";")
          unlink(temp)
     }
     data[data=="?"] = NA
     data$DateTime <- apply(data[,c('Date','Time')],1,paste,collapse=" ")
     data$DateTime <- strptime(data$DateTime, format="%d/%m/%Y %H:%M:%S")
     closeAllConnections()
     data
}