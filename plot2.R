
## Plot the given data and save this to a PNG file.
plotfast2 <- function(data) {
     png('plot2.png')
     par(bg=NA)
     with(data,plot(DateTime,Global_active_power,
                    type="n",xlab=NA,
                    ylab="Global Active Power (kilowatts)"))
     with(data,lines(DateTime,Global_active_power,xlab=NA))
     dev.off()
}


## Call downloadAndReadData to get the data,
## then call plotfast to plot the given data as desired,
## also creating a PNG file of the plot.
plot2 <- function() {
     data <- downloadAndReadData() 
     plotfast2(data)
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