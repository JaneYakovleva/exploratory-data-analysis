library("data.table")

## Change time locality to prevent label  in Russian language
Sys.setlocale("LC_TIME", "C")

## 
## Code for reading the data
##

firstRow <- rowNum <- NULL

readData <- function(sourceFilePath) {
  
  ## calculates index of rows for read
  if (is.null(firstRow)) {
    # takes only 2007-02-01 and 2007-02-02 measurements
    allOccurrencesIndexes <- grep("^(1|2){1}/2/2007",readLines(sourceFilePath))
    firstRow <<- allOccurrencesIndexes[1]
    rowNum <<- length(allOccurrencesIndexes)
  }
  
  ## reads only particular rows
  householdEnergyUsage <- read.table(sourceFilePath, header = F, sep = ';', na.strings = "?", skip = firstRow-1,  
                                     nrows=rowNum, check.names = F, stringsAsFactors = F, quote='\"')
  
  ## setups meaningful column names
  colNames <- c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity",
                "Sub_metering_1","Sub_metering_2", "Sub_metering_3")
  setnames(householdEnergyUsage, names(householdEnergyUsage), colNames)
  
  householdEnergyUsage$Date <- as.Date(householdEnergyUsage$Date, format = "%d/%m/%Y")
  dateTime <- paste(householdEnergyUsage$Date, householdEnergyUsage$Time)
  householdEnergyUsage$DateTime <- as.POSIXct(dateTime)
  
  householdEnergyUsage
}
## Read data
householdEnergyUsage <- readData("data/household_power_consumption.txt")

## Reconstruct the plot 2
plot(Global_active_power ~ DateTime, data = householdEnergyUsage, type = "l",
     ylab = "Global Active Power (kilowatts)", xlab = "")

## Save plot to a PNG file with a width of 480 pixels and a height of 480 pixels.
dev.copy(png, file = "plot2.png", height = 480, width = 480)
dev.off()