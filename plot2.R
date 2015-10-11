# This scripts reads the household power consumption data and outputs a PNG file with the required plot
# for the course project
# Usage : 
# 	source(plot2.R)
#	PlotData()	# default arguments match project requirements

Sys.setlocale("LC_TIME", "C")	# To have English names for dates when the default locale is different
library(dplyr)

colNames <- c("date", "time", "globalActivePower", "globalReactivePower","voltage",
			  "globalintensity", "subMetering1", "subMetering2", "subMetering3")
defaultFileName <- "household_power_consumption.txt"
plotMinDate <- as.Date("2007-02-01", "%Y-%m-%d")
plotMaxDate <- as.Date("2007-02-02", "%Y-%m-%d")


GeneratePlot2 <- function(file = defaultFileName, minDate = plotMinDate, maxDate = plotMaxDate) {
	# Column types. Date and time variables will be converted later
	colClasses <- c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
	
	# Read and process data
	powerData <- read.table(defaultFileName, skip = 1, sep = ";", col.names = colNames, 
							colClasses = colClasses, stringsAsFactors = FALSE, na.strings = "?")
	
	# Create 1 Date/Time variable instead of the 2 date and time variables
	# as.POSIXct to be compatible with the dplyr package
	powerData$dateTime <- as.POSIXct(strptime(paste(powerData$date, powerData$time), "%d/%m/%Y %H:%M:%S"))	
	powerData$date <- NULL
	powerData$time <- NULL
	
	
	# Keep only the days needed
	powerSubset <- filter(powerData, as.Date(dateTime) >= plotMinDate & as.Date(dateTime) <= plotMaxDate)
	
	
	# Generate and export plot to a PNG file
	png("plot2.png", width = 480, height = 480)
	plot(powerSubset$dateTime, powerSubset$globalActivePower, type = "l", xlab = "", ylab = "Global active Power (kilowatts)")
	dev.off()
}