## Function plot1
## Exploaratory data analysis of electric power consumption in one household:
## Histogram of "Global Active Power"
## (data file: household_power_consumption.txt)
##
## Call
##    plot1(x)
##
## Arguments
##    x     : string, optional
##            directory containing data file "exdata_data_household_power_consumption/household_power_consumption.txt"
##            default: current working directory
##
## Returns
##    N/A
##
## Result
##    Creates file plot1.png in current working directory
##
##
## Note
## Requires approx. 150 MB free memory to read data file

plot1 <- function(directory = getwd()) {

    ## Read data
    datafile <- paste0(directory, "/exdata_data_household_power_consumption/household_power_consumption.txt")
    columnClasses <- c("character", "character",
                       "numeric", "numeric", "numeric", "numeric", 
                       "numeric", "numeric", "numeric")
    consumption <- read.csv(datafile, sep = ";", dec = ".", na.strings = "?", 
                            colClasses = columnClasses, stringsAsFactors = FALSE)
    
    ## Build subset
    consumption <- subset(consumption, Date == "1/2/2007" | Date == "2/2/2007")
    
    ## Combine original columns Date and Time into new column DTime
    DTime <- as.data.frame(paste(consumption$Date, consumption$Time), stringsAsFactors = FALSE)
    names(DTime) <- "DTime"
    DTime$DTime <- strptime(DTime$DTime, format = "%d/%m/%Y %H:%M:%S")
    consumption <- cbind(consumption, DTime)
    
    ## Plot
    ## Open device
    png(filename = "plot1.png")
    fileDevice <- dev.cur()
    
    ## Create image
    hist(consumption$Global_active_power, col = "red", 
         main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
    
    ##Clean up
    dev.off(fileDevice)
    graphics.off()
}