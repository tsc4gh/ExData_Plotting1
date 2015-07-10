## Function plot3
## Exploaratory data analysis of electric power consumption in one household:
## Line chart of "Energy sub metering", overlay of sub_meterings 1, 2, and 3
## (data file: household_power_consumption.txt)
##
## Call
##    plot3(x)
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
##    Creates file plot3.png in current working directory
##
##
## Note
## Requires approx. 150 MB free memory to read data file

plot3 <- function(directory = getwd()) {
    
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
    png(filename = "plot3.png")
    fileDevice <- dev.cur()
    
    ## Set locale to ensure English names for day of week; MS Windows only
    lc_time = Sys.getlocale("LC_TIME")
    if (grepl("windows",Sys.getenv("OS"), ignore.case = TRUE)) {
        Sys.setlocale(category = "LC_TIME", locale = "English_United States")
    }
    
    ## Create image
    with(consumption, plot(DTime, Sub_metering_1, type = "l",
                           xlab = "", ylab = "Energy sub metering"))
    lines(consumption$DTime, consumption$Sub_metering_2, type = "l", col = "red")
    lines(consumption$DTime, consumption$Sub_metering_3, type = "l", col = "blue")
    legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
           col = c("black", "red", "blue"), lty = 1)
    
    ## Clean up
    Sys.setlocale(category = "LC_TIME", locale = lc_time)
    dev.off(fileDevice)
    graphics.off()
}