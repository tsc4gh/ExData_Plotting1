## Function plot2
## Exploaratory data analysis of electric power consumption in one household:
## Line chart of "Global Active Power"
## (data file: household_power_consumption.txt)
##
## Call
##    plot2(x)
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
##    Creates file plot2.png in current working directory
##
##
## Note
## Requires approx. 150 MB free memory to read data file

plot2 <- function(directory = getwd()) {
    
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
    png(filename = "plot2.png")
    fileDevice <- dev.cur()
    
    ## Set locale to ensure English names for day of week; MS Windows only
    lc_time = Sys.getlocale("LC_TIME")
    if (grepl("windows",Sys.getenv("OS"), ignore.case = TRUE)) {
        Sys.setlocale(category = "LC_TIME", locale = "English_United States")
    }
    
    ## Create image
    with(consumption, plot(DTime, Global_active_power, type = "l",
                           xlab = "", ylab = "Global Active Power (kilowatts)"))
    
    ## Clean up
    Sys.setlocale(category = "LC_TIME", locale = lc_time)
    dev.off(fileDevice)
    graphics.off()
}