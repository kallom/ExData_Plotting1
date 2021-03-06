proj1GetData <- function() {
    # In order to make testing less time consuming I include this function
    # which downloads and unzips the data to the working directory
    # I DID NOT call this function at the beginning of each function
    # because the large download and the unzip would be unnecessary to 
    # my opinion.
    # This function can be separately tested and the file could be put to
    # whatever location
    
    fUrl <- "https://d396qusza40orc.cloudfront.net/exdata/data/household_power_consumption.zip"
    download.file(fUrl, destfile = "./household_power_consumption.zip", mode="wb")
    unzip("household_power_consumption.zip")
}

proj1p1 <- function(){
    library(data.table)
    library(dplyr)
    message("This function assumes your extracted data is at your working directory")
    message("It should be called household_power_consumption.txt.")
    
    message("Loading data...")    
    # This seems to work but it is buggy, according to the error messages doesn't seem to properly handle ? values as NA's
    # Still it works OK despite the messages - so I suppressed the messages
    suppressWarnings(
        df1 <<- fread(".\\household_power_consumption.txt", 
                      na.strings = "?", 
                      sep=";", 
                      header=TRUE, 
                      verbose = FALSE)
    )
    
    # Easy subsetting based on char values 
    df1 <<- filter(df1, Date == "1/2/2007" | Date == "2/2/2007" )
    # Adding the DateTime column converted from the Date and Time strings
    df1[, DateTime := as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S") ] 

    
    
    # ----------------------------------------------------
    # For the histogram
    df2 <<- select(df1, Global_active_power)
    # Some columns must be numeric
    df2$Global_active_power <<- as.numeric(df2$Global_active_power)
    
    # Open PNG device
    plotFName <- "plot1.png"
    png(filename = plotFName,  width = 480, height = 480)
    # Do the actual plot
    hist(df2$Global_active_power, col="red", 
         main="Global Active Power", 
         xlab = "Global Active Power (kilowatts)", 
         ylab = "Frequency", 
         cex.axis = 0.8, 
         cex.lab = 0.8)
    dev.off()
    message("Plot is created in the working directory, file name: ", plotFName)
    message("Some information about it:")
    return(file.info(plotFName))

}