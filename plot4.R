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

proj1p4 <- function(){
    library(dplyr)
    message("This function assumes your extracted data is at your working directory")
    message("It should be called household_power_consumption.txt.")
    
    # Save system locale first (the project requires an English locale for X axis labels)
    mySysLocale <- Sys.getlocale("LC_TIME")
    # Now set locale to English
    Sys.setlocale("LC_TIME", "English")
    
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
    # For the plot
    df2 <<- select(df1, DateTime, Global_active_power, 
                   Global_reactive_power, Voltage, 
                   Sub_metering_1, Sub_metering_2, Sub_metering_3)
    # df1 <<- NULL
    # Some columns must be numeric - fread seems to have a problem with colclasses 
    # so better safe then sorry
    df2$Global_active_power <<- as.numeric(df2$Global_active_power)
    df2$Global_reactive_power <<- as.numeric(df2$Global_reactive_power)
    df2$Voltage <<- as.numeric(df2$Voltage)
    df2$Sub_metering_1 <<- as.numeric(df2$Sub_metering_1)
    df2$Sub_metering_2 <<- as.numeric(df2$Sub_metering_2)
    df2$Sub_metering_3 <<- as.numeric(df2$Sub_metering_3)
    
    # Open PNG device
    plotFName <- "plot4.png"
    png(filename = plotFName,  width = 480, height = 480)

    # We want 4 plots (2 by 2)
    par(mfrow = c(2,2))  
    
    # Do the 1st plot
    plot(df2$DateTime, df2$Global_active_power, type = "l",
         xlab = "", 
         ylab = "Global Active Power", 
         cex.axis = 1, 
         cex.lab = 1)
    
    # Do the 2nd plot
    plot(df2$DateTime, df2$Voltage, type = "l",
         xlab = "datetime", 
         ylab = "Voltage", 
         cex.axis = 1, 
         cex.lab = 1)
    
    # Do the 3rd plot
    # Create the plot without data points first
    plot(df2$DateTime, df2$Sub_metering_1, type = "n",
                  xlab = "", 
                  ylab = "Energy sub metering", 
                  cex.axis = 1, 
                  cex.lab = 1)    
    #  Then add data points
    points(df2$DateTime, df2$Sub_metering_1, type = "l", col = "black")
    points(df2$DateTime, df2$Sub_metering_2, type = "l", col = "red")
    points(df2$DateTime, df2$Sub_metering_3, type = "l", col = "blue")
    # Add legend
    legend("topright", 
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
           bty = "n",
           lty=c(1,1,1), 
           col = c("black", "red", "blue"), 
           cex = 0.8)
    
    # Do the 4th plot
    plot(df2$DateTime, df2$Global_reactive_power, type = "l",
         xlab = "datetime", 
         ylab = "Global_reactive_power", 
         cex.axis = 1, 
         cex.lab = 1)
    
    
    dev.off()
    
    
    message("Plot is created in the working directory, file name: ", plotFName)
    
    # Now set locale to what it was before
    Sys.setlocale("LC_TIME", mySysLocale)
    return("Done.")
    
}