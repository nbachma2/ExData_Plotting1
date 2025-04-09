#This function will take the Electric Power Consumption dataset and plot 
# a lineplot of the Energy Sub Metering measurements (overlaid)  over time for specified dates

plot3<- function(){
        library(dplyr)
        #Get data
        url<- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        if (!file.exists("household_power_consumption.txt")) download.file(url, dest = "zipfile1.zip", method = "curl", quiet = TRUE)
        if (!file.exists("household_power_consumption.txt")) unzip("zipfile1.zip")
        data1<- read.table("household_power_consumption.txt", na.strings = "?", sep=";", header=TRUE)
        
        #convert dates and times into R readable formats
        data1$Date <- as.Date(data1$Date, format = "%d/%m/%Y")
        data1$Time <- strptime(data1$Time, format = "%H:%M:%S") %>% format("%H:%M:%S")
        
        
        #Select data for 2007-02-01 & 2007-02-02
        feb1<- filter(data1, Date == "2007-02-01" | Date == "2007-02-02" & !is.na(Date))
        
        #Combine date and time into one variable for plotting
        feb1<- mutate(feb1, datetime = as.POSIXct(paste(feb1$Date, feb1$Time)))
        
        
        #generate plot
        plot(feb1$datetime, feb1$Sub_metering_1, type = 'n', xaxt='n', xlab = "", ylab = "Energy sub metering", main = "")
        
        #generate weekday tick labels
        wdays<-weekdays(unique(feb1$Date), abbreviate=TRUE)
        wdays<-c(wdays, "Sat")
        at_pos<-c(which(!duplicated(feb1$Date)),length(feb1$Sub_metering_1))
        axis(side=1, at=feb1$datetime[at_pos], labels = wdays)
        
        #plot lines
        lines(feb1$datetime, feb1$Sub_metering_1, type = "l", col = "black")
        lines(feb1$datetime, feb1$Sub_metering_2, type = "l", col = "red")
        lines(feb1$datetime, feb1$Sub_metering_3, type = "l", col = "blue")
        
        
        #generate legend
        legend("topright", lty = 1,col =  c("black","red","blue"), legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), cex=0.5 )
        
        #save to .png file
        dev.copy(png,file = "plot3.png")
        dev.off()
        
}