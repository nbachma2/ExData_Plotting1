#This function will take the Electric Power Consumption dataset and plot 
# a multiplot containing data of Global activity power over time, voltage over time, 
# Energy sub metering over time and global reactive power over time

plot4<- function(){
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
        
        #prepare x label vectors
        wdays<-weekdays(unique(feb1$Date), abbreviate=TRUE)
        wdays<-c(wdays, "Sat")
        at_pos<-c(which(!duplicated(feb1$Date)),length(feb1$Sub_metering_1))
        
        
        #set up plotting area
        par(mfrow=c(2,2), mar=c(4,4,2,1))
        
        #generate plot 1
        plot(feb1$datetime, feb1$Global_active_power, col="black", type = "l", xaxt='n',xlab = '', ylab = "Global Active power (kilowatts)", main = "")
        axis(side=1, at=feb1$datetime[at_pos], labels = wdays)
        

        #generate plot 2
        plot(feb1$datetime, feb1$Voltage, col="black", type = "l", xaxt='n',xlab = 'datetime', ylab = "Voltage", main = "")
        axis(side=1, at=feb1$datetime[at_pos], labels = wdays)
        
        
        #generate plot 3
        plot(feb1$datetime, feb1$Sub_metering_1, type = 'n', xaxt='n', xlab = "", ylab = "Energy sub metering", main = "")
        lines(feb1$datetime, feb1$Sub_metering_1, type = "l", col = "black")
        lines(feb1$datetime, feb1$Sub_metering_2, type = "l", col = "red")
        lines(feb1$datetime, feb1$Sub_metering_3, type = "l", col = "blue")
        legend("topright", lty = 1,col =  c("black","red","blue"), legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), cex=0.3 )
        axis(side=1, at=feb1$datetime[at_pos], labels = wdays)
        
        
        #generate plot 4
        plot(feb1$datetime, feb1$Global_reactive_power, col="black", type = "l", xaxt='n',xlab = 'datetime', ylab = "Global_reactive_power", main = "")
        axis(side=1, at=feb1$datetime[at_pos], labels = wdays)
        
        #generate weekday tick labels
        
        #save to .png file
        dev.copy(png,file = "plot4.png")
        dev.off()
        
}