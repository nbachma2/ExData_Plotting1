#This function will take the Electric Power Consumption dataset and plot 
# a lineplot of the Global Active  over time for specified dates

plot2<- function(){
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
        
        
        #generate plot
        plot(feb1$Global_active_power, col="black", type = "l", xaxt='n', xlab = '', ylab = "Global Active power (kilowatts)", main = "")
        
        #generate weekday tick labels
        wdays<-weekdays(unique(feb1$Date), abbreviate=TRUE)
        wdays<-c(wdays, "Sat")
        at_pos<-c(which(!duplicated(feb1$Date)),length(feb1$Global_active_power+1))
        axis(side=1, at=at_pos, labels = wdays)
        
        
        #save to .png file
        dev.copy(png,file = "plot2.png")
        dev.off()
        
}