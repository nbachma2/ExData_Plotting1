#This function will take the Electric Power Consumption dataset and plot 
# a histogram of the distribution of Frequencies in bins of 0.5kW Global Active Power

plot1<- function(){
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

        
        #generate histogram
        hist(feb1$Global_active_power, col="red",xlab = "Global Active Power (kilowatts)", main = "Global Active Power")

        #save to .png file
        dev.copy(png,file = "plot1.png")
        dev.off()
        
}