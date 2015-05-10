plot4 <- function(){
        ## zip file url location
        fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip" 
        
        ## download zip file with name "project1.zip"
        download.file(fileUrl,dest="project1.zip", method="curl", mode="wb") 
        
        ## unzip the downlaoded "project1.zip" file and it is saved as "household_power_consumption.tx"
        unzip("project1.zip")
        
        ## read the file "household_power_consumption.tx" into data table
        data <- read.table("household_power_consumption.txt",sep=";", stringsAsFactors=FALSE, header=TRUE, na.strings="?") 
        
        ##convert the first column to Date format
        data[1,] <- as.Date(data[,1],"%d/%m/%Y")

        ##logical vectors to filter data for dates 2/1/2007 and 2/2/2007
        l1 <- as.Date(data[,1],"%d/%m/%Y") == as.Date("2007-02-01") 
        l2 <- as.Date(data[,1],"%d/%m/%Y") == as.Date("2007-02-02") 
        
        ##filteredData contains data for Feb 1st and 2nd dates only
        filteredData <- data[l1 | l2 , ];
        
        ##remove any na rows on Date
        filteredData <- filteredData[!is.na(filteredData$Date),];
        
        datetime <- paste(as.Date(filteredData$Date,format="%d/%m/%Y"), filteredData$Time)
        filteredData$DateTime <- as.POSIXct(datetime)

        
        #Open png resource and save to file with specific dimensions 480*480
        png( filename="plot4.png",height=480, width=480)
        
        ##Plot consists of 4 plots arranged in 2*2 order as below 
        par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
        with(filteredData,{
                
                #plot1 - call plot on Global_active_power and DateTime column to produce plot1 of type line, y label
                plot(filteredData$Global_active_power~filteredData$DateTime,type="l", ylab="Global Active Power (kilowatts)", xlab="");
                
                #plot2 - call plot on Global_active_power and DateTime column to produce plot2 of type line, y label
                plot(filteredData$Voltage~filteredData$DateTime,type="l", ylab="Voltage", xlab="datetime");
                
                
                #plot3 - call plot on Sub_metering_1 and DateTime column to produce first line of plot3 with y label and line color black
                plot(filteredData$Sub_metering_1~filteredData$DateTime,type="l", ylab="Energy sub metering", xlab="", col="black");
                #plot3 - call lines on Sub_metering_2 and DateTime column to produce second line of plot3 with y label and line color red
                lines(filteredData$Sub_metering_2~filteredData$DateTime,type="l", ylab="Energy sub metering", xlab="", col="red");
                #plot3 - call lines on Sub_metering_3 and DateTime column to produce third line of plot3 with y label and line color blue
                lines(filteredData$Sub_metering_3~filteredData$DateTime,type="l", ylab="Energy sub metering", xlab="", col="blue");
                #plot3 - call legend to produce legend of plot3 with Sub_metering_1, Sub_metering_2, Sub_metering_3
                legend("topright", lty=c(1,1), col=c("black","red", "blue"), legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
                
                #plot4 - call plot on Global_active_power and DateTime column to produce plot2 of type line, y label
                plot(filteredData$Global_reactive_power~filteredData$DateTime,type="l", ylab="Global_reactive_power", xlab="datetime");
                
                
        })
        
        #Close the png resource
        dev.off()

}