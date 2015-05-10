plot1 <- function(){
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

        #Open png resource and save to file with specific dimensions 480*480
        png( filename="plot1.png",height=480, width=480)
        
        #plot1 - call hist on Global_active_power column to produce plot1 with main title, x label, y label and fill with red color
        hist(filteredData$Global_active_power, main="Global Active Power", xlab="Global Active Power (kilowatts)", ylab="Frequency", col="red");
        
        
        #close the png resource
        dev.off()
        
}