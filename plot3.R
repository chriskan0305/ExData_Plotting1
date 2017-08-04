# Exploratory Data Analysis - Course Project 1
# Chris Kan
#
# to generate the graphs just run plot3()

library(dplyr)
library(data.table)
library(sqldf)

plot3 <- function() {
        master <- NULL

        downloadData <- function() {
                fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
                dataDir <- "downloadedData"
                destFile <- paste(dataDir, "dataset.zip", sep = "/") 
                
                # create a directory to hold downloaded data
                if (!file.exists(dataDir)) {
                        dir.create(dataDir)
                }
                
                # check if downloaded before
                if (!file.exists(destFile)) {
                        download.file(fileURL, destFile, method = "curl")
                } 
                
                # check if unzipped
                if (file.exists(destFile)) {
                        unzip(destFile)
                }
        }
        
        destFile <- list.files(path = ".", pattern = "household_power_consumption.txt")
        
        ## 1. Merges the training and the test sets to create one data set.
        loadData <- function () {
                
                df <- NULL
                if (file.exists(destFile)) {
                        print (paste(destFile, "Found", sep=" "))
                        #estimateMemoryReqt()
                        #data <- read.delim(destFile, header = TRUE, sep = ";", na.strings = "?")
                        df <- read.csv.sql(destFile, sql = 'select * from file where Date = "1/2/2007" or Date = "2/2/2007"', header = TRUE, sep =";")
                        
                } else {
                        print ("No such file.")
                }
                
                as.data.frame(df)
        }
        
        ## 2. Massage data
        massageData <- function(df) {
                
                # add datetime
                df$DateTime <- strptime(paste(df$Date, df$Time, sep = " "), format = "%d/%m/%Y %H:%M:%S")
                
                df
        }
        
        ## 3. Plot datarm=
        plotData <- function(df) {
                colNames <- c("Sub_metering_1","Sub_metering_2","Sub_metering_3")
                colors <- c("black","red","blue")
                
                ylimit <- 0
                # get y axis limits
                for (i in 1:3) {
                        if (ylimit < max(df[colNames[i]])) {
                                ylimit <- max(df[colNames[i]])
                        }
                }
                #ylimit
                
                # setup plots
                par(mfcol=c(1,1),pch=".",lty=1)
                x <- df['DateTime']
                for (i in 1:3) {
                        y <- df[colNames[i]]
                        plot(x[,1],y[,1],
                             type="l",
                             xlab="",
                             ylab="Global active power (kilowatts)",
                             ylim= c(0,ylimit),
                             col=colors[i]
                             )
                        par(new=TRUE)
                }
                
                # add legend
                legend("topright", legend=colNames, col=colors, lty = 1, xjust=1, cex=0.7)
        }
        
        createOutputFile <- function(filename) {
                dev.copy(png, filename, width=480, height=480)
                dev.off()
        }
        
        # set working directory
        scriptDir <- getSrcDirectory(plot3)
        setwd(scriptDir)
        
        # main steps
        downloadData()
        master <- loadData()
        master <- massageData(master)
        plotData(master)
        createOutputFile('plot3.png')
        
}

