# Exploratory Data Analysis - Course Project 1
# Chris Kan
#
# to generate the graphs just run plot2()

library(dplyr)
library(data.table)
library(sqldf)

plot2 <- function() {
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
                
                data <- NULL
                if (file.exists(destFile)) {
                        print (paste(destFile, "Found", sep=" "))
                        #estimateMemoryReqt()
                        #data <- read.delim(destFile, header = TRUE, sep = ";", na.strings = "?")
                        data <- read.csv.sql(destFile, sql = 'select * from file where Date = "1/2/2007" or Date = "2/2/2007"', header = TRUE, sep =";")
                        
                } else {
                        print ("No such file.")
                }
                
                as.data.frame(data)
        }
        
        ## 2. Massage data
        massageData <- function() {
                
                # add datetime
                master$DateTime <- strptime(paste(master$Date, master$Time, sep = " "), format = "%d/%m/%Y %H:%M:%S")
                
                master
        }
        
        plotData <- function() {
                # setup plots
                par(mfcol=c(1,1),pch=".",lty=1)
                
                # plot data into subgraph
                with(master, plot(DateTime,
                                  Global_active_power,
                                  type="l",
                                  xlab="",
                                  ylab="Global active power (kilowatts)"))
        }
        
        createOutputFile <- function(filename) {
                dev.copy(png, filename, width=480, height=480)
                dev.off()
        }
        
        # set working directory
        scriptDir <- getSrcDirectory(plot2)
        setwd(scriptDir)
        
        # main steps
        downloadData()
        master <- loadData()
        master <- massageData()
        plotData()
        createOutputFile('plot2.png')
}

