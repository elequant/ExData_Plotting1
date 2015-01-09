# =============================================================================
# plot3.R (Construction of plot3.png)
# Coursera Specialization in Data Science: Exploratory Data Analysis, Project 1
# -----------------------------------------------------------------------------
# created:  January 2015
# author:   Elequant
# =============================================================================

Sys.setlocale("LC_TIME", "en_US.UTF-8")

# =============================================================================
# Pre-Step: Download and extract the zipped data archive if the data set is not 
# already present.
# =============================================================================
downloadAndSaveCompleteData <- function(){
  
  if (!file.exists("data")) {
    dir.create("data")
  }
  
  if (!file.exists("data/household_power_consumption.txt")) { 
    fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    archive <- "data/exdata_data_household_power_consumption.zip"
    
    download.file(fileUrl, destfile=archive, method="curl")
    unzip(archive, exdir="data")
  }
}

# =============================================================================
# The function getRelevantSubset() returns the household electric power
# consumption data from the dates 2007-02-01 and 2007-02-02, i.e. the data
# which are to be visualized.
# In order to speed up the processing of the large original data set the
# data of the relevant 2-day period is extracted and stored in a tidy data
# file with converted time variables.
# This data file (household_power_consumption_2007-02-01+2007-02-02.txt)
# can be used if repeated runs of this scripts or further plots are required.
# =============================================================================
getRelevantSubset <- function(){
  if (!file.exists("data/household_power_consumption_2007-02-01+2007-02-02.txt")) {
    
    if (!file.exists("data/household_power_consumption.txt")) {
      downloadAndSaveCompleteData()
    }  
    
    data <- read.table("data/household_power_consumption.txt", sep=";", dec=".", head = TRUE)
    data <- transform(data, Time = strptime( paste(Date, Time), format="%d/%m/%Y %T"))
    data <- transform(data, Date = as.Date(Time))
    twoDaysData <- data[data$Date >= as.Date("2007-02-01") & data$Date <= as.Date("2007-02-02"), ]
    
    write.table( twoDaysData, 
                 "data/household_power_consumption_2007-02-01+2007-02-02.txt", 
                 sep=";", dec=".", col.names = TRUE, row.names = FALSE)
  }
  
  twoDaysData <- read.table("data/household_power_consumption_2007-02-01+2007-02-02.txt", 
                            sep=";", dec=".", head = TRUE)
  twoDaysData <- transform(twoDaysData, Time = strptime( Time, format="%Y-%m-%d %T"))
  twoDaysData
}

# =============================================================================
# Making plot 3
# =============================================================================
plot3 <- function(data){
  png(file = "plot3.png", 
      width = 480, height = 480, units = "px")
  
  plot(  
    data$Time, data$Sub_metering_1,
    type = "n",
    xaxt = "n", # the standard x-axis is hidden
    xlab = "",
    ylab = "Energy sub metering")
  
  # now we need to add an axis with weekday abbreviations to the current plot
  timeRange <- as.POSIXct(round(range(data$Time), "days"))
  axis.POSIXct(1, at=seq(timeRange[1], timeRange[2], by="days"), format="%a")
  
  lines(data$Time, data$Sub_metering_1, type="l")
  lines(data$Time, data$Sub_metering_2, type="l", col="red")
  lines(data$Time, data$Sub_metering_3, type="l", col="blue")
  
  legend("topright", lty=1,
         col = c("black", "red", "blue"), 
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  dev.off()
}

# =============================================================================
# Main
# =============================================================================
twoDaysData <- getRelevantSubset()
plot3(twoDaysData)