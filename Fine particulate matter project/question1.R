# Question 1
# Have total emissions from PM2.5 decreased in the United States from 
# 1999 to 2008? Using the base plotting system, make a plot showing the 
# total PM2.5 emission from all sources for each of the years 1999, 2002, 
# 2005, and 2008.

# Load data
NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

# Format it a bit
USdata <- data.frame(NEI$year, NEI$Emissions)
USdata$NEI.Emissions <- as.numeric(USdata$NEI.Emissions)
names(USdata) <- c("year", "Emissions")
# Organize data by year-total emissions
org <- tapply(USdata$Emissions, USdata$year, sum)

# Creates device for png file
png(filename = "question1.png", width = 480, height = 480)
#par(mfrow = c(2,2)) #sets the viewport to 2 rows x 2 columns

# Plot it
plot(names(org), org, xlab = "Year", type="l", 
     ylab = expression("Total " * PM[2.5] * " emissions (tons)"), 
     pch = 5, col="blue",xaxt = "n" )
axis(side=1,at=c(1999, 2002, 2005,2008))
title("1. US Emissions")

#Explicitly turns off the device
dev.off()