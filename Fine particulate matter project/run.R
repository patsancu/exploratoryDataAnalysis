setwd("~/Dropbox/Cursos MOOC/Exploratory Data Analysis//Week 3/Project/")
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

# Question 1
# Have total emissions from PM2.5 decreased in the United States from 
# 1999 to 2008? Using the base plotting system, make a plot showing the 
# total PM2.5 emission from all sources for each of the years 1999, 2002, 
# 2005, and 2008.

# Format it a bit
USdata <- data.frame(NEI$year, NEI$Emissions)
USdata$NEI.Emissions <- as.numeric(USdata$NEI.Emissions)
names(USdata) <- c("year", "Emissions")
# Organize data by year-total emissions
org <- tapply(USdata$Emissions, USdata$year, sum)
# Plot it
plot(names(org), org, xlab = "Year", type="l", 
     ylab = expression("Total " * PM[2.5] * " emissions (tons)"), 
     pch = 5, col="blue",xaxt = "n" )
axis(side=1,at=c(1999, 2002, 2005,2008))
title("US Emissions")


# Question 2
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
# (fips == "24510") from 1999 to 2008? Use the base plotting system to make a 
# plot answering this question.

# Subset Baltimore data
baltimoreData <- NEI[NEI$fips == "24510",]
# Cast, just in case
baltimoreData$Emissions <- as.numeric(as.character(baltimoreData$Emissions))
# Organize data by year-total emissions
baltimoreDataSummarized <- data.frame(baltimoreData$year, baltimoreData$Emissions)
names(baltimoreDataSummarized) <- c("year", "Emissions")
orgBaltimore <- tapply(baltimoreDataSummarized$Emissions, baltimoreDataSummarized$year, sum)
# Plot it
plot(names(orgBaltimore), type = "line", orgBaltimore, xlab = "Year", ylab = expression("Total " * PM[2.5] * " emissions(tons), "), xaxt = names(orgBaltimore),col="blue",xaxt = "n"   )
axis(side=1,at=c(1999, 2002, 2005,2008))
title("Emissions in Baltimore")


# Question 3
# Of the four types of sources indicated by the type (point, nonpoint, onroad, 
# nonroad) variable, which of these four sources have seen decreases in emissions 
# from 1999–2008 for Baltimore City? Which have seen increases in emissions from 
# 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
library(ggplot2)
# Subset Baltimore data
baltimoreData <- NEI[NEI$fips == "24510",]
# Cast, just in case
baltimoreData$Emissions <- as.numeric(as.character(baltimoreData$Emissions))
# Create helper data frame
baltimoreDataSummarized <- data.frame(baltimoreData$year, baltimoreData$Emissions, baltimoreData$type)
names(baltimoreDataSummarized) <- c("year", "Emissions", "type")
# Organize data by type
orgBaltimoreTypes<- aggregate(baltimoreDataSummarized$Emissions, by=list(baltimoreDataSummarized$type, baltimoreDataSummarized$year), FUN=sum)                      
names(orgBaltimoreTypes) <- c("type", "year", "Emissions")
# Plot it
g <- ggplot(orgBaltimoreTypes, aes(year, Emissions)) 
g + geom_line(aes(color = type, size=1, alpha=1/2))
axis(side=1,at=c(1999, 2002, 2005,2008))


# Question 4
# Across the United States, how have emissions from coal combustion-related 
# sources changed from 1999–2008?
# Get the SCCs related to coal combustion
coalCombustionSCC <- SCC[grep("(comb.*)(Coal)", SCC$EI.Sector, ignore.case = T),]
# Match it with the Emissions Data
coalCombustionData <- subset(NEI, subset = SCC %in% coalCombustionSCC$SCC)
# Get the sum of all emissions
orgCoal <- tapply(coalCombustionData$Emissions, coalCombustionData$year, sum)
#Plot it
plot(names(orgCoal), type = "l", orgCoal, xlab = "Year", ylab = expression(PM[2.5] * " emissions (tons)"), col="blue",xaxt = "n"   )
title("US emissions related to coal combustion")
axis(side=1,at=c(1999, 2002, 2005,2008))


# Question 5
# How have emissions from motor vehicle sources changed from 
# 1999–2008 in Baltimore City?
#Get the SCCs related to motor vehicles
motorVehicleSCC <- SCC[grep("vehicle", SCC$EI.Sector, ignore.case = T,na.exclude = T),]$SCC
#Match it with the Emissions Data
vehicleData <- subset(NEI, subset = SCC %in% motorVehicleSCC, na.exclude = T)
# Subset Baltimore data
baltimoreVehicleData <- subset(vehicleData, subset = fips == "24510" )
# Get the sum of all emissions
orgBaltimoreVehicles <- tapply(baltimoreVehicleData$Emissions, baltimoreVehicleData$year, sum )
# Plot it
plot(c(1999, 2002, 2005,2008), type="l", orgBaltimoreVehicles, xlab = "Year",ylab =  expression(PM[2.5] * " emissions (tons)"), col="blue",xaxt="n")
title("Baltimore vehicle emissions")
axis(side=1,at=c(1999, 2002, 2005,2008))

# Question 6
# Compare emissions from motor vehicle sources in Baltimore City 
# with emissions from motor vehicle sources in Los Angeles County, California 
# (fips == "06037"). Which city has seen greater changes over time in 
# motor vehicle emissions?

#Get the SCCs related to motor vehicles
motorVehicleSCC <- SCC[grep("vehicle", SCC$EI.Sector, ignore.case = T,na.exclude = T),]$SCC
#Match it with the Emissions Data
vehicleData <- subset(NEI, subset = SCC %in% motorVehicleSCC, na.exclude = T)
# Subset LA and Baltimore data
LAVehicleData <- subset(vehicleData, subset = fips == "06037" )
baltimoreVehicleData <- subset(vehicleData, subset = fips == "24510" )
# Get the sum of all emissions for both LA and Baltimore
orgLAVehicles <- tapply(LAVehicleData$Emissions, LAVehicleData$year, sum )
orgBaltimoreVehicles <- tapply(baltimoreVehicleData$Emissions, baltimoreVehicleData$year, sum )
# Plot it
# First, let it plot Baltimore
plot(c(1999, 2002, 2005,2008), type="l", orgBaltimoreVehicles, 
     xlab = "Year", ylab = expression(PM[2.5] * " emissions (tons)"), 
     col="blue",xaxt="n",ylim=range(c(orgBaltimoreVehicles,orgLAVehicles)))
# Then, let it plot LA
lines(c(1999, 2002, 2005,2008), orgLAVehicles, col="red")
legend("center", pch = ".", lty=1,col = c("red", "blue"), legend = c("LA", "Baltimore"))
title(expression("LA-Baltimore vehicle comparison" ))
axis(side=1,at=c(1999, 2002, 2005,2008))


#Prettier version
# df <- data.frame(c(1999,2002, 2005,2009), orgLAVehicles, orgBaltimoreVehicles)
#       ggplot(df, aes(c(1999, 2002, 2005,2008))) +
#       geom_line(aes(y=orgLAVehicles), colour="red") +
#       geom_line(aes(y=orgBaltimoreVehicles), colour="blue") 
