# Question 5
# How have emissions from motor vehicle sources changed from 
# 1999–2008 in Baltimore City?
# Load data
NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
#Get the SCCs related to motor vehicles
motorVehicleSCC <- SCC[grep("vehicle", SCC$EI.Sector, ignore.case = T),]$SCC
#Match it with the Emissions Data
vehicleData <- subset(NEI, subset = SCC %in% motorVehicleSCC, na.exclude = T)
# Subset Baltimore data
baltimoreVehicleData <- subset(vehicleData, subset = fips == "24510" )
# Get the sum of all emissions
orgBaltimoreVehicles <- tapply(baltimoreVehicleData$Emissions, baltimoreVehicleData$year, sum )
# Creates device for png file
png(filename = "question5.png", width = 480, height = 480)
# Plot it
plot(c(1999, 2002, 2005,2008), type="l", orgBaltimoreVehicles, xlab = "Year",ylab =  expression(PM[2.5] * " emissions (tons)"), col="blue",xaxt="n")
title("5. Baltimore vehicle emissions")
axis(side=1,at=c(1999, 2002, 2005,2008))

dev.off()