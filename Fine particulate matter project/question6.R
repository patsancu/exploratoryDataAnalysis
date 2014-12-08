# Question 6
# Compare emissions from motor vehicle sources in Baltimore City 
# with emissions from motor vehicle sources in Los Angeles County, California 
# (fips == "06037"). Which city has seen greater changes over time in 
# motor vehicle emissions?
# Load data
NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
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
# Creates device for png file
png(filename = "question6.png", width = 480, height = 480)
# Plot it
# First, let it plot Baltimore
plot(c(1999, 2002, 2005,2008), type="l", orgBaltimoreVehicles, 
     xlab = "Year", ylab = expression(PM[2.5] * " emissions (tons)"), 
     col="blue",xaxt="n",ylim=range(c(orgBaltimoreVehicles,orgLAVehicles)))
# Then, let it plot LA
lines(c(1999, 2002, 2005,2008), orgLAVehicles, col="red")
legend("center", pch = ".", lty=1,col = c("red", "blue"), legend = c("LA", "Baltimore"))
title(expression("6. LA-Baltimore vehicle comparison" ))
axis(side=1,at=c(1999, 2002, 2005,2008))


#Prettier version
# df <- data.frame(c(1999,2002, 2005,2009), orgLAVehicles, orgBaltimoreVehicles)
#       ggplot(df, aes(c(1999, 2002, 2005,2008))) +
#       geom_line(aes(y=orgLAVehicles), colour="red") +
#       geom_line(aes(y=orgBaltimoreVehicles), colour="blue") 
dev.off()