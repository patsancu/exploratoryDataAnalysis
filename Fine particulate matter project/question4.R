# Question 4
# Across the United States, how have emissions from coal combustion-related 
# sources changed from 1999–2008?

# Load data
NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
# Get the SCCs related to coal combustion
coalCombustionSCC <- SCC[grep("(comb.*)(Coal)", SCC$EI.Sector, ignore.case = T),]
# Match it with the Emissions Data
coalCombustionData <- subset(NEI, subset = SCC %in% coalCombustionSCC$SCC)
# Get the sum of all emissions
orgCoal <- tapply(coalCombustionData$Emissions, coalCombustionData$year, sum)
# Creates device for png file
png(filename = "question4.png", width = 480, height = 480)
#Plot it
plot(names(orgCoal), type = "l", orgCoal, xlab = "Year", ylab = expression(PM[2.5] * " emissions (tons)"), col="blue",xaxt = "n"   )
title("4. US emissions related to coal combustion")
axis(side=1,at=c(1999, 2002, 2005,2008))

#Explicitly turns off the device
dev.off()