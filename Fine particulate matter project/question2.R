# Question 2
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
# (fips == "24510") from 1999 to 2008? Use the base plotting system to make a 
# plot answering this question.

# Load data
NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

# Subset Baltimore data
baltimoreData <- NEI[NEI$fips == "24510",]
# Cast, just in case
baltimoreData$Emissions <- as.numeric(as.character(baltimoreData$Emissions))
# Organize data by year-total emissions
baltimoreDataSummarized <- data.frame(baltimoreData$year, baltimoreData$Emissions)
names(baltimoreDataSummarized) <- c("year", "Emissions")
orgBaltimore <- tapply(baltimoreDataSummarized$Emissions, baltimoreDataSummarized$year, sum)
# Creates device for png file
png(filename = "question2.png", width = 480, height = 480)
# Plot it
plot(names(orgBaltimore), type = "line", orgBaltimore, xlab = "Year", ylab = expression("Total " * PM[2.5] * " emissions (tons), "),col="blue",xaxt = "n"   )
axis(side=1,at=c(1999, 2002, 2005,2008))
title("2. Emissions in Baltimore")

#Explicitly turns off the device
dev.off()