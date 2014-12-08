# Question 3
# Of the four types of sources indicated by the type (point, nonpoint, onroad, 
# nonroad) variable, which of these four sources have seen decreases in emissions 
# from 1999–2008 for Baltimore City? Which have seen increases in emissions from 
# 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
library(ggplot2)

# Load data
NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

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
# Creates device for png file
png(filename = "question3.png", width = 480, height = 480)
# Plot it
g <- ggplot(orgBaltimoreTypes, aes(year, Emissions)) 
g + geom_line(aes(color = type, size=1, alpha=1/2))
axis(side=1,at=c(1999, 2002, 2005,2008))

#Explicitly turns off the device
dev.off()