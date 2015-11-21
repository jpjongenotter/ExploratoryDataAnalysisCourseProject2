create_png2 <- function() {
        library(plyr)
        library(dplyr)
        
        # Read data files
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")

        # filter NEI data for Baltimore City, Maryland        
        NEI_Baltimore <- filter(NEI, fips == "24510")
        
        png(file = "plot2.png", width = 480, height = 480, units = "px")
        
        # Calculate total pollution by summing per year
        pollution_by_year <- ddply(NEI_Baltimore, c("year"), summarise, total_pollution = sum(Emissions))
        
        # Do not use scientific notation for axis labels
        options("scipen" = 20)
        
        # Create a line plot for total pollution by year
        plot(x = pollution_by_year$year, y = pollution_by_year$total_pollution, xlab = "Year", ylab = "Total PM2.5 emmision (tons)", main = "Baltimore City PM2.5 emmisions by year")
        
        # Add a linear model to display the trend
        model <- lm(pollution_by_year$total_pollution ~ pollution_by_year$year)
        abline(model, lwd = 2)
        
        dev.off()        
}