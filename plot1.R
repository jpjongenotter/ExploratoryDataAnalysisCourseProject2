create_png1 <- function() {
        library(plyr)
        
        # Read data files
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        
        png(file = "plot1.png", width = 480, height = 480, units = "px")
        
        # Calculate total pollution by summing per year
        pollution_by_year <- ddply(NEI, c("year"), summarise, total_pollution = sum(Emissions))
        
        # Do not use scientific notation for axis labels
        options("scipen" = 20)
        
        # Create a line plot for total pollution by yeae
        plot(x = pollution_by_year$year, y = pollution_by_year$total_pollution, xlab = "Year", ylab = "Total PM2.5 emmision (tons)", main = "Total PM2.5 emmisions by year")
        
        # Add a linear model to display the trend
        model <- lm(pollution_by_year$total_pollution ~ pollution_by_year$year)
        abline(model, lwd = 2)
        
        dev.off()        
}