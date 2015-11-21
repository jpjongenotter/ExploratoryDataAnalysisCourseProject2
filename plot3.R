create_png3 <- function() {
        library(plyr)
        library(dplyr)
        library(ggplot2)
        
        # Read data files
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        
        # filter NEI data for Baltimore City, Maryland        
        NEI_Baltimore <- filter(NEI, fips == "24510")
        
        png(file = "plot3.png", width = 480, height = 480, units = "px")
        
        # Calculate total pollution by summing per year and type
        pollution_by_year_and_type <- ddply(NEI_Baltimore, c("year", "type"), summarise, total_pollution = sum(Emissions))
        
        # Create a line plot for total pollution by year, for each of the types
        g <- ggplot(pollution_by_year_and_type, aes(year, total_pollution))
        chart <- g + geom_point() + facet_grid(.~type) + geom_smooth(method = "lm") + ylab("Total PM2.5 emmision (tons)") + ggtitle("Baltimore City PM2.5 emmisions by year")
        print(chart)
        
        dev.off()        
}