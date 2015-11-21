create_png1 <- function() {
        library(plyr)
        
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        
        png(file = "plot1.png", width = 480, height = 480, units = "px")
        
        pollution_by_year <- ddply(NEI, c("year"), summarise, total_pollution = sum(Emissions))
        
        # Do not use scientific notation for axis labels
        options("scipen" = 20)
        
        barplot(pollution_by_year$total_pollution, names.arg = pollution_by_year$year, ylab = "Total PM2.5 emmision (tons)", main = "Total PM2.5 emmisions by year")
        
        dev.off()        
}