create_png3 <- function() {
        library(plyr)
        library(dplyr)
        library(ggplot2)
        
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        
        NEI_Baltimore <- filter(NEI, fips == "24510")
        png(file = "plot3.png", width = 480, height = 480, units = "px")
        
        pollution_by_year_and_type <- ddply(NEI_Baltimore, c("year", "type"), summarise, total_pollution = sum(Emissions))
        
        p <- qplot(year, total_pollution, data = pollution_by_year_and_type, facets = .~type, type = "l", geom = c("point", "smooth"), method = "lm")
        print(p)
        
        dev.off()        
}