create_png5 <- function() {
        library(plyr)
        library(dplyr)
        library(ggplot2)
        
        # Read data files
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        
        # Determine SCC codes that have motor vehicles or motor cycles in Short.Name
        motor <- grep("Motor vehicles | Motorcycles", SCC$Short.Name, ignore.case = TRUE)
        scc_motor <- as.character(SCC$SCC[motor])
        
        # filter NEI data for Motor vehicles or motor cycles and for Baltimore City, Maryland
        NEI_motor <- NEI[NEI$SCC %in% scc_motor,]
        NEI_motor_Baltimore <- filter(NEI_motor, fips == "24510")
        
        png(file = "plot5.png", width = 480, height = 480, units = "px")
        
        # Calculate total pollution by summing per year
        pollution_by_year <- ddply(NEI_motor_Baltimore, c("year"), summarise, total_pollution = sum(Emissions))
        
        # Create a line plot for total pollution by year, for each of the types
        g <- ggplot(pollution_by_year, aes(year, total_pollution))
        chart <- g + geom_point() + geom_smooth(method = "lm") + ylab("Total PM2.5 emmision (tons)") + ggtitle("Motor vehicles/cycles Baltimore City PM2.5 emmisions by year")
        print(chart)
                
        dev.off()        
}