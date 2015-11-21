create_png5 <- function() {
        library(plyr)
        library(dplyr)
        library(ggplot2)
        
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        
        motor <- grep("Motor vehicles | Motorcycles", SCC$Short.Name, ignore.case = TRUE)
        scc_motor <- as.character(SCC$SCC[motor])
        
        NEI_motor <- NEI[NEI$SCC %in% scc_motor,]
        NEI_motor_Baltimore <- filter(NEI_motor, fips == "24510")
        
        png(file = "plot5.png", width = 480, height = 480, units = "px")
        
        pollution_by_year <- ddply(NEI_motor_Baltimore, c("year"), summarise, total_pollution = sum(Emissions))
        
        p <- qplot(year, total_pollution, data = pollution_by_year, type = "l", geom = c("point", "smooth"), method = "lm")
        print(p)
        
        dev.off()        
}