create_png4 <- function() {
        library(plyr)
        library(dplyr)
        library(ggplot2)
        
        # Read data files
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")

        # Determine SCC codes that have coal and comb in Short.Name
        coal <- grep("coal", SCC$Short.Name, ignore.case = TRUE)
        comb_coal <- grep("comb", SCC[coal,3], ignore.case = TRUE)
        scc_coal <- as.character(SCC$SCC[comb_coal])
        
        # filter NEI data for Coal and Comb codes        
        NEI_coal_comb <- NEI[NEI$SCC %in% scc_coal,]
        
        png(file = "plot4.png", width = 480, height = 480, units = "px")
        
        # Calculate total pollution by summing per year
        pollution_by_year <- ddply(NEI_coal_comb, c("year"), summarise, total_pollution = sum(Emissions))
        
        # Create a line plot for total pollution by year, for each of the types
        g <- ggplot(pollution_by_year, aes(year, total_pollution))
        chart <- g + geom_point() + geom_smooth(method = "lm") + ylab("Total PM2.5 emmision (tons)") + ggtitle("Coal Combustion PM2.5 emmisions by year")
        print(chart)
        
        dev.off()        
}