create_png4 <- function() {
        library(plyr)
        library(dplyr)
        library(ggplot2)
        
        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
        
        coal <- grep("coal", SCC$Short.Name, ignore.case = TRUE)
        comb_coal <- grep("comb", SCC[coal,3], ignore.case = TRUE)
        scc_coal <- as.character(SCC$SCC[comb_coal])
        
        NEI_coal_comb <- NEI[NEI$SCC %in% scc_coal,]
        png(file = "plot4.png", width = 480, height = 480, units = "px")
        
        pollution_by_year <- ddply(NEI_coal_comb, c("year"), summarise, total_pollution = sum(Emissions))
        
        p <- qplot(year, total_pollution, data = pollution_by_year, type = "l", geom = c("point", "smooth"), method = "lm")
        print(p)
        
        dev.off()        
}