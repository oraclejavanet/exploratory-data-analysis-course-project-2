# plot3.R

# Load libraries
library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(scales, warn.conflicts = FALSE)

# load data
setwd("~jhunter/repos/coursera/data-science-specialization/exploratory-data-analysis-course-project-2")
extNeiDataFileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
extNeiDataFile <- "data/exdata_data_NEI_data.zip"
if (!file.exists(extNeiDataFile)) {
    if (!file.exists('data')) {
        dir.create('data')
    }
    download.file(url = extNeiDataFileURL, destfile = extNeiDataFile)
    unzip(extNeiDataFile, exdir = "data")
}
stopifnot(file.size(extNeiDataFile) == 30643310)
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")
stopifnot(dim(NEI) == c(6497651, 6))
stopifnot(dim(SCC) == c(11717, 15))

# summarize total emissions by source type and year for Baltimore (fips == "24510")
totalBaltimoreBySource <- NEI %>%
    filter(fips == "24510") %>%
    group_by(type, year) %>%
    filter(year == 1999|2002|2005|2008) %>%
    summarize(totalEmissions = sum(Emissions))

# optional: specify the order of the plots
totalBaltimoreBySource$type <- factor(totalBaltimoreBySource$type,
                                      levels = c("POINT", "NONPOINT", "ON-ROAD", "NON-ROAD"))

# create image file
png('plot3.png', width = 480, height = 480)

# create plot
gTotBalSrc <- ggplot(data = totalBaltimoreBySource, aes(x = factor(year),
                                                        y = totalEmissions,
                                                        fill = type)) +
    geom_bar(stat = "identity") +
    facet_grid(. ~ type) +
    xlab("Year") +
    ylab("Total PM2.5 Emissions (in tons)") +
    guides(fill = FALSE) +
    theme(plot.title = element_text(size = 14, hjust = 0.5, vjust = 0.5),
          axis.text.x = element_text(angle = 45,
                                     hjust = 0.5,
                                     vjust = 0.5,
                                     margin = margin(b = 10))) +
    scale_y_continuous(labels = comma) +
    ggtitle("Total PM2.5 Emissions in\nBaltimore City, MD by Source Type 1999-2008")
print(gTotBalSrc)

# close device
dev.off()
