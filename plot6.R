# plot6.R

# Load libraries
library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(scales, warn.conflicts = FALSE)
library(stringi, warn.conflicts = FALSE)

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

# select SCC values in SCC where EI.Sector includes "[Vv]ehicle"
vehicleSCC <- subset(SCC,
                     stri_detect_regex(EI.Sector, "Vehicle", case_insensitive = TRUE),
                     select = c(SCC))

# convert to numeric factor
vehicleSCC <- vehicleSCC$SCC

# select year, Emissions, fips from NEI where SCC in (filtered list of SCC values)
vehicleNEI <- subset(NEI, SCC %in% vehicleSCC, select = c(Emissions, year, fips))

# summarize vehicle emissions by year for Baltimore (fips == "24510")
vehicleBaltimoreNEI <- vehicleNEI %>%
    filter(fips == "24510") %>%
    mutate(city = "Baltimore") %>%
    group_by(city, year) %>%
    filter(year == 1999|2002|2005|2008) %>%
    summarize(totalEmissions = sum(Emissions))

# summarize vehicle emissions by year for Los Angeles (fips == "06037")
vehicleLosAngelesNEI <- vehicleNEI %>%
    filter(fips == "06037") %>%
    mutate(city = "Los Angeles") %>%
    group_by(city, year) %>%
    filter(year == 1999|2002|2005|2008) %>%
    summarize(totalEmissions = sum(Emissions))

# combine both datasets
totalVehicleNEI <- rbind(vehicleBaltimoreNEI, vehicleLosAngelesNEI)

# optional: specify the order of the plots
totalVehicleNEI$city <- factor(totalVehicleNEI$city, levels = c("Baltimore", "Los Angeles"))

# create image file
png('plot6.png', width = 480, height = 480)

# create plot
gtotalVehicleNEI <- ggplot(data = totalVehicleNEI, aes(x = factor(year),
                                                       y = totalEmissions,
                                                       fill = city)) +
    geom_bar(stat = "identity") +
    facet_grid(. ~ city) +
    xlab("Year") +
    ylab("Total PM2.5 Emissions (in tons)") +
    guides(fill = FALSE) +
    theme(plot.title = element_text(size = 14, hjust = 0.5, vjust = 0.5),
          axis.text.x = element_text(angle = 45,
                                     hjust = 0.5,
                                     vjust = 0.5,
                                     margin = margin(b = 10))) +
    scale_y_continuous(labels = comma) +
    ggtitle("Total PM2.5 Emissions\nBaltimore City, MD versus Los Angeles, CA\nfor Motor Vehicles 1999-2008")
print(gtotalVehicleNEI)

# close device
dev.off()
