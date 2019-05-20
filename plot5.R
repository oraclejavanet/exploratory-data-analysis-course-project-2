# plot5.R

# Load libraries
library(dplyr, warn.conflicts = FALSE)
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
totalVehicleNEI <- vehicleNEI %>%
    filter(fips == "24510") %>%
    group_by(year) %>%
    filter(year == 1999|2002|2005|2008) %>%
    summarize(totalEmissions = sum(Emissions))

# manually specify axis parameters
xYears <- c(1999, 2002, 2005, 2008)
yEmiss <- pretty(totalVehicleNEI$totalEmissions, n = 4)

# create image file
png('plot5.png', width = 480, height = 480)

# create plot with custom axes
plot(totalVehicleNEI$year,
     totalVehicleNEI$totalEmissions,
     type = "b",
     bty = "l",
     lwd = 3,
     pch = 19,
     col = rgb(0.2, 0.4, 0.6, 0.8),
     axes = FALSE,
     xlab = "Year",
     ylab = "Total PM2.5 Emissions (in tons)",
     main = "Total PM2.5 Emissions in Baltimore City, MD for\nMotor Vehicles 1999-2008")
axis(1, at = xYears, labels = xYears)
axis(2, at = yEmiss, labels = yEmiss)

# close device
dev.off()
