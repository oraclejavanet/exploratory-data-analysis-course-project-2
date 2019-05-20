# plot1.R

# Load libraries
library(dplyr, warn.conflicts = FALSE)

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

# summarize total emissions by year for U.S.
totalByYear <- NEI %>%
    group_by(year) %>%
    filter(year == 1999|2002|2005|2008) %>%
    summarize(totalEmissions = sum(Emissions))

# manually specify axis parameters
xYears <- c(1999, 2002, 2005, 2008)
yEmiss <- pretty(totalByYear$totalEmissions/10^3, n = 4)

# create image file
png('plot1.png', width = 480, height = 480)

# create plot with custom axes
plot(totalByYear$year,
     totalByYear$totalEmissions/10^3,
     type = "b",
     bty = "l",
     lwd = 3,
     pch = 19,
     col = rgb(0.2, 0.4, 0.6, 0.8),
     axes = FALSE,
     yaxt = "n",
     xlab = "Year",
     ylab = "Total PM2.5 Emissions (in kilotons)",
     main = "Total PM2.5 Emissions in the U.S. 1999-2008")
axis(1, at = xYears, labels = xYears)
axis(2, at = yEmiss, labels = prettyNum(yEmiss, big.mark = ","))

# close device
dev.off()
