library(ggplot2)
source("HM_year.R")
library (lmomco)
library (maptools)t
library (maps)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
attach(NEI)
attach(SCC)

SCC1_50 <- SCC[1:50, "SCC"]
NEI_SCC1_50 <- NEI[NEI$SCC == SCC1_50, ]
# But pay attention: there is no data for every pollutant for every year!

# SUBSET – calculate the harmonic mean of the emissons for every year 
NEI_1_50_HM  <- data.frame(year = unique(NEI_SCC1_50$year), HM = HM_year(NEI_SCC1_50))

# SUBSET – create a combined plot of every emission + harmonic mean
p  <- ggplot(NEI_SCC1_50, aes(x=year, y=Emissions)) + geom_point() + 
    scale_x_continuous(breaks=year) +
    labs(x="Year", y="Emissions") + 
    geom_line(data=NEI_1_50_HM, aes(x=year, y=HM)) +
    geom_point(data=NEI_1_50_HM, aes(x=year, y=HM), shape=1, colour="Red") + 
    geom_text(data=NEI_1_50_HM, aes(x=year, y=HM, label=round(NEI_1_50_HM$HM, digits=3)), 
              hjust=-0.3, vjust=-0.4, size=4, colour="Red")
# fuck this is crap, the harmonic mean doesnt really work...

# I need to see how the data is distributed and whats the case with the outliers.
# For that, the best tools are the histogram, density plot, boxplot.
# So lets check a histogram first
# Use year as the faceting variable
h <- ggplot(NEI_SCC1_50, aes(x=Emissions)) + geom_histogram(fill="red", binwidth=50) +
    facet_grid(year ~ .)
ggsave(h, filename="1_hist.svg")

# density plot
d <- ggplot(NEI_SCC1_50, aes(x=Emissions))
d + geom_histogram(aes(y=..density..), fill="red", binwidth=50) + 
    geom_density(fill=NA, colour="black") +
    facet_grid(year ~ .)
ggsave(d, filename="2_density.svg")

# boxplot
b <- ggplot(NEI_SCC1_50, aes(x=factor(year), y=Emissions)) + 
    geom_boxplot(outlier.size=1.5, outlier.shape=21) +
    labs(x="Year")
ggsave(b, filename="3_boxplot.svg")


