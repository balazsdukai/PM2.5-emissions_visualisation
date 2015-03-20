library(ggplot2)
library(psych)
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
attach(NEI)
attach(SCC)

SCC1_50 <- SCC[1:50, "SCC"]
NEI_SCC1_50 <- NEI[NEI$SCC == SCC1_50, ]
## But pay attention: there is no data for every pollutant for every year!

## Play aroung with ggplot2
SCC_row <- NEI_SCC1_50[,"SCC"]
emi <- NEI_SCC1_50[,"Emissions"] 
year <- NEI_SCC1_50[, "year"]
geom_bar(NEI_SCC1_50, aes(x = SCC_row, y = emi))
geom_histogram(emi)
plot(SCC_row, emi, type="l")
qplot(, emi)


SCC1_1 <- SCC[1, "SCC"]
NEI_SCC1_1 <- NEI[NEI$SCC == SCC1_1, ]
emi1 <- NEI_SCC1_1[,"Emissions"] 
year1 <- NEI_SCC1_1[, "year"]
qplot(, emi)
