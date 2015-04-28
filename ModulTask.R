library(ggplot2)
source("HM_year.R")
source("mean_year.R")
source("~/Documents/lib_R/sum_year.R")
source("~/Documents/lib_R/sum_by_category.R")
library (lmomco)
library (maptools)
library (maps)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")
attach(NEI)
attach(SCC)


### Data preparation––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# I need to see how the data is distributed and whats the case with the outliers.
# For that, the best tools are the histogram, density plot, boxplot.
# So lets check a histogram first
# Use year as the faceting variable
h <- ggplot(NEI, aes(x=Emissions)) + geom_histogram(fill="red", binwidth=50) +
    facet_grid(year ~ .)
ggsave(h, filename="1_hist.png")

# density plot
d <- ggplot(NEI, aes(x=Emissions))
d + geom_histogram(aes(y=..density..), fill="red", binwidth=50) + 
    geom_density(fill=NA, colour="black") +
    facet_grid(year ~ .)
ggsave(d, filename="2_density.png")

# boxplot
b <- ggplot(NEI, aes(x=factor(year), y=Emissions)) + 
    geom_boxplot(outlier.size=1.5, outlier.shape=21) +
    labs(x="Year")
ggsave(b, filename="3_boxplot.png")

# Lets exclude the outliers that were identified by the boxplot, but also store them in a separate data frame.
summary(NEI)
upper.limit <- quantile(NEI$Emissions)[4] + 1.5*IQR(NEI$Emissions)
lower.limit <- quantile(NEI$Emissions)[2] - 1.5*IQR(NEI$Emissions)
lower.limit <- 0 # because lower.limit results in negative and there are no negative emissions
NEI_sub <- subset(NEI, Emissions<upper.limit & Emissions>lower.limit)
NEI_outl <- subset(NEI, Emissions>upper.limit)

# try the boxplot again
b <- ggplot(NEI_sub, aes(x=factor(year), y=Emissions)) + 
    geom_boxplot(outlier.size=1.5, outlier.shape=21) +
    labs(x="Year")
ggsave(b, filename="3_boxplot_sub.png")
# ok, this made the boxplot, it looks better, but again, there are a lot of outliers, so I guess this is just the nature of the data, because



# 1) lets plot the total emissions per year–––––––––––––––––––––––––––––––––––––
NEI_sum  <- data.frame(year = unique(NEI_sub$year), S = sum_year(NEI_sub))

total_year  <- ggplot(NEI_sum, aes(x=year, y=S)) +
    geom_point(shape=1) + 
    scale_x_continuous(breaks=NEI_sum$year) +
    labs(x="Year", y="Total PM2.5 emissions (tons)") + 
    geom_line()
   # geom_text(aes(x=year, y=S, label=round(NEI_sum$S, digits=1)), 
   #           hjust=1.2, vjust=-0.8, size=2.5)
ggsave(total_year, filename="figs/v2_1_total_year.png", width=130, height=120, units="mm")


# 2) lets compare the emissions by type–––––––––––––––––––––––––––––––––––––––––
# first, add up the emissions by type for every year
NEI_type_sum  <- sum_by_category(NEI_sub)

# but check this!!!!!
# library(plyr)
# ddply(NEI_sub, c("type", "year"), summarise, Emissions_total=sum(Emissions))

type_year <- ggplot(NEI_type_sum, aes(x=year, y=Emissions_total, linetype=type)) + 
    geom_line() +
    scale_x_continuous(breaks=NEI_sum$year) +
    labs(x="Year", y="Total PM2.5 emissions (tons)") +
    geom_point(shape=21, size=4, fill="white")
ggsave(type_year, filename="figs/v2_2_type_year.png", width=130, height=120, units="mm")









# try the histogram again
h <- ggplot(NEI_sub, aes(x=Emissions)) + geom_histogram(fill="red") +
    facet_grid(year ~ .)
ggsave(h, filename="1_hist_sub.png")

# also for the outliers
h <- ggplot(NEI_outl, aes(x=Emissions)) + geom_histogram(fill="red") +
    facet_grid(year ~ .)
ggsave(h, filename="1_hist_outl.png")


# compare the two groups by the sum of emissions and the number of unique SCCs (IDs)
min(sum(NEI_outl$Emissions), sum(NEI_sub$Emissions))
sum(NEI_outl$Emissions)
sum(NEI_sub$Emissions)

length(unique(NEI_outl$SCC))
length(unique(NEI_sub$SCC))
length(unique(NEI$SCC)) == length(unique(NEI_sub$SCC)) + length(unique(NEI_outl$SCC))
# well, the SCC are not matching...what to do...
# take the intersection of both data frames by common SCC. These SCC then probably require closer attention.

NEI_merge2 <- subset(NEI_sub, SCC %in% NEI_outl$SCC)

h <- ggplot(NEI_merge, aes(x=Emissions)) + geom_histogram(fill="red") +
    facet_grid(year ~ .)
ggsave(h, filename="1_hist_merge.png")

# lets calculate the harmonic mean again and plot it
NEI_sub_HM  <- data.frame(year = unique(NEI_sub$year), HM = HM_year(NEI_sub))
NEI_sub_M  <- data.frame(year = unique(NEI_sub$year), M = mean_year(NEI_sub))

sub_hm  <- ggplot(NEI_sub, aes(x=year, y=Emissions)) + geom_point() + 
    scale_x_continuous(breaks=year) +
    labs(x="Year", y="Emissions") + 
    geom_line(data=NEI_sub_HM, aes(x=year, y=HM)) +
    geom_point(data=NEI_sub_HM, aes(x=year, y=HM), shape=1, colour="Red") + 
    geom_text(data=NEI_sub_HM, aes(x=year, y=HM, label=round(NEI_sub_HM$HM, digits=3)), 
              hjust=-0.3, vjust=-0.4, size=4, colour="Red")
ggsave(sub_hm, filename="3_sub_hm.png")

sub_m  <- ggplot(NEI_sub, aes(x=year, y=Emissions)) + geom_point() + 
    scale_x_continuous(breaks=year) +
    labs(x="Year", y="Emissions") + 
    geom_line(data=NEI_sub_M, aes(x=year, y=M)) +
    geom_point(data=NEI_sub_M, aes(x=year, y=M), shape=1, colour="Red") + 
    geom_text(data=NEI_sub_M, aes(x=year, y=M, label=round(NEI_sub_M$M, digits=3)), 
              hjust=-0.3, vjust=-0.4, size=4, colour="Red")
ggsave(sub_m, filename="4_sub_m.png") # its nice, but doesn't really need all of the dots

# lets check the mean emissions by type
meanem <- tapply(NEI_sub$Emissions, NEI_sub$type, mean)
type <- factor(unique(NEI_sub$type))
qplot(type, meanem, xlab(x=Type, y=Mean emissions))

dot <- ggplot(NEI_sub, aes(x=type))
dot + stat_function(fun=tapply(NEI_sub$Emissions, NEI_sub$type, mean))
