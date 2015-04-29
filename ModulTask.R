library(ggplot2)
library(plyr)
#library(dplyr)
library(rgdal)
library(raster)
source("HM_year.R")
source("mean_year.R")
source("~/Documents/lib_R/sum_year.R")
source("~/Documents/lib_R/sum_by_category.R")
source("~/Documents/lib_R/percent_change.R")



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
    geom_boxplot(outlier.size=1.5, outlier.shape=21)
ggsave(b, filename="figs/3_boxplot.png")

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
ggsave(b, filename="figs/3_boxplot_sub.png")
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
# a <- ddply(NEI_sub, c("type", "year"), summarise, Emissions_total=sum(Emissions))

type_year <- ggplot(NEI_type_sum, aes(x=year, y=Emissions_total, linetype=type)) + 
    geom_line() +
    scale_x_continuous(breaks=NEI_sum$year) +
    labs(x="Year", y="Total PM2.5 emissions (tons)") +
    geom_point(shape=21, size=4, fill="white")
ggsave(type_year, filename="figs/v2_2_type_year.png", width=130, height=120, units="mm")

#####
# There was a drastical increase in the point type of sources between 2005 and 2008, so we could focus on those in the later analysis


# 3) Percentage change in emissions–––––––––––––––––––––––––––––––––––––––––––––
# subset NEI_sub for type==POINT
NEI_POINT <- NEI_sub[NEI_sub$type=="POINT", ]
# calculate the total emissions per year for every fips
fips_year_all <- ddply(NEI_POINT, c("fips", "year"), summarise, Emissions_total=sum(Emissions))
# select those fips that have a value for 2005 OR 2008
fips_year <- fips_year_all[fips_year_all$year==2005 | fips_year_all$year==2008, ]
# delete the fips that dont have a value for both 2005 and 2008
fips_05 <- fips_year[fips_year$year==2005, "fips"]
fips_08 <- fips_year[fips_year$year==2008, "fips"]
fips.include <- intersect(fips_05, fips_08)
fips_year2 <- fips_year[fips_year$fips %in% fips.include, ]
# then calcuate the percentage change by every fips
fips_CHG  <- data.frame(fips=as.numeric(), Em.change=as.numeric())
for (i in unique(fips_year2$fips)) {
    e_05 <- fips_year2[fips_year2$fips==i & fips_year2$year==2005, "Emissions_total"]
    e_08 <- fips_year2[fips_year2$fips==i & fips_year2$year==2008, "Emissions_total"]
    fips_CHG <- rbind(fips_CHG, data.frame(fips=i, Em.change=pc_change(e_05, e_08)))
}
head(fips_CHG)

# R for geospatial analysis
county  <-  readOGR("/home/balazs/Downloads/R books/Learning-R-for-Geospatial-Analysis_Code/Data files/", 
                    "USA_2_GADM_fips", stringsAsFactors = FALSE) # need to use the FULL path
county_f  <-  fortify(county, region = "FIPS")
head(county_f)

colnames(county_f)[which(colnames(county_f) == "id")] = "fips"
county_f  <-  join(county_f, fips_CHG, "fips")
head(county_f)

states  <-  getData("GADM", country = "USA", level = 1)
states  <-  states[!(states$NAME_1 %in% c("Alaska", "Hawaii")), ]
states  <-  spTransform(states, CRS(proj4string(county)))
states_f  <-  fortify(states, region = "NAME_1")
head(states_f)

sp_minimal  <- 
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank())

ggplot() + 
    geom_polygon(data = states_f, 
                 aes(x = long, y = lat, group = group), 
                 colour = "black", fill = NA) +
    coord_equal() +
    sp_minimal

ggplot() + 
    geom_polygon(data = county_f, 
                 colour = NA, 
                 aes(x = long, y = lat, group = group, fill = Em.change)) +
    geom_polygon(data = states_f, 
                 colour = "white", size = 0.25, fill = NA,
                 aes(x = long, y = lat, group = group)) +
    coord_equal() +
    sp_minimal
    scale_fill_gradientn(
        name = expression(paste("Emission change ("%")")), 
        colours = rainbow(7), 
        trans = "log10", 
        labels = as.character,
        breaks = 10^(-1:5))
ggsave("figs/v2_3_CHGmap.png", width = 5.5, height = 3.25)

# check data(county.fips) (maps) AND data(countyMapEnv)
data(county.fips) # but it doesn't have all the fips I need!!!!!!!
county_CHG <- merge(fips_CHG, county.fips, by="fips")
us_county_map <- map_data("county")

tidyr::separate(county_CHG, polyname, c("region", "subregion")


# website

map_data <- merge(us_state_map, dataset, by='region', all=T)
map_data <- map_data[order(map_data$order), ]
(qplot(long, lat, data=map_data, geom="polygon", group=group, fill=val)
 + theme_bw() + labs(x="", y="", fill="")
 + scale_fill_gradient(low='#EEEEEE', high='darkgreen')
 + opts(title="I was created using gplot2!",
        legend.position="bottom", legend.direction="horizontal"))



