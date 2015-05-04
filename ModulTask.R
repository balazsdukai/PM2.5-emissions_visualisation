library(ggplot2)
library(plyr)
library(rgdal)
library(raster)
library(grid)
source("R/sum_year.R")
source("R/percent_change.R")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("data/summarySCC_PM25.rds")
attach(NEI)

### Data preparation––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# I need to see how the data is distributed and whats the case with the outliers.
# For that, the best tools are the histogram, density plot, boxplot.
# So lets check a histogram first
# Use year as the faceting variable
h <- ggplot(NEI, aes(x=Emissions)) + 
    geom_histogram(fill="red", binwidth=50) +
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



# 1) lets plot the total emissions per year–––––––––––––––––––––––––––––––––––––
NEI_sum  <- data.frame(year = unique(NEI_sub$year), S = sum_year(NEI_sub))

total_year  <- ggplot(NEI_sum, aes(x=year, y=S)) +
    geom_line(lwd=0.9) + 
    scale_x_continuous(breaks=NEI_sum$year) +
    scale_y_continuous(limits=c(0, 38500)) +
    labs(x="Year", y="Total PM2.5 emissions (tons)") + 
    geom_point(shape=21, size=2, fill="white") +
    coord_fixed(ratio = 0.0003) +
    theme_minimal(base_size = 10, base_family = "Verdana")
   # geom_text(aes(x=year, y=S, label=round(NEI_sum$S, digits=1)), 
   #           hjust=1.2, vjust=-0.8, size=2.5)
ggsave(total_year, filename="figs/v2_1_total_year.png", width=80, height=80, units="mm")


# 2) lets compare the emissions by type–––––––––––––––––––––––––––––––––––––––––
# first, add up the emissions by type for every year
NEI_type_sum  <- ddply(NEI_sub, c("type", "year"), summarise, Emissions_total=sum(Emissions))

#####
# There was a drastical increase in the point type of sources between 2005 and 2008, so we could focus on those in the later analysis
# so lets highlight it

type_year <- ggplot(NEI_type_sum, aes(x=year, y=Emissions_total, 
                                      linetype=type)) + 
    geom_line(lwd=0.6) +
    scale_x_continuous(breaks=NEI_sum$year) +
    scale_y_continuous(limits=c(0, 38500)) +
    labs(x="Year", y="Total PM2.5 emissions (tons)", linetype="PM2.5 emission source") +
    geom_line(data=(NEI_type_sum[NEI_type_sum$type=="POINT" & NEI_type_sum$year==c(2005,2008), ]),
              aes(colour="#b2182b"), lwd=1.2) + 
    guides(colour=FALSE) +
    geom_point(shape=21, size=2, fill="white") +
    theme_minimal(base_size = 10, base_family = "Verdana") +
    coord_fixed(ratio = 0.0003) 
ggsave(type_year, filename="figs/v2_2_type_year.png", width=150, height=80, units="mm")


# 3) histogram to check the emission change for extreme values––––––––––––––––––
# first calculate the percentage change in emissions
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
fips_year <- fips_year[fips_year$fips %in% fips.include, ]
# then calcuate the percentage change by every fips
fips_CHG  <- data.frame(fips=as.numeric(), Em.change=as.numeric())
for (i in unique(fips_year$fips)) {
    e_05 <- fips_year[fips_year$fips==i & fips_year$year==2005, "Emissions_total"]
    e_08 <- fips_year[fips_year$fips==i & fips_year$year==2008, "Emissions_total"]
    fips_CHG <- rbind(fips_CHG, data.frame(fips=i, Em.change=pc_change(e_05, e_08)))
}
head(fips_CHG)

# then we analyse it with a histogram
ggplot() +
    geom_histogram(data=fips_CHG, aes(x=Em.change, fill="#ef8a62")) +
    theme_minimal(base_size = 12, base_family = "Verdana") +
    labs(x="PM2.5 Emission change (%)", y="Number of observations") +
    guides(fill=F) +
    coord_fixed(ratio = 1000) +
    annotate("text", 
             x=1250000, 
             y=1850, 
             label="Emission source:\nPOINT\n Year: 2005–2008", 
             lineheight=1.2,
             size=5) 
ggsave("figs/v2_3_hist1.png", width=140, height=140, units="mm")

# if I zoom in:
v <- 600
br <- seq.int(from=0, to=5000, by=1000)
ggplot() +
    geom_histogram(data=fips_CHG, aes(x=Em.change, fill="#ef8a62"), binwidth=100) +
    scale_x_continuous(limits=c(-100, 5000), breaks=c(br, v)) +
    labs(x="PM2.5 Emission change (%)" , y="Number of observations") +
    guides(fill=F) +
    geom_vline(xintercept=v, linetype="dashed") +
    coord_fixed(ratio = 5) +
    annotate("text", 
             x=4500, 
             y=620, 
             label="Emission source:\nPOINT\n Year: 2005–2008", 
             lineheight=1.2,
             size=5) + 
    theme_minimal(base_size = 12, base_family = "Verdana")
ggsave("figs/v2_3_hist2.png", width=180, height=140, units="mm")

# therefore we limit the data frame to Em.change<=600
fips_CHG_lim <- fips_CHG[fips_CHG$Em.change<=600, ]


# 4) Map––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

# Recode the continuous variables to categorical
fips_CHG_lim$category <- cut(fips_CHG_lim$Em.change,
                     breaks=c(-100, 0, 100, 200, 600), # -100 because of min(fips_CHG_lim$Em.change)
                     labels=c("-100–0%","0–100%","100–200%","200–600%"))

# Ch.9 from Learning-R-for-Geospatial-Analysis_Dorman_2014
# the geographical data for the map is directly downloaded from the database of global administrative boundaries
county  <-  readOGR("/home/balazs/Documents/MSE_Geoinformationstechnologie/InfVis/ModulTask/data", "USA_2_GADM_fips", stringsAsFactors = FALSE) # need to provide a full path
county = county[
    county$NAME_1 != "Alaska" &
        county$NAME_1 != "Hawaii", ]
county = county[
    county$TYPE_2 != "Water body", ]
newProj = CRS(
"+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")
county = spTransform(county, newProj)
county_f  <-  fortify(county, region = "FIPS")
head(county_f)

colnames(county_f)[which(colnames(county_f) == "id")] = "fips"
county_f  <-  join(county_f, fips_CHG_lim, "fips")
head(county_f)

states  <-  getData("GADM", country = "USA", level = 1) # get the data for the state boundaries
states  <-  states[!(states$NAME_1 %in% c("Alaska", "Hawaii")), ]
states  <-  spTransform(states, CRS(proj4string(county)))
states_f  <-  fortify(states, region = "NAME_1")
head(states_f)

sp_minimal  <- 
    theme_minimal(base_size = 12, base_family = "Verdana") +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank())
# create the map 
ggplot() + 
    geom_polygon(data = county_f, 
                 colour = "white",
                 size = 0.15,
                 aes(x = long, y = lat, group = group, fill = category)) +
    geom_polygon(data = states_f, 
                 colour = "#999999", 
                 size = 0.18, 
                 fill = NA,
                 aes(x = long, y = lat, group = group)) +
    coord_equal() +
    sp_minimal +
    scale_fill_manual(values = c("-100–0%"="#d1e5f0",
                                 "0–100%"="#fddbc7",
                                 "100–200%"="#ef8a62",
                                 "200–600%"="#b2182b"),
                      name = expression(paste("Change in emissions"))) +
    theme(
        legend.position=c(1,1), 
        legend.justification=c(0, 1), 
        legend.key.width=unit(1, "lines"), 
        plot.margin=unit(c(1, 8, 0.5, 0.5), "lines")
    )
ggsave("figs/v2_4_CHGmap.png")
# this map shows in which counties were more than increase in the emissions between 2005 and 2008

     
    


