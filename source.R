h <- ggplot(NEI, aes(x=Emissions)) + geom_histogram(fill="red") +
    facet_grid(year ~ .)
ggsave(h, filename="1_hist.png")

h <- ggplot(NEI, aes(x=Emissions)) + geom_histogram(fill="red") +
    scale_x_log10() + 
    facet_grid(year ~ .)
ggsave(h, filename="1_hist_log.png")

d <- ggplot(NEI, aes(x=Emissions))
d + geom_histogram(aes(y=..density..), fill="red") + 
    geom_density(fill=NA, colour="black") +
    facet_grid(year ~ .)
ggsave(d, filename="2_density.png")

b <- ggplot(NEI, aes(x=factor(year), y=Emissions)) + 
    scale_y_log10() +
    geom_boxplot(outlier.size=1.5, outlier.shape=21) +
    labs(x="Year")
ggsave(b, filename="3_boxplot_log.png")

b <- ggplot(NEI, aes(x=factor(year), y=Emissions)) + 
    scale_y_log10() +
    geom_boxplot(outlier.size=1.5, outlier.shape=21) +
    labs(x="Year")
ggsave(b, filename="3_boxplot_log.png")