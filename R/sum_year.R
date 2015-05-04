# calculates a total value per group in a dataset
# in this case, it calculates the total emissions in every year in the dataset

sum_year <- function (x) {
    S  <- NULL
    for (i in unique(x$year)) {
        s <- sum(x[x$year == i, "Emissions"])
        S <- append(S, s)
    }
    S
}

# for example creates a new data frame with the total values for every year
# x_sum  <- data.frame(year = unique(x$year), S = sum_year(x))