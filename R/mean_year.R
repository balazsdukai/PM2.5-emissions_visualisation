mean_year <- function (x) {
    M  <- NULL
    for (i in unique(x$year)) {
        m <- mean(x[x$year == i, "Emissions"])
        M <- append(M, m)
    }
    M
}