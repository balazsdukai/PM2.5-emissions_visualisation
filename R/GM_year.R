# the function takes NEI as an argument and calcucates the geometric mean emissions per year
GM_year <- function (x) {
    y <- unique(x$year)
    GM  <- NULL
    for (i in unique(x$year)) 
        m <- geometric.mean(x[x$year == i, "Emissions"])
       
}
