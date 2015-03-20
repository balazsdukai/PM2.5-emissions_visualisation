# The function takes NEI as an argument and calcucates the harmonic mean of the 
# emissions per year.
# The function for the harmonic mean calculation is obtained from the package
# "lmomco". http://cran.r-project.org/web/packages/lmomco/index.html

# install.packages(lmomco)
library (lmomco)

HM_year <- function (x) {
    y <- unique(x$year)
    HM  <- NULL
    for (i in unique(x$year)) {
        m <- harmonic.mean(x[x$year == i, "Emissions"])
        HM <- append(HM, m$correction)
    }
    HM
}