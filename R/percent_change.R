# calculates the percentage change between two values

pc_change <- function (x, y) {
    ((y - x)/abs(x))*100
}