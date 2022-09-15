cutVector <- function(vec, m) {
    n <- length(vec) / m
    lapply(0:(m-1),
           \(x) vec[seq(n * x + 1, n * (x + 1))])
}

dist0 <- function(m, n = 1e4) {
    dat <- sample(0:9,
                  size = n * m,
                  replace = TRUE)
    dat_chopped <- cutVector(dat, m)
    sapply(dat_chopped, \(v) mean(v == 0))
}