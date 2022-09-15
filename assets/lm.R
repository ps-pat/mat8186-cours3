lmFast <- function(m) {
    res <- numeric()
    
    for (kk in 1:m) {
        random_values <- rnorm(1e6)
        X <- matrix(random_values, ncol = 20)
        X <- cbind(1, X)
        y <- rnorm(5e4)
        
        qrX <- qr(X)
        β <- qr.coef(qrX, y)
        res <- cbind(res, β)
    }
    rownames(res) <- c("(Intercept)", paste0("X", 1:20))
    res
}

lmSlow <- function(m) {
    Xs <- rnorm(1e6 * m) |>       ## Génération des données.
        cutVector(m) |>           ## 1 entrée = 1 simulation.
        lapply(matrix, ncol = 20) ## vecteur -> matrice.
    
    ys <- cutVector(rnorm(5e4 * m), m)
    
    mapply(\(X, y) coef(lm(y ~ X)), Xs, ys)
}