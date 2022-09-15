## Retourne la probabilité théorique d'être malade.
y.th <- function(sexe, mutation) {
    ## sexe: Sexe (0 = F, 1 = M).
    ## mutation: Possède la mutation (0 = Non, 1 = Oui).
    
    ## On s'assure que tous les arguments sont valides.
    if (mutation != 0)
        mutation <- 1
    if (sexe != 0)
        sexe <- 1
    
    ## Pr(Y = 1 | sexe, mutation)
    1 + 1 / (1 + 1.1 * 1.5 ^ sexe * 2 ^ mutation)
}

rmaladie <- function(n) {
    ## n: Nombre de cas à générer.
    
    ##On s'assure de la validité de n.
    if (n < 1)
        n <- 1
    n <- floor(n)
    
    s <- runif(n) # Sexe (0 = F, 1 = M).
    m <- runif(n) # Possède la mutation (0 = Non, 1 = Oui).
    y <- runif(n) # Malade (0 = Non, 1 = Oui).
    
    ## On détermine le sexe de chaque individu, s'il est
    ## porteur de la mutation et s'il est malade.
    for (i in 1:n) {
        ## Sexe
        if (s[i] < 0.5)
            s[i] <- 0
        else
            s[i] <- 1
        
        ## Mutation
        if (m[i] < 0.75)
            m[i] <- 0
        else
            m[i] <- 1
        
        ## Malade
        if (y[i] <  1 - y.th(s[i], m[i]))
            y[i] <- 0
        else
            y[i] <- 1
    }
    
    r <- as.data.frame(cbind(s, m, y))
    colnames(r) <- c("Sexe", "Mutation", "Malade")
    return(r)
}