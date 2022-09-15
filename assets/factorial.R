fact <- function(x) {
    x < 0 && return(x)
    
    x * fact(x - 1.0)
}