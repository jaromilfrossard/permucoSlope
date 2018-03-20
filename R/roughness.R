#' @importFrom stats sd
roughness <- function(x){apply(apply(apply(x,1,function(row)diff(row/sd(row))),2,diff),2,sd)}
