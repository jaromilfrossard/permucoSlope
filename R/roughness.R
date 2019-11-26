#' @importFrom stats sd
roughness <- function(x){
  #apply(apply(apply(x,1,function(row)diff(row/sd(row))),2,diff),2,sd)
  apply(apply(apply(x,1,function(row)diff(scale(row))),2,diff),2,sd)

  # x%>%
  #   t%>%
  #   apply(2,scale)%>%
  #   apply(2,diff)%>%
  #   apply(2,diff)%>%
  #   apply(2,sd)

  }
