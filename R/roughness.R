#' Roughness of Signals
#'
#' @description Roughness of signals using the variance of the second derivative
#' @param x matrix of signals. The time is in column and each row represents one signal.
#' @importFrom stats var
#' @export
roughness <- function(x){
  #apply(apply(apply(x,1,function(row)diff(row/sd(row))),2,diff),2,sd)
  apply(apply(apply(x,1,function(row)diff(scale(row))),2,diff),2,var)

  # x%>%
  #   t%>%
  #   apply(2,scale)%>%
  #   apply(2,diff)%>%
  #   apply(2,diff)%>%
  #   apply(2,var)

  }


# roughness2 <- function(x){
#   #apply(apply(apply(x,1,function(row)diff(row/sd(row))),2,diff),2,sd)
#   apply(apply(apply(x,1,function(row)diff(scale(row))),2,diff),2,function(ri)mean(abs(ri)))
#
#   # x%>%
#   #   t%>%
#   #   apply(2,scale)%>%
#   #   apply(2,diff)%>%
#   #   apply(2,diff)%>%
#   #   abs%>%
#   #   mean
#
# }