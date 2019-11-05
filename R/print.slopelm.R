#' @export
print.slopelm <- function(x, multcomp = NULL, alternative = "two.sided",...){
  print(summary(x, multcomp = multcomp, alternative = alternative, ... = ...))
}