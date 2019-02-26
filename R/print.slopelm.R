#' @export
print.slopelm <- function(x, alternative = "two.sided", slope = TRUE,...){
  if(slope){
    cat("Cluster ", x$test, " test using ", x$method, " to handle nuisance variables \n with ",
        paste(x$np, sep = ", ", collapse = ", "), " permutations and ",
        x$fun_name, " as mass function.\n\n", sep = "")

    cat("Alternative Hypothesis : two.sided.\n \n", sep = "")

    cat("Testing mean and slope different to 0.\n \n", sep = "")

    permuco:::print.listof_cluster_table(x$slope_table, ...)

  }else{
    permuco:::print.clusterlm(x, alternative = "two.sided", ...)
  }
}