#' Compute cluster-mass statistics using the mean and the slope of the signal
#'
#' @description Compute the clustermass statistic on the combined hypothesis that the slope or the mean of the signal are different from zero.
#' @param formula A formula object where the left part is a matrix defined in the global environnement.
#' @param data A data frame for the independant variables.
#' @param np The number of permutations. Default value is \code{5000}.
#' @param method A character string indicating the method used to handle nuisance variables. Default is \code{NULL} and will switch to \code{"freedman_lane"} for the fixed effects model and to \code{"Rd_kheradPajouh_renaud"} for the repeated measures ANOVA. See \link{lmperm} or \link{aovperm} for details on the permutation methods.
#' @param test A character string to specify the name of the test. Default is \code{"fisher"}. \code{"t"} is available for the fixed effects model.
#' @param threshold A numerical vector that specify the limit of a cluster for the \code{"clustermass"} multiple comparisons procedure. If it is a vector each value will be associated to an effect. If it is vector the same threshold will be used for each test. Default value is \code{NULL} and will compute a threshold based on the 0.95 quantile of the choosen test statistic.
#' @param aggr_FUN A function that will be used to aggregate the statistics of a cluster into one scalar. Default is the sum of squares fot t statistic and sum for F statistic.
#' @param slope_FUN A function that will be used to compute the slope of the response variables. \code{slope_spline}, \code{slope_locpol}, \code{slope_spectral} and \code{slope_diff} are available.
#' @param multcomp A vector of character defining the methods of multiple comparisons to compute. Default is \code{"slope"}, and \code{"glue"} is an additional option available. \code{"slope"} considers tests on signals and slopes equally and \code{"glue"} uses tests on slopes only to bind clusters.
#' @param ... Futher arguments, see details.
#' @return A list containing : a table of the clusters, or a \code{multcomp} object for the other multiple comparison procedures. Use the \link{plot.slopelm} method to have a quick overview of the results.
#' @details
#'
#' See the \link{clusterlm} function for detail about
#' @author jaromil.frossard@unige.ch
#' @export
#' @importFrom stats model.frame terms model.matrix contrasts<- update
slopelm <- function (formula, data = NULL, np = 5000, method = NULL, test = "fisher",
                     threshold = NULL, aggr_FUN = NULL, slope_FUN = NULL, multcomp = "slope",
                     ...){
  cl = match.call()
  if (is.null(data)) {
    data <- model.frame(formula = formula)
  }
  Terms <- terms(formula, special = "Error", data = data)
  indError <- attr(Terms, "specials")$Error
  dotargs = list(...)

  if (is.null(slope_FUN)) {
    slope_FUN = slope_spline
  }

  if (is.null(dotargs$alpha)) {
    dotargs$alpha = 0.05
  }
  if (is.null(dotargs$p_scale)) {
    dotargs$p_scale = F
  }
  if (is.null(dotargs$H)) {
    switch(test, t = {
      dotargs$H = 2
    }, fisher = {
      dotargs$H = 1
    })
  }
  if (is.null(dotargs$E)) {
    dotargs$E = 0.5
  }
  if (is.null(dotargs$ndh)) {
    dotargs$ndh = 500
  }
  if (is.null(dotargs$return_distribution)) {
    dotargs$return_distribution = F
  }
  if (is.null(dotargs$new_method)) {
    dotargs$new_method = F
  }
  if (is.null(dotargs$coding_sum)) {
    switch(test, t = {
      dotargs$coding_sum = F
    }, fisher = {
      dotargs$coding_sum = T
    })
  }
  if (is.null(indError)) {
    result <- slopelm_fix(formula = formula, data = data, slope_FUN = slope_FUN,
                            method = method, test = test, np = np, P = dotargs$P,
                            rnd_rotation = dotargs$rnd_rotation, aggr_FUN = aggr_FUN,
                            E = dotargs$E, H = dotargs$H, threshold = threshold,
                            return_distribution = dotargs$return_distribution,
                            cl = cl, multcomp = multcomp, alpha = dotargs$alpha,
                            p_scale = dotargs$p_scale, coding_sum = dotargs$coding_sum,
                            ndh = dotargs$ndh, new_method = dotargs$new_method)
  }
  else if (!is.null(indError)) {
    if (test != "fisher") {
      warning("Random effects model only accept fisher statistics. Test statistic is set to fisher.")
      test = "fisher"
    }
    result <- slopelm_rnd(formula = formula, data = data, slope_FUN = slope_FUN,
                            method = method, test = test, np = np, P = dotargs$P,
                            rnd_rotation = dotargs$rnd_rotation, aggr_FUN = aggr_FUN,
                            E = dotargs$E, H = dotargs$H, threshold = threshold,
                            return_distribution = dotargs$return_distribution,
                            cl = cl, multcomp = multcomp, alpha = dotargs$alpha,
                            p_scale = dotargs$p_scale, coding_sum = dotargs$coding_sum,
                            ndh = dotargs$ndh, new_method = dotargs$new_method)
  }
  return(result)
}

