#' Compute the optimal smoothing parameter of slope_FUN
#'
#' @description Find smoothing paramters that matches rougness of original signal and of their slope
#' @param y matrix of the signals.
#' @param slope_FUN function that compute the slope
#' @param par initial value of the smoothing paramter. Default is 0.5.
#' @param avg_FUN function to aggregate the rougness of each signal. Defaults is the method.
#' @param roughness_FUN function of roughness to optimize
#' @param method optimizer from \code{optim}. Defaults is "Brent".
#' @param lower a scalar indicating the lower bound of \code{par}.
#' @param upper a scalar indicating the upper bound of \code{par}.
#' @details Check \code{optim} function.
#' @importFrom stats optim
#' @export
optim_roughness = function(y, slope_FUN, par = 0.5, avg_FUN = mean,roughness_FUN = roughness, method = c("Brent"),lower = 0,upper = 1){
  rough_y = avg_FUN(roughness_FUN(y))

  fn = function(x)abs(avg_FUN (roughness_FUN(slope_FUN(y, x)))-rough_y)

  optim(par = 0.01, fn = fn,
        method = method,lower = lower, upper = upper)
}
