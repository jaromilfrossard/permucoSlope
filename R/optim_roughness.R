#' Compute the optimal smoothing parameter of slope_FUN
#'
#' @description Find smoothing paramters that matches rougness of original signal and of their slope
#' @param y matrix of the signals.
#' @param slope_FUN function that compute the slope
#' @param par initial value of the smoothing paramter. Default is 0.5.
#' @param avg_FUN function to aggregate the rougness of each signal. Defaults is the methd.
#' @param method optimizer from \code{optim}. Defaults is "Nelder-Mead".
#' @importFrom stats optim
#' @export
optim_roughness = function(y, slope_FUN, par = 0.5, avg_FUN = mean, method = c("Nelder-Mead")){
  rough_y = avg_FUN(roughness(y))

  fn = function(x)abs(avg_FUN (roughness(slope_FUN(y, x)))-rough_y)

  optim(par = 0.01, fn = fn,
        method = method)
}
