#' Compute the slope of multiple signal using splines
#'
#' @description compute the slope of multiple signal using smothing spline
#' @param y a matrix with multiple signals in rows and time in column
#' @param spar a scalar representing the smoothing parameter. The Default value is \code{.5}.
#' @export
#' @importFrom stats predict smooth.spline
slope_spline <- function(y, spar = .5){t(apply(y, 1, function(yi)predict(smooth.spline(yi, spar = spar), deriv = 1)$y))}


#' Compute the slope of multiple signal using local polynormal
#'
#' @description compute the slope of multiple signal using local polynomial from the \code{locpol} package.
#' @param y a matrix with multiple signals in rows and time in column
#' @param bw a scalar representing the bandwidth. The Default value is \code{.01}.
#' @param kernel a function representing for the kernel. By default the Epanechnikov Kernel.
#' @param deg a integer representing the degree of the polynomial use to fit the signal. Default value is \code{1}.
#' @importFrom locpol locpol fitted.locpol EpaK
#' @export
slope_locpol = function(y, bw = 0.01, kernel = EpaK , deg = 1){
  xi = c(1:NCOL(y))/NCOL(y)
  out= apply(y, 1, function(yi){
    df = data.frame(xi = xi, y = yi)
    out = locpol(yi~xi, data = df, bw = bw, kernel = kernel, deg = deg, xeval = xi)
    fitted.locpol(out, deg = 1)})
  t(out)
}


#' Compute the slope of multiple signal using and adjust the smoothing paramter
#'
#' @description compute the slope of multiple signal and adjust the smoothing parameter to match the roughness from the original signal to the slope.
#' @param y a matrix with multiple signals in rows and time in column
#' @param slope_FUN the function to compute the slope. The second argument of this function is the parameter to adjust.
#' @export
slope_roughtness = function(y, slope_FUN = NULL){
  param = match_roughness(y = y, slope_FUN = slope_FUN)[1,1]
  return(slope_FUN(y, param))
}

