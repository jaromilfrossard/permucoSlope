#' Plot method for slopelm object
#'
#' @description shows the statistics for the test on the mean and on the slope
#' @param x a slopelm object
#' @param effect a character string indicating the object to plot. Default is \code{"all"}.
#' @param type a character string indicating if the statistic of the coefficient should be plotted. Default is \code{"statistic"}.
#' @param multcomp a character string indicating which multiple comparison procedure should be plotted. Default is \code{"slope_clustermass"}.
#' @param laterality a character string indicating the laterality of the test in the case of a \code{t} test. Default is \code{"bilateral"}.
#' @param enhanced_stat a logical indicating if the enhanced statistic should be plotted instead of the univarite one. Default is \code{FALSE}.
#' @param ... other argument pass to \code{plot}
#'@export
#'@importFrom graphics abline lines par plot points
plot.slopelm <- function (x, effect = "all", type = "statistic", multcomp = "slope_clustermass",
                          laterality = "bilateral", enhanced_stat = FALSE, ...){

  if( (multcomp == "slope_clustermass")&(laterality != "bilateral")){
    warning("laterality argument switch to bilateral, change multcomp argument. Only the bilateral test is available for slope_clustermass")
    laterality = "bilateral"
  }

  if(multcomp != "slope_clustermass"){
    permuco:::plot.clusterlm(x = x, effect = effect, type = type, multcomp = multcomp,
                             laterality = laterality, enhanced_stat = enhanced_stat, ...)
  }else{

    if ("all" %in% effect) {
      effect = names(x$multiple_comparison)
    }else if ((names(x$multiple_comparison) %in% effect) == 0) {
      warning(" the specified effects do not exist. Plot 'all' effects.")
      effect = names(x$multiple_comparison)
    }
    effect_sel <- names(x$multiple_comparison) %in% effect
    multiple_comparison = x$multiple_comparison[effect_sel]

    pvalue = t(sapply(multiple_comparison, function(m) {
      m[[multcomp]]$main_split$pvalue[, 1]
    }))

    spvalue = t(sapply(multiple_comparison, function(m) {
      m[[multcomp]]$main_split$pvalue[, 2]
    }))


    statistic = t(sapply(multiple_comparison, function(m) {
      m[["uncorrected"]]$main[, 1]
    }))

    sstatistic = t(sapply(multiple_comparison, function(m) {
      m[["uncorrected_slope"]]$main[, 1]
    }))

    if (enhanced_stat) {
      statistic = t(sapply(multiple_comparison, function(m) {
        m[[multcomp]]$main_split$statistic[, 1]
      }))
      sstatistic = t(sapply(multiple_comparison, function(m) {
        m[[multcomp]]$main_split$statistic[, 2]
      }))
    }
    switch(type, coef = {
      data <- x$coef[effect_sel, ]
      title <- "coefficients"
      hl <- NULL
    }, statistic = {
      data <- statistic
      sdata <- sstatistic
      title <- paste(x$test, " statistic", sep = "", collapse = "")
      switch(x$test,
             fisher = {hl <- x$threshold},
             t = { hl <- c(-x$threshold, x$threshold)})})


    title = paste(title, " : ", multcomp, " correction", sep = "",
                  collapse = "")

    p = sum(NROW(data))
    rnames = row.names(data)
    par0 <- list(mfcol = par()$mfcol, mar = par()$mar, oma = par()$oma)
    par(mfcol = c(p, 1), mar = c(0, 4, 0, 0), oma = c(4, 0, 4,
                                                      1), ... = ...)
    for (i in 1:p) {
      if (i == p) {
        xaxt = NULL
      }
      else {
        xaxt = "n"
      }
      ylim = range(data[i, ])
      if (type == "statistic"){
        ylim = range(c(0,ylim,sdata[i,]))
      }
      plot(data[i, ], type = "l", xaxt = xaxt, xlab = "", ylab = rnames[i],ylim =ylim)
      if (type == "statistic") {
        lines(y = sdata[i, ], x = 1:length(sdata[i, ]),lty = 5)
        xi = which(pvalue[i, ] < x$alpha)
        y = data[i, xi]
        col = "red"
        points(x = xi, y = y, pch = par()$pch, col = col)

        xi = which(spvalue[i, ] < x$alpha)
        y = sdata[i, xi]
        col = "blue"
        points(x = xi, y = y, pch = par()$pch, col = col)


        if (x$test == "fisher") {
          abline(h = hl[i], lty = 3)
        }else if(x$test == "t"){
          abline(h = hl[i], lty = 3)
          abline(h = -hl[i], lty = 3)

        }
      }
    }
    title(title, outer = T, cex = 2)
    par(mfcol = par0$mfcol, mar = par0$mar, oma = par0$oma)}
}