#' @importFrom stats as.formula contr.sum model.response rnorm qt qf lm.fit
#' @importFrom permuco Pmat
slopelm_fix <- function(formula, data, method, type, test, threshold, np, P, rnd_rotation,
            aggr_FUN, E, H, cl, multcomp, alpha, p_scale, coding_sum,
            ndh, return_distribution, new_method,bw){
  if (is.null(method)) {
    method = "freedman_lane"
  }
  if (!new_method) {
    method <- match.arg(method, c("freedman_lane", "kennedy",
                                  "huh_jhun", "manly", "terBraak", "draper_stoneman",
                                  "dekker"))
  }
  if (is.null(aggr_FUN)) {
    switch(test, t = {
      fun_name = "the sum of squares"
      aggr_FUN = function(x) sum(x^2)
    }, fisher = {
      fun_name = "the sum"
      aggr_FUN = function(x) sum(x)
    })
  }
  else {
    fun_name = "a user-defined function"
  }
  switch(method, draper_stoneman = {
    funP = function(...) {
      permuco:::cluster_draper_stoneman(...)
    }
  }, manly = {
    funP = function(...) {
      permuco:::cluster_manly(...)
    }
  }, huh_jhun = {
    funP = function(...) {
      permuco:::cluster_huh_jhun(...)
    }
  }, freedman_lane = {
    funP = function(...) {
      permuco:::cluster_freedman_lane(...)
    }
  }, kennedy = {
    funP = function(...) {
      permuco:::cluster_kennedy(...)
    }
  }, dekker = {
    funP = function(...) {
      permuco:::cluster_dekker(...)
    }
  }, terBraak = {
    funP = function(...) {
      permuco:::cluster_terBraak(...)
    }
  }, {
    warning(paste("the method", method, "is not defined. Choose between freedman_lane, huh_jhun, dekker, terBraak or see help."))
    funP = function(...) {
      eval(parse(text = paste("cluster_", test, "_", method,
                              "(...)", sep = "", collpase = "")))
    }
  })
  if (!(class(formula[[2]]) == "matrix")) {
    formula[[2]] <- call("as.matrix", formula[[2]])
  }
  m <- match(c("formula", "data"), names(cl), 0L)
  mf <- cl[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf$formula = as.formula(paste(c(formula[[2]], "~", formula[[3]]),
                                collapse = ""))
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame(n = 2))
  if (coding_sum) {
    mf <- permuco:::changeContrast(mf, contr = contr.sum)
  }
  mm <- model.matrix(attr(mf, "terms"), data = mf)
  y <- model.response(mf)
  terms <- terms(formula, special = "Error", data = data)
  indError <- attr(terms, "specials")$Error
  switch(test, fisher = {
    col_ref <- attr(mm, "assign")
    colx <- 1:max(attr(mm, "assign"))
    names(colx) <- attr(terms, "term.labels")
  }, t = {
    col_ref <- 1:length(attr(mm, "assign"))
    qr_mm = qr(mm)
    colx <- qr_mm$pivot[1:qr_mm$rank]
    if (method != "huh_jhun") {
      colx <- colx[(attr(mm, "assign")[colx]) != 0]
    }
    names(colx) <- colnames(mm)[colx]
  })
  if (!is.null(P)) {
    check_P <- permuco:::check_P(P = P, method = method, test = test,
                       n = NROW(y), ncol_x2 = as.numeric(table(attr(mm,
                                                                    "assign")[attr(mm, "assign") != 0])), ncol_x = NCOL(mm))
    if (!check_P) {
      np = permuco:::np.matrix(P)
      P = NULL
      warnings("P argument is not valid and will be recomputed")
    }
  }
  if (is.null(P)) {
    switch(method, huh_jhun = {
      switch(test, t = {
        P <- Pmat(np = np, n = NROW(y) - NCOL(mm) + 1, type = type)
      }, fisher = {
        {
          P <- lapply(as.numeric(table(col_ref))[-1],
                      function(cx) {
                        Pmat(np = np, n = NROW(y) - NCOL(mm) +
                               cx, type = type)
                      })
        }
      })
    }, {
      P = Pmat(np = np, n = NROW(y),  type = type)
    })
  }
  if (sum(permuco:::np.matrix(P) <= 1999) > 0) {
    warning("The number of permutations is below 2000, p-values might be unreliable.")
  }
  np <- permuco:::np.matrix(P)
  if (method == "huh_jhun" & is.null(rnd_rotation)) {
    rnd_rotation <- matrix(rnorm(NROW(y)^2), ncol = NROW(y))
  }
  multiple_comparison  <- list()
  length(multiple_comparison) <- length(colx)
  names(multiple_comparison)  <- names(colx)
  if (test == "t") {
    multiple_comparison_less <- multiple_comparison_greater <- list()
    length(multiple_comparison_less) <- length(multiple_comparison_greater) <- length(colx)
    names(multiple_comparison_less) <- names(multiple_comparison_greater) <- names(colx)
  }
  else {
    multiple_comparison_less <- multiple_comparison_greater <- NULL
  }
  if (is.null(threshold)) {
    switch(test, t = {
      df = permuco:::compute_degree_freedom_fix(test = test, mm = mm,
                                      assigni = colx)
      threshold = qt(p = 0.975, df = df)
    }, fisher = {
      df = permuco:::compute_degree_freedom_fix(test = test, mm = mm,
                                      assigni = attr(mm, "assign"))
      threshold = qf(p = 0.95, df1 = df[, 1], df2 = df[,
                                                       2])
    })
  }
  else if (length(threshold) == 1) {
    threshold = rep(threshold, length(colx))
  }
  else if (length(threshold) > 1) {
    threshold = as.numeric(matrix(threshold, nrow = length(colx)))
  }

  slope_FUN <- eval(cl$slope_FUN)
  slope_FUN_name <- paste(cl$slope_FUN)

  if(slope_FUN_name%in%c("slope_lpepa","slope_locpol","slope_spline")){
    cat("Optimizsation of smoothing parameter such that roughess(y) = roughess(slope_FUN(y)).\n")
    optim <- optim_roughness(y,slope_FUN = slope_FUN )
    slope_FUN_par <- optim$par
    sy = slope_FUN(y, slope_FUN_par)
  }else{
    slope_FUN_par <- NULL
    sy = eval(slope_FUN)(y)

  }



  args <- list(y = y, mm = mm, P = P, rnd_rotation = rnd_rotation,
               test = test)
  sargs <- list(y = sy, mm = mm, P = P, rnd_rotation = rnd_rotation,
               test = test)
  for (i in 1:length(colx)) {
    args$colx <- which(col_ref == colx[i])
    sargs$colx <- which(col_ref == colx[i])
    if (method == "huh_jhun" & test == "fisher") {
      args$P = P[[i]]
      sargs$P = P[[i]]
    }
    distribution <- t(funP(args = args))
    sdistribution <- t(funP(args = sargs))

    pvalue <- apply(distribution, 2, function(col) permuco:::compute_pvalue(distribution = col))
    spvalue <- apply(sdistribution, 2, function(col) permuco:::compute_pvalue(distribution = col))

    multiple_comparison[[i]]$uncorrected = list(main_avg = cbind(statistic = distribution[1,], pvalue = pvalue),
                                                main_slope = cbind(statistic = sdistribution[1,], pvalue = spvalue),
                                                test_info = list(test = test, df = df[i,], alternative = "two.sided", method = method, np = np,
                                                                 nDV = ncol(y), fun_name = fun_name, type = attr(args$P,"type"),
                                                                 slope_FUN_name = slope_FUN_name, slope_FUN_par = slope_FUN_par))

    if (return_distribution) {
      multiple_comparison[[i]]$uncorrected$distribution = distribution
      multiple_comparison[[i]]$uncorrected$sdistribution = sdistribution

    }
    if (p_scale) {
      distribution0 <- distribution
      distribution <- permuco:::distribution_to_pscale(distribution0,
                                             test = test, alternative = "two.sided")
      sdistribution0 <- sdistribution
      sdistribution <- permuco:::distribution_to_pscale(sdistribution0,
                                             test = test, alternative = "two.sided")

    }
    # multiple_comparison[[i]] = c(multiple_comparison[[i]],
    #                              permuco:::switch_multcomp(multcomp = c(multcomp),
    #                                              distribution = distribution, threshold = threshold[i],
    #                                              aggr_FUN = aggr_FUN, alternative = "two.sided",
    #                                              E = E, H = H, ndh = ndh, pvalue = pvalue, alpha = alpha))

    # arg <<- list(distribution = distribution, sdistribution = sdistribution,
    #            threshold = threshold[i], aggr_FUN =aggr_FUN,alternative = "two.sided")
    # stop()
    ##switch multcomp

    if("slope"%in%multcomp){
      multiple_comparison[[i]]$slope = compute_clustermass_slope(distribution = distribution, sdistribution = sdistribution,
                                                                 threshold = threshold[i], aggr_FUN =aggr_FUN,alternative = "two.sided")
    }
    if("glue"%in%multcomp){
      multiple_comparison[[i]]$glue = compute_clustermass_glue(distribution = distribution, sdistribution = sdistribution,
                                                               threshold = threshold[i], aggr_FUN =aggr_FUN,alternative = "two.sided")

    }

    if("halfbw"%in%multcomp){
      multiple_comparison[[i]]$halfbw = compute_clustermass_halfbw(distribution = distribution, sdistribution = sdistribution,
                                                               threshold = threshold[i], aggr_FUN =aggr_FUN,alternative = "two.sided",
                                                               bw = bw)

    }

    if("slopebinder"%in%multcomp){
      multiple_comparison[[i]]$slopebinder = compute_clustermass_slopebinder(distribution = distribution, sdistribution = sdistribution,
                                                                   threshold = threshold[i], aggr_FUN =aggr_FUN,alternative = "two.sided")

    }


    if (test == "t") {
      alternative = "greater"
      if (p_scale) {
        distribution <- permuco:::distribution_to_pscale(distribution0,
                                               test = test, alternative = alternative)
      }
      pvalue <- apply(distribution, 2, function(col) permuco:::compute_pvalue(distribution = col,
                                                                    alternative = alternative))
      multiple_comparison_greater[[i]]$uncorrected = list(main = cbind(statistic = distribution[1,
                                                                                              ], pvalue = pvalue))
      multiple_comparison_greater[[i]] = c(multiple_comparison_greater[[i]],
                                         permuco:::switch_multcomp(multcomp = c("clustermass", multcomp[!multcomp %in%
                                                                                                "tfce"]), distribution = distribution, threshold = threshold[i],
                                                         aggr_FUN = aggr_FUN, alternative = alternative,
                                                         E = E, H = H, ndh = ndh, pvalue = pvalue, alpha = alpha))
      alternative = "less"
      if (p_scale) {
        distribution <- permuco:::distribution_to_pscale(distribution0,
                                               test = test, alternative = "less")
        alternative = "greater"
      }
      pvalue <- apply(distribution, 2, function(col) permuco:::compute_pvalue(distribution = col,
                                                                    alternative = alternative))
      multiple_comparison_less[[i]]$uncorrected = list(main = cbind(statistic = distribution[1,
                                                                                             ], pvalue = pvalue))
      multiple_comparison_less[[i]] = c(multiple_comparison_less[[i]],
                                        permuco:::switch_multcomp(multcomp = c("clustermass", multcomp[!multcomp %in%
                                                                                               "tfce"]), distribution = distribution, threshold = threshold[i],
                                                        aggr_FUN = aggr_FUN, alternative = alternative,
                                                        E = E, H = H, ndh = ndh, pvalue = pvalue, alpha = alpha))
    }
  }
  # cluster_table <- permuco:::cluster_table(multiple_comparison)
  # slope_table <- slope_table(multiple_comparison)
  #
  # cluster_table_greater <- cluster_table_less <- NULL
  # if (test == "t") {
  #   cluster_table_greater <- permuco:::cluster_table(multiple_comparison_greater)
  #   cluster_table_less <- permuco:::cluster_table(multiple_comparison_less)
  # }
  mod_lm = lm.fit(x = mm, y = y)
  # multcomp = unique(c("uncorrected", "clustermass", multcomp))[unique(c("uncorrected",
  #                                                                       "clustermass", multcomp)) %in% c("uncorrected", "clustermass",
  #                                                                                                        "tfce", "troendle", "bonferroni", "holm", "benjaminin_hochberg")]
  out = list()
  out$y = y
  out$sy = sy
  out$coefficients = mod_lm$coefficients
  out$residuals = mod_lm$residuals
  out$effects = mod_lm$effects
  out$rank = mod_lm$rank
  out$fitted.values = mod_lm$fitted.values
  out$assign = mod_lm$assign
  out$qr = mod_lm$qr
  out$df.residual = mod_lm$df.residual
  out$xlevels = mod_lm$xlevels
  out$terms = mod_lm$terms
  out$model = mod_lm$model
  out$model.matrix = mm
  out$test = test
  out$threshold = threshold
  out$bw = bw
  out$P = P
  out$np = np
  out$rnd_rotation = rnd_rotation
  out$multcomp = multcomp
  out$multiple_comparison = multiple_comparison
  out$multiple_comparison_greater = multiple_comparison_greater
  out$multiple_comparison_less = multiple_comparison_less
  # out$cluster_table = cluster_table
  # out$slope_table = slope_table
  # out$cluster_table_greater = cluster_table_greater
  # out$cluster_table_less = cluster_table_less

  out$slope_FUN_name = slope_FUN_name
  out$slope_FUN = slope_FUN
  out$slope_FUN_par = slope_FUN_par

  out$alpha = alpha
  out$method = method
  out$fun_name = fun_name
  class(out) <- c("slopelm","clusterlm")
  return(out)
}