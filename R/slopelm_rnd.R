slopelm_rnd <- function (formula, data, method, test, coding_sum, threshold,
          np, P, rnd_rotation, aggr_FUN, E, H, cl, multcomp, alpha, slope_FUN,
          p_scale, return_distribution, ndh, new_method)
{
  if (is.null(method)) {
    method = "Rd_kheradPajouh_renaud"
  }
  if (!new_method) {
    method = match.arg(method, c("Rd_kheradPajouh_renaud",
                                 "Rde_kheradPajouh_renaud"))
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
  switch(paste(method, sep = "_"), Rd_kheradPajouh_renaud = {
    funP = function(...) {
      permuco:::cluster_Rd_kheradPajouh_renaud_rnd(...)
    }
  }, Rde_kheradPajouh_renaud = {
    funP = function(...) {
      permuco:::cluster_Rde_kheradPajouh_renaud_rnd(...)
    }
  }, {
    warning(paste("the method", method, "is not defined. Choose between Rd_kheradPajouh_renaud or Rde_kheradPajouh_renaud."))
    funP = function(...) {
      eval(parse(text = paste("cluster_fisher_", method,
                              "_rnd(...)", sep = "", collpase = "")))
    }
  })
  if (!(class(formula[[2]]) == "matrix")) {
    formula[[2]] <- call("as.matrix", formula[[2]])
  }
  terms <- terms(formula, special = "Error", data = data)
  ind_error <- attr(terms, "specials")$Error
  error_term <- attr(terms, "variables")[[1 + ind_error]]
  formula_f <- update(formula, paste(". ~ .-", deparse(error_term,
                                                       width.cutoff = 500L, backtick = TRUE)))
  e_term <- deparse(error_term[[2L]], width.cutoff = 500L,
                    backtick = TRUE)
  formula_allfixed <- as.formula(paste(c(formula_f[[2]], "~",
                                         formula_f[[3]], "+", e_term), collapse = ""))
  formula_allfixed_design <- as.formula(paste(c("~", formula_f[[3]],
                                                "+", e_term), collapse = ""))
  formula_within <- formula(paste("~", e_term, collapse = ""))
  formula_within <- formula(paste("~", deparse(error_term[[2]][[3]]),
                                  collapse = ""))
  formula_id <- formula(paste("~", deparse(error_term[[2]][[2]]),
                              collapse = ""))
  mf <- model.frame(formula = formula_allfixed, data = data)
  mf_design <- model.frame(formula = formula_allfixed_design,
                           data = data)
  if (coding_sum) {
    mf_design <- permuco:::changeContrast(mf_design, contr = contr.sum)
  }
  mf_f <- model.frame(formula = formula_f, data = mf_design)
  mf_id <- model.frame(formula = formula_id, data = as.data.frame(lapply(mf_design,
                                                                         function(col) {
                                                                           col = as.factor(col)
                                                                           contrasts(col) = contr.sum
                                                                           col
                                                                         })))
  mf <- eval(mf, parent.frame(n = 2))
  y <- model.response(mf)
  link = permuco:::link(formula_f = formula_f, formula_within = formula_within)
  mm_f <- model.matrix(attr(mf_f, "terms"), data = mf_f)
  mm_id <- model.matrix(attr(mf_id, "terms"), data = mf_id)[,
                                                            -1, drop = F]
  name <- colnames(mm_f)
  permuco:::checkBalancedData(fixed_formula = formula_f, data = cbind(mf))
  if (is.null(P)) {
    P = Pmat(np = np, n = NROW(y))
  }
  if (sum(permuco:::np.matrix(P) <= 1999) > 0){
    warning("The number of permutations is below 2000, p-values might be unreliable.")
  }
  np <- permuco:::np.matrix(P)

  sy = slope_FUN(y)

  args <- list(y = y, mm = mm_f, mm_id = mm_id, link = link,
               P = P)
  sargs <- list(y = sy, mm = mm_f, mm_id = mm_id, link = link,
               P = P)

  multiple_comparison <- list()
  length(multiple_comparison) <- max(attr(mm_f, "assign"))
  names(multiple_comparison) <- attr(attr(mf_f, "terms"), "term.labels")
  if (is.null(threshold)) {
    df = permuco:::compute_degree_freedom_rnd(test = test, mm = mm_f,
                                    assigni = attr(mm_f, "assign"), mm_id = mm_id, link = link)
    threshold = qf(p = 0.95, df1 = df[, 1], df2 = df[, 2])
  }
  else if (length(threshold) == 1) {
    threshold = rep(threshold, length(multiple_comparison))
  }
  else if (length(threshold) > 1) {
    threshold = as.numeric(matrix(threshold, nrow = length(multiple_comparison)))
  }
  for (i in 1:max(attr(mm_f, "assign"))) {
    args$i = i
    sargs$i = i

    distribution = funP(args = args)
    sdistribution = funP(args = sargs)


    pvalue <- apply(distribution, 2, function(col) permuco:::compute_pvalue(distribution = col))
    spvalue <- apply(sdistribution, 2, function(col) permuco:::compute_pvalue(distribution = col))

    multiple_comparison[[i]]$uncorrected = list(main = cbind(statistic = distribution[1,], pvalue = pvalue))
    multiple_comparison[[i]]$uncorrected_slope = list(main = cbind(statistic = sdistribution[1,], pvalue = spvalue))

    if (return_distribution) {
      multiple_comparison[[i]]$uncorrected$distribution = distribution
    }
    if (p_scale) {
      distribution0 <- distribution
      distribution <- permuco:::distribution_to_pscale(distribution0,
                                             test = test, lateraltiy = "bilateral")
    }
    multiple_comparison[[i]] = c(multiple_comparison[[i]],
                                 permuco:::switch_multcomp(multcomp = c("clustermass", multcomp),
                                                 distribution = distribution, threshold = threshold[i],
                                                 aggr_FUN = aggr_FUN, laterality = "bilateral",
                                                 E = E, H = H, ndh = ndh, pvalue = pvalue, alpha = alpha))
    multiple_comparison[[i]]$slope_clustermass = compute_clustermass_slope(distribution = distribution, sdistribution = sdistribution,
                                                                           threshold = threshold[i], aggr_FUN =aggr_FUN,laterality = "bilateral")

  }
  cluster_table <- permuco:::cluster_table(multiple_comparison[order(link[3,
                                                                ], link[1, ])])
  slope_table <- slope_table(multiple_comparison[order(link[3,
                                                              ], link[1, ])])
  out = list()
  out$y = y
  out$sy = sy
  out$model.matrix = mm_f
  out$model.matrix_id = mm_id = mm_id
  out$link = link
  out$P = P
  out$np = np
  out$cluster_table = cluster_table
  out$slope_table = slope_table
  out$multiple_comparison = multiple_comparison
  out$data = mf
  out$method = method
  out$alpha = alpha
  out$multcomp = multcomp
  out$threshold = threshold
  out$test = test
  out$fun_name = fun_name
  class(out) <- c("slopelm","clusterlm")
  return(out)
}