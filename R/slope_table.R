slope_table = function(x,...){
  dotargs = list(...)
  ct = lapply(1:length(x), function(j) {
    effect = x[[j]]
    unique_cluster = unique(effect$slope_clustermass$main[, 3])
    unique_cluster = unique_cluster[unique_cluster != 0]
    if (length(unique_cluster) == 0) {
      attr(table, "effect_name") = names(x)[j]
      attr(table, "threshold") = effect$slope_clustermass$threshold
      table = paste(names(x)[j], ", no cluster above a threshold of : ",
                    round(effect$slope_clustermass$threshold, 5), sep = "")
      return(table)
    }
    tab = t(sapply(unique_cluster, function(i) {
      cl_select = effect$slope_clustermass$main[, 3] == i
      timepoint = c(1:length(cl_select))[cl_select]
      c(timepoint[1], timepoint[length(timepoint)], effect$slope_clustermass$main[timepoint[1],
                                                                            1], effect$slope_clustermass$main[timepoint[1], 2])
    }))
    tab = data.frame(tab)
    colnames(tab) = c("start", "end", "cluster mass", "P(>mass)")
    rownames(tab) = unique_cluster
    attr(tab, "threshold") = effect$slope_clustermass$threshold
    attr(tab, "effect_name") = names(x)[j]
    class(tab) = append("cluster_table", class(tab))
    tab
  })
  class(ct) = append("listof_cluster_table", class(ct))
  names(ct) = names(x)
  ct

}