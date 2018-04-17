#distribution= arg$distribution;sdistribution=arg$sdistribution;threshold=arg$threshold;aggr_FUN=arg$aggr_FUN;laterality=arg$laterality


compute_clustermass_glue <- function (distribution, sdistribution, threshold, aggr_FUN, laterality = "bilateral"){
  switch(laterality,
         bilateral = {
           distribution <- abs(distribution)
           sdistribution <- abs(sdistribution)
           threshold <- abs(threshold)
           selected <- (distribution > threshold)
           sselected <- (sdistribution > threshold)
           extreme = function(x) max(x, na.rm = T)
         })

  cl = (selected-cbind(0,selected[,-NCOL(selected), drop = F]))==1
  cl = t(apply(cl,1,cumsum))*selected

  scl = (sselected-cbind(0,sselected[,-NCOL(sselected), drop = F]))==1
  scl = t(apply(scl,1,cumsum))*sselected

  sselected = sapply(1:nrow(cl),function(ri){
    scl_ri = scl[ri,]
    sclid_ri = unique(scl_ri);sclid_ri=sclid_ri[sclid_ri!=0]
    cl_ri = cl[ri,]
    if(length(sclid_ri)==0){
      scl_sel=integer(0)
    }else{
    scl_sel=sapply(sclid_ri,function(id){
      ui = unique(cl_ri[scl_ri == id])
      ui=ui[ui!=0]
      length(ui)>1
    })
    scl_sel = which(scl_sel)}
    scl_ri%in%scl_sel})
  sselected=t(sselected)
  scl = (sselected-cbind(0,sselected[,-NCOL(sselected), drop = F]))==1
  scl = t(apply(scl,1,cumsum))*sselected
  selected_join <- selected|sselected


  cl_join = (selected_join-cbind(0,selected_join[,-NCOL(selected_join), drop = F]))==1
  cl_join = t(apply(cl_join,1,cumsum))*selected_join

  cl = selected*cl_join
  scl = sselected*cl_join


  mass_distribution = sapply(1:(dim(selected_join)[1]),function(permi){
    max(sapply(1:max(1,max(cl_join[permi,])),function(i_in_p){
      aggr_FUN(c(distribution[permi,cl[permi,]==i_in_p],
                 sdistribution[permi,scl[permi,]==i_in_p]))}))})

  mass_statistic = sapply(1:max(1,max(cl_join[1,])), function(i) {
    aggr_FUN(c(distribution[1,cl[1,]==i],
               sdistribution[1,scl[1,]==i]))
  })

  pvalue = sapply(mass_statistic, function(mi) permuco:::compute_pvalue(stat = mi,
                                                                        distribution = mass_distribution, laterality = "bilateral"))


  main = cbind(statistic =c(NA, mass_statistic)[cl_join[1, ] + 1],
               pvalue = c(NA, pvalue)[cl_join[1, ] + 1],
               cluster_id = cl_join[1, ])

  statistic = cbind(mean = c(NA, mass_statistic)[cl[1,]+1],slope = c(NA, mass_statistic)[scl[1,]+1])
  pvalue = cbind(mean = c(NA, pvalue)[cl[1,]+1],slope = c(NA, pvalue)[scl[1,]+1])
  cluster_id = cbind(mean = cl[1,], slope = scl[1,])



  main_split = list(statistic = statistic, pvalue = pvalue,
                    cluster_id = cluster_id)

  out = list(main = main, main_split = main_split, distribution = mass_distribution, threshold = threshold)
  return(out)
}
