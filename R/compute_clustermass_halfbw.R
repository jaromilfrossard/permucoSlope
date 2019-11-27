
compute_clustermass_halfbw <- function (distribution, sdistribution, threshold, aggr_FUN, bw, alternative = "two.sided"){
  switch(alternative,
         two.sided = {
           distribution <- abs(distribution)
           sdistribution <- abs(sdistribution)
           threshold <- abs(threshold)
           selected <- (distribution > threshold)
           sselected <- (sdistribution > threshold)
           extreme = function(x) max(x, na.rm = T)
         })

  halfbw = ceiling(bw*ncol(distribution)/2)

  cl = (selected-cbind(0,selected[,-NCOL(selected), drop = F]))==1
  cl = t(apply(cl,1,cumsum))*selected

  scl = (sselected-cbind(0,sselected[,-NCOL(sselected), drop = F]))==1
  scl = t(apply(scl,1,cumsum))*sselected
  ri=1

  sselected = sapply(1:nrow(cl),function(ri){
    cl_ri = cl[ri,]
    scl_ri = scl[ri,]
    sclid_ri = unique(scl_ri);sclid_ri=sclid_ri[sclid_ri!=0]
    if(length(sclid_ri)==0){
      scl_type = NA
    }else{
      scl_type = sapply(sclid_ri,function(id){
        ui = cl_ri[scl_ri == id]
        ui = ui[c(TRUE, !ui[-length(ui)] == ui[-1])]
        if(length(ui)==1){
          if(ui==0){return("alone")}else{
            return("inside")
          }
        }else if(length(ui)==2){
          if(ui[1]==0){return("loco")}else{
            return("tail")}
        }else if(length(ui)==3){
          if(length(unique(ui))==2){return("loco_tail")}else{
            return("glue")
          }
        }else(return("glue"))})

    }
    whichtime = do.call("c",lapply(sclid_ri,function(id){
      wid = which(scl_ri==id)
      wid = switch(scl_type[id],
             glue = {wid},
             loco = {
               if(length(wid)<=halfbw){integer(0L)}else
                 (wid[-c(1:halfbw)])
                 },
             tail = {if(length(wid)<=halfbw){integer(0L)}else
               (wid[c(1:(length(wid)-halfbw))])
               },
             loco_tail = {if(length(wid)<= 2*halfbw){integer(0L)}else
               (wid[c(halfbw:(length(wid)-halfbw))])
             },
             {integer(0L)})
      wid
      }))
    return((1:length(scl_ri))%in%whichtime)

    })
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
                                                                        distribution = mass_distribution, alternative = "two.sided"))


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
