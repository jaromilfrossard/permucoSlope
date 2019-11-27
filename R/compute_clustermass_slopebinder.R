# distribution = gl$multiple_comparison$visibility$uncorrected$distribution
# sdistribution = gl$multiple_comparison$visibility$uncorrected$sdistribution
# threshold = gl$multiple_comparison$visibility$slope$threshold
# aggr_FUN = sum
# bw = bw
# alternative = "two.sided"
#


compute_clustermass_slopebinder <- function (distribution, sdistribution, threshold, aggr_FUN, alternative = "two.sided"){
  switch(alternative,
         two.sided = {
           distribution <- abs(distribution)
           sdistribution <- abs(sdistribution)
           threshold <- abs(threshold)
           selected <- (distribution > threshold)
           sselected <- (sdistribution > threshold)
           selected_join <- selected|sselected
           extreme = function(x) max(x, na.rm = T)
         })

  cl_join = (selected_join-cbind(0,selected_join[,-NCOL(selected_join), drop = F]))==1
  cl_join = t(apply(cl_join,1,cumsum))*selected_join

  cl = selected*cl_join
  scl = sselected*cl_join


  mass_distribution = sapply(1:(dim(selected_join)[1]),function(permi){
    max(sapply(1:max(1,max(cl_join[permi,])),function(i_in_p){
      aggr_FUN(c(distribution[permi,cl[permi,]==i_in_p]))}))})

  mass_statistic = sapply(1:max(1,max(cl_join[1,])), function(i) {
    aggr_FUN(c(distribution[1,cl[1,]==i]))
  })

  pvalue = sapply(mass_statistic, function(mi) permuco:::compute_pvalue(stat = mi,
                                                                        distribution = mass_distribution, alternative = "two.sided"))


  main = cbind(statistic =c(NA, mass_statistic)[cl[1, ] + 1],
               pvalue = c(NA, pvalue)[cl[1, ] + 1],
               cluster_id = cl[1, ])

  statistic = cbind(mean = c(NA, mass_statistic)[cl[1,]+1],slope = rep(NA,dim(distribution)[2]))

  pvalue = cbind(mean = c(NA, pvalue)[cl[1,]+1],slope = rep(NA,dim(distribution)[2]))
  cluster_id = cbind(mean = cl[1,], slope = rep(0,dim(distribution)[2]))



  main_split = list(statistic = statistic, pvalue = pvalue,
                    cluster_id = cluster_id)

  out = list(main = main, main_split = main_split, distribution = mass_distribution, threshold = threshold)
  return(out)
}




# compute_clustermass_slopebinder2 <- function (distribution, sdistribution, threshold, aggr_FUN, alternative = "two.sided"){
#   switch(alternative,
#          two.sided = {
#            distribution <- abs(distribution)
#            sdistribution <- abs(sdistribution)
#            threshold <- abs(threshold)
#            selected <- (distribution > threshold)
#            sselected <- (sdistribution > threshold)
#            selected_join <- selected|sselected
#            extreme = function(x) max(x, na.rm = T)
#          })
#
#   cl_join = (selected_join-cbind(0,selected_join[,-NCOL(selected_join), drop = F]))==1
#   cl_join = t(apply(cl_join,1,cumsum))*selected_join
#
#   cl = selected*cl_join
#   #scl = sselected*cl_join
#   cl_join = t(sapply(1:(dim(selected_join)[1]),function(permi){
#     cli = cl[permi,]
#     ui = unique(cli)
#     ui = ui[ui!=0]
#     if(length(ui)==0){
#       cli = rep(0,ncol(cl))}else{
#      for(ii in 1:length(ui)){
#        cli[cli==(ui[[ii]])]=ii
#      }}
#     cli}
#     ))
#
#   mass_distribution = sapply(1:(dim(selected_join)[1]),function(permi){
#     max(sapply(1:max(1,max(cl_join[permi,])),function(i_in_p){
#       aggr_FUN(c(distribution[permi,cl_join[permi,]==i_in_p]))}))})
#
#   mass_statistic = sapply(1:max(1,max(cl_join[1,])), function(i) {
#     aggr_FUN(c(distribution[1,cl[1,]==i]))
#   })
#
#   pvalue = sapply(mass_statistic, function(mi) permuco:::compute_pvalue(stat = mi,
#                                                                         distribution = mass_distribution, alternative = "two.sided"))
#
#
#   main = cbind(statistic =c(NA, mass_statistic)[cl_join[1, ] + 1],
#                pvalue = c(NA, pvalue)[cl_join[1, ] + 1],
#                cluster_id = cl_join[1, ])
#
#   statistic = cbind(mean = c(NA, mass_statistic)[cl[1,]+1],slope = c(NA, mass_statistic)[scl[1,]+1])
#   pvalue = cbind(mean = c(NA, pvalue)[cl[1,]+1],slope = c(NA, pvalue)[scl[1,]+1])
#   cluster_id = cbind(mean = cl[1,], slope = scl[1,])
#
#
#
#   main_split = list(statistic = statistic, pvalue = pvalue,
#                     cluster_id = cluster_id)
#
#   out = list(main = main, main_split = main_split, distribution = mass_distribution, threshold = threshold)
#   return(out)
# }
