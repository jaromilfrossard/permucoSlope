summary.slopelm = function(object, alternative = "two.sided", multcomp = NULL, table_type = NULL, ...){
  dotargs = list(...)
  if(is.null(table_type)){
    if(ncol(object$y)>15){
      table_type = "cluster"
    }else{
      table_type = "full"
    }
  }

  if(is.null(multcomp)){multcomp <- object$multcomp[1]}

  switch(table_type,
         "cluster" = {getTable = function(x, multcomp,... ){slope_table(x, multcomp = multcomp, ... = ...)}},
         "full" = {getTable = function(x, multcomp,... ){permuco:::full_table(x, multcomp = multcomp, ... = ...)}}
  )

  switch(alternative,
         "two.sided" = {
           return(getTable(object$multiple_comparison, multcomp = multcomp, ... = ...))},
         "greater" = {
           return(getTable(object$multiple_comparison_greater, multcomp = multcomp, ... = ...))},
         "less" = {
           return(getTable(object$multiple_comparison_less, multcomp = multcomp, ... = ...))})

}