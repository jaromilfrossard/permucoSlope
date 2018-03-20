
match_roughness <- function(y, slope_FUN, par0 = 0.5, opt_FUN = roughness, tol = 1e-05, step0 = 0.02,max_it = 200,cold = 0.89){
  optim = mean(opt_FUN(y))
  pari = stepi = optimi = numeric(max_it+1)
  pari[1] = par0
  stepi[1] = step0

  for(i in 1:max_it){
    optimi[i] = mean(opt_FUN(slope_FUN(y,pari[i])))
    if (abs(abs(optim)-abs(optimi[i])) < tol){
      return(cbind(parameter = pari[i],step = i, optim = optim, optimi = optimi[i], conv = 1))
    }else if(optimi[i] > optim){
      pari[i+1] = pari[i]+stepi[i]
    } else if (optimi[i] < optim){
      pari[i+1] = pari[i]-stepi[i]
    }
    stepi[i+1] = stepi[i]*cold
  }
  best = which.min(abs(abs(optim)-abs(optimi)))
  return(cbind(parameter = pari[best],step = best, optim = optim, optimi = optimi[best],conv=0))
}