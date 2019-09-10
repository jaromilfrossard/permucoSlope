set.seed(42)
library(permuco)

lf = list.files("R/")
for(fi in lf){
  print(fi)
  source(paste0("R/",fi))
}

data("attentionshifting_design")
data("attentionshifting_signal")

y = as.matrix(attentionshifting_signal)
dy = slope_spectral(y)

i = 2
yi =scale(y[i,])
dyi =scale(dy[i,])
ylim = range(c(yi,dyi))
plot(yi,ylim=ylim)
lines(dyi,col="red")


a = slopelm(attentionshifting_signal~visibility*emotion*direction+Error(id/(visibility*emotion*direction)),
        data=attentionshifting_design, slope_FUN = slope_spectral,np =10)
