set.seed(42)
library(permuco)

lf = list.files("R/")
for(fi in lf){
  print(fi)
  source(paste0("R/",fi))
}

data("attentionshifting_design")
data("attentionshifting_signal")

a = slopelm(attentionshifting_signal~visibility*emotion*direction+Error(id/(visibility*emotion*direction)),
        data=attentionshifting_design, slope_FUN = slope_diff,np =10)
