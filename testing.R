set.seed(42)
library(permuco)
library(spectral)

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



gl = slopelm(attentionshifting_signal~visibility*emotion*direction,
             data=attentionshifting_design,np =10,multcomp = "glue")


plot(gl)

gl = slopelm(attentionshifting_signal~visibility*emotion*direction+Error(id/(visibility*emotion*direction)),
             data=attentionshifting_design,np =10,multcomp = "glue")

gl = slopelm(attentionshifting_signal~visibility*emotion*direction,
             data=attentionshifting_design,np =10,multcomp = "glue")




sl = slopelm(attentionshifting_signal~visibility*emotion*direction+Error(id/(visibility*emotion*direction)),
        data=attentionshifting_design,np =10,multcomp = "slope")


x=gl
effect = "all"; type = "statistic";  multcomp = "glue";
alternative = "two.sided";  enhanced_stat = FALSE;

dotargs=list()



gl
summary(a)

plot(a)


slope_table(a$multiple_comparison,multcomp = "slope")
x= a$multiple_comparison
