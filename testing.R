set.seed(42)
library(permuco)
library(spectral)
library(locpol)
library(tidyr)
library(dplyr)
library(lpridge)
library(ggplot2)

lf = list.files("R/")
for(fi in lf){
  print(fi)
  source(paste0("R/",fi))
}

data("attentionshifting_design")
data("attentionshifting_signal")

y = as.matrix(attentionshifting_signal)
dy = slope_locpol(y)
design = attentionshifting_design

rough_y = mean(roughness(y))

t0=proc.time()
optim(par = 0.2, fn = function(bwi){abs(mean(roughness(slope_lpepa(y,bw =bwi)))-rough_y)},
      method = c("L-BFGS-B"),
      lower = 0, upper = 1)
t1=proc.time()
t1-t0

t0=proc.time()
optim(par = 0.01, fn = function(bwi){abs(mean(roughness(slope_lpepa(y,bw =bwi)))-rough_y)},
      method = c("Nelder-Mead"),
      lower = 0, upper = 1)
t1=proc.time()
t1-t0

match_roughness(y,slope_lpepa,par0 = 0.1)

ts.plot(t(slope_lpepa(y,bw = 0.04816434)))

ts.plot(t(y))

plot(slope_lpepa(y))


plot(out[1,],dy[1,]/819)
abline(0,1)

tidydata = cbind(design,y)%>%
  gather(time,ampl,10:828)%>%
  mutate(time=as.numeric(time))%>%
  unite(id_obs, id, visibility,emotion,direction,remove = F)

tidydata%>%
  filter(id%in%c("S09","S10"))%>%
  ggplot(aes(x=time,y=ampl,color=id,group=id_obs))+
  geom_line()


design$roughness = roughness(y)
design$ii = 1:nrow(design)
design%>%
  filter(id%in%c("S07","S09"))

design%>%
  ggplot(aes(y=roughness,color=id,x=ii))+
  geom_point()




mr = match_roughness(y, slope_FUN = slope_locpol, par0 = 0.02, opt_FUN = roughness, tol = 1e-05, step0 = 0.02,max_it = 200,cold = 0.89)

rs = roughness(slope_locpol(y,bw = 0.04826193))

plot(cbind(rs,roughness(y)))

r1 = y%>%
  t%>%
  apply(2,diff)%>%
  apply(2,diff)%>%
  apply(2,sd)

y%>%
  apply(1,mean)



plot(roughness(y),r1,col=)


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
