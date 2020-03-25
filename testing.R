set.seed(42)
rm(list = ls())
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


###real data eda channel 48
### load eda
#dir = "../../article/eeg_instruction/data/"

data("attentionshifting_design")
data("attentionshifting_signal")
my_FUN <- slope_locpol

slm_lpepa = slopelm(attentionshifting_signal~visibility*emotion*direction,
             data=attentionshifting_design,np =10,slope_FUN = slope_lpepa)


slm_lpepa = slopelm(attentionshifting_signal~visibility*emotion*direction+Error(id/(visibility*emotion*direction)),
                    data=attentionshifting_design,np =10,slope_FUN = slope_spline)

slm_lpepa$slope_FUN()

si<- summary(slm_lpepa)
class(si)
summary.slopelm

slm_spline  = slopelm(attentionshifting_signal~visibility*emotion*direction,
             data=attentionshifting_design,np =10,slope_FUN = slope_spline)


ts.plot(t(slm_lpepa$sy))
ts.plot(t(slm_spline$sy))

callslop_FUNi(attentionshifting_signal)
call(slop_FUNi)

fo[!names(fo)%in%c(names(cl)[-1],"...")]
cli <- as.call(c(as.list(cl),fo[!names(fo)%in%c(names(cl)[-1],"...")]))
cl$slope_FUN

y <- attentionshifting_signal
slope_FUN = slope_locpol
t0 <- proc.time()
opt <- optim_roughness(y,slope_FUN = slope_FUN )
t1<- proc.time()
t0-t1
mean(roughness(slope_FUN(y,opt$par)))


slope_FUN = slope_lpepa
t0 <- proc.time()
opt2 <- optim_roughness(y,slope_FUN = slope_FUN )
t1 <- proc.time()
t0-t1
mean(roughness(y))
mean(roughness(slope_FUN(y,opt2$par)))


sl = slopelm(attentionshifting_signal~visibility*emotion*direction,
             data=attentionshifting_design,np =10)
sl

cl = clusterlm(attentionshifting_signal~visibility*emotion*direction,
               data=attentionshifting_design,np =10)


#load(paste0(dir,"signal_design_anova_3f.RData"))
finstr = signali~instruction*emotion*sex_stimuli +Error(participant/(instruction*emotion*sex_stimuli))
i = 48
signali = signal[,,i]


optim = optim_roughness(signali,slope_FUN = slope_lpepa)
mybw =optim$par
myslope_FUN = function(x){slope_lpepa(x,bw = mybw)}


scm_lpepa = slopelm(finstr,data=design,np=5,slope_FUN = myslope_FUN,multcomp = c("slope","glue","slopebinder","halfbw"))

distribution = scm_lpepa$distribution
sdistribution = scm_lpepa$sdistribution
threshold = scm_lpepa$threshold
aggr_FUN = scm_lpepa$aggr_FUN
alternative = scm_lpepa$alternative
bw = scm_lpepa$bw




data("attentionshifting_design")
data("attentionshifting_signal")

y = as.matrix(attentionshifting_signal)
design = attentionshifting_design

optimal = optim_roughness(y,slope_lpepa,roughness_FUN = roughness)
bw = optimal$par
myslope_FUN = function(x)(slope_lpepa(x,bw = bw))

cl = clusterlm(attentionshifting_signal~visibility*emotion*direction, slope_FUN = myslope_FUN,
             data=attentionshifting_design,np =10)
cl$multiple_comparison$visibility$clustermass$threshold

gl = slopelm(attentionshifting_signal~visibility*emotion*direction, slope_FUN = myslope_FUN,
             data=attentionshifting_design,np =2000,multcomp = c("slope","glue","halfbw","slopebinder"),
             bw = bw, return_distribution=T)


diffs= (sapply(gl$multiple_comparison,function(eff){
  eff$slopebinder$distribution-eff$glue$distribution
}))
d_sb = gl$multiple_comparison$visibility$slopebinder$distribution
d_gl = gl$multiple_comparison$visibility$glue$distribution

cbind(d_sb,d_gl)


summary(gl,multcomp = "slope")$visibility

summary(gl,multcomp = "slopebinder")$visibility
summary(gl,multcomp = "glue")$visibility

plot(gl,multcomp = "slopebinder")

plot(gl,multcomp = "slope")


main = gl$multiple_comparison$visibility$slopebinder$main
main = cbind(main,gl$multiple_comparison$visibility$uncorrected$main_avg[,1])

main[-c(1:300),]

distribution = gl$multiple_comparison$visibility$uncorrected$distribution
sdistribution = gl$multiple_comparison$visibility$uncorrected$sdistribution
threshold = gl$multiple_comparison$visibility$slope$threshold
aggr_FUN = sum
bw = bw
alternative = "two.sided"

compute_clustermass_halfbw()
gl$multiple_comparison$visibility$halfbw$distribution
gl$multiple_comparison$visibility$glue$distribution

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
