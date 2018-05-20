model <- function() { 
  # Prior 
  p ~ dbeta(1, 1) 
  
  # Likelihood 
  y ~ dbin(p, N) 
}


library(R2OpenBUGS) 
model.file <- file.path(tempdir(),"model.txt") 
write.model(model, model.file)


library(MASS)
tbl<-table(survey$Smoke)
N<-as.numeric(sum(tbl)); N
y<-N-as.numeric(tbl["Never"]);y

data<-list("N","y")
params<-c("p")

inits <- function() { list(p=0.5) }

#out<-bugs(data,inits,params,model.file,n.iter=10000)

#starting checks
#all(out$summary[,"Rhat"] < 1.1) 
#out$mean["p"] 
#out$sd["p"] 
#print(out, digits=5)#


#coda phase
out<-bugs(data,inits,params,model.file,codaPkg=TRUE,n.iter=10000)
out.coda <- read.bugs(out) 
library(coda)
#install.packages('lattice')
library(lattice)
xyplot(out.coda)
densityplot(out.coda)
acfplot(out.coda)
gelman.diag(out.coda) 
gelman.plot(out.coda)


####ggmcmc- APISTEUTO 
#install.packages('ggmcmc')
library(ggmcmc)
S<-ggs(out.coda)
str(S)
ggmcmc(S)
ggmcmc(S,plot=c("density","running","caterpillar"))
ggs_histogram(S)
ggs_density(S)
ggs_traceplot(S)
ggs_running(S)
ggs_compare_partial(S)
ggs_autocorrelation(S)
ggs_crosscorrelation(S)
ggs_Rhat(S)+ xlab("R_hat")
ggs_geweke(S)
ggs_pairs(S,lower=list(continuous='density'))
