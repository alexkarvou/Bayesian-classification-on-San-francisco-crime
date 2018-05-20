library(R2OpenBUGS)
data(schools)

#preparation
J<-nrow(schools)
y<-schools$estimate
sigma.y<-schools$sd
data<-list("J","y","sigma.y")

#inits
inits<-function(){
  list(theta = rnorm(J, 0, 100), mu.theta = rnorm(1, 0, 100),
       sigma.theta = runif(1, 0, 100))
}

schools.sim<-bugs(data,inits,model.file="/home/alex/R/schools.txt",
                  parameters = c("theta", "mu.theta", "sigma.theta"),
                  n.chains = 3, n.iter = 1000)
#kanto arxeio mcmc
traceplot(mcmc(schools.sim))

schools.sim<-read.openbugs(schools.sim)

autocorr.plot(schools.sim)