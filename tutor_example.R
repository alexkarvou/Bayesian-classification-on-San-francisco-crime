#descriptive
library(ggplot2)
grade<-c(84,58,100,51,28,89,97,50,76,83,45,42,83,64,47,83,81,83,34,61,77,69,94,80,55,79)
tutor<-c(rep(1,13),rep(2,13))
my_data<-cbind(grade,tutor)
dataz<-as.data.frame(my_data)
dataz$tutor<-as.factor(dataz$tutor)
tutor1<-grade[1:13]
tutor2<-grade[14:26]
#density plot
a<-ggplot(dataz,aes(x=grade,fill=tutor))+
  geom_density(alpha=0.2)+
  geom_vline(xintercept=mean(tutor1),linetype='longdash',colour='#FF3366')+
  geom_vline(xintercept=mean(tutor2),linetype='longdash',colour='#3366FF');a
#coord_polar
b<-ggplot(dataz,aes(x=grade,fill=tutor))+
  geom_density(alpha=0.2)+
  geom_vline(xintercept=mean(tutor1),linetype='longdash',colour='#FF3366')+
  geom_vline(xintercept=mean(tutor2),linetype='longdash',colour='#3366FF')+
  coord_polar();b
#multiplot(a,b,cols=2,main='Γραφήματα')
#h enallaktika
#install.packages('grid')
#install.packages('gridExtra')
#library(grid);library(gridExtra)
#grid.arrange(a, b, ncol = 2, main = "Main title")
#louse

########################

model<-function(){
  #likelihoods
  for( i in 1:n){
    grade1[i] ~ dnorm(mu1,tau)
    grade2[i] ~ dnorm(mu2,tau)
  }
  #priors
  mu1~dnorm( 50, 1.0E-04)
  mu2~dnorm( 50, 1.0E-04)
  tau~dgamma( 0.01, 0.01) # precision
  s <- sqrt(1/tau)
  mu <- mu1-mu2
  
}
library(R2OpenBUGS) 
model.file <- file.path(tempdir(),"model.txt") 
write.model(model, model.file)
file.show(model.file) 
#data
grade<-c(84,58,100,51,28,89,97,50,76,83,45,42,83,64,47,83,81,83,34,61,77,69,94,80,55,79)
grade1<-grade[1:13]
grade2<-grade[14:26]
n<-13
data<-list("n","grade1","grade2")
params<-c("mu1","mu2","tau","s",'mu')
inits <- function() { list(mu1=50,mu2=50,tau=1.0,mu=0,s=10) }


#ggmcmc
out<-bugs(data,inits,params,model.file,n.chains = 2
          ,n.iter=6000,codaPkg = TRUE,n.burnin = 1000,DIC = TRUE)

out.skinik<-read.bugs(out)
View(head((out.skinik[[1]])))
summary(out.skinik)
library(ggmcmc)
S<-ggs(out.skinik)
k1<-ggs_histogram(S,family='mu')
k2<-ggs_density(S,family='mu')
multiplot(k1,k2,cols=2)
ggs_traceplot(S,family='mu')
ggs_running(S,family='mu')
ggs_compare_partial(S,family='mu')
ggs_autocorrelation(S,family='mu')
ggs_crosscorrelation(S)
ggs_Rhat(S)+ xlab("R_hat")
ggs_geweke(S)
ggs_pairs(S,lower=list(continuous='density'))
ggs_ppmean(S) # mean ths ppd ws pros to mean ths pd #!!
ggs_ppsd(S)#sd ths ppd ws pros to sd ths pd
ac(out.skinik[[1]],nLags=1000)
ci(S)
ggs_caterpillar(S,family='mu')
ggs_rocplot(S) # de douleuei -thelei outcome
gl_unq(S) #de douleuei -thelei outcome