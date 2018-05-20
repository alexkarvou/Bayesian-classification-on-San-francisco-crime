#categorical_logistic_again
library(MASS)
set.seed(43)
painters_new<-painters[sample(nrow(painters)) , ] #randomizing rows
real_values<-painters_new$School[50:54]
painters_new$School[50:54]<-NA
#

model<-function(){
  #likelihood
  for( i in 1:N){
    y[i] ~ dcat(p[i,1:J])#categorical response variable
    
    for(j in 2:J){
      log(q[i,j])<-b[j-1,1]+inprod(b[j-1,2:5],t_X[1:4,i])#Categorical regression model
      p[i,j]<-q[i,j]/sum(q[i,])#computing each probability
    }
    #baseline category
    q[i,1]<-1
    p[i,1]<-1/sum(q[i,])
    
    }
  #priors
  
  for(k in 1:7){
    b[k,1]~dflat() #improper uniform prior 
    b[k,2:5]~dmnorm(zeros[],precision[,])# multivariate normal prior
  }
  #Compute PR(correct classification|data)
  for(i in 1:49){
    ind1[i]<-equals(y[i],1)
    ind2[i]<-equals(y[i],2)
    ind3[i]<-equals(y[i],3)
    ind4[i]<-equals(y[i],4)
    ind5[i]<-equals(y[i],5)
    ind6[i]<-equals(y[i],6)
    ind7[i]<-equals(y[i],7)
    ind8[i]<-equals(y[i],8)
    
    ind.correct[i]<-equals(p[i,y[i]],ranked(p[i,],8))#correct classification
    ind1.correct[i]<-ind1[i]*ind.correct[i]#correct classifications per category
    ind2.correct[i]<-ind2[i]*ind.correct[i]
    ind3.correct[i]<-ind3[i]*ind.correct[i]
    ind4.correct[i]<-ind4[i]*ind.correct[i]
    ind5.correct[i]<-ind5[i]*ind.correct[i]
    ind6.correct[i]<-ind6[i]*ind.correct[i]
    ind7.correct[i]<-ind7[i]*ind.correct[i]
    ind8.correct[i]<-ind8[i]*ind.correct[i]
  }
  p1.correct<-sum(ind1.correct[])/sum(ind1[])#probabilities of correct classification  
  p2.correct<-sum(ind2.correct[])/sum(ind2[])#per category and as a whole
  p3.correct<-sum(ind3.correct[])/sum(ind3[])
  p4.correct<-sum(ind4.correct[])/sum(ind4[])
  p5.correct<-sum(ind5.correct[])/sum(ind5[])
  p6.correct<-sum(ind6.correct[])/sum(ind6[])
  p7.correct<-sum(ind7.correct[])/sum(ind7[])
  p8.correct<-sum(ind8.correct[])/sum(ind8[])
  p.correct<-mean(ind.correct[])
  }
  
library(R2OpenBUGS) 
model.file <- file.path(tempdir(),"model.txt") 
write.model(model, model.file)
y<-as.numeric(painters_new$School)
N<-54
J<-8
painters_new[,1]<-(painters_new[,1]-mean(painters_new[,1]))/sd(painters_new[,1])
painters_new[,2]<-(painters_new[,2]-mean(painters_new[,2]))/sd(painters_new[,2])
painters_new[,3]<-(painters_new[,3]-mean(painters_new[,3]))/sd(painters_new[,3])
painters_new[,4]<-(painters_new[,4]-mean(painters_new[,4]))/sd(painters_new[,4])
X<-as.matrix(painters_new[,1:4])
t_X<-t(X)
precision<-49*(J^2/(J-1))*solve(t(X)%*%X)
zeros<-rep(0,4)
aux<-c(0,0,0,0,0)
initials<-as.matrix(rbind(aux,aux,aux,aux,aux,aux,aux))
data<-list("y","J","N","t_X","zeros","precision")
params<-c("b","y[50]","y[51]","y[52]","y[53]","y[54]",
          "p1.correct","p2.correct","p3.correct","p4.correct",
          "p5.correct","p6.correct","p7.correct","p8.correct","p.correct")
inits<-function(){list(b=initials)}
ptm<-proc.time()
catlog<-bugs(data,inits,params,model.file,n.chains = 2
                 ,n.iter=15000,n.thin =5 ,codaPkg = TRUE,n.burnin = 5000)
proc.time()-ptm
benjamin<-read.bugs(catlog)
summary(benjamin)
library(ggmcmc)
library(ggthemes)
S<-ggs(benjamin,family='b\\[3,.\\]')
#S<-ggs(benjamin,family='y')
t1<-ggs_histogram(S)
t2<-ggs_density(S)
multiplot(t1,t2,cols=2)
ggs_running(S)
g1<-ggs_traceplot(S)
g2<-ggs_running(S)
source("/home/alex/R/multiplotting.R")
multiplot(g1,g2,cols=2)
ggs_compare_partial(S)
ggs_autocorrelation(S)
ggs_crosscorrelation(S)
s1<-ggs_Rhat(S)+ xlab("R_hat")
s2<-ggs_geweke(S)
multiplot(s1,s2,cols=2)
ggs_pairs(S,lower=list(continuous='density'))
ggs_pairs(S,diag=list(continuous='barDiag'),lower=list(continuous='density'))
ggs_caterpillar(S)
ggs_pairs(S,diag=list(continuous='barDiag'),lower=list(continuous='cor'),
          upper=list(continuous='blank'),showStrips=TRUE)
#generate single value from predictive posteriors of every data point
y50<-c(as.vector(benjamin[[2]][,46]),as.vector(benjamin[[1]][,46]))
y51<-c(as.vector(benjamin[[2]][,47]),as.vector(benjamin[[1]][,47]))
y52<-c(as.vector(benjamin[[2]][,48]),as.vector(benjamin[[1]][,48]))
y53<-c(as.vector(benjamin[[2]][,49]),as.vector(benjamin[[1]][,49]))
y54<-c(as.vector(benjamin[[2]][,50]),as.vector(benjamin[[1]][,50]))
predicted_values<-c(names(which.max(table(y50))),names(which.max(table(y51))),
                    names(which.max(table(y52))),names(which.max(table(y53))),
                    names(which.max(table(y54))))
