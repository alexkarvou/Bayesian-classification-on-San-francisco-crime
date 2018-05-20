#reads
train<-read.csv(file="C:/Users/User/Desktop/save_me/train.csv",header=TRUE,sep=",")
sample_subm<-read.csv(file="/home/alex/R/san francisco crime-kaggle/sampleSubmission.csv",header=TRUE,sep=",")
test<-read.csv(file="/home/alex/R/san francisco crime-kaggle/test.csv",header=TRUE,sep=",")
#
train_dim<-dim(train)[1]
steps<-seq(1,451848,by=1000)#400-1200
library(plyr)
library(dplyr)
deita<-select(train[steps,],Dates,DayOfWeek,PdDistrict,Address,X,Y,Category)
length<-dim(deita)[1];length
library(lubridate)
deita$year<-year(deita$Dates)
deita$month<-month(deita$Dates)
deita$hour<-hour(deita$Dates)
deita<-select(deita,-1) #wraia
deita$Category<-as.character(deita$Category)
deita$Category<-as.factor(deita$Category)
deita$Category<-relevel(deita$Category,"LARCENY/THEFT")#prwto to pio frequent
deita$DayOfWeek<-factor(deita$DayOfWeek,levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday',
                                           'Sunday'))
#revgeo

library(ggmap)
deita$neighborhood<-'a'
for( i in 1:dim(deita)[1]){
  a<-as.character(revgeocode(c(deita$X[i],deita$Y[i]),output='more')$neighborhood)
  if(length(a)==0){
    a<-as.character(deita$Address[i])
  }
  deita$neighborhood[i]<-a
}
#diorthwseis
sum(deita$neighborhood=='POWELL ST / MARKET ST') #posa einai
deita$neighborhood[deita$neighborhood=='5TH ST / MARKET ST']='Tenderloin' # allagh
unique(deita$neighborhood)
geocode(' DUBOCE AVENUE / MISSION STREET San francisco')
revgeocode(c(-122.4191,37.75888),output='more')$neighborhood
#backup
#auxilliary<-deita$neighborhood
deita$neighborhood<-auxilliary
#diorthwse ta lathos X,Y
#deita$X[deita$Y==90.0]<-mean(deita$X)
#deita$Y[deita$Y==90.0]<-mean(deita$Y)
#kanonikopoihse ta X
deita$X<-deita$X-mean(deita$X)
deita$X<-deita$X/sd(deita$X)
deita$Y<-deita$Y-mean(deita$Y)
deita$Y<-deita$Y/sd(deita$Y)
#dummies
library(dummies)
pd_dummies<-as.data.frame(dummy('PdDistrict',data=deita))
pd_dummies<-select(pd_dummies,-8)
deita<-select(deita,-2)
deita<-select(deita,-2)
year_dummies<-as.data.frame(dummy('year',data=deita))
year_dummies<-select(year_dummies,-1)
deita<-select(deita,-5)
#nb_dummies<-as.data.frame(dummy('neighborhood',data=deita))
#nb_dummies<-select(nb_dummies,-56)
#Day of week
deita$DayOfWeek<-as.character(deita$DayOfWeek)
for ( i in 1:dim(deita)[1]){
  if(deita$DayOfWeek[i]%in%c('Monday','Tuesday','Wednesday','Thursday','Friday')){
    deita$DayOfWeek[i]<-'Weekdays'
  }
  else{ deita$DayOfWeek[i]<-'Weekend'}
}
unique(deita$DayOfWeek)
day_dummies<-as.data.frame(dummy('DayOfWeek',data=deita))
day_dummies<-select(day_dummies,-1)
deita<-select(deita,-1)
#deita<-select(deita,-7)
deita<-cbind(deita,day_dummies,pd_dummies,year_dummies)
#address transformation
#deita$Intersection<-grepl("/", deita$Address)
#deita$Intersection<-plyr::mapvalues(deita$Intersection,from=c("TRUE","FALSE"),to=c(1,0))
#deita<-select(deita,-1)
#randomize ta 5000
unique(deita$Category)
deita<-deita[sample(nrow(deita)),]
Real_values<-deita$Category[(length-29):length]
deita$Category[(length-29):length]<-NA
#cool?
#dokimastiko set
#aux_deita<-deita[sample(nrow(my_data),size=50,replace=FALSE),]
#dieita<-deita[is.na(deita$Category),]
#aux_deita<-rbind(aux_deita,dieita[sample(nrow(dieita),size=10,replace=FALSE),])
#
model<-function(){
  #likelihood
  for( i in 1:N){
    y[i] ~ dcat(p[i,1:J]) #categorical response variable
    
    for(j in 2:J){
      log(q[i,j])<-b[j-1,1]+inprod(b[j-1,2:12],t_X[1:21,i]) #Categorical regression model
      p[i,j]<-q[i,j]/sum(q[i,]) #computing each probability
    }
    #baseline category
    q[i,1]<-1
    p[i,1]<-1/sum(q[i,])
    
  }
  #priors
  
  for(k in 1:23){
    b[k,1]~dflat()
    b[k,2]~dnorm(0,1.0E-02)
    b[k,3]~dnorm(0,1.0E-02)
    b[k,4]~dnorm(0,1.0E-02)
    b[k,5]~dnorm(0,1.0E-02)
    b[k,6:16]~dmnorm(zeros1[],precision1[,])
    
   
}}
#b[k,5:10]~dmnorm(zeros1[],precision1[,])
#b[k,11:21]~dmnorm(zeros2[],precision2[,])
#b[k,6:16]~dmnorm(zeros1[],precision1[,])
##b[k,5]~dnorm(0,1.0E-02)
#b[k,6]~dnorm(0,1.0E-02)
#b[k,7]~dnorm(0,1.0E-02)
#b[k,8]~dnorm(0,1.0E-02)
#b[k,9]~dnorm(0,1.0E-02)
#b[k,10]~dnorm(0,1.0E-02)
#b[k,11]~dnorm(0,1.0E-02)
#b[k,12]~dnorm(0,1.0E-02)
#b[k,13]~dnorm(0,1.0E-02)
#b[k,14]~dnorm(0,1.0E-02)
#b[k,15]~dnorm(0,1.0E-02)
#b[k,16]~dnorm(0,1.0E-02)
#b[k,17]~dnorm(0,1.0E-02)
#b[k,18]~dnorm(0,1.0E-02)
#b[k,19]~dnorm(0,1.0E-02)
#b[k,20]~dnorm(0,1.0E-02)
#b[k,21]~dnorm(0,1.0E-02)
library(R2OpenBUGS) 
model.file <- file.path(tempdir(),"model.txt") 
write.model(model, model.file)
N<-dim(deita)[1]
J<-24
y<-as.numeric(deita$Category)
X<-as.matrix(select(deita,-3))
X<-X[,c(3,4,5,6,1,2,7:15)]
t_X<-t(X)
X1<-X[1:(length-29),c(5:15)]
#X1<-X[1:(length-49),c(5:15)]
t_X1<-t(X1)
#t_X2<-t(X2)
precision1<-(length-30)*(J^2/(J-1))*solve(t_X1%*%X1)
#precision2<-(length-50)*(J^2/(J-1))*solve(t_X2%*%X2)
zeros1<-rep(0,11)
#<-rep(0,11)
initials<-matrix(0,nrow=23,ncol=16)
data<-list("y","J","N","t_X","zeros1","precision1") #"zeros2","precision1"
reSult<-as.character((length-29):length)
#reSult<-grep(rx,x)
reSult<-paste("y[",reSult,sep='')
res2<-rep("]",30)
res_fin<-paste(reSult,res2,sep='')
params<-c("b",res_fin)
#y_inits<-rep(1,11)
inits<-function(){list(b=initials)}#,y[402]=one,y[403]=1,y[404]=1,y[405]=1,y[406]=1,y[407]=1,
                       #y[408]=1,y[409]=1,y[410]=1,y[411]=1,y[412]=1,
                       #y[413]=1,y[414]=1,y[415]=1,y[416]=1,y[417]=1,
                       #y[418]=1,y[419]=1,y[420]=1,y[421]=1,y[422]=1,y[423]=1,y[424]=1,
                       #y[425]=1,y[426]=1,y[427]=1,y[428]=1,y[429]=1,y[430]=1,y[431]=1)}
atlast<-bugs(data,inits,params,model.file,n.chains = 2
             ,n.iter=7500,n.thin = 2 ,codaPkg = TRUE,n.burnin=2300,debug=TRUE)

fin<-read.bugs(atlast)
#jags way
model.string<-"
  model{
  #likelihood
for( i in 1:N){
y[i] ~ dcat(p[i,1:J]) #categorical response variable

for(j in 2:J){
log(q[i,j])<-b[j-1,1]+inprod(b[j-1,2:21],t_X[1:20,i]) #Categorical regression model
p[i,j]<-q[i,j]/sum(q[i,]) #computing each probability
}
#baseline category
q[i,1]<-1
p[i,1]<-1/sum(q[i,])

}
#priors

for(k in 1:26){
b[k,1]~dnorm(0,1.0E-04)
b[k,2]~dnorm(0,1.0E-04)
b[k,3]~dnorm(0,1.0E-04)
b[k,4]~dnorm(0,1.0E-04)
b[k,5]~dnorm(0,1.0E-04)
b[k,6]~dnorm(0,1.0E-04)
b[k,7]~dnorm(0,1.0E-04)
b[k,8]~dnorm(0,1.0E-04)
b[k,9]~dnorm(0,1.0E-04)
b[k,10]~dnorm(0,1.0E-04)
b[k,11:21]~dmnorm(zeros1[],precision1[,])


}}
"
model.spec<-textConnection(model.string)
N<-dim(deita)[1]
J<-27
y<-as.numeric(deita$Category)
X<-as.matrix(select(deita,-3))
X<-X[,c(3,4,5,15,16,17,18,19,20,1,2,6:14)]
t_X<-t(X)
X1<-X[1:(length-29),c(10:20)]
#X1<-X[1:(length-49),c(5:15)]
t_X1<-t(X1)
#t_X2<-t(X2)
precision1<-(length-30)*(J^2/(J-1))*solve(t_X1%*%X1)
#precision2<-(length-50)*(J^2/(J-1))*solve(t_X2%*%X2)
zeros1<-rep(0,11)
#<-rep(0,11)
initials<-matrix(10^(-4),nrow=26,ncol=21)
jags.data<-list("y","J","N","t_X","zeros1","precision1") #"zeros2","precision1"
jags.params<-c("b","y[423:452]")
jags.inits<-function(){list("b"=initials)}
library(rjags)
library(R2jags)
#sanf<-jags.model(model.spec,data=list('y'=y,'J'=J,'N'=N,'t_X'=t_X),n.chains=2,n.adapt=100)
san_f<-jags(data=jags.data,inits=jags.inits,parameters.to.save =jags.params,n.iter=100000,n.thin = 25,
            n.burnin =2150,DIC=FALSE,model.file=model.spec,n.chains=2)
#recompile(san_f)
#autojags(san_f)
#recompile(san_f)
#san_f2<-update(san_f,n.iter=20000,n.thin=4)
alfa<-as.mcmc(san_f)
library(ggmcmc)
S<-ggs(alfa,family='b\\[2,.\\]')
S<-ggs(alfa,family='y')
ggs_histogram(S)
ggs_density(S)
ggs_geweke(S)
ggs_caterpillar(S)
ggs_traceplot(S)
ggs_running(S)
ggs_autocorrelation(S)
ggs_crosscorrelation(S)
ggs_compare_partial(S)
ggs_Rhat(S)+ xlab('R_hat')
ggs_pairs(S)
#
library(gridExtra)
S<-ggs(alfa,family='b\\[2,.\\]')
p1<-ggs_caterpillar(S)
S<-ggs(alfa,family='b\\[2,1.\\]')
p2<-ggs_caterpillar(S)
S<-ggs(alfa,family='b\\[2,2.\\]')
p3<-ggs_caterpillar(S)
grid.arrange(p1,p2,p3,nrow=1)
#ypred
S<-ggs(alfa,family='y\\[42.\\]')
p1<-ggs_Rhat(S)+ xlab('R_hat')
S<-ggs(alfa,family='y\\[43.\\]')
p2<-ggs_Rhat(S)+ xlab('R_hat')
S<-ggs(alfa,family='y\\[44.\\]')
p3<-ggs_Rhat(S)+ xlab('R_hat')
S<-ggs(alfa,family='y\\[45.\\]')
p4<-ggs_Rhat(S)+ xlab('R_hat')
grid.arrange(p1,p2,nrow=1)
grid.arrange(p3,p4,nrow=1)


S<-ggs(alfa,family='y\\[42.\\]')
ggs_pairs(S,diag=list(continuous="barDiag"),lower=list(continuous='cor'),
          upper=list(continuous='blank'),showStrips=TRUE)
S<-ggs(alfa,family='y\\[43.\\]')
ggs_pairs(S,diag=list(continuous="barDiag"),lower=list(continuous='cor'),
              upper=list(continuous='blank'),showStrips=TRUE)
S<-ggs(alfa,family='y\\[44.\\]')
ggs_pairs(S,diag=list(continuous="barDiag"),lower=list(continuous='cor'),
              upper=list(continuous='blank'),showStrips=TRUE)
S<-ggs(alfa,family='y\\[45.\\]')
ggs_pairs(S,diag=list(continuous="barDiag"),lower=list(continuous='cor'),
              upper=list(continuous='blank'),showStrips=TRUE)
grid.arrange(p1,p2,nrow=1)
#generate single value from predictive posteriors of every data point 423-352
k<-1
ypredicted<-rep(NULL,30)
for( i in 547:576){
  ypredicted[k]<-names(which.max(table(c(as.vector(alfa[[1]][,i]),as.vector(alfa[[2]][,i])))))
  k<-k+1
}
streiver<-c(as.vector(alfa[[2]][,547]),as.vector(alfa[[1]][,547]))

