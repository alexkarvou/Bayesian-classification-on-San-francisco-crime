library(ggplot2)
mosaicplot(prop.table(table(my_data)))
ggplot(my_data,aes(x=V1))+
  geom_bar(stat='identity')+
  coord_polar()
######
library(ggmcmc)
S<-ggs(out.skiniks)
ggs_histogram(S,family='y.fut')
ggs_density(S)
ggs_traceplot(S,family='theta')
ggs_running(S,family='theta')
ggs_compare_partial(S,family='y.fut')
ggs_autocorrelation(S,family='y.fut')
ggs_crosscorrelation(S,family='y.fut')
ggs_pairs
ggs_Rhat(S)+ xlab("R_hat")
ggs_geweke(S)
ggs_caterpillar(S,family=c('y.fut'))
ggs_ppmean(S,outcome=c('y.fut'))

p1<-ggs_traceplot(S,family='theta')
p2<-ggs_running(S,family='theta')+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
grid.arrange(p1,p2,ncol=2)
#predictions
library(MASS)
painters_new<-painters[sample(nrow(painters)) , ] #randomizing rows 
answers<-painters_new$School[50:54]
painters_new$School[50:54]<-NA
y<-as.numeric(painters_new$School)
model<-function(){
  #likelihood
  for( i in 1:N){
    y[i] ~ dcat(theta[])
  }
  #prior
  theta[1:8] ~ ddirch(alpha[]) 
  
}
library(R2OpenBUGS) 
model.file <- file.path(tempdir(),"model.txt") 
write.model(model, model.file)


N<-54
alpha<-c(1,1,1,1,1,1,1,1)
data<-list("y","N","alpha")
params<-c('theta','y[50]','y[51]','y[52]','y[53]','y[54]')
inits<-function(){list(theta=c(1/8,1/8,1/8,1/8,1/8,1/8,1/8,1/8))}
out<-bugs(data,inits,params,model.file,n.chains = 3
          ,n.iter=6000,codaPkg = TRUE,n.burnin = 1000)
out.skiniks<-read.bugs(out)
summary(out.skiniks)
#######bar plot
my_data<-as.data.frame(out.skiniks[[1]])
my_data<-as.data.frame(my_data$y.fut)
names(my_data)<-'yfut'
library(car)
my_data$yfut<-recode(my_data$yfut,"1='A';2='B';3='C';4='D';5='E';6='F';7='G';else='H'")
my_data$yfut<-as.factor(my_data$yfut)
library(ggplot2)
#y.fut
g1<-ggplot(my_data,aes(x=yfut))+
  geom_bar(aes(y = (..count..)/sum(..count..)),colour='orange',fill='blue',alpha=0.3)+
  labs(x='School',y='%',title='Simulated values of the Posterior Predictive Distribution')
#data
g2<-ggplot(painters_new,aes(x=School))+
  geom_bar(aes(y = (..count..)/sum(..count..)),colour='orange',fill='blue',alpha=0.3)
#together
plot_data<-data.frame(y=c(my_data$yfut,painters_new$School),
                      Category=c(rep('replicated values',length(my_data$yfut))
                                 ,rep('data',length(painters_new$School))))
plot_data$y<-recode(plot_data$y,"1='A';2='B';3='C';4='D';5='E';6='F';7='G';else='H'")
plot_data$y<-as.factor(plot_data$y)
library(reshape2)
plot<-melt(ddply(plot_data,.(Category),function(x){prop.table(table(x$y))}))
g1<-ggplot(plot,aes(x=variable,y=value,fill=factor(Category)))+
  geom_bar(position='dodge',stat='identity',alpha=0.8)+
  labs(x='School',y='%')+
  scale_fill_discrete('')
g2<-g1+coord_polar()
multiplot(g1,g2,cols=1)


#diagnostics and ggmcmc
library(ggmcmc)
library(ggthemes)
S<-ggs(out.skiniks,family='y')
#ggs_pairs(S,family=c('theta','y.fut'), lower = list(continuous = "density"))
g1<-ggs_traceplot(S)
g2<-ggs_running(S)
multiplot(g1,g2,cols=2)
ggs_density(S)+
  theme_fivethirtyeight()
#library(GGally)
ggs_pairs(S,diag=list(continuous='barDiag'),lower=list(continuous='cor'),
          upper=list(continuous='blank'),showStrips=TRUE)+
  theme_economist()
ggs_compare_partial(S)
ggs_autocorrelation(S)+
  theme_economist()
ggs_crosscorrelation(S)+
  theme_economist()
ggs_Rhat(S)+ xlab("R_hat")
ggs_geweke(S)
ggs_caterpillar(S)
ggs_ppmean(S,outcome='y')
ggs_pairs(S,
         upper=list(continuous="density", params=c(color="black")),
         lower=list(params=c(alpha=.2, shape=1)))
ggs_rocplot(S,outcome='',fully_bayesian = TRUE)
ggs_separation(S,outcome='y')

#posterior sample modes
y50<-c(as.vector(out.skiniks[[2]][,10]),as.vector(out.skiniks[[1]][,10])
       ,as.vector(out.skiniks[[3]][,10]))
y51<-c(as.vector(out.skiniks[[2]][,11]),as.vector(out.skiniks[[1]][,11])
       ,as.vector(out.skiniks[[3]][,11]))
y52<-c(as.vector(out.skiniks[[2]][,12]),as.vector(out.skiniks[[1]][,12])
       ,as.vector(out.skiniks[[3]][,12]))
y53<-c(as.vector(out.skiniks[[2]][,13]),as.vector(out.skiniks[[1]][,13])
       ,as.vector(out.skiniks[[3]][,13]))
y54<-c(as.vector(out.skiniks[[2]][,14]),as.vector(out.skiniks[[1]][,14])
       ,as.vector(out.skiniks[[3]][,14]))
predicted_values<-c(names(which.max(table(y50))),names(which.max(table(y51))),
                    names(which.max(table(y52))),names(which.max(table(y53))),
                    names(which.max(table(y54))))


