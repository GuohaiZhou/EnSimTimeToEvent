# https://teuder.github.io/rcpp4everyone_en/030_basic_usage.html#writing-your-rcpp-code
# https://cran.r-project.org/web/views/Optimization.html

# minqa # bobyqa 
rm(list=ls()); gc()
setwd('C:/Users/gz085/Downloads/OneDrive - Partners HealthCare/BWH/StevenEmSimCRM')
library(Rcpp)
library(RcppArmadillo)
library(roptim)
library(data.table)
library(ggplot2)
sourceCpp('Roptm_exp_mle.cpp')
set.seed(1)
intercept= -3.5
# when subject 5 is available for enrollment 
dat = data.table(delta=c(1,0,0,0),y=c(0,0.5,0.5,0.5),t=c(68,28,21,15),ki=c(30,30,30,30))
out = logistic_reg_slope_only_mle_with_simulated_data(1e-7, as.matrix(dat),30,50,0.15,0.25,71,log(0.03), -0.18,intercept,1000)
parameter_est= as.data.table( out[[1001]])
names(parameter_est)=c('logbeta','lamb1')
parameter_est[,dose:=( (log(0.2/0.8)-intercept)/exp(logbeta))]
parameter_est[dose<25,doseD:=20]
parameter_est[dose>=25&dose<35,doseD:=30]
parameter_est[dose>=35,doseD:=40]
parameter_est[,doseD:=factor(doseD,levels = c('20','30','40'))]
# quantile(parameter_est[,dose],probs = seq(0,1,by=0.05))
 g1h = ggplot(parameter_est,aes(x=doseD))  + geom_bar()+theme_classic()+labs(y='Frequency as target dose',x='Applicable doses',title='At enrollment of subject 5')+scale_y_continuous(breaks = seq(200,800,by=200)) + scale_x_discrete(drop=FALSE); g1h  # ,labels = paste0(seq(20,80,by=20),'%')
 # logi = function(x) 1/(1+exp(-intercept-exp(-3)*x))   

# when subject 9 is available for enrollment
 dat = data.table(delta=c(1,1,1,1,1,1,1,0),y=c(0,0,0,0,0,0,0,0.5),t=c(67, 55, 66, 52, 63, 70, 63, 42),ki=c(30,30,30,30,40,40,40,40))
 out = logistic_reg_slope_only_mle_with_simulated_data(1e-7, as.matrix(dat),30,50,0.15,0.25,71,log(0.03), -0.18,intercept,1000)
 parameter_est= as.data.table( out[[1001]])
 names(parameter_est)=c('logbeta','lamb1')
 parameter_est[,dose:=( (log(0.2/0.8)-intercept)/exp(logbeta))]
 parameter_est[dose<25,doseD:=20]
 parameter_est[dose>=25&dose<35,doseD:=30]
 parameter_est[dose>=35&dose<45,doseD:=40]
 parameter_est[dose>=45,doseD:=50]
 parameter_est[,doseD:=factor(doseD,levels = c('20','30','40','50'))]
 # quantile(parameter_est[,dose],probs = seq(0,1,by=0.05))
 g2h = ggplot(parameter_est,aes(x=doseD))  + geom_bar()+theme_classic()+labs(y='Frequency as target dose',x='Applicable doses',title='At enrollment of subject 9')+scale_y_continuous(breaks = seq(200,800,by=200)) + scale_x_discrete(drop=FALSE); g2h  # ,labels = paste0(seq(20,80,by=20),'%')
 
# when subject 12 is available for enrollment
 dat = data.table(delta=c(rep(1,10),0),y=c(rep(0,10),0.5),t=c(67, 55, 66, 52, 63, 70, 63, 56, 60, 67, 21),ki=c(30,30,30,30,40,40,40,40,50,50,50))
 out = logistic_reg_slope_only_mle_with_simulated_data(1e-7, as.matrix(dat),30,50,0.15,0.25,71,log(0.03), -0.18,intercept,1000)
 parameter_est= as.data.table( out[[1001]])
 names(parameter_est)=c('logbeta','lamb1')
 parameter_est[,dose:=( (log(0.2/0.8)-intercept)/exp(logbeta))]
 parameter_est[dose<25,doseD:=20]
 parameter_est[dose>=25&dose<35,doseD:=30]
 parameter_est[dose>=35&dose<45,doseD:=40]
 parameter_est[dose>=45,doseD:=50]
 parameter_est[,doseD:=factor(doseD,levels = c('20','30','40','50'))]
 # quantile(parameter_est[,dose],probs = seq(0,1,by=0.05))
 g3h = ggplot(parameter_est,aes(x=doseD))  + geom_bar()+theme_classic()+labs(y='Frequency as target dose',x='Applicable doses',title='At enrollment of subject 12')+scale_y_continuous(breaks = seq(200,800,by=200)) + scale_x_discrete(drop=FALSE); g3h  # ,labels = paste0(seq(20,80,by=20),'%')
  
# when subject 16 is available for enrollment 
set.seed(1)
dat = data.table(delta=c(rep(1,12),rep(0,3)),y=c(rep(0,10),1,1,0.5,0.5,0.5),t=c(67, 55, 66 ,52, 63, 70, 63, 56, 60, 67, 23, 46, 42, 35, 21),ki=c(30,30,30,30,40,40,40,40,rep(50,7))) 
out = logistic_reg_slope_only_mle_with_simulated_data(1e-7, as.matrix(dat),30,50,0.15,0.25,71,log(0.03), -0.18,intercept,1000)
parameter_est= as.data.table( out[[1001]])
names(parameter_est)=c('logbeta','lamb1')
parameter_est[,dose:=( (log(0.2/0.8)-intercept)/exp(logbeta))]
parameter_est[dose<25,doseD:=20]
parameter_est[dose>=25&dose<35,doseD:=30]
parameter_est[dose>=35&dose<45,doseD:=40]
parameter_est[dose>=45,doseD:=50]
parameter_est[,doseD:=factor(doseD,levels = c('20','30','40','50'))]
 g4h = ggplot(parameter_est,aes(x=doseD))  + geom_bar()+theme_classic()+labs(y='Frequency as target dose',x='Applicable doses',title='At enrollment of subject 16')+scale_y_continuous(breaks = seq(200,800,by=200)) + scale_x_discrete(drop=FALSE); g4h  # ,labels = paste0(seq(20,80,by=20),'%')


# when subject 18 is available for enrollment 
set.seed(1)
dat = data.table(delta= rep(1,17) ,y=c(rep(0,10),1,1,0,0,1,0,1),t=c(67, 55, 66 ,52, 63, 70, 63, 56, 60, 67, 23, 46, 60, 60, 29, 59, 37),ki=c(30,30,30,30,40,40,40,40,rep(50,7),40,50)) 
out = logistic_reg_slope_only_mle_with_simulated_data(1e-7, as.matrix(dat),30,50,0.15,0.25,71,log(0.03), -0.18,intercept,1000)
parameter_est= as.data.table( out[[1001]])
names(parameter_est)=c('logbeta','lamb1')
parameter_est[,dose:=( (log(0.2/0.8)-intercept)/exp(logbeta))]
parameter_est[dose<25,doseD:=20]
parameter_est[dose>=25&dose<35,doseD:=30]
parameter_est[dose>=35&dose<45,doseD:=40]
parameter_est[dose>=45,doseD:=50]
parameter_est[,doseD:=factor(doseD,levels = c('20','30','40','50'))]
 g5h = ggplot(parameter_est,aes(x=doseD))  + geom_bar()+theme_classic()+labs(y='Frequency as target dose',x='Applicable doses',title='At enrollment of subject 18')+scale_y_continuous(breaks = seq(200,800,by=200)) + scale_x_discrete(drop=FALSE); g5h  # ,labels = paste0(seq(20,80,by=20),'%')


# at the end of the trial 
set.seed(1)
dat = data.table(delta=c(rep(1,18)),y=c(rep(0,10),1,1,0,0,1,0,1,0),t=c(67, 55, 66 ,52, 63, 70, 63, 56, 60, 67, 23, 46, 60, 60, 29, 59, 37, 73),ki=c(30,30,30,30,40,40,40,40,rep(50,7),40,50,30)) 
out = logistic_reg_slope_only_mle_with_simulated_data(1e-7, as.matrix(dat),30,50,0.15,0.25,71,log(0.03), -0.18,intercept,1000)
parameter_est= as.data.table( out[[1001]])
names(parameter_est)=c('logbeta','lamb1')
parameter_est[,dose:=( (log(0.2/0.8)-intercept)/exp(logbeta))]

parameter_est[dose<25,doseD:=20]
parameter_est[dose>=25&dose<35,doseD:=30]
parameter_est[dose>=35&dose<45,doseD:=40]
parameter_est[dose>=45,doseD:=50]
parameter_est[,doseD:=factor(doseD,levels = c('20','30','40','50'))]
g6h = ggplot(parameter_est,aes(x=doseD))  + geom_bar()+theme_classic()+labs(y='Frequency as target dose',x='Applicable doses',title='At end of trial')+scale_y_continuous(breaks = seq(200,800,by=200)) + scale_x_discrete(drop=FALSE); g6h  # ,labels = paste0(seq(20,80,by=20),'%')
library(grid)
library(gridExtra)
#g <- arrangeGrob(g1h, g2h,g3h,g4h,g5h,g6h, nrow=2) # produce figure 6
#ggsave(file="Figure 4 real data dose recommendation.png", g,width=10,height = 5)
# ggsave(file="Figure 6.eps", g,width=5,height = 10)
 
ilogit_vec=function(x) 1/(1+exp(-x))
library(ggrepel)
betafinal=exp(median(parameter_est[,logbeta]))
parameter_est[,probatdose40:=ilogit_vec(exp(logbeta)*40+intercept)]
quantile(parameter_est[,probatdose40],probs = c(0.025,0.975))
logi = function(x) 1/(1+exp(3.5-betafinal*x))
# curve(logi,from=20,to=50)
set.seed(3)
library(ggplot2)
library(data.table)
dat[1:18,ki_jitter:=ki+rnorm(18,mean =0,sd=1.4)]
dat[,subjectNo:=1:18]
dat[,labelx:=ki_jitter]
dat[,labely:=y-0.05]
dat[subjectNo==7,labely:=y+0.05]
dat[subjectNo==17,labely:=y+0.05]
g1=ggplot(dat,aes(x=ki_jitter,y=y,label=subjectNo))+geom_point(size=6,alpha=0.5)+stat_function(fun =logi, colour = "black",size=1.1)+theme(plot.title = element_text(size=11.5))+theme_classic()
g1 = g1+annotate("rect", xmin=30, xmax=50, ymin=0.15, ymax=0.25,fill='gray30', alpha = .5)
g1 = g1+labs(title='Final model Pr(Y=1|d)= 1/( 1 + exp(3.5-0.055*d) ).',x='Dose d',y='0/1 response or Pr(Y=1|d)')
g1 = g1 + scale_x_continuous(breaks = c(20,30,40,50)) + scale_y_continuous(breaks = c(0,0.1,0.15,0.2,0.25,1)) +geom_text(aes(label=subjectNo,x=labelx,y=labely),size=2.2) # + geom_text_repel( nudge_y = 0.01, direction    = "y", angle        = 0, vjust        = 0, hjust = 1, segment.color = "white",segment.alpha=0 ,point.padding = NA) # , segment.size = 0.2
#+geom_text(aes(label=subjectNo),  position=position_jitter(height=0.08))
g1
ggsave('final_dose_response_model.png',g1,width = 8,height=6)





g1=ggplot(data.table(x=c(20,50),y=logi(c(20,50))),aes(x=x,y=y))+stat_function(fun =logi, colour = "black",size=1.1)+theme(plot.title = element_text(size=11.5))+theme_classic()    + scale_x_continuous(breaks = c(20,30,40,50)) + scale_y_continuous(breaks = c(0,0.1,0.15,0.2,0.25),limits = c(0,0.32))+geom_point(data=dat[1],aes(x=ki,y=y))
g1 = g1+annotate("rect", xmin=30, xmax=50, ymin=0.15, ymax=0.25,fill='gray30', alpha = .5)
g1 = g1+labs(title='Initial guess Pr(Y=1|d)= 1/( 1 + exp(3.5-3*d) ). Target dose is for Pr(Y=1|d)=0.2.',x='Dose d',y='Pr(Y=1|d)')
g1 



# summary(glm(y~ki,family=binomial,data=dat[1:12]))
intercept= -5.5
out = logistic_reg_slope_only_mle_with_simulated_data(1e-7, as.matrix(dat),30,50,0.125,0.225,71,0.002, -0.18,intercept,1000)# double step_size, arma::mat datin, double envelop_dose_min, double envelop_dose_max, double envelop_response_min, double envelop_response_max, double eNplus1, double currentbeta, double currentlamb, double fixed_intercept_in_dose_response_model, int num_simulation
parameter_est= as.data.table( out[[1001]])
names(parameter_est)=c('beta','lambda')
parameter_est[,dose:=( (log(0.2/0.8)-intercept)/beta)]
quantile(parameter_est[,dose],probs = seq(0,1,by=0.05))
g1h = ggplot(parameter_est,aes(x=doseD))+geom_histogram()+theme_classic()+labs(x='Distribution of target dose')+geom_vline(xintercept = median(parameter_est[,dose]),color='black',linetype='dashed',size=0.8); g1h  

 


# out = logistic_reg_slope_only_mle(1e-7, as.matrix(dat[1:12,.(y,ki)]),-0.03,intercept)
# (double step_size, arma::mat datin, double currentbeta, double fixed_intercept_in_dose_response_model) 
# SE = sqrt(1/Hessian) = sqrt(1/3362.45)
beta=median(parameter_est[,beta]); logi = function(x) 1/(1+exp(-intercept-beta*x))
beta; (log(0.2/0.8)-intercept)/beta 
library(ggplot2)
dat[1:12,ki_jitter:=ki+rnorm(12,mean =0,sd=2)]
g1=ggplot(data=dat[1:12],aes(x=ki_jitter,y=y))+geom_point()+stat_function(fun =logi, colour = "black",size=1.1)+theme_classic() # +geom_jitter(width = 3,height = 0)
g1

 

##### Sim envelop: mixture of discrete uniform model 
# data generating model: ti follows independent pUniform(0, 40) + (1?p)Uniform(40, 80) with p = exp(0.001ki)/[1+exp(0.001ki)], if deltai=1, ti is observed, if deltai=0, ti is known to be greater than eNplus1-ei.
 

# ilogit(1.1)
set.seed(1)
# when subject 5 is available for enrollment 
dat = data.table(delta=c(1,0,0,0),y=c(0,0.5,0.5,0.5),t=c(68,28,21,15),ki=c(30,30,30,30)) # y=1 DLT, 0 non=DLT, 0.5 censored 
# mixture_uniform_mod_mle(double step_size, arma::mat datin, double envelop_dose_min, double envelop_dose_max, double envelop_response_min, double envelop_response_max, double eNplus1,double currentlamb0, double currentbeta, int num_simulation) # 
out = mixture_uniform_mod_mle(1e-7, as.matrix(dat),30,50,0.125,0.225,71,-0.001, -0.18,1000)
out[[1002]][1:23,1]

parameter_est= as.data.table( out[[1001]])
names(parameter_est)=c('lamb','beta')
parameter_est[,dose:=( (log(0.2/0.8)-3.5)/beta)]
function(logit)
quantile(parameter_est[,dose],probs = seq(0,1,by=0.1))
quantile(parameter_est[,beta],probs = seq(0,1,by=0.1))

set.seed(1)
# when subject 9 and 10 is available for enrollment 
dat = data.table(delta=c(1,1,1,1,1,1,1,1,0),y=c(0,0,0,0,0,0,0,0,0.5),t=c(68,56,67,53,64,71,64,43),ki=c(30,30,30,30,40,40,40,40)) 
out2  = mixture_uniform_mod_mle(1e-7, as.matrix(dat),30,50,0.125,0.225,71,-0.001, -0.18,1000)
parameter_est2= as.data.table( out2[[1001]])
 
names(parameter_est2)=c('lamb','beta')
parameter_est2[,dose:=( (log(0.2/0.8)-3.5)/beta)]
 quantile(parameter_est2[,dose],probs = seq(0,1,by=0.1))
quantile(parameter_est2[,beta],probs = seq(0,1,by=0.1))

# when subject 12 is available for enrollment 
dat = data.table(delta=c(1,1,1,1,1,1,1,1,1,1,0),y=c(0,0,0,0,0,0,0,0,0,0,0.5),t=c(68,56,67,53,64,71,64,57,61,68,22),ki=c(30,30,30,30,40,40,40,40,50,50,50)) 
set.seed(1)
out3  = mixture_uniform_mod_mle(1e-7, as.matrix(dat),30,50,0.125,0.225,71,-0.001, -0.18,1000)
parameter_est3= as.data.table( out3[[1001]])

names(parameter_est3)=c('lamb','beta')
parameter_est3[,dose:=( (log(0.2/0.8)-3.5)/beta)]
quantile(parameter_est3[,dose],probs = seq(0,1,by=0.1))
quantile(parameter_est3[,beta],probs = seq(0,1,by=0.1))

# when subject 16 is available for enrollment 
dat = data.table(delta=c(rep(1,12),rep(0,3)),y=c(rep(0,10),1,1,0.5,0.5,0.5),t=c(67, 55, 66 ,52, 63, 70, 63, 56, 60, 67, 23, 46, 42, 35, 21),ki=c(30,30,30,30,40,40,40,40,rep(50,7))) 
set.seed(1)
out3  = mixture_uniform_mod_mle(1e-7, as.matrix(dat),30,50,0.125,0.225,71,-0.001, -0.18,1000)
parameter_est3= as.data.table( out3[[1001]])

names(parameter_est3)=c('lamb','beta')
parameter_est3[,dose:=( (log(0.2/0.8)-3.5)/beta)]
quantile(parameter_est3[,dose],probs = seq(0,1,by=0.1))
quantile(parameter_est3[,beta],probs = seq(0,1,by=0.1))
# summary(glm(y~ki,family=binomial,data=dat[1:12]))
# logistic_reg_slope_only_mle(1e-7, as.matrix(dat[1:12,.(y,ki)]),0.5)
beta=0.05; logi = function(x) 1/(1+exp(3.5-beta*x))
library(ggplot2)
dat[1:12,ki_jitter:=ki+rnorm(12,mean =0,sd=2)]
g1=ggplot(data=dat[1:12],aes(x=ki_jitter,y=y))+geom_point()+stat_function(fun =logi, colour = "black",size=1.1)+theme_classic() # +geom_jitter(width = 3,height = 0)
g1

