rm(list=ls()); gc()
library(readxl)
library(data.table)
setwd('C:/Users/gz085/Downloads/OneDrive - Partners HealthCare/BWH/StevenEmSimCRM')
dt = as.data.table(read_xlsx("MulerData.xlsx"))
dt[,start:=as.Date(paste0(StartMonth,StartDay,StartYear),format='%b%d,%Y')]
dt[,end:=as.Date(paste0(EndMonth,EndDay,EndYear),format='%b%d,%Y')]
# as.Date('April 26, 2001',format='%B %d, %Y')
dt=dt[DLT!='NA',.(patient,Dose,DLT,start,end)] # 
dt[,patient:=1:nrow(dt)]
dt[,startday:= as.numeric( ( (start- dt[1,start])+1 ) )]
dt[,endday:= as.numeric( ( (end- dt[1,start])+1 ) )]
dt[,y:=rev(dt[,patient])]
sort(dt[,endday]-dt[,startday]+1)
dt[,end_start_diff:=(endday-startday)]
dt[,day71_diff:=(71-startday+1)]
dt[,day225_diff:=(225-startday)]
dt[,day302_diff:=(302-startday)]
dt[,day365_diff:=(365-startday)]
dt[,day456_diff:=(456-startday)]
dt[1:4,.(patient,Dose,startday,endday,end_start_diff,day71_diff)]
d9=dt[1:9,.(patient,Dose,startday,endday,end_start_diff,day71_diff,day225_diff)]
d9[,t:=ifelse(endday<225,end_start_diff,day225_diff)]
d9[,t]

d12=dt[1:12,.(patient,Dose,startday,endday,end_start_diff,day302_diff)]
d12[,t:=ifelse(endday<302,end_start_diff,day302_diff)]
d12[,t]

d16=dt[1:15,.(patient,Dose,startday,endday, end_start_diff,day365_diff)]
d16[,t:=ifelse(endday<365,end_start_diff,day365_diff)]
d16


d18=dt[1:17,.(patient,Dose,startday,endday, end_start_diff,day456_diff)]
d18[,t:=ifelse(endday<456,end_start_diff,day456_diff)]
d18[,t]

d19 = dt[,.(patient,Dose,DLT,startday,endday, end_start_diff)]
library(ggplot2)
library(reshape2)
data_long <- melt(dt,
                  # ID variables - all the variables to keep but not split apart on
                  id.vars=c("patient"),
                  # The source columns
                  measure.vars=c("startday", "endday"),
                  # Name of the destination column that will identify the original
                  # column that the measurement came from
                  variable.name="daytype",
                  value.name="day"
)
data_long=data_long[order(patient)]
data_long
dt2=merge(dt,data_long,by.x='patient',by.y='patient')
dt2[,Dose:=as.factor(Dose)]
g=ggplot(data=dt2,aes(x=day,y=y,group=patient))+theme_classic()+geom_line(aes(color=Dose),size=1.2)+scale_y_continuous(breaks = 1:nrow(dt),labels = dt[,y])+labs(x='Day since trial onset',y='Patient No.',title='Actual progression of the Muler et al. (2004) trial')+scale_x_continuous(breaks = c(1,seq(50,500,by=50),529),limits = c(1,529))+geom_point(data=dt,aes(x=endday,y=y,shape=DLT),size=2)+scale_shape_manual(values=c(1, 16)) +geom_vline(xintercept = dt[patient==5,startday],linetype=2)+geom_vline(xintercept = dt[patient==9,startday],linetype=2)+geom_vline(xintercept = dt[patient==12,startday],linetype=2)+geom_vline(xintercept = dt[patient==16,startday],linetype=2)+geom_vline(xintercept = dt[patient==18,startday],linetype=2)
g
  ggsave('mular2014progression.png',g,width = 8,height=6)

beta= (  log(0.3/0.7) - log(0.2/0.8) )/10
beta0 = log(0.2/0.8) - 40*beta
beta=0.05
 logi = function(x) 1/(1+exp(3.5-beta*x))
 # curve(logi,from=20,to=50)
 library(ggplot2)
 library(data.table)
 g1=ggplot(data.table(x=c(20,50),y=logi(c(20,50))),aes(x=x,y=y))+stat_function(fun =logi, colour = "black",size=1.1)+theme(plot.title = element_text(size=11.5))+theme_classic()
 g1
 g1 = g1+annotate("rect", xmin=30, xmax=50, ymin=0.15, ymax=0.25,fill='gray30', alpha = .5)
 g1 = g1+labs(title='Initial guess Pr(Y=1|d)= 1/( 1 + exp(3.5-0.05*d) ). Target dose is for Pr(Y=1|d)=0.2.',x='Dose d',y='Pr(Y=1|d)')
 g1 = g1 + scale_x_continuous(breaks = c(20,30,40,50)) + scale_y_continuous(breaks = c(0.1,0.15,0.2,0.25))
 ggsave('initial_guess.png',g1,width = 8,height=6)
 
  