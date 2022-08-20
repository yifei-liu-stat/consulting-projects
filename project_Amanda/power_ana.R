library(glmnet)
library(ggplot2)

power_analysis<-function(alter.prob=c(1/2,1/3,1/6),null.prob=0.05,
                         insit.trials=c(5000,5000,5000),insit.ratio=c(1/3,1/3,1/3),
                         null.level=0.95,initial.sample.size=1000,rep.num=1000){
  sample.size<-initial.sample.size
  each.insit.size<-c(sample.size*insit.ratio[1:2],sample.size-sum(sample.size*insit.ratio[1:2]))
  total.trials<-c(rep(insit.trials[1],each.insit.size[1]),rep(insit.trials[2],each.insit.size[2]),rep(insit.trials[3],each.insit.size[3]))
  
  data.x<-factor(c(rep(0,each.insit.size[1]),rep(1,each.insit.size[2]),rep(2,each.insit.size[3])))
  
  #Null data (rep.num 1000)
  null.data<-lapply(1:rep.num,function(x) {
    succ<-Reduce(c,lapply(1:3,function(y) rbinom(each.insit.size[y],insit.trials[y],null.prob)))
    fail<-total.trials-succ
    data.frame('y_succ'=succ,'y_fail'=fail,'x'=data.x)
  })
  #Alter data
  alter.data<-lapply(1:rep.num,function(x) {
    succ<-Reduce(c,lapply(1:3,function(y) rbinom(each.insit.size[y],insit.trials[y],alter.prob[y])))
    fail<-total.trials-succ
    data.frame('y_succ'=succ,'y_fail'=fail,'x'=data.x)
    })
  
  ## deviance quantile under the Null model
  dev<-Reduce(rbind,lapply(1:rep.num,function(x) {
    null.dev<-deviance(glm(cbind(y_succ,y_fail)~1,family=binomial,data = null.data[[x]]))-
      deviance(glm(cbind(y_succ,y_fail)~x,family=binomial,data = null.data[[x]]))
    
    alter.dev<-deviance(glm(cbind(y_succ,y_fail)~1,family=binomial,data = alter.data[[x]]))-
      deviance(glm(cbind(y_succ,y_fail)~x,family=binomial,data = alter.data[[x]]))
    
    return(c(null.dev,alter.dev))
  }))
  
  critical.val<-quantile(dev[,1],probs = null.level)
  power<-sum(dev[,2]>=critical.val)/rep.num
  return(power)
}



arglist <- list(
  alter.prob = c(0.9, 0.9, 0.9),
  null.prob = 0.9,
  insit.trials = c(20, 20, 20) / 2,
  insit.ratio = c(115, 132, 10) / 257,
  null.level = 0.95,
  initial.sample.size = 100,
  rep.num = 1000
)
do.call("power_analysis", arglist)



### -------
power_analysis_plot<-function(alter.prob=c(1/2,1/3,1/6),null.prob=0.05,
                         insit.trials=c(20,50,50),insit.ratio=c(1/3,1/3,1/3),max.iter=1000,
                         null.level=0.95,alter.power=0.8,initial.sample.size=100,rep.num=1000,thres=0.01){
  record.sample.size=as.data.frame(t(as.matrix(c(initial.sample.size,0))))
  colnames(record.sample.size)=c('sample.size','power')
  sample.size<-initial.sample.size
  
  for(i in 1:max.iter){
  each.insit.size<-c(sample.size*insit.ratio[1:2],sample.size-sum(sample.size*insit.ratio[1:2]))
  total.trials<-c(rep(insit.trials[1],each.insit.size[1]),rep(insit.trials[2],each.insit.size[2]),rep(insit.trials[3],each.insit.size[3]))
  
  data.x<-factor(c(rep(0,each.insit.size[1]),rep(1,each.insit.size[2]),rep(2,each.insit.size[3])))
  
  #Null data (rep.num 1000)
  null.data<-lapply(1:rep.num,function(x) {
    succ<-Reduce(c,lapply(1:3,function(y) rbinom(each.insit.size[y],insit.trials[y],null.prob)))
    fail<-total.trials-succ
    data.frame('y_succ'=succ,'y_fail'=fail,'x'=data.x)
  })
  #Alter data
  alter.data<-lapply(1:rep.num,function(x) {
    succ<-Reduce(c,lapply(1:3,function(y) rbinom(each.insit.size[y],insit.trials[y],alter.prob[y])))
    fail<-total.trials-succ
    data.frame('y_succ'=succ,'y_fail'=fail,'x'=data.x)
  })
  
  ## deviance quantile under the Null model
  dev<-Reduce(rbind,lapply(1:rep.num,function(x) {
    null.dev<-deviance(glm(cbind(y_succ,y_fail)~1,family=binomial,data = null.data[[x]]))-
      deviance(glm(cbind(y_succ,y_fail)~x,family=binomial,data = null.data[[x]]))
    
    alter.dev<-deviance(glm(cbind(y_succ,y_fail)~1,family=binomial,data = alter.data[[x]]))-
      deviance(glm(cbind(y_succ,y_fail)~x,family=binomial,data = alter.data[[x]]))
    
    return(c(null.dev,alter.dev))
  }))
  
  critical.val<-quantile(dev[,1],probs = null.level)
  power<-sum(dev[,2]>=critical.val)/rep.num
  record.sample.size[i,2]=power
  
  # 2 fen fa
  if(i==1){
  if(alter.power-power>thres) {
    sample.size=2*sample.size
    record.sample.size=rbind(record.sample.size,c(sample.size,0))
  }
  else if(alter.power-power<(-thres)){
    sample.size=floor(sample.size/2)
    record.sample.size=rbind(record.sample.size,c(sample.size,0))
  }
    else if(abs(alter.power-power)<thres) {
      return(ggplot(data = record.sample.size,aes(x=sample.size,y=power))+geom_line()+geom_point())
    }
  }
  
  else{
    if((alter.power-power>thres) & (alter.power-record.sample.size[i-1,2]>thres)) {
      sample.size=2*sample.size
      record.sample.size=rbind(record.sample.size,c(sample.size,0))
    }
    else if((alter.power-power<(-thres)) & (alter.power-record.sample.size[i-1,2]<(-thres))){
      sample.size=floor(sample.size/2)
      record.sample.size=rbind(record.sample.size,c(sample.size,0))
    }
    
    else if(((alter.power-power<(-thres)) & (alter.power-record.sample.size[i-1,2]>thres))| 
            ((alter.power-power>thres) & (alter.power-record.sample.size[i-1,2]<(-thres)))){
      sample.size=floor((sample.size+record.sample.size[i-1,1])/2)
      record.sample.size=rbind(record.sample.size,c(sample.size,0))
    }
    else if(abs(alter.power-power)<thres) {
      return(ggplot(data = record.sample.size,aes(x=sample.size,y=power))+geom_line()+geom_point()+scale_x_continuous(breaks = record.sample.size$sample.size))
    }
  }
  }
}



arg.list <- list(
  alter.prob = c(0.9, 0.9, 0.95),
  null.prob = 0.9,
  insit.trials = c(10, 10, 10),
  insit.ratio = c(115, 132, 10) / 257,
  max.iter = 1000,
  null.level = 0.95,
  alter.power = 0.8,
  initial.sample.size = 10,
  rep.num = 2000,
  thres = 0.05
)

do.call("power_analysis_plot", arg.list)


power.institute <- function(null.prob, alter.prob, insit.trials)








