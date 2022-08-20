library(shiny)
library(ggplot2)
#library(parallel)


# Define UI ----
ui <- fluidPage(
  titlePanel("Power Analysis"),
  sidebarLayout(
    sidebarPanel(
      h4('# Students with Diasbilities per institution type'),
      fluidRow(
        column(3,
               numericInput("t1", 
                            h5("Public"), 
                            min=1,
                            max=NA,
                            value = 15
               )),
        column(4,
               numericInput("t2", 
                            h5("Private (Profit)"), 
                            min=1,
                            max=NA,
                            value = 15
               )),
        column(5,
               numericInput("t3", 
                            h5("Private (Non Profit)"), 
                            min=1,
                            max=NA,
                            value = 15
               )),
        
        
      ),
      h4('---------------------------------------------------------'),
      fluidRow(
        column(4, 
               numericInput("initial_sample_size", 
                            h4("Sample Size"), 
                            min=1,
                            max=NA,
                            value = 100
               )),
        column(5,
               numericInput("null_level", 
                            h4("Significant Level"), 
                            min=0,
                            max=1,
                            value = 0.05
               )
               
        ),
        
        column(3,
               numericInput("rep", 
                            h4("Replicates"), 
                            min=0,
                            max=NA,
                            value = 500
               )
               
               
        )),
      h4("---------------------------------------------------------"),
      fluidRow(
        column(4,
               h4("Option"),
               checkboxInput("checkbox", "Calculate sample size?", value = FALSE),
               ),
        conditionalPanel(
          condition = "input.checkbox == true",
          column(4,
          numericInput("alter_level", 
                       h4("Power Level"), 
                       min=0,
                       max=1,
                       value = 0.8)
        ),
        column(3, 
               numericInput("thres", 
                            h4("Threshold"), 
                            min=0,
                            max=1,
                            value = 0.02
               ))
        )),
      h4('---------------------------------------------------------'),
      h4('Effect Size (Graduation Rate per Institution Type)'),
      fluidRow(
        column(3,
               numericInput("p1", 
                            h5("Public"), 
                            min=0,
                            max=1,
                            value = 0.9
               )),
        column(4,
               numericInput("p2", 
                            h5("Private (Profit)"), 
                            min=0,
                            max=1,
                            value = 0.95
               )),
        column(5,
               numericInput("p3", 
                            h5("Private (non Profit)"), 
                            min=0,
                            max=1,
                            value = 0.95
               ))
      ),
      
      
      fluidRow(column(3,offset = 4,
      actionButton('Submit','Run Graph',icon("paper-plane"), 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
      )
),
    
    
    
    mainPanel(
    h3('Power Graph'),
    plotOutput("power"),
    )
  )
)

#write up function
power_analysis_plot<-function(alter.prob=c(0.9,0.9,0.95),null.prob=0.9,
                              insit.trials=c(10,10,10),insit.ratio=c(115,132,10)/257,max.iter=1000,
                              null.level=0.95,alter.power=0.8,initial.sample.size=100,rep.num=200,thres=0.02,curve=T){
  record.sample.size=as.data.frame(t(as.matrix(c(initial.sample.size,0))))
  colnames(record.sample.size)=c('sample.size','power')
  sample.size<-initial.sample.size
  
  for(i in 1:max.iter){
    each.insit.size<-c(sample.size*insit.ratio[1:2],sample.size-sum(sample.size*insit.ratio[1:2]))
    total.trials<-c(rep(insit.trials[1],each.insit.size[1]),rep(insit.trials[2],each.insit.size[2]),rep(insit.trials[3],each.insit.size[3]))
    
    data.x<-factor(c(rep(0,each.insit.size[1]),rep(1,each.insit.size[2]),rep(2,each.insit.size[3])))
    
    # #Null data (rep.num 1000)
    # null.data<-lapply(1:rep.num,function(x) {
    #   succ<-Reduce(c,lapply(1:3,function(y) rbinom(each.insit.size[y],insit.trials[y],null.prob)))
    #   fail<-total.trials-succ
    #   data.frame('y_succ'=succ,'y_fail'=fail,'x'=data.x)
    # })
    
    #Alter data
    alter.data<-lapply(1:rep.num,function(x) {
      succ<-Reduce(c,lapply(1:3,function(y) rbinom(each.insit.size[y],insit.trials[y],alter.prob[y])))
      fail<-total.trials-succ
      data.frame('y_succ'=succ,'y_fail'=fail,'x'=data.x)
    })
    
    # ## deviance quantile under the Null model
    # dev<-Reduce(rbind,lapply(1:rep.num,function(x) {
    #   null.dev<-deviance(glm(cbind(y_succ,y_fail)~1,family=binomial,data = null.data[[x]]))-
    #     deviance(glm(cbind(y_succ,y_fail)~x,family=binomial,data = null.data[[x]]))
      
    #   alter.dev<-deviance(glm(cbind(y_succ,y_fail)~1,family=binomial,data = alter.data[[x]]))-
    #     deviance(glm(cbind(y_succ,y_fail)~x,family=binomial,data = alter.data[[x]]))
      
    #   return(c(null.dev,alter.dev))
    # }))
    
    # critical.val<-quantile(dev[,1],probs = null.level)
    # power<-sum(dev[,2]>=critical.val)/rep.num
    # record.sample.size[i,2]=power
    
    
    ## p value based on chisquare test
    pvalues <-Reduce(rbind,lapply(1:rep.num,function(x) {
      model.null <- glm(cbind(y_succ,y_fail)~1,family=binomial,data = alter.data[[x]])
      model.alter <- glm(cbind(y_succ,y_fail)~x,family=binomial,data = alter.data[[x]])
      p.value <- anova(model.alter, model.null, test = "Chisq")[2, 5]
      return(p.value)
    }))
    pvalues <- pvalues[, 1]
    
    power <- mean(pvalues <= 1 - null.level)
    record.sample.size[i,2] <- power
    
    
    
    # no curve
    if(curve == F){
      record.sample.size<-data.frame(record.sample.size,'Legend'=c(paste('Size',sep = ' ',record.sample.size[1,1],',','Power',round(record.sample.size[1,2],3))
      ))
      return(ggplot(data = record.sample.size,aes(x=sample.size,y=power))+geom_point(size=3,aes(color=Legend,shape=Legend))+theme_bw()
             +scale_x_continuous(breaks = record.sample.size$sample.size)+scale_y_continuous(breaks = round(record.sample.size$power,3))+scale_shape_manual(values=16))
    }
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
        record.sample.size<-data.frame(record.sample.size,'Legend'=c(paste('Size',sep = ' ',record.sample.size[1,1],',','Power',round(record.sample.size[1,2],3))
                                                                           ))
          return(ggplot(data = record.sample.size,aes(x=sample.size,y=power))+geom_point(size=3,aes(color=Legend,shape=Legend))+theme_bw()
               +scale_x_continuous(breaks = record.sample.size$sample.size)+scale_y_continuous(breaks = round(record.sample.size$power,3))+scale_shape_manual(values=16))
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
        len<-nrow(record.sample.size)
        if(len>2)
        {
          name1<-paste('Size',sep = ' ',record.sample.size[1,1],',','Power',round(record.sample.size[1,2],3))
          name2<-paste('Size',sep = ' ',record.sample.size[len,1],',','Power',round(record.sample.size[len,2],3))
          record.sample.size<-data.frame(record.sample.size,'Legend'=c(name1
                                                                      ,rep('Other',len-2),name2))
        
        col<-c(name1 = "red", "Other" = "black", name2='red')
        names(col)=c(name1,'Other',name2)
        sha<-c(name1 = 7, "Other" = 16, name2 = 8)
        names(sha)=names(col)
        return(ggplot(data = record.sample.size,aes(x=sample.size,y=power,group=1))+geom_line()+geom_point(size=2,aes(color=Legend,shape=Legend))+theme_bw()
               +scale_x_continuous(breaks = record.sample.size$sample.size)+scale_y_continuous(breaks = round(record.sample.size$power,3))+
                scale_color_manual(values = col)+
                  scale_shape_manual(values= sha))}
        else if(len==2)
        {record.sample.size<-data.frame(record.sample.size,'Legend'=c(paste('Size',sep = ' ',record.sample.size[1,1],',','Power',round(record.sample.size[1,2],3)),
                                                                      paste('Size',sep = ' ',record.sample.size[len,1],',','Power',round(record.sample.size[len,2],3))))
        return(ggplot(data = record.sample.size,aes(x=sample.size,y=power,group=1))+geom_line()+geom_point(size=2,aes(color=Legend,shape=Legend))+theme_bw()
               +scale_x_continuous(breaks = record.sample.size$sample.size)+scale_y_continuous(breaks = round(record.sample.size$power,3))+
                                                                                                 scale_color_manual(values = c('red','red'))+scale_shape_manual(values=c(7,8)))}
      }
    }
  }
}


# Define server logic ----
server <- function(input, output) {
  plot_out<-eventReactive(input$Submit,{power_analysis_plot(alter.prob = c(input$p1,input$p2,input$p3),
                      null.prob = 0.9,
                      insit.trials = c(input$t1,input$t2,input$t3),
                      null.level = 1-input$null_level,
                      alter.power = input$alter_level,
                      rep.num = input$rep,
                      initial.sample.size = input$initial_sample_size,
                      thres = input$thres,curve = input$checkbox)})
  

  output$power<-renderPlot({
   plot_out()
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

