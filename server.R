library(shiny)
library(plotly)
library(pscl)

# Define server logic required to draw the plots
shinyServer(function(input, output,session) {

  #obtain value for shading
  endval<-reactive({
    #get inputs
    alphaVal<-input$alpha
    lambdaVal<-input$lambda
    shade<-input$Animate   
    if (is.na(alphaVal)){alphaVal<-1}
    if (is.na(lambdaVal)){lambdaVal<-1}

    #get plot range
    if(alphaVal>2){
      maxVal<-lambdaVal/(alphaVal-1)+3*sqrt(lambdaVal^2/((alphaVal-1)^2*(alphaVal-2)))
    } else {
      maxVal<-10
    }  
    
    #shading seq
    end<-(shade)/100*maxVal+0.0002
    
  })
  
  
  #plot gamma PDF
  output$gammaPDF<-renderPlotly({
    #get inputs
    alphaVal<-input$alpha
    lambdaVal<-input$lambda
    shade<-input$Animate  
    if (is.na(alphaVal)){alphaVal<-1}
    if (is.na(lambdaVal)){lambdaVal<-1}

    #max value for graph
    maxVal<-alphaVal/lambdaVal+3*sqrt(alphaVal/lambdaVal^2)
    
    #for shading
    end<-endval()
    shortseq<-seq(from=1/end,to=maxVal,length=100)
    
    #Plot points
    df<-data.frame(x=seq(from=0,to=maxVal,by=0.001),y=dgamma(seq(from=0,to=maxVal,by=0.001),shape=alphaVal,rate=lambdaVal))
    
    #cases for plots to look ok
    if(end==0.0002){
      plot_ly(df,x=~x,y=~y,type="scatter",mode='lines') %>% 
        layout(title="Plot of Gamma PDF",xaxis=list(title="x"),yaxis=list(title="f(x)"),showlegend=FALSE)
    } else {
      plot_ly(df,x=~x,y=~y,type="scatter",mode='lines') %>% 
        layout(title="Plot of Gamma PDF",xaxis=list(title="x"),yaxis=list(title="f(x)"),showlegend=FALSE)%>%
        add_lines(x=shortseq,y=dgamma(shortseq,shape=alphaVal,rate=lambdaVal),fill="tozeroy")
    }
    
  })
  
  
  #plot gamma CDF
  output$gammaCDF<-renderPlotly({
    #get inputs
    alphaVal<-input$alpha
    lambdaVal<-input$lambda
    shade<-input$Animate  
    if (is.na(alphaVal)){alphaVal<-1}
    if (is.na(lambdaVal)){lambdaVal<-1}

    #max for graphing
    maxVal<-alphaVal/lambdaVal+3*sqrt(alphaVal/lambdaVal^2)
    
    #for shading
    end<-endval()
    
    #Plot points
    df<-data.frame(x=seq(from=0,to=maxVal,by=0.001),y=pgamma(seq(from=0,to=maxVal,by=0.001),shape=alphaVal,rate=lambdaVal))
    
    #cases for plots to show nicely
    if(end==0.0002){
      plot_ly(df,x=~x,y=~y,type="scatter",mode='lines') %>% layout(title="Plot of Gamma CDF",xaxis=list(title="x"),yaxis=list(title="F(x)"))%>%
        layout(showlegend = FALSE)
    } else {
      plot_ly(df,x=~x,y=~y,type="scatter",mode='lines') %>% layout(title="Plot of Gamma CDF",xaxis=list(title="x"),yaxis=list(title="F(x)"))%>%
        layout(showlegend = FALSE)%>% 
        add_lines(x=c(0,1/end),y=rep(pgamma(1/end,shape=alphaVal,rate=lambdaVal),2),color=I("blue"))%>%
        add_lines(x=rep(1/end,2),y=c(0,pgamma(1/end,shape=alphaVal,rate=lambdaVal)),color=I("blue"))
    }
    
  })
  
  
  #plot of inverse gamma
  output$invgammaPDF<-renderPlotly({
    #get inputs
    alphaVal<-input$alpha
    lambdaVal<-input$lambda
    shade<-input$Animate   
    if (is.na(alphaVal)){alphaVal<-1}
    if (is.na(lambdaVal)){lambdaVal<-1}

    #max for plotting
    if(alphaVal>2){
      maxVal<-lambdaVal/(alphaVal-1)+3*sqrt(lambdaVal^2/((alphaVal-1)^2*(alphaVal-2)))
    } else {
      maxVal<-10
    }  
    
    #shading seq
    end<-endval()
    shortseq<-seq(from=0.0001,to=end,length=100)
    
    #Plot points
    df<-data.frame(x=seq(from=0.001,to=maxVal,by=0.001),y=densigamma(seq(from=0.001,to=maxVal,by=0.001),alpha=alphaVal,beta=lambdaVal))
    
    plot_ly(df,x=~x,y=~y,type="scatter",mode='lines') %>% layout(title="Plot of Inverse Gamma PDF",xaxis=list(title="x"),yaxis=list(title="f(x)"))%>%
      add_lines(x=shortseq,y=densigamma(shortseq,alpha=alphaVal,beta=lambdaVal),fill="tozeroy")%>%layout(showlegend = FALSE)
    
  })
  
  
  #plot of inverse gamma CDF
  output$invgammaCDF<-renderPlotly({
    #get inputs
    alphaVal<-input$alpha
    lambdaVal<-input$lambda
    shade<-input$Animate  
    if (is.na(alphaVal)){alphaVal<-1}
    if (is.na(lambdaVal)){lambdaVal<-1}

    #max value for plot
    if(alphaVal>2){
      maxVal<-lambdaVal/(alphaVal-1)+3*sqrt(lambdaVal^2/((alphaVal-1)^2*(alphaVal-2)))
    } else {
      maxVal<-10#qigamma(0.95,alpha=alphaVal,beta=lambdaVal)
    }  
    
    #end point for inv gamma
    end<-endval()
    
    #Plot points
    df<-data.frame(x=seq(from=0.001,to=maxVal,by=0.001),y=pigamma(seq(from=0.001,to=maxVal,by=0.001),alpha=alphaVal,beta=lambdaVal))
    
    plot_ly(df,x=~x,y=~y,type="scatter",mode='lines') %>% layout(title="Plot of Inverse Gamma CDF",xaxis=list(title="x"),yaxis=list(title="f(x)"))%>%
      add_lines(x=c(0,end),y=rep(pigamma(end,alpha=alphaVal,beta=lambdaVal),2),color=I("blue"))%>%
      add_lines(x=rep(end,2),y=c(0,pigamma(end,alpha=alphaVal,beta=lambdaVal)),color=I("blue"))%>%  layout(showlegend = FALSE)
    
  })
  
})
