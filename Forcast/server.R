library(shiny)
library(forecast)
library(fma)
data(airpass, package="fma")
DData<-data.frame(Time=seq(1,144),airpass)
shinyServer(function(input,output,session){
  MyData <- reactive({
    if(input$RD1==2){
    inFile<-input$file1
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
    }else{
      DData
    }
})
  Col<-reactive({input$Col})
  Start<-reactive({input$Start})
  End<-reactive({input$End})
  Fre<-reactive({input$freq})
  Hstar<-reactive({input$Starth})
  Hend<-reactive({input$Endh})
  output$summary<-renderPrint({
    summary(MyData()[,Col()])
  })
  output$table<-renderDataTable({
    MyData()
  })
  output$PlotG<-renderPlot({
    if(is.null(MyData())!=T){
    plot(MyData()[,Col()],ylab="Observations")+lines(MyData()[,Col()])
    }
  })
  output$Plot1<-renderPlot({
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
   OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
     if(input$F2==1){
       forecast1<-ses(InsampleTs,h=length(OutsampleTs),level = input$CI)
       plot(forecast1,xlab="Time",ylab="Observations")
       lines(forecast1$fit,col="red",lty=2)
       lines(OutsampleTs,col="green",lty=2)
     }
     if(input$F2==2){
       forecast1<-ses(InsampleTs,h=length(OutsampleTs),initial = "simple",level = input$CI,alpha = input$AlphaS)
       plot(forecast1,xlab="Time",ylab="Observations")
       lines(forecast1$fit,col="red",lty=2)
       lines(OutsampleTs,col="green",lty=2)
     }
  })
  output$Plot2<-renderPlot({
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    
      if(input$F2==1){
        forecast1<-ses(InsampleTs,h=length(OutsampleTs),level = input$CI)
        barplot(accuracy(forecast1,OutsampleTs),legend=rownames(accuracy(forecast1,OutsampleTs)),main="Accuracy Test",beside=TRUE, col=c("red","blue"))
      }
      if(input$F2==2){
        forecast1<-ses(InsampleTs,h=length(OutsampleTs),initial = "simple",level = input$CI,alpha = input$AlphaS)
        barplot(accuracy(forecast1,OutsampleTs),legend=rownames(accuracy(forecast1,OutsampleTs)),main="Accuracy Test",beside=TRUE, col=c("red","blue"))
        }
  })
  output$accu1 <-renderTable({
    
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    
    if(input$F2==1){
      forecast1<-ses(InsampleTs,h=length(OutsampleTs),level = input$CI)
    }
    if(input$F2==2){
      forecast1<-ses(InsampleTs,h=length(OutsampleTs),initial = "simple",level = input$CI,alpha = input$AlphaS)
    }
    data.frame(Item=c('In Sample Error','Out Sample Error'),accuracy(forecast1,OutsampleTs))
    
    
  })
  
  output$Plot3<-renderPlot({
    
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    
      if(input$F4==1){
        forecast<-holt(InsampleTs,h=length(OutsampleTs),level = input$CI1)
        plot(forecast,xlab="Time",ylab="Observations")
        lines(forecast$fit,col="red",lty=2)
        lines(OutsampleTs,col="green",lty=2)
      }
      if(input$F4==2){
        forecast<-holt(InsampleTs,h=length(OutsampleTs),level = input$CI1,initial = "simple",alpha = input$AlphaL,beta = input$BetaL)
        plot(forecast,xlab="Time",ylab="Observations")
        lines(forecast$fit,col="red",lty=2)
        lines(OutsampleTs,col="green",lty=2)
      }
      
    })
  output$Plot4<-renderPlot({
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
      if(input$F4==1){
        forecast2<-holt(InsampleTs,h=length(OutsampleTs),level = input$CI1)
        barplot(accuracy(forecast2,OutsampleTs),legend=rownames(accuracy(forecast2,OutsampleTs)),main="Accuracy Test",beside=TRUE, col=c("red","blue"))
      }
      if(input$F4==2){
        forecast2<-holt(InsampleTs,h=length(OutsampleTs),level = input$CI1,initial = "simple",alpha = input$AlphaL,beta = input$BetaL)
        barplot(accuracy(forecast2,OutsampleTs),legend=rownames(accuracy(forecast2,OutsampleTs)),main="Accuracy Test",beside=TRUE, col=c("red","blue"))
      }
  })
  
  output$accu2 <- renderTable({
    
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    if(input$F4==1){
      forecast2<-holt(InsampleTs,h=length(OutsampleTs),level = input$CI1)
      
    }
    if(input$F4==2){
      forecast2<-holt(InsampleTs,h=length(OutsampleTs),level = input$CI1,initial = "simple",alpha = input$AlphaL,beta = input$BetaL)
    }
    
    data.frame(Item=c('In Sample Error','Out Sample Error'),accuracy(forecast2,OutsampleTs))
    
    
    
  })
  
  output$Plot5<-renderPlot({
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    
      if(input$F6==1){
        if(input$AM==1){
          forecast3<-hw(InsampleTs,h=length(OutsampleTs),level = input$CI2)
         plot(forecast3)
         lines(forecast3$fit,col="red",lty=2)
         lines(OutsampleTs,col="green",lty=2)
        }
        if(input$AM==2){
          forecast3<-hw(InsampleTs,h=length(OutsampleTs),level = input$CI2,"multiplicative")
          plot(forecast3)
          lines(forecast3$fit,col="red",lty=2)
          lines(OutsampleTs,col="green",lty=2)
        }
      }
      if(input$F6==2){
        if(input$AM==1){
          forecast3<-hw(InsampleTs,h=length(OutsampleTs),level = input$CI2,initial = "simple",alpha = input$AlphaH,beta = input$BetaH,gamma = input$GammaH)
          plot(forecast3)
          lines(forecast3$fit,col="red",lty=2)
          lines(OutsampleTs,col="green",lty=2)
        }
        if(input$AM==2){
          forecast3<-hw(InsampleTs,h=length(OutsampleTs),level = input$CI2,"multiplicative",initial = "simple",alpha = input$AlphaH,beta = input$BetaH,gamma = input$GammaH)
          plot(forecast3)
          lines(forecast3$fit,col="red",lty=2)
          lines(OutsampleTs,col="green",lty=2)
        }
      }
    
})
  output$Plot6<-renderPlot({
    
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    
    
      if(input$F6==1){
        if(input$AM==1){
          forecast3<-hw(InsampleTs,h=length(OutsampleTs),level = input$CI2)
          barplot(accuracy(forecast3,OutsampleTs),legend=rownames(accuracy(forecast3,OutsampleTs)),main="Accuracy",beside=TRUE, col=c("red","blue"))
        }
        if(input$AM==2){
          forecast3<-hw(InsampleTs,h=length(OutsampleTs),level = input$CI2,"multiplicative")
          barplot(accuracy(forecast3,OutsampleTs),legend=rownames(accuracy(forecast3,OutsampleTs)),main="Accuracy",beside=TRUE, col=c("red","blue"))
        }
      }
      if(input$F6==2){
        if(input$AM==1){
          forecast3<-hw(InsampleTs,h=length(OutsampleTs),level = input$CI2,initial = "simple",alpha = input$AlphaH,beta = input$BetaH,gamma = input$GammaH)
          barplot(accuracy(forecast3,OutsampleTs),legend=rownames(accuracy(forecast3,OutsampleTs)),main="Accuracy",beside=TRUE, col=c("red","blue"))
        }
        if(input$AM==2){
          forecast3<-hw(InsampleTs,h=length(OutsampleTs),level = input$CI2,"multiplicative",initial = "simple",alpha = input$AlphaH,beta = input$BetaH,gamma = input$GammaH)
          barplot(accuracy(forecast3,OutsampleTs),legend=rownames(accuracy(forecast3,OutsampleTs)),main="Accuracy",beside=TRUE, col=c("red","blue"))
        }
      }
    })
  output$accu3 <- renderTable({
    
    
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    
    
    if(input$F6==1){
      if(input$AM==1){
        forecast3<-hw(InsampleTs,h=length(OutsampleTs),level = input$CI2)
        
      }
      if(input$AM==2){
        forecast3<-hw(InsampleTs,h=length(OutsampleTs),level = input$CI2,"multiplicative")
      }
    }
    if(input$F6==2){
      if(input$AM==1){
        forecast3<-hw(InsampleTs,h=length(OutsampleTs),level = input$CI2,initial = "simple",alpha = input$AlphaH,beta = input$BetaH,gamma = input$GammaH)
      }
      if(input$AM==2){
        forecast3<-hw(InsampleTs,h=length(OutsampleTs),level = input$CI2,"multiplicative",initial = "simple",alpha = input$AlphaH,beta = input$BetaH,gamma = input$GammaH)
      }
    }
    data.frame(Item=c('In Sample Error','Out Sample Error'),accuracy(forecast3,OutsampleTs))
    
  })
  
  
  output$Plot7<-renderPlot({
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    Endpoint<-time(InsampleTs)[length(time(InsampleTs))]
    if(input$Trans==2){
      InsampleTs<-log(InsampleTs)
    }
    if(input$Trans==3){
      InsampleTs<-InsampleTs^0.5
    }
    x<-time(InsampleTs)
    New<-data.frame(x=seq(Endpoint,Endpoint+(1/Fre())*length(OutsampleTs),by=1/Fre()))
    reg<-lm(InsampleTs~x)
    pred<-predict(reg,New,interval = "prediction",level=input$CI3)
    if(input$Trans==2){
      plot(exp(InsampleTs),xlim=c(Start(),ceiling(Endpoint+(1/Fre())*length(OutsampleTs))),ylim=c(floor(min(exp(InsampleTs))),ceiling(exp(max(pred[,3])))))+points(New$x,exp(pred[,1]),pch=1)
      lines(New$x,exp(pred[,2]),lty=2,col="red")
      lines(New$x,exp(pred[,3]),lty=2,col="red")
      points(x,exp(reg$fitted.values),col="blue")
  
    }
    if(input$Trans==3){
      plot(InsampleTs^2,xlim=c(Start(),ceiling(Endpoint+(1/Fre())*length(OutsampleTs))),ylim=c(floor(min((InsampleTs)^2)),ceiling(max(pred[,3])^2)))+points(New$x,(pred[,1])^2,pch=1)
      lines(New$x,(pred[,2])^2,lty=2,col="red")
      lines(New$x,(pred[,3])^2,lty=2,col="red")
      points(x,reg$fitted.values^2,col="blue")
}
    if(input$Trans==1){
      plot(InsampleTs,xlim=c(Start(),ceiling(Endpoint+(1/Fre())*length(OutsampleTs))),ylim=c(floor(min(InsampleTs)),ceiling(max(pred[,3]))))+points(New$x,pred[,1],pch=1)
      lines(New$x,pred[,2],lty=2,col="red")
      lines(New$x,pred[,3],lty=2,col="red")
      abline(reg$coefficients,col="blue")
    }
  })
  output$plot8<-renderPlot({
    
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    Endpoint<-time(InsampleTs)[length(time(InsampleTs))]
    if(input$Trans==2){
      InsampleTs<-log(InsampleTs)
    }
    if(input$Trans==3){
      InsampleTs<-InsampleTs^0.5
    }
    x<-time(InsampleTs)
    New<-data.frame(x=seq(Endpoint,Endpoint+(1/Fre())*length(OutsampleTs),by=1/Fre()))
    reg<-lm(InsampleTs~x)
    pred<-predict(reg,New,interval = "prediction",level=input$CI3)
    if(input$Trans==2){
      FR<-exp(pred[,1])
      barplot(accuracy(FR,OutsampleTs),main="Outsample Accuracy of Forecasting")
    }
    if(input$Trans==3){
      FR<-pred[,1]^2
      barplot(accuracy(FR,OutsampleTs),main="Outsample Accuracy of Forecasting")
    }
    if(input$Trans==1){
      FR<-pred[,1]
      barplot(accuracy(FR,OutsampleTs),main="Outsample Accuracy of Forecasting")
    }
    })
  output$accu4 <- renderTable({
    
    
    
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    Endpoint<-time(InsampleTs)[length(time(InsampleTs))]
    if(input$Trans==2){
      InsampleTs<-log(InsampleTs)
    }
    if(input$Trans==3){
      InsampleTs<-InsampleTs^0.5
    }
    x<-time(InsampleTs)
    New<-data.frame(x=seq(Endpoint,Endpoint+(1/Fre())*length(OutsampleTs),by=1/Fre()))
    reg<-lm(InsampleTs~x)
    pred<-predict(reg,New,interval = "prediction",level=input$CI3)
    if(input$Trans==2){
      FR<-exp(pred[,1])
     
    }
    if(input$Trans==3){
      FR<-pred[,1]^2
      
    }
    if(input$Trans==1){
      FR<-pred[,1]
     
    }
    data.frame(Item=c('Out Sample Error'),accuracy(FR,OutsampleTs))
    
  })
  
  output$Plot9<-renderPlot({
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    forecast5<-forecast(nnetar(InsampleTs,level = input$CI4,h=length(OutsampleTs)))
    plot(forecast5)
    lines(forecast5$fit,col="red",lty=2)
    lines(OutsampleTs,col="green",lty=2)
    
  })
  output$Plot10<-renderPlot({
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    forecast5<-forecast(nnetar(InsampleTs,level = input$CI4,h=length(OutsampleTs)))
    barplot(accuracy(forecast5,OutsampleTs),legend=rownames(accuracy(forecast5,OutsampleTs)),main="Accuracy",beside=TRUE, col=c("red","blue"))

    })
  output$accu5 <- renderTable({
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    forecast5<-forecast(nnetar(InsampleTs,level = input$CI4,h=length(OutsampleTs)))
    data.frame(Item=c('In Sample Error','Out Sample Error'),accuracy(forecast5,OutsampleTs))
  })
  
  output$Plot11<- renderPlot({
    
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    forecast6<-auto.arima(InsampleTs)
    plot(forecast(forecast6,level = input$CI5,h=length(OutsampleTs)))
    lines(fitted(forecast6),col="red",lty=2)
    lines(OutsampleTs,col="green",lty=2)
    
    
  })
  output$Plot12 <- renderPlot({
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    forecast6<-forecast(auto.arima(InsampleTs),level = input$CI5,h=length(OutsampleTs))
    barplot(accuracy(forecast6,OutsampleTs),legend=rownames(accuracy(forecast6,OutsampleTs)),main="Accuracy",beside=TRUE, col=c("red","blue"))
    
    
  })
  output$accu6 <- renderTable({
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    forecast6<-forecast(auto.arima(InsampleTs),level = input$CI5,h=length(OutsampleTs))
    data.frame(Item=c('In Sample Error','Out Sample Error'),accuracy(forecast6,OutsampleTs))
  })

  ###
  
  output$Plot0<- renderPlot({
    
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    forecast0<-naive(InsampleTs,h=length(OutsampleTs))
    plot(forecast(forecast0,h=length(OutsampleTs)))
    lines(forecast0$fit,col="red",lty=2)
    lines(OutsampleTs,col="green",lty=2)
    
    
  })
  output$Plot00 <- renderPlot({
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    forecast0<-naive(InsampleTs,h=length(OutsampleTs))
    barplot(accuracy(forecast0,OutsampleTs),legend=rownames(accuracy(forecast0,OutsampleTs)),main="Accuracy",beside=TRUE, col=c("red","blue"))
    
    
  })
  output$accu0 <- renderTable({
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    forecast0<-naive(InsampleTs,h=length(OutsampleTs))
    data.frame(Item=c('In Sample Error','Out Sample Error'),accuracy(forecast0,OutsampleTs))
  })
  
  
   
})


