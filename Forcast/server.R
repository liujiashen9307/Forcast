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
  output$table<-renderTable({
    MyData()[seq(1:input$Row),]
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
    if(input$F1==1){
      H <- length(OutsampleTs)  
      Quant<-length(InsampleTs)
      forecasts <- rep(0,H)
      if(input$F2==1){
        for (i in (1:H)) {
          OneData<-MyData()[(1:(Quant+i)),Col()]
          training <- ts(OneData,start=Start(),frequency = Fre())
          ses <- ses(training,h=1)
          forecasts[i] <- ses$mean
          plot(ses,xlim=c(Hstar(),(Hstar()+i/Fre())))
          }
        forecasts<-ts(forecasts,start=Hstar(),freq=Fre())
        lines(forecasts,col="red",lty=2,lwd=2)
        }
      if(input$F2==2){
        for (i in (1:H)) {
          OneData<-MyData()[(1:(Quant+i)),Col()]
          training <- ts(OneData,start=Start(),frequency = Fre())
          ses <- ses(training,h=1,initial = "simple",alpha = input$AlphaS)
          forecasts[i] <- ses$mean
          plot(ses,xlim=c(Hstar(),(Hstar()+i/Fre())))
        }
        forecasts<-ts(forecasts,start=Hstar(),freq=Fre())
        lines(forecasts,col="red",lty=2,lwd=2)
      }
    }
   if(input$F1==2){
     if(input$F2==1){
       forecast1<-ses(InsampleTs,h=input$num,level = input$CI)
       plot(forecast1,xlab="Time",ylab="Observations")
       lines(forecast1$fit,col="red",lty=2)
       lines(OutsampleTs,col="green",lty=2)
     }
     if(input$F2==2){
       forecast1<-ses(InsampleTs,h=input$num,initial = "simple",level = input$CI,alpha = input$AlphaS)
       plot(forecast1,xlab="Time",ylab="Observations")
       lines(forecast1$fit,col="red",lty=2)
       lines(OutsampleTs,col="green",lty=2)
     }
   }
  })
  output$Plot2<-renderPlot({
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    if(input$F1==1){
      
      H <- length(OutsampleTs)  
      Quant<-length(InsampleTs)
      forecasts <- rep(0,H)
      if(input$F2==1){
        for (i in (1:H)) {
          OneData<-MyData()[(1:(Quant+i)),Col()]
          training <- ts(OneData,start=Start(),frequency = Fre())
          ses <- ses(training,h=1)
          forecasts[i] <- ses$mean
        }
        forecasts<-ts(forecasts,start=Hstar(),freq=Fre())
        barplot(accuracy(forecasts,OutsampleTs),main="One-Step-Forecasting Accuracy")
      }
      if(input$F2==2){
        for (i in (1:H)) {
          OneData<-MyData()[(1:(Quant+i)),Col()]
          training <- ts(OneData,start=Start(),frequency = Fre())
          ses <- ses(training,h=1,initial = "simple",alpha = input$AlphaS)
          forecasts[i] <- ses$mean
        }
        forecasts<-ts(forecasts,start=Hstar(),freq=Fre())
        barplot(accuracy(forecasts,OutsampleTs),main="One-Step-Forecasting Accuracy")
      }
      }
    if(input$F1==2){
      if(input$F2==1){
        forecast1<-ses(InsampleTs,h=input$num,level = input$CI)
        barplot(accuracy(forecast1,OutsampleTs),main="Accuracy Test")
      }
      if(input$F2==2){
        forecast1<-ses(InsampleTs,h=input$num,initial = "simple",level = input$CI,alpha = input$AlphaS)
        barplot(accuracy(forecast1,OutsampleTs),main="Accuracy Test")
        }
     
    }
  })
  output$Plot3<-renderPlot({
    
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    if(input$F3==1){
      H <- length(OutsampleTs)  
      Quant<-length(InsampleTs)
      forecasts <- rep(0,H)
      if(input$F4==1){
        for (i in (1:H)) {
          OneData<-MyData()[(1:(Quant+i)),Col()]
          training <- ts(OneData,start=Start(),frequency = Fre())
          les <- holt(training,h=1)
          forecasts[i] <- les$mean
          plot(les,xlim=c(Hstar(),(Hstar()+i/Fre())))
        }
        forecasts<-ts(forecasts,start=Hstar(),freq=Fre())
        lines(forecasts,col="red",lty=2,lwd=2)
      }
      if(input$F4==2){
        for (i in (1:H)) {
          OneData<-MyData()[(1:(Quant+i)),Col()]
          training <- ts(OneData,start=Start(),frequency = Fre())
          les <- holt(training,h=1,initial = "simple",alpha = input$AlphaL,beta=input$BetaL)
          forecasts[i] <- les$mean
          plot(les,xlim=c(Hstar(),(Hstar()+i/Fre())))
        }
        forecasts<-ts(forecasts,start=Hstar(),freq=Fre())
        lines(forecasts,col="red",lty=2,lwd=2)
      }
    }
    if(input$F3==2){
      if(input$F4==1){
        forecast<-holt(InsampleTs,h=input$num1,level = input$CI1)
        plot(forecast,xlab="Time",ylab="Observations")
        lines(forecast$fit,col="red",lty=2)
        lines(OutsampleTs,col="green",lty=2)
      }
      if(input$F4==2){
        forecast<-holt(InsampleTs,h=input$num1,level = input$CI1,initial = "simple",alpha = input$AlphaL,beta = input$BetaL)
        plot(forecast,xlab="Time",ylab="Observations")
        lines(forecast$fit,col="red",lty=2)
        lines(OutsampleTs,col="green",lty=2)
      }
      }
    })
  output$Plot4<-renderPlot({
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    if(input$F3==1){
      H <- length(OutsampleTs)  
      Quant<-length(InsampleTs)
      forecasts <- rep(0,H)
      if(input$F4==1){
        for (i in (1:H)) {
          OneData<-MyData()[(1:(Quant+i)),Col()]
          training <- ts(OneData,start=Start(),frequency = Fre())
          les <- holt(training,h=1)
          forecasts[i] <- les$mean
        }
        forecasts<-ts(forecasts,start=Hstar(),freq=Fre())
        barplot(accuracy(forecasts,OutsampleTs),main="One-Step-Forecast Accuracy")
      }
      if(input$F4==2){
        for (i in (1:H)) {
          OneData<-MyData()[(1:(Quant+i)),Col()]
          training <- ts(OneData,start=Start(),frequency = Fre())
          les <- holt(training,h=1,initial = "simple",alpha = input$AlphaL,beta=input$BetaL)
          forecasts[i] <- les$mean
        }
        forecasts<-ts(forecasts,start=Hstar(),freq=Fre())
        barplot(accuracy(forecasts,OutsampleTs),main="One-Step-Forecast Accuracy")
      }
      }
    if(input$F3==2){
      if(input$F4==1){
        forecast2<-holt(InsampleTs,h=input$num1,level = input$CI1)
        barplot(accuracy(forecast2,OutsampleTs),main="Accuracy Test")
      }
      if(input$F4==2){
        forecast2<-holt(InsampleTs,h=input$num1,level = input$CI1,initial = "simple",alpha = input$AlphaL,beta = input$BetaL)
        barplot(accuracy(forecast2,OutsampleTs),main="Accuracy Test")
      }
    }
  })
  
  output$Plot5<-renderPlot({
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    if(input$F5==1){
      H <- length(OutsampleTs)  
      Quant<-length(InsampleTs)
      forecasts <- rep(0,H)
      if(input$F6==1){
        for (i in (1:H)) {
          OneData<-MyData()[(1:(Quant+i)),Col()]
          training <- ts(OneData,start=Start(),frequency = Fre())
          if(input$AM==1){
          holw <- hw(training,h=1)
          }else{
            holw<-hw(training,h=1,seasonal = "m")
          }
          forecasts[i] <- holw$mean
          plot(holw,xlim=c(Hstar(),(Hstar()+i/Fre())))
        }
        forecasts<-ts(forecasts,start=Hstar(),freq=Fre())
        lines(forecasts,col="red",lty=2,lwd=2)
      }
      if(input$F6==2){
        for (i in (1:H)) {
          OneData<-MyData()[(1:(Quant+i)),Col()]
          training <- ts(OneData,start=Start(),frequency = Fre())
          if(input$AM==1){
            holw <- hw(training,h=1,initial = "simple",alpha = input$AlphaH,beta = input$BetaH,gamma = input$GammaH)
          }else{
            holw<-hw(training,h=1,seasonal = "m",initial = "simple",alpha = input$AlphaH,beta = input$BetaH,gamma = input$GammaH)
          }
          forecasts[i] <- holw$mean
          plot(holw,xlim=c(Hstar(),(Hstar()+i/Fre())))
        }
        forecasts<-ts(forecasts,start=Hstar(),freq=Fre())
        lines(forecasts,col="red",lty=2,lwd=2)
      }
      }
    if(input$F5==2){
      if(input$F6==1){
        if(input$AM==1){
          forecast3<-hw(InsampleTs,h=input$num2,level = input$CI2)
         plot(forecast3)
         lines(forecast3$fit,col="red",lty=2)
         lines(OutsampleTs,col="green",lty=2)
        }
        if(input$AM==2){
          forecast3<-hw(InsampleTs,h=input$num2,level = input$CI2,"multiplicative")
          plot(forecast3)
          lines(forecast3$fit,col="red",lty=2)
          lines(OutsampleTs,col="green",lty=2)
        }
      }
      if(input$F6==2){
        if(input$AM==1){
          forecast3<-hw(InsampleTs,h=input$num2,level = input$CI2,initial = "simple",alpha = input$AlphaH,beta = input$BetaH,gamma = input$GammaH)
          plot(forecast3)
          lines(forecast3$fit,col="red",lty=2)
          lines(OutsampleTs,col="green",lty=2)
        }
        if(input$AM==2){
          forecast3<-hw(InsampleTs,h=input$num2,level = input$CI2,"multiplicative",initial = "simple",alpha = input$AlphaH,beta = input$BetaH,gamma = input$GammaH)
          plot(forecast3)
          lines(forecast3$fit,col="red",lty=2)
          lines(OutsampleTs,col="green",lty=2)
        }
      }
    }
})
  output$Plot6<-renderPlot({
    
    TotalTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
    InsampleTs<-window(TotalTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
    OutsampleTs<-window(TotalTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
    if(input$F5==1){
      H <- length(OutsampleTs)  
      Quant<-length(InsampleTs)
      forecasts <- rep(0,H)
      if(input$F6==1){
        for (i in (1:H)) {
          OneData<-MyData()[(1:(Quant+i)),Col()]
          training <- ts(OneData,start=Start(),frequency = Fre())
          if(input$AM==1){
            holw <- hw(training,h=1)
          }else{
            holw<-hw(training,h=1,seasonal = "m")
          }
          forecasts[i] <- holw$mean
        }
        forecasts<-ts(forecasts,start=Hstar(),freq=Fre())
        barplot(accuracy(forecasts,OutsampleTs),main="One-Step-Forecasting Accuracy")
      }
      if(input$F6==2){
        for (i in (1:H)) {
          OneData<-MyData()[(1:(Quant+i)),Col()]
          training <- ts(OneData,start=Start(),frequency = Fre())
          if(input$AM==1){
            holw <- hw(training,h=1,initial = "simple",alpha = input$AlphaH,beta = input$BetaH,gamma = input$GammaH)
          }else{
            holw<-hw(training,h=1,seasonal = "m",initial = "simple",alpha = input$AlphaH,beta = input$BetaH,gamma = input$GammaH)
          }
          forecasts[i] <- holw$mean
        }
        forecasts<-ts(forecasts,start=Hstar(),freq=Fre())
        barplot(accuracy(forecasts,OutsampleTs),main="One-Step-Forecasting Accuracy")
      }
    }
    if(input$F5==2){
      if(input$F6==1){
        if(input$AM==1){
          forecast3<-hw(InsampleTs,h=input$num2,level = input$CI2)
          barplot(accuracy(forecast3,OutsampleTs),main="Accuracy")
        }
        if(input$AM==2){
          forecast3<-hw(InsampleTs,h=input$num2,level = input$CI2,"multiplicative")
          barplot(accuracy(forecast3,OutsampleTs),main="Accuracy")
        }
      }
      if(input$F6==2){
        if(input$AM==1){
          forecast3<-hw(InsampleTs,h=input$num2,level = input$CI2,initial = "simple",alpha = input$AlphaH,beta = input$BetaH,gamma = input$GammaH)
          barplot(accuracy(forecast3,OutsampleTs),main="Accuracy")
        }
        if(input$AM==2){
          forecast3<-hw(InsampleTs,h=input$num2,level = input$CI2,"multiplicative",initial = "simple",alpha = input$AlphaH,beta = input$BetaH,gamma = input$GammaH)
          barplot(accuracy(forecast3,OutsampleTs),main="Accuracy")
        }
      }
    }
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
    New<-data.frame(x=seq(Endpoint,Endpoint+(1/Fre())*input$num3,by=1/Fre()))
    reg<-lm(InsampleTs~x)
    pred<-predict(reg,New,interval = "prediction",level=input$CI3)
    if(input$Trans==2){
      plot(exp(InsampleTs),xlim=c(Start(),ceiling(Endpoint+(1/Fre())*input$num3)),ylim=c(floor(min(exp(InsampleTs))),ceiling(exp(max(pred[,3])))))+points(New$x,exp(pred[,1]),pch=1)
      lines(New$x,exp(pred[,2]),lty=2,col="red")
      lines(New$x,exp(pred[,3]),lty=2,col="red")
      points(x,exp(reg$fitted.values),col="blue")
  
    }
    if(input$Trans==3){
      plot(InsampleTs^2,xlim=c(Start(),ceiling(Endpoint+(1/Fre())*input$num3)),ylim=c(floor(min((InsampleTs)^2)),ceiling(max(pred[,3])^2)))+points(New$x,(pred[,1])^2,pch=1)
      lines(New$x,(pred[,2])^2,lty=2,col="red")
      lines(New$x,(pred[,3])^2,lty=2,col="red")
      points(x,reg$fitted.values^2,col="blue")
}
    if(input$Trans==1){
      plot(InsampleTs,xlim=c(Start(),ceiling(Endpoint+(1/Fre())*input$num3)),ylim=c(floor(min(InsampleTs)),ceiling(max(pred[,3]))))+points(New$x,pred[,1],pch=1)
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
    New<-data.frame(x=seq(Endpoint,Endpoint+(1/Fre())*input$num3,by=1/Fre()))
    reg<-lm(InsampleTs~x)
    pred<-predict(reg,New,interval = "prediction",level=input$CI3)
    if(input$Trans==2){
      FR<-exp(pred[,1])
      barplot(accuracy(FR,OutsampleTs),main="Accuracy of Forecasting")
    }
    if(input$Trans==3){
      FR<-pred[,1]^2
      barplot(accuracy(FR,OutsampleTs),main="Accuracy of Forecasting")
    }
    if(input$Trans==1){
      FR<-pred[,1]
      barplot(accuracy(FR,OutsampleTs),main="Accuracy of Forecasting")
    }
    })
})


