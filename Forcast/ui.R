library(shiny)
shinyUI(navbarPage(strong("Time_Series Forecasting Tool"),
                  tabPanel("Data Summary",
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("RD1",label=h3(strong("Data Type")),choices = list("Demo"=1,"Your Data"=2),selected = 1),
                                
                                conditionalPanel(condition="input.RD1==2",fileInput('file1', h4(strong('Choose csv File')),
                                          accept=c('text/csv', 
                                                   'text/comma-separated-values,text/plain', 
                                                   '.csv')),
                                checkboxInput('header', 'Header', TRUE),
                                radioButtons('sep', 'Separator',
                                             c(Comma=',',
                                               Semicolon=';',
                                               Tab='\t'),
                                             ','),
                                radioButtons('quote', 'Quote',
                                             c(None='',
                                               'Double Quote'='"',
                                               'Single Quote'="'"),
                                             '"'))
                                ,
                                numericInput("Col",strong("Column to Analysis"),value = 2)
                                ,
                                numericInput("Row",strong("Number of Rows Display"),value = 3)
                                ,
                                
                                h4(strong("Time Series Setting")),
                                numericInput("Start",strong("Start Period"),value = 1949),
                            
                                numericInput("freq",strong("Frequency"),value=12),
                                h4(strong("Holdout Sample Setting")),
                              numericInput("Starth",strong("Start Period"),value = 1960),
                              numericInput("Endh",strong("End Period"),value = 1961)
                              ),
                              
                             
                              mainPanel(fluidRow(
                                column(10,h3("Summary"),verbatimTextOutput("summary")),
                                column(10,h3("Table"),tableOutput("table")),
                                column(10,h3("Plot"),plotOutput("PlotG"))
                               
                              )
                                
                              )
                              
                            ))
                   ,
                   
            navbarMenu(   "Forecasting Method"    ,tabPanel("Simple Exponential Smoothing",
                            sidebarLayout(
                              sidebarPanel(
                        
                               radioButtons("F1",h4(strong("Test Method or Forecast")),choices = list("One-Step-Ahead Forecasting"=1,"Forecasting"=2),selected = 1),
                                
                            
                                conditionalPanel(condition="input.F1==2",h4(strong("Forecasting Options")),numericInput("num",label = "Forecasting Horizon",value = 12)
                                ,
                                sliderInput("CI",label="Confience Interval",min=0.01,max=0.99,value=0.9))
                                ,
                                radioButtons("F2",h4(strong("Determinant or Optimal")),choices = list("Optimal"=1,"Determinant"=2),selected = 1)
                                ,
                                conditionalPanel(condition="input.F2==2",sliderInput("AlphaS","Your Alpha Value",min=0,max=1,value=0.2))
                                ),
                              mainPanel(
                                fluidRow(
              
                                  column(10,plotOutput("Plot1")),
                                  column(10,plotOutput("Plot2"))
                                )
                              )
                            )),
                   tabPanel("Linear Exponential Smoothing",
                            sidebarLayout(
                              (sidebarPanel(
                                
                                radioButtons("F3",h4(strong("Test Method or Forecast")),choices = list("One-Step-Ahead Forecasting"=1,"Forecasting"=2),selected = 1),
                                
                                
                                conditionalPanel(condition="input.F3==2",h4(strong("Forecasting Options")),numericInput("num1",label = "Forecasting Horizon",value = 12)
                                                 ,
                                                 sliderInput("CI1",label="Confience Interval",min=0.01,max=0.99,value=0.9))
                                ,
                                radioButtons("F4",h4(strong("Determinant or Optimal")),choices = list("Optimal"=1,"Determinant"=2),selected = 1)
                                ,
                                conditionalPanel(condition="input.F4==2",sliderInput("AlphaL","Your Alpha Value",min=0,max=1,value=0.2),sliderInput("BetaL","Your Beta Value",min = 0,max = 1,value = 0.2))
                              )
                                ),
                                mainPanel(
                                  fluidRow(
                                    
                                    column(10,plotOutput("Plot3")),
                                    column(10,plotOutput("Plot4"))
                                  )
                                )
                              )),
                   tabPanel("Holt Winter Method",
                            sidebarLayout(
                              sidebarPanel(
                                
                                
                                radioButtons("F5",h4(strong("Test Method or Forecast")),choices = list("One-Step-Ahead Forecasting"=1,"Forecasting"=2),selected = 1),
                                
                                
                                conditionalPanel(condition="input.F5==2",h4(strong("Forecasting Options")),numericInput("num2",label = "Forecasting Horizon",value = 12)
                                                 ,
                                                 sliderInput("CI2",label="Confience Interval",min=0.01,max=0.99,value=0.9))
                                ,
                                radioButtons("F6",h4(strong("Determinant or Optimal")),choices = list("Optimal"=1,"Determinant"=2),selected = 1),
                                radioButtons("AM",h4(strong("Additive or Multiplicative")),choices=list("Additive"=1,"Multiplicative"=2),selected=1)
                                ,
                                conditionalPanel(condition="input.F6==2",sliderInput("AlphaH","Your Alpha Value",min=0,max=1,value=0.2),sliderInput("BetaH","Your Beta Value",min = 0,max = 1,value = 0.2),sliderInput("GammaH","Your Gamma Value",min=0,max=1,value=0.2))
                                
                                
                              ),
                              mainPanel(
                                fluidRow(
                                  
                                  column(10,plotOutput("Plot5")),
                                  column(10,plotOutput("Plot6"))
                                )
                              )
                            )))
                   ,
                   tabPanel("Statistical Method",
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("Trans",h4(strong("Data Transformation")),choices = list("Normal"=1,"Logarithm"=2,"Powered"=3),selected = 1),
                                numericInput("num3","Forecasting Horizon",value = 12),
                                sliderInput("CI3","Confidence Interval",min = 0.01,max = 0.99,value = 0.9)
                                
                                ),
                              mainPanel(
                                fluidRow(
                                  column(10,plotOutput("Plot7")),
                                  column(10,plotOutput("plot8"))
                                )
                              )
                            ))
                   
                   ))