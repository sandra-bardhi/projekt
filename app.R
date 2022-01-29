#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## ui.R ## 
library(sm) 
library(shiny)
library(lubridate)
library(htmltools)
library(shinydashboard) 
library(forecastHybrid)
library(tidyverse) 
library(dplyr)
library("Hmisc") 
library(corrplot) 
library(ggplot2)
library("ggpubr")
library(sm) 
library(factoextra) 
library(NbClust)
library (cluster)
library (clustertend)
library("ggdendro")
library(gridExtra)
library(GGally)
library(magick)
library(car)
library(knitr)
library(plot3D)
library(scatterD3)
library(plotly)
library(tsibble)
library(fable)
library(feasts)
library(tsibbledata) 
library(corrplot)
library("zoo")
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(uroot)
library(dygraphs)
library(xts)
library(feasts)
library(tsibble)
library(tidyverse)
library(tsibbledata)
library(fable)
library(fpp3)
library(forecast)
library(ggplot2)
library(ggfortify) 
library(vars)

#require(MTS)
ds <-read.csv("https://raw.githubusercontent.com/sandra-bardhi/projekt/main/SeriaKohoreTS1.csv", encoding = "UTF-8") 
#ds <-read.csv("C:\\Users\\User\\Documents\\SeriaKohoreTS1.csv", encoding = "UTF-8")  

seria1<- data.frame(ds) 
seria1<-na.omit(seria1) 
ICK_T.ts<-ts(seria1$Gjithsej,start=c(2016,1),end=c(2020,12),frequency=12) 
USHQIME.ts<-ts(seria1$Ushqime,start=c(2016,1),end=c(2020,12),frequency=12)
VESHJE.ts<-ts(seria1$Veshjedhekepuce,start=c(2016,1),end=c(2020,12),frequency=12)
QERA.ts<-ts(seria1$Qera,start=c(2016,1),end=c(2020,12),frequency=12)
KREDIA.ts<-ts(seria1$KrediTOT,start=c(2016,1),end=c(2020,12),frequency=12) 
EURO.ts<-ts(seria1$EURO, start=c(2016,1), end=c(2020,12), frequency=12)
BMON.ts<-ts(seria1$BazaMonetare,start=c(2016,1), end=c(2020,12), frequency=12)
PARAJASHT.ts<-ts(seria1$ParajaJashteKorporataveDepozituese, start=c(2016,1), end=c(2020,12), frequency=12)
serite1<-data.frame(ICK_T.ts,USHQIME.ts,EURO.ts,PARAJASHT.ts,VESHJE.ts,KREDIA.ts,BMON.ts, QERA.ts) 
t=seq(1,length(ICK_T.ts))
MLinear<-lm(ICK_T.ts~t) 
train_ICK<-head(ICK_T.ts,48)
test_ICK <- tail(ICK_T.ts,12)
train_qera<-head(QERA.ts, 48)
test_qera<-tail(QERA.ts,12)
train_para<-head(PARAJASHT.ts,48)
test_para<-tail(PARAJASHT.ts,12) 
trainT<-data.frame(train_para, train_qera)

dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
) 
## app.R ##
library(shiny)
library(shinydashboard)

## app.R ##
library(shinydashboard)
if (interactive()) {
ui <- dashboardPage(
  dashboardHeader(title = "Projekti Final"), skin = "purple", 
  dashboardSidebar(
    sidebarMenu(
      menuItem("HYRJE", tabName = "hyrje", icon = icon("home")),
      menuItem("Perpunimi i te dhenave", tabName = "perpunimi", icon = icon("th")) ,
      menuItem("ANALIZA", tabName = "analiza", icon = icon("th")),
      menuItem("Interaktiv me shume", tabName = "interaktiv", icon=icon("th")), 
      menuItem("Training dhe Testing/Slider" , tabName ="tt", icon=icon("th")),
      menuItem("Parashikime dhe  modele ICK", tabName = "parashikime", icon = icon("th")), 
      menuItem("Miresia e modeleve", tabName = "miresia", icon = icon("th")),
      menuItem("Perfundime", tabName = "perfundime", icon =icon("th")), 
      menuItem("Referenca", tabName = "referenca", icon = icon("th")) 
      
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "hyrje",
              h2("Indeksi i ndryshimeve te cmimeve, PARASHIKIME DHE MODELE "),
              p("Indeksi i Çmimeve të Konsumit (IÇK) mat ndryshimin e nivelit të çmimeve të mallrave dhe 
shërbimeve të përdorura nga njësitë ekonomike familjare për konsum vetjak. Indeksi llogaritet 
                duke përdorur strukturën e konsumit final të njësive ekonomike familjare. Indeksi përdoret si 
                matësi zyrtar i inflacionit në Shqipëri.
                Inflacioni është një nga variablat më të rëndësishme e cila mund të ndikojë në të gjithë
                ekonominë dhe politikave që mund të ndiqen bazuar në ndryshimin e saj. Rrjedhimisht inflacioni
                ndikon dhe individin. Një norme e lartë e inflacionit është një indikator tregues për një ekonomi 
                në vështirësi. 
                Parashikimi i inflacionit është njëlloj i rëndësishëm si për politikat fiskale dhe për ato monetare.. 
                Agjentët privatë si dhe tregjet financiare gjithashtu ndryshojnë në bazë të inflacionit. Për të
                kuptuar në vija të përgjithshme ekonominë e një shteti përdoret indeksi i ndryshimeve të
                çmimeve. 
                Të dhënat e këtij projekti janë marrë nga INSTAT dhe Banka e Shqiperise.  
                Në këtë projekt do të trajtohen ndryshimet mujore të indeksit të çmimeve në Shqipëri nga viti 
                2016 deri në 2020 të cilat do të konsiderohen seri kohore. Për të parashikuar do të përdorim 
                modelim me shumë variabla dhe me frekuencë të lartë. Pra databaza e përdorur është me 
                frekuence mujore e cila konsiderohet si më e mirë për të parashikuar në terma afatgjatë. 
                Objektivi kryesor i këtij studimi është parashikimi sa më i saktë i IÇK. Databaza e zgjedhur
                perbehet nga 7 variabla te cilat do te trajtohen si seri kohore. Variabli kryesor eshte Ndyshimi i pergjithshem i ICK.  Do te shikojme se si
                kane ndikuar ndyshimet e serive te tjera kohore ne serie tone dhe do te bejme parashikimet perkatese."), 
              
              h3("Shikojme serite si me poshte"),
              fluidRow(
                box(dygraphOutput("plot31", height = 250)), 
                box(selectInput( inputId='serite' , label='zgjidh nje nga serite',
                                 choices=names(serite1)
                )), 
                
                submitButton("Shiko") 
              )
      ),
      
      # Second tab content
      tabItem(tabName = "perpunimi",
              h2("Perpunojme") ,
              h3("Ne databazen tone nuk ka vlera qe mungojne."),
              p("  ") ,
             fluidRow(
               box(numericInput("number", "Vendosni sa te dhena doni te shihni",1,100 )),
             tableOutput("plot4"),
             submitButton("Shiko") ) , 
             fluidRow(
               box(verbatimTextOutput("summary1"), width = 500)
               
               
             )
              
        
      ) ,
      # third tab contet
      tabItem(tabName = "analiza", 
              tabsetPanel(type = "tabs",
                          tabPanel("Varesite", 
                                   fluidRow(
                                     box(plotOutput("plot9", height = 250)), 
                                     box(selectInput( inputId='serite11' , label='zgjidh nje nga serite per te pare regresin ne lidhje me ICK',
                                                      choices=names(serite1)
                                     )), 
                                     submitButton("Shiko") 
                                   ),  
                                   fluidRow( 
                                     #plotOutput("plot5"), 
                                     
                                     radioButtons("serite12", "zgjidh paraqitjen e matrices se korrelacionit",
                                                  c("Me numra" = "nr", "e thjeshte"="thj")
                                     ), 
                                     submitButton("Shiko") ,
                                    box(plotOutput("plot5.1"), width =500)  
                                    
                                     ), 
                                 fluidRow(
                                   plotOutput("plot5.2")
                                 )  
                                   
                                   ),
                                   
                                   
                                  
                          tabPanel("Paraqitje grafike", 
                                   
                                  
                                   
                                    
                                   sidebarPanel(
                                     radioButtons("pgraf", "Zgjidhni Grafikun",
                                                  c("SeasonPlot" = "season", "MonthPlot"="month", "LagPlot"= "lag", "BoxPlot"="box", "ACFplot"= "acf", "ACFdiff"= "acfdiff")
                                     ),
                                     selectInput( inputId='seriteg' , label='zgjidh nje nga serite',
                                                      choices=names(serite1), width = 250
                                     ), 
                                     submitButton("Shiko")
                                     
                                     ),  
                                   fluidRow(
                                     box(plotOutput("plot14.5") ) 
                                      ), 
                                   
                                   fluidRow(
                                     p(" Ne boxplotet per secilen seri dhe shikojme nese kemi vlera te huaja dhe gjithashtu shikojme se si levizin te dhenat ."), 
                                     p(" Neqofte se lag-u i autokorrelacionit eshte i madh ne lidhje me vellimin e zgjedhjes, atehere vleresimet per acf
                                       nuk jane shume te sakta. Per kete arsye duhet qe lagu te jete me i vogel ose i barabarte me n/4, ku n eshte
                                       vellimi i zgjedhjes. Grafiku i autokorrelacioneve quhet
                                       korrelograma e te dhenave, e cila mund te ndertohet dhe ne rastin tone."), 
                                     p("Ndertimi i serise se diferencave yn=Xn-Xn-1 eshte nje menyre per te menjanuar trendin. Ne R perdorim funksionin diff() qe llogarit diferencen e pare ose ndryshimin e serise.")
                                     
                                   )
                                   
                                   
                                   ),
                          tabPanel("Dekompozim i serive", 
                                   fluidRow( 
                                     selectInput( inputId='seriteg1' , label='zgjidh nje nga serite',
                                                  choices=names(serite1), width = 250
                                     ), 
                                     
                                     radioButtons("dec", "Zgjidh menyren e dekomozimit",
                                                  c("Additive" = "additive", "Multiplicative"="multiplicative")
                                     ), 
                                     submitButton("Shiko"),
                                     box(plotOutput("plot15"), width =500) 
                                  
                                   ), 
                                   fluidRow(
                                     p("Nga grafiket me siper kuptojme me shume ne lidhje me trendin dhe sezonalitetin e serive")
                                   )
                                   )  
                          
                          
                          )),

      
    #tab4 content 
    tabItem(tabName = "parashikime" , 
            tabsetPanel(type = "tabs", 
                        tabPanel("Modeli1", h2("Modeli Linear per serine kohore ICK")  , 
                                             
                                               plotOutput("plot16") 
                                               
                                               ),
                        tabPanel("Modeli2" ,h2("ARIMA per ICK"), 
                                 plotOutput("plot19"),  
                                 verbatimTextOutput("plot19.1"),  
                                 plotOutput("plot19.1.1"), 
                                 h3("modeli me disa regresante Arima ICK me 2 regresante, ndryshimi i cmimit te qerase dhe vleres se parase"),
                                 plotOutput("plot19.2") 
                                 
                                 
                                 ),
                
                        
                        tabPanel("Modeli3", h2("Holt per ICK"), 
                                 plotOutput("plot17") 
                                #plotOutput("plot18")  
                               
                                 ),
                        tabPanel("Modeli4", h2("Modeli Neural Netwotk per ICK"),
                                 h3("Univariate"), 
                                 plotOutput("nnuni"), 
                                 h3("Me regresant "), 
                                
                                 selectInput("variable", "Variabli:",
                                             
                                             list("train_qera" = "train_qera", 
                                                  
                                                  "train_para" = "train_para"
                                                  
                                                  )), 
                                 submitButton("Shiko") ,
                                 plotOutput("NNMESHUMEPLOT") 
                                 
                                 
                                 ) 
                                 
                                  ,
                        tabPanel("Modeli5", h2("VAR"),  
                                 plotOutput("plot20"), 
                                 plotOutput("plot20.1")
                                 )
                        ) )
            
            ,
    #tab5 
    tabItem(tabName = "miresia",  
            h3("Modeli konsiderohet i mire nese mbjetjet jane zhurme e bardhe"),
            tabsetPanel(type = "tabs",
                        tabPanel("Miresi modeli1 , Arima Univariate. ", 
                                 verbatimTextOutput("plot22"), 
                                 plotOutput("plot23")
                                 ),
                        tabPanel("Miresia Modeli Holt", 
                                 plotOutput("plot18.0"), 
                                 verbatimTextOutput("miresiaHW")
                                 ),
                        tabPanel("Miresia Modeli Arima Multivariate" , 
                                 plotOutput("plot24"), 
                                 verbatimTextOutput("plot24.1")
                                 ), 
                        tabPanel("Miresia Modeli Neural Network " , 
                                 h3("Modeli univariate"), 
                                 plotOutput("plot21.0"),  
                                 verbatimTextOutput("plot21.01"),
                                 h3("Modeli me regresant ndryshimin e cmimit te qerase"), 
                                 plotOutput("plot21.1"), 
                                 verbatimTextOutput("plot21.11")
                                 
                                 ),
                        tabPanel("Miresia Modeli VAR", 
                                 verbatimTextOutput("plot20.2") 
                                 #verbatimTextOutput("plot20.3")
                                 ) 
                        )),
    #tab6 
    tabItem(tabName = "interaktiv", 
            #zgjedh variablin per parashikim 
            sidebarPanel(
              
              selectInput("variablikryesor", "Variabli per parashikim:",
                          
                          list("ICK" = "ICK_T.ts", 
                               
                               "Qera" = "QERA.ts",
                               
                               "Ushqime" = "USHQIME.ts")),  
              h4("KUJDES:"),
h5("MOS VENDOSNI DY SERI TE NJEJTA PER ARIMA ME REG", col='red'),
              
              selectInput("variablireg", "Variabli regresant per Arima:",
                          
                          list("ICK" = "ICK_T.ts", 
                               
                               "Qera" = "QERA.ts",
                               
                               "Ushqime" = "USHQIME.ts"
                               
                          )), 
              
              
              
              
              numericInput("ahead1", "Per sa muaj doni te parashikoni", 12),
              
              
              
              submitButton("Shikoni Parashikimin") 
              
            ),
            mainPanel(
              
              h3(textOutput("caption")),
              
              
              
              tabsetPanel(
                
                tabPanel("Parashikimi me sheshimin Eksponencial", plotOutput("etsForecastPlot1")), 
                
                tabPanel("Parashikimi sipas modelit Arima", plotOutput("arimaForecastPlot1")),
                tabPanel("Parashikimi Arima Me 1 regresant", plotOutput("arimaForecastPlotReg")), 
                
                tabPanel("Dekompozimi", plotOutput("dcompPlot1")), 
                tabPanel("Residualet EXP", plotOutput("residualsexp")), 
                tabPanel("Residualet ARIMA", plotOutput("residualsarima"))
                
              )
              
            )            
            
            ), 
#tab123 

tabItem("tt", 
        fluidRow(
          sliderInput(
            "slider1.1",
            label = h3("Train/Test Ndarja "),
            min = 0,
            max = 60,
            value = 48
          ), 
          
          selectInput( inputId='seritegg' , label='zgjidh nje nga serite',
                       choices=names(serite1), width = 250
          ), 
          
          
          submitButton("shiko")
          
        ), 
        
      fluidRow(  tabsetPanel(
          
          tabPanel("PARASHIKIMI ARIMA", plotOutput("plot26")),
        tabPanel("Residualet", plotOutput("plot27")) ,
        tabPanel("Miresia", verbatimTextOutput("plot28"))
        
        
        
        )) 
        
        ),


    #tab7

tabItem("perfundime" , 
        h4("modeli1, ARIMA"),
        verbatimTextOutput("plot25"),  
        h4("modeli2, HOLT") ,
      
        verbatimTextOutput("plot25.1") , 
        h4("Modeli3, ARIMA me reg QERA"),
        verbatimTextOutput("plot25.2"), 
        h4("modeli5, RRJETA NERVORE PA REGRESANTE"),
        verbatimTextOutput("plot25.3"), 
        h4("Perfundime:"), 
        h5("Duke marre ne konsiderate rezultatet e mesiperme dalim ne perfundimin se parashikimi me i mire per ICK esht Arima Univariate, (Pa regresante)")
        ),  

#tab8 

tabItem("referenca", 
        fluidPage(
          tags$h1("Referenca"),
          tags$a(href="https://otexts.com/fpp2/", 
                 "Forecasting Principles and practises, Rob J Hyndman"),  
          br(),
          tags$a(href="http://www.instat.gov.al/", 
                 "INSTAT"),  
          br(),
          tags$a(href="https://www.bankofalbania.org/Statistikat/Seri_Kohore/", 
                 "Banka e Shqiperise"),  
          br(),
          tags$a(href="https://github.com/rstudio-conf-2020/time-series-forecasting", 
                 "GITHUB, Rob J Hyndman"), 
          br(), 
          tags$a(href="https://www.analyticsvidhya.com/blog/2021/05/build-interactive-models-with-r-shiny/",
                 "www.analyticsvidhya.com" 
                 )
        
        
        ))
  
) ))}

server <- function(input, output) {
  set.seed(122) 
  
  getDataset1 <- reactive({
    
    if (input$variablikryesor=="ICK_T.ts")
      
    {
      
      return(serite1$ICK_T.ts)
      
    }
    
    else if (input$variablikryesor=="QERA.ts")
      
    {
      
      return(serite1$QERA.ts)
      
    }
    
    else
      
    {
      
      return(serite1$USHQIME.ts)
      
    }
    
  })  
  
  
  getDataset2 <- reactive({
    
    if (input$variablireg=="ICK_T.ts")
      
    {
      
      return(serite1$ICK_T.ts)
      
    }
    
    else if (input$variablireg=="QERA.ts")
      
    {
      
      return(serite1$QERA.ts)
      
    }
    
    else  
      
    {
      
      return(serite1$USHQIME.ts)
      
    }
    
  })
  
  
  output$etsForecastPlot1 <- renderPlot({
    
    fit <- ets(getDataset1())
    
    plot(forecast(fit, h=input$ahead1))
    
  }) 
  
  output$arimaForecastPlot1 <- renderPlot({
    
    fit <- auto.arima(getDataset1())
    
    plot(forecast(fit ,h=input$ahead1))
    
  }) 
  
  
  output$dcompPlot1 <- renderPlot({
    
    ds_ts <- ts(getDataset1(), frequency=12)
    
    f <- decompose(ds_ts)
    
    plot(f)
    
  })  
  
  output$arimaForecastPlotReg <- renderPlot({
    
    fit <- auto.arima(getDataset1(), xreg= getDataset2())
    
    plot(forecast(fit ,xreg= getDataset2(),h=input$ahead1))
    
  })  
  
  
  output$residualsexp <- renderPlot({
    
    fit <- ets(getDataset1())
    
    (checkresiduals(fit))
    
  })  
  
  output$residualsarima <- renderPlot({
    
    fit <- auto.arima(getDataset1())
    
    (checkresiduals(fit))
    
  }) 
  
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data) 
    }) 
  output$plot2 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data) 
    
  })  
  
  output$plot31<-renderDygraph(dygraph(serite1[,input$serite]))
                               
                  
output$plot4<-renderTable(head(serite1, input$number))  
  
#output$plot5<-renderPlot(corrplot(cor(serite1)))  

output$plot5.1<-renderPlot( 
  switch(input$serite12,
                 "thj" =(corrplot(cor(serite1))) ,
                 "nr" = (corrplot(cor(serite1), method="number"))  )) 

output$plot5.2<-renderPlot(ggpairs(coredata(serite1)))                 
                
                  
output$plot6<-renderPlot(plot(forecast(auto.arima(ts(serite1 [, input$serite123]))))) 
output$plot7<-renderDygraph(dygraph(ICK_T.ts))  
output$plot8<-renderPlot(plot(ICK_T.ts))
output$summary1<-renderPrint(summary(serite1))  
output$plot9<-renderPlot(ggscatter(serite1, x = "ICK_T.ts", y = input$serite11, 
                                   add = "reg.line", conf.int = TRUE, 
                                   cor.coef = TRUE, cor.method = "pearson") ) 
output$plot10<-renderPlot(boxplot(serite1$ICK_T.ts~ cycle(serite1$ICK_T.ts)))
output$plot11<-renderPlot(ggseasonplot(ICK_T.ts, col=c("red","blue","green","purple"), year.labels=TRUE,main="ICK ne muaj"))

 # per te kuptuar sezonalitetin. 

output$plot12<-renderPlot(ggseasonplot(ICK_T.ts, col=c("red","blue","green"), year.labels=TRUE,main="ICK ne muaj ",polar=TRUE))

output$plot13<-renderPlot(monthplot(ICK_T.ts))
output$plot14<-renderPlot(gglagplot(ICK_T.ts,15))  
output$plot14.5<-renderPlot( 
  switch(input$pgraf,
         "season" =ggseasonplot(serite1[,input$seriteg], col=c("red","blue","green","purple"), year.labels=TRUE,main="ICK ne muaj") ,
         "month" = monthplot(serite1[,input$seriteg]), 
         "lag" = gglagplot(serite1[,input$seriteg],15), 
         "box"= boxplot(serite1[,input$seriteg]~ cycle(serite1[,input$seriteg])), 
         "acf"= acf(serite1[,input$seriteg]), 
         "acfdiff"= acf(diff(serite1[,input$seriteg]))
         )  )  
output$plot15<-renderPlot( 
  switch(input$dec,
         "additive" =plot(decompose(serite1[,input$seriteg1],type="additive")) ,
         "multiplicative" = plot(decompose(serite1[,input$seriteg1],type="multiplicative"))  ))  

output$plot16<-renderPlot(
  
  plot(t,ICK_T.ts, col="purple")+  
  abline(MLinear,col="red") 
  )

HW1= HoltWinters(train_ICK, seasonal = "add") 
output$plot17<-renderPlot(plot(forecast(HW1,  h=24, level=c(80,95)))) # nivel besimi 80-95 
#output$plot18<-renderPlot(acf((HW1$residuals, na.action=na.pass)) )
output$plot18.0<-renderPlot(checkresiduals(HW1))
output$miresiaHW<-renderPrint(accuracy(forecast(HW1), test_ICK))  
sarima.ICK<-auto.arima(train_ICK) 
output$plot19<-renderPlot(plot(forecast(sarima.ICK)))  
ICK1_acf<-acf(ICK_T.ts,15,type="correlation", plot= T) 
output$plot19.1.1<-renderPlot(acf(ICK_T.ts, 15, type = "correlation", plot=T ))
output$plot19.1<-renderPrint(summary(sarima.ICK))   
#modeli me disa regresante Arima 
arima_Per2<- auto.arima(train_ICK, xreg=train_qera+train_para) 
#arima_per21<-auto.arima(train_ICK, xreg= input$serite13)  
#shifeme KUJDES!!!!!!
output$plot19.13<-renderPlot( reactive(plot(forecast(auto.arima(ts(train_ICK, xreg = trainT[, input$serite1234]))))))

output$plot19.2<-renderPlot(plot(forecast(arima_Per2, xreg= train_qera+ train_para))) 
train_ushqime <-head(USHQIME.ts,48)
tail_ushqime<- tail(USHQIME.ts,12)
x<-cbind(train_ICK,train_qera, train_ushqime)
plot.ts(x , main = "", xlab = "") 
output$plot20<-renderPlot( plot.ts(x , main = "", xlab = "")) 
fitvar1= VAR(x, p=1, type="both")
output$plot20.1<-renderPlot(plot(forecast(fitvar1))  ) 
output$plot20.2<-renderPrint(summary(fitvar1)) 
#output$plot20.3<-renderPrint(accuracy(forecast(fitvar1)))
fit2<- nnetar(train_ICK) 
fit3 = nnetar(train_ICK, xreg = train_qera) 

getDataset <- reactive({
  
  if (input$variable=="train_qera")
    
  {
    
    return(train_qera)
    
  }
  
  else (input$variable=="train_para")
    
  {
    
    return(train_para)
    
  }
  
 
  
})

output$NNMESHUMEPLOT <- renderPlot({
  
   
  fit4 = nnetar(train_ICK, xreg = getDataset()) 
  
  plot(forecast(fit4, xreg= getDataset()))
  
})

output$nnuni<-renderPlot(plot(forecast(fit2)))
output$plot21<-renderPlot(plot( forecast(fit3, xreg=train_qera, PI = F))) 
output$plot21.0<-renderPlot(checkresiduals(fit2)) 
output$plot21.1<-renderPlot(checkresiduals(fit3)) 
#y= reactive(input$serite13)
output$plot21.01<-renderPrint(accuracy(forecast(fit2), test_ICK)) 
output$plot21.11<-renderPrint(accuracy(forecast(fit3, xreg=train_qera ), test_ICK))  
#output$plot21.12<-renderPlot(plot(forecast(fit4, xreg=train_parashikime$y, PI= F)))
output$plot22<-renderPrint(Box.test(sarima.ICK$residuals)) 
output$plot23<-renderPlot(checkresiduals(sarima.ICK))
output$plot24<- renderPlot(checkresiduals(arima_Per2)) 
output$plot24.1<-renderPrint(accuracy(forecast(arima_Per2, xreg=train_qera+train_para ), test_ICK))
modeli1<-accuracy(forecast(sarima.ICK), test_ICK)
modeli2<-(accuracy(forecast(HW1), test_ICK))
modeli3<-(accuracy(forecast(arima_Per2,xreg= train_qera),test_ICK)) 
modeli5<-(accuracy(forecast(fit2, PI = F),test_qera))
#Gabimet<- data.frame(modeli1,modeli2,modeli3,modeli5)    
output$plot25<-renderText(modeli1)
output$plot25.1<-renderText(modeli2) 
output$plot25.2<-renderText(modeli3)  
output$plot25.3<-renderText(modeli5) 
output$plot26<-renderPlot(plot(forecast(auto.arima(head(serite1 [, input$seritegg], input$slider1.1)))))
output$plot27<-renderPlot(checkresiduals(auto.arima(head(serite1 [, input$seritegg], input$slider1.1))))
output$plot28<-renderPrint(accuracy(forecast(auto.arima(head(serite1 [, input$seritegg], input$slider1.1))), tail(serite1 [, input$seritegg], 60 - input$slider1.1) ))
} 

shinyApp(ui=ui, server=server) 
         

