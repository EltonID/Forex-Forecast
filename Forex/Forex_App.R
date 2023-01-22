# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(quantmod)
library(tsbox)
library(prophet)

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("FOREX"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select pair indice
                    selectInput(inputId = "pair", label = strong("Forex Instrument"),
                                choices = list(USDJPY="USDJPY",GBPUSD="GBPUSD",GBPNZD="GBPNZD",USDZAR="USDZAR"),
                                selected = "USDZAR"),
                    
                    # Select date range to be plotted
                    dateRangeInput("date", strong("Select Range"), start = "2022-01-01", end = "2022-12-31",
                                   min = "2022-01-01", max = "2022-12-31"),
                    # Select forecast range
                    sliderInput(inputId = "num",
                                label=strong("Forecasting Range (days)"),
                                min = 1,max = 30, value=5)
                    ),
                
                  # Outputs
                  mainPanel(
                    plotOutput(outputId = "lineplot", height = "400px"),
                    textOutput(outputId = "desc"),
                    verbatimTextOutput("predict"),
                    plotOutput(outputId = "components", height = "400px"),
                    tags$a(href = "www.oanda.com", "Source: OANDA Forex Indices Trading", target = "_blank")
                  )
                )
)

###
# Define server function
server <- function(input, output) {
#get model  
model<-reactive({
    getSymbols("USD/JPY",src="oanda") 
    getSymbols("GBP/USD",src="oanda") 
    getSymbols("GBP/NZD",src="oanda") 
    getSymbols("USD/ZAR",src="oanda") 
    
    
    if (input$pair=="USDJPY"){
      df=USDJPY}
    else if (input$pair=="GBPUSD"){
      df=GBPUSD}
    else if (input$pair=="GBPNZD"){
      df=GBPNZD}
    else {
      df=USDZAR}
#data frame
df<-ts_df(df)
names(df)<-c("ds","y")
df=df[(df$ds>=input$date[1] & df$ds<=input$date[2]),]
prophet(df,daily.seasonality = TRUE,weekly.seasonality = T, yearly.seasonality = TRUE)
})
#future data frame  
future<-reactive({
   make_future_dataframe(model(), periods = input$num, freq='day')
})
#forecast
forecast<-reactive({predict(model(), future())})
#plot components                
comp<-reactive({prophet_plot_components(model(), forecast())})
#plot time serie & forecast
plt<-reactive({plot(model(), forecast(),ylab="Rate")})
  
# Create output functions
  output$lineplot <- renderPlot({
    plt()})
  
  output$desc<-renderText({
    "Forecast" })
  
  output$predict<-renderPrint({
    tail((forecast()[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]))[-1,]
    })
  
  output$components <- renderPlot({
    comp()
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)