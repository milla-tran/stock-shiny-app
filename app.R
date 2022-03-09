library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(tidyverse)
library(tidyquant)
library(shinydashboard)
library(dplyr)


tickers <- c("AAPL","MSFT","GOOG","AMZN","TSLA","PYPL","EA")


prices <- tq_get(tickers, 
                 get  = "stock.prices",
                 from = today()-months(12),
                 to   = today(),
                 complete_cases = F) %>%
  select(symbol,date,close)

benchmarks <- c("^NDX","^GSPC") # Nasdaq100 and SP500
bench <- tq_get(benchmarks,
                get  = "stock.prices",
                from = today()-months(12),
                to   = today()) %>%
  select(symbol,date,close)


# Define UI for application that draws a histogram
ui <- fluidPage(theme = bslib::bs_theme(bg = "#010779", 
                                        fg = "white", 
                                        base_font = "Source Sans Pro"),
                img(src='stockgraphic.jpg', align = "center"),
                
                # Application title
                titlePanel("Stock Prices"),
                
                # Choosing stocks 
                sidebarLayout(
                  sidebarPanel(width = 3,
                               
                               # Let user pick stocks
                               pickerInput(
                                 inputId = "stocks",
                                 label = h4("Please select the stocks you would like to study"),
                                 choices = c(
                                   "Apple"           = tickers[1], 
                                   "Microsoft"       = tickers[2],
                                   "Google"          = tickers[3],
                                   "Amazon"          = tickers[4],
                                   "Tesla"           = tickers[5],
                                   "Paypal"          = tickers[6],
                                   "Electronic Arts" = tickers[7]),
                                 selected = tickers,   
                                 options = list(`actions-box` = TRUE), 
                                 multiple = T
                              
                  ),
                  
                  # Pick time period
                  radioButtons("period", label = h4("Period"),
                               choices = list("1 month" = 1, "3 months" = 2, "6 months" = 3, "12 months" = 4, "YTD" = 5), 
                               selected = 4
                  ),
                  
                  # Pick benchmark
                  radioButtons("benchmark", label = h4("Benchmark"),
                               choices = list("SP500" = 1, "Nasdaq100" = 2,"None" = 3),
                               selected = 3)
                ),
                  # Plot results
                  mainPanel(
                    plotlyOutput("plot", height=800),
                    
                  )
                  
                )

)

server <- function(input, output) {
  
  
  observeEvent(c(input$period,input$stocks,input$benchmark), {
    
    prices <- prices %>%
      filter(symbol %in% input$stocks)
    
    if (input$period == 1) {
      prices <- prices %>%
        filter(
          date >= today()-months(1)) }
    
    if (input$period == 2) {
      prices <- prices %>%
        filter(date >= today()-months(3)) }
    
    if (input$period == 3) {
      prices <- prices %>%
        filter(date >= today()-months(6)) }
    
    if (input$period == 5) {
      prices <- prices %>%
        filter(year(date) == year(today())) }
    
    if (input$benchmark == 1) {
      bench <- bench %>%
        filter(symbol=="^GSPC",
               date >= min(prices$date))
      prices <- rbind(prices,bench) }
    
    if (input$benchmark == 2) {
      bench <- bench %>%
        filter(symbol=="^NDX",
               date >= min(prices$date))
      prices <- rbind(prices,bench) }
  
  # create output
  output$plot <- renderPlotly({
    print(
    ggplotly(prices %>% filter(symbol %in% input$stocks) %>%
               group_by(symbol) %>%
               mutate(init_close = if_else(date == min(date),close,NA_real_)) %>%
               mutate(value = round(100 * close / sum(init_close,na.rm=T),1)) %>%
               ungroup() %>%
               ggplot(aes(date, value,colour = symbol)) +
               geom_line(size = 1, alpha = .9) +
               theme_minimal(base_size=16) +
               labs(title = "Stock Prices over Time", x = "Date", y = "Price") +
               theme(plot.title = element_text(size = 35, colour = "white"),
                     axis.title = element_blank(),
                     axis.title.x = element_text(size = 20, colour = "white"),
                     axis.title.y = element_text(size = 20, colour = "white"),
                     plot.background = element_rect(fill = "#010779"),
                     panel.background = element_rect(fill="#010779"),
                     panel.grid = element_blank(),
                     legend.text = element_text(colour="white"))
           )
      )
  })  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
  
