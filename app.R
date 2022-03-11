
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(tidyverse)
library(tidyquant)
library(shinydashboard)
library(dplyr)
library(tseries)
library(tsibble)
library(ggthemes)

Stocks <- tq_exchange("NASDAQ")

tickers2 <- paste(Stocks$symbol, "-", Stocks$company)

#tickers <- c("AAPL","MSFT","GOOG","AMZN","TSLA","PYPL","EA")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(bg = "#010779",
                          fg = "white",
                          base_font = "Source Sans Pro"),
  img(src = 'stockgraphic.jpg', align = "center"),
  
  # Application title
  titlePanel("Stock Prices"),
  
  # Choosing stocks
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Let user pick stocks
      # pickerInput(
      #   inputId = "stocks",
      #   label = h4("Please select the stocks you would like to study"),
      #   choices = c(
      #     "Apple"           = tickers[1],
      #     "Microsoft"       = tickers[2],
      #     "Google"          = tickers[3],
      #     "Amazon"          = tickers[4],
      #     "Tesla"           = tickers[5],
      #     "Paypal"          = tickers[6],
      #     "Electronic Arts" = tickers[7]),
      #   selected = tickers,
      #   options = list(`actions-box` = TRUE),
      #   multiple = T
      # ),
      
      selectInput(
        inputId = 'stocks',
        label = 'Search for tickers or companies',
        choices = tickers2,
        selected = NULL,
        multiple = TRUE
        # allow for multiple inputs
        #options = list(create = FALSE) # if TRUE, allows newly created inputs
      ),
      
      # dateRangeInput(inputId = "date", 
      #                label = "Select date range", 
      #                start = today()-months(12),
      #                end = today(), 
      #                min = today()-months(240),
      #                max = today(), 
      #                format = "yyyy-mm-dd", 
      #                startview = "month", 
      #                weekstart = 0,
      #                language = "en", 
      #                separator = " to ", 
      #                width = NULL),
      
      dateInput(inputId = "date1", label = "Select start date", 
                value = today()-months(12), 
                min = today()-months(240), 
                max = today(),
                format = "yyyy-mm-dd", 
                startview = "month", 
                weekstart = 0,
                language = "en", width = NULL),
      
      dateInput(inputId = "date2", label = "Select start date", 
                value = today(), 
                min = today()-months(240), 
                max = today(),
                format = "yyyy-mm-dd", 
                startview = "month", 
                weekstart = 0,
                language = "en", width = NULL),
      
      
      submitButton(text = "Submit search criteria")
    ),
    
    # Plot results
    mainPanel(#plotlyOutput("plot", height = 800),),
      tabsetPanel(
        tabPanel(
          "Price Highs", 
          plotOutput("highs", height = 600)
        )),
      tabsetPanel(
        tabPanel(
          "Closing Prices",  
      plotOutput('plot'))
  )
)))

server <- function(input, output) {
  
  observe({
    
    # prices <- tq_get(
    #   input$stocks,
    #   get  = "stock.prices",
    #   from = today() - months(12),
    #   to   = today(),
    #   complete_cases = F
    # ) %>%
    #   select(symbol, date, close)
    
    if(is.null(input$stocks) == TRUE){
      prices <- tq_get(
        c("AAPL","MSFT","NFLX", "GOOG", "AMZN"),
        get  = "stock.prices",
        from = input$date1,
        to   = input$date2,
        complete_cases = F
      ) %>%
        select(symbol, date, close, high)
    }
    
    if(is.null(input$stocks) == FALSE){
      # input$stocks <- separate(input$stocks,
      #                            col = input$stocks[,1],
      #                            into = c(ticker,name),
      #                            sep = " - ",
      #                            remove = TRUE,
      #                            convert = FALSE,
      #                            extra = "warn",
      #                            fill = "warn"
      # )
      
      tickers3 <- str_split_fixed(input$stocks, " - ", 2)
      
      prices <- tq_get(
        tickers3[,1],
        get  = "stock.prices",
        from = input$date1,
        to   = input$date2,
        complete_cases = F
      ) %>%
        select(symbol, date, close, high)
    }
    
    #autoplot(prices)
    
    output$plot <- 
      
      #renderPlot({   
      #plot_stock <- input$stocks==Stocks[,1]
      #autoplot(plot_stock, .vars = close)
      
      renderPlot(
        prices %>%
          group_by(symbol) %>%
          ggplot(aes(x = date, y = close, color = symbol)) +
          geom_line(size=1) +
          labs(title = "Stock Line Chart", y = "Closing Price", x = "", caption = "If blank, stock ticker is not valid") +
          theme_fivethirtyeight())
    
    output$highs <- renderPlot(
      prices %>%
        group_by(symbol) %>%
        ggplot(aes(x = date, y = high, color = symbol)) + 
        geom_line(size=1, alpha = .9) +
        #theme_minimal(base_size=16) +
        labs(title = "Stock Price Highs Over Time", y = "Price", x = "Date", caption = "If blank, stock ticker is not valid") +
        theme_fivethirtyeight()
        # theme(plot.title = element_text(size = 35, colour = "white", family = "Lucida Sans"),
        #       axis.title = element_blank(),
        #       axis.title.x = element_text(size = 20, colour = "white", family = "Lucida Sans"),
        #       axis.title.y = element_text(size = 20, colour = "white", family = "Lucida Sans"),
        #       legend.title = element_text(colour="#010779", size=15),
        #       plot.background = element_rect(fill = "#010779"),
        #       panel.background = element_rect(fill="#010779"),
        #       panel.grid = element_blank(),
        #       legend.text = element_text(colour="white", family = "Lucida Sans"))
    )
    
    # output$highs <- renderPlotly({
    #   ggplotly(prices %>% filter(symbol %in% input$stocks) %>% 
    #              ggplot(aes(date, high, colour = symbol)) +
    #              geom_line(size = 1, alpha = .9) +
    #              theme_minimal(base_size=16) +
    #              labs(title = "Stock Price Highs over Time", x = "Date", y = "Price") +
    #              theme(plot.title = element_text(size = 35, colour = "white", family = "Source Sans Pro"),
    #                    axis.title = element_blank(),
    #                    axis.title.x = element_text(size = 20, colour = "white", family = "Source Sans Pro"),
    #                    axis.title.y = element_text(size = 20, colour = "white", family = "Source Sans Pro"),
    #                    legend.title = element_text(colour="#010779", size=15),
    #                    plot.background = element_rect(fill = "#010779"),
    #                    panel.background = element_rect(fill="#010779"),
    #                    panel.grid = element_blank(),
    #                    legend.text = element_text(colour="white", family = "Source Sans Pro"))
    #   )
    # })
    
    # renderPlot(  
    #   ggplot(data = prices, aes(x = date, y = close, group = symbol, color = "red")) + 
    #     geom_point() + theme_bw() )
    
    # create output
    # output$plot <- renderPlotly({
    #   print(
    #     ggplotly(
    #       prices %>%
    #         group_by(symbol) %>%
    #         mutate(init_close = if_else(date == min(date), close, NA_real_)) %>%
    #         mutate(value = round(
    #           100 * close / sum(init_close, na.rm = T), 1
    #         )) %>%
    #         ungroup() %>%
    #         ggplot(aes(date, value, colour = symbol)) +
    #         geom_line(size = 1, alpha = .9) +
    #         theme_minimal(base_size = 16) +
    #         theme(
    #           axis.title = element_blank(),
    #           plot.background = element_rect(fill = "black"),
    #           panel.background = element_rect(fill = "black"),
    #           panel.grid = element_blank(),
    #           legend.text = element_text(colour = "white")
    #         )
    #     )
    #   )
    # })
  }
  )
}

# Run the application
shinyApp(ui = ui, server = server)

