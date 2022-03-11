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



# Define UI for application that draws a graphic
ui <- fluidPage(
  theme = bslib::bs_theme(bg = "black",
                          fg = "white",
                          base_font = "Source Sans Pro"),

  
  # Application title
  titlePanel("Stock Prices"),
  
  # Choosing stocks
  sidebarLayout(
    sidebarPanel(
      width = 3,

      
      selectInput(
        inputId = 'stocks',
        label = 'Search for tickers or companies',
        choices = tickers2,
        selected = NULL,
        multiple = TRUE
        # allow for multiple inputs
        #options = list(create = FALSE) # if TRUE, allows newly created inputs
      ),
      
      
      dateInput(inputId = "date1", label = "Select start date", 
                value = today()-months(12), 
                min = today()-months(240), 
                max = today(),
                format = "yyyy-mm-dd", 
                startview = "month", 
                weekstart = 0,
                language = "en", width = NULL),
      
      dateInput(inputId = "date2", label = "Select end date", 
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
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Price Highs", 
          plotOutput("highs", height = 600)
        ),
        
        tabPanel(
          "Closing Prices",  
          plotOutput('plot', height = 600),
          )
      
      )
    ),
  )
)

server <- function(input, output) {
  
  observe({
    
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
    
    
    output$plot <- 
      renderPlot(
        prices %>%
          group_by(symbol) %>%
          ggplot(aes(x = date, y = close, color = symbol)) +
          geom_line(size=1) +
          labs(title = "Stock Line Chart", y = "Closing Price", x = "Date", caption = "If blank, stock ticker is not valid") +
          theme(plot.title = element_text(size = 35, colour = "white", family = "Source Sans Pro"),
                axis.title = element_blank(),
                axis.title.x = element_text(size = 20, colour = "white", family = "Source Sans Pro"),
                axis.title.y = element_text(size = 20, colour = "white", family = "Source Sans Pro"),
                legend.title = element_text(colour="black", size=15),
                plot.background = element_rect(fill = "black"),
                panel.background = element_rect(fill="black"),
                panel.grid = element_blank(),
                legend.text = element_text(colour="white", family = "Source Sans Pro"),
                legend.background = element_rect(fill = "black"),
                legend.key = element_blank())
        )
    
    output$highs <- renderPlot(
      prices %>%
        group_by(symbol) %>%
        ggplot(aes(x = date, y = high, color = symbol)) + 
        geom_line(size=1, alpha = .9) +
        labs(title = "Stock Price Highs Over Time", y = "Price", x = "Date", caption = "If blank, stock ticker is not valid") +
        theme(plot.title = element_text(size = 35, colour = "white", family = "Source Sans Pro"),
              axis.title = element_blank(),
              axis.title.x = element_text(size = 20, colour = "white", family = "Source Sans Pro"),
              axis.title.y = element_text(size = 20, colour = "white", family = "Source Sans Pro"),
              legend.title = element_text(colour="black", size=15),
              plot.background = element_rect(fill = "black"),
              panel.background = element_rect(fill="black"),
              panel.grid = element_blank(),
              legend.text = element_text(colour="white", family = "Source Sans Pro"),
              legend.background = element_rect(fill = "black"),
              legend.key = element_blank())
        )
    
  }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
