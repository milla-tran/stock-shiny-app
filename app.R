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
                 complete_cases = F) 


# Define UI for application that draws a histogram
ui <- fluidPage(theme = bslib::bs_theme(bg = "#010779", 
                                        fg = "white", 
                                        base_font = "Source Sans Pro"),
                
                
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
                  ),
                  
                  # Plot results
     
                  mainPanel(
  
                  tabsetPanel(
                    tabPanel(
                      "Price Levels", 
                      plotlyOutput("plot", height = 800)
                    ),
    
                    tabPanel("High Prices",
                      plotlyOutput("highs", height = 800)
                    ),
    
              )
          ),
    )
)



server <- function(input, output) {
  
  # create output
  output$plot <- renderPlotly({
    ggplotly(prices %>% filter(symbol %in% input$stocks) %>%
               group_by(symbol) %>%
               mutate(init_close = if_else(date == min(date),close,NA_real_)) %>%
               mutate(value = round(100 * close / sum(init_close,na.rm=T),1)) %>%
               ungroup() %>%
               ggplot(aes(date, value,colour = symbol)) +
               geom_line(size = 1, alpha = .9) +
               theme_minimal(base_size=16) +
               labs(title = "Stock Price Levels over Time", x = "Date", y = "Price (indexed at 100)") +
               theme(plot.title = element_text(size = 35, colour = "white", family = "Source Sans Pro"),
                     axis.title = element_blank(),
                     axis.title.x = element_text(size = 20, colour = "white", family = "Source Sans Pro"),
                     axis.title.y = element_text(size = 20, colour = "white", family = "Source Sans Pro"),
                     legend.title = element_text(colour="#010779", size=15),
                     plot.background = element_rect(fill = "#010779"),
                     panel.background = element_rect(fill="#010779"),
                     panel.grid = element_blank(),
                     legend.text = element_text(colour="white", family = "Source Sans Pro"))
    )
  })
  
  output$highs <- renderPlotly({
    ggplotly(prices %>% filter(symbol %in% input$stocks) %>% 
               ggplot(aes(date, high, colour = symbol)) +
               geom_line(size = 1, alpha = .9) +
               theme_minimal(base_size=16) +
               labs(title = "Stock Price Highs over Time", x = "Date", y = "Price") +
               theme(plot.title = element_text(size = 35, colour = "white", family = "Source Sans Pro"),
                     axis.title = element_blank(),
                     axis.title.x = element_text(size = 20, colour = "white", family = "Source Sans Pro"),
                     axis.title.y = element_text(size = 20, colour = "white", family = "Source Sans Pro"),
                     legend.title = element_text(colour="#010779", size=15),
                     plot.background = element_rect(fill = "#010779"),
                     panel.background = element_rect(fill="#010779"),
                     panel.grid = element_blank(),
                     legend.text = element_text(colour="white", family = "Source Sans Pro"))
             )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
