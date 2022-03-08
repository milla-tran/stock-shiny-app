
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


# Define UI for application that draws a histogram
ui <- fluidPage(theme = bslib::bs_theme(bg = "#010779", 
                                        fg = "white", 
                                        base_font = "Source Sans Pro"),
                img(src='stockgraphic.jpg', align = "center"),

    # Application title
    titlePanel("Exploring Stocks!"),

    # Choosing stocks 
    sidebarLayout(
      sidebarPanel(width = 3,
                   
                   # Let user pick stocks
                   pickerInput(
                     inputId = "stocks",
                     label = h4("Please select the stocks you would like to observe"),
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
          plotlyOutput("plot", height=800)
        )
    
  )
)


server <- function(input, output) {
    
    # create output
    output$plot <- renderPlotly({
      
       print(
         ggplotly(prices %>%
                    group_by(symbol) %>%
                    mutate(init_close = if_else(date == min(date),close,NA_real_)) %>%
                    mutate(value = round(100 * close / sum(init_close,na.rm=T),1)) %>%
                    ungroup() %>%
                    ggplot(aes(date, value,colour = symbol)) +
                    geom_line(size = 1, alpha = .9) +
                    theme_minimal(base_size=16) +
                    theme(axis.title=element_blank(),
                          plot.background = element_rect(fill = "#010779"),
                          panel.background = element_rect(fill="#010779"),
                          panel.grid = element_blank(),
                          legend.text = element_text(colour="white"))
                    )
          )
      })  
}

# Run the application 
shinyApp(ui = ui, server = server)
