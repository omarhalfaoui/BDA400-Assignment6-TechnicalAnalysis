# Assignment 6 - Technical Analysis Using R
# Visualization Phase
# Omar Halfaoui

library(shiny)
library(ggplot2)
library(quantmod)
# Step 1: Data Collection and Setup
stock_symbol <- "AAPL"
start_date <- "2023-01-01"
end_date <- "2023-07-01"

stock_data <- getSymbols(
  stock_symbol,
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)
# Step 2: User Interface
ui <- fluidPage(
  
  titlePanel("Stock Technical Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = "2023-01-01",
        end = "2023-07-01"
      ),
      
      selectInput(
        "time_frame",
        "Select Time Frame:",
        choices = c("Daily", "Weekly", "Monthly")
      ),
      checkboxGroupInput(
        "technical_indicators",
        "Technical Indicators:",
        choices = c("Moving Averages", "RSI", "MACD"),
        selected = "Moving Averages"
      )
    ),
    
    mainPanel(
      plotOutput("stock_chart")
    )
  )
)
# Server
server <- function(input, output) {
  
  output$stock_chart <- renderPlot({
    
    # Filter data based on selected date range
    filtered_data <- stock_data[
      index(stock_data) >= input$date_range[1] &
        index(stock_data) <= input$date_range[2]
    ]
    
    
    # Create data frame
    plot_data <- data.frame(
      Date = index(filtered_data),
      Close = as.numeric(Cl(filtered_data))
    )
    
    # Calculate Moving Averages
    plot_data$MA20 <- SMA(plot_data$Close, n = 20)
    plot_data$MA50 <- SMA(plot_data$Close, n = 50)
    
    # Calculate RSI
    plot_data$RSI <- RSI(plot_data$Close, n = 14)
    
    # Calculate MACD
    macd_values <- MACD(plot_data$Close, nFast = 12, nSlow = 26, nSig = 9)
    plot_data$MACD <- macd_values[, 1]
    
    # Generate signals only when Moving Averages cross
    plot_data$Signal <- "Hold"
    
    buy_signal <- plot_data$MA20 > plot_data$MA50 &
      dplyr::lag(plot_data$MA20) <= dplyr::lag(plot_data$MA50)
    
    sell_signal <- plot_data$MA20 < plot_data$MA50 &
      dplyr::lag(plot_data$MA20) >= dplyr::lag(plot_data$MA50)
    
    plot_data$Signal[which(buy_signal)] <- "Buy"
    plot_data$Signal[which(sell_signal)] <- "Sell"
    
    # Basic stock price chart
    p <- ggplot(plot_data, aes(x = Date, y = Close)) +
      geom_line() +
      labs(
        title = paste(stock_symbol, "Technical Analysis"),
        x = "Date",
        y = "Closing Price"
      ) +
      theme_minimal()
    
    # Moving Average overlay
    if ("Moving Averages" %in% input$technical_indicators) {
      p <- p +
        geom_line(aes(y = MA20), linewidth = 0.8) +
        geom_line(aes(y = MA50), linewidth = 0.8)
    }
    # RSI overlay
    if ("RSI" %in% input$technical_indicators) {
      rsi_scaled <- scales::rescale(
        plot_data$RSI,
        to = range(plot_data$Close, na.rm = TRUE)
      )
      
      p <- p + geom_line(
        aes(y = rsi_scaled),
        linetype = "dashed",
        linewidth = 0.8
      )
    }
    
    # MACD overlay
    if ("MACD" %in% input$technical_indicators) {
      macd_scaled <- scales::rescale(
        plot_data$MACD,
        to = range(plot_data$Close, na.rm = TRUE)
      )
      
      p <- p + geom_line(
        aes(y = macd_scaled),
        linetype = "dotted",
        linewidth = 0.8
      )
    }
    # Trading signal annotations
    signal_data <- plot_data[
      !is.na(plot_data$MA20) &
        !is.na(plot_data$MA50) &
        plot_data$Signal != "Hold",
    ]
    
    p <- p +
      geom_text(
        data = signal_data,
        aes(label = Signal),
        vjust = -0.5,
        size = 3
      )
    
    print(p)
  })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)