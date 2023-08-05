library(shiny)
library(forecast)
library(ggplot2)


ui <- fluidPage(
  titlePanel("Time Series Shiny App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV file"),
      selectInput("model", "Select Model", choices = c("AR", "MA", "ARMA", "ARIMA", "SARIMA")),
      numericInput("order", "Order", value = 1),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })

  output$plot <- renderPlot({
    req(input$submit)
    ts_data <- ts(data())

    if (input$model == "AR") {
      model <- Arima(ts_data, order = c(input$order, 0, 0))
      forecast <- forecast(model)
    } else if (input$model == "MA") {
      model <- Arima(ts_data, order = c(0, 0, input$order))
      forecast <- forecast(model)
    } else if (input$model == "ARMA") {
      model <- Arima(ts_data, order = c(input$order, 0, input$order))
      forecast <- forecast(model)
    } else if (input$model == "ARIMA") {
      model <- auto.arima(ts_data, order = c(input$order, 0, input$order))
      forecast <- forecast(model)
    } else if (input$model == "SARIMA") {
      model <- auto.arima(ts_data)
      forecast <- forecast(model)
    }

    plot(forecast, main = "Time Series Forecast")
  })
}

shinyApp(ui = ui, server = server)