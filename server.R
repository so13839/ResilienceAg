library(shiny)
model <- readRDS("model.RDS")
# Server
server <- function(input, output) {
  output$yield_plot <- renderPlot({
    plot(data$yield ~ data$rainfall,
         xlab = "Rainfall",
         ylab = "Yield",
         main = "Yield vs. Rainfall",
         col = "blue",
         pch = 19)
  })
  
  predicted_yield <- eventReactive(input$predict_button, {
    new_data <- data.frame(rainfall = input$rainfall_input, nevi = input$nevi_input)
    predict(model, new_data)
  })
  
  output$yield_output <- renderText({
    year <- input$year_input
    yield_value <- predicted_yield()
    paste("Yield for", year, ":", round(yield_value, 2), "Kg")
  })
  
  output$score_output <- renderText({
    year <- input$year_input
    yield_value <- predicted_yield()
    score <- ifelse(yield_value < 1000, "Bad",
                    ifelse(yield_value >= 1000 && yield_value <= 1299, "Fair",
                           ifelse(yield_value >= 1300 && yield_value <= 1799, "Good",
                                  ifelse(yield_value >= 1800, "Excellent", ""))))
    paste("Score for", year, ":", score)
  })
}

# Create a linear regression model
#model <- lm(yield ~ rainfall + nevi, data = data)


# Run the app
#shinyApp(ui, server)