library(shiny)
library(dplyr)
library(readr)
# Sample data for demonstration
model <- readRDS("model.RDS")

# UI
ui <- fluidPage(
  titlePanel("ResilienceAg Risks Assessment Platform"),
  sidebarLayout(
    sidebarPanel(
      textInput("farmers_input", "Farmer:", value = ""),
      numericInput("year_input", "Year Input:", value = 0, min = 0, max = 0),
      numericInput("rainfall_input", "Average rainfall:", value = 0, min = 0, max = 2000),
      numericInput("nevi_input", "Average NDVI:", value = 0, min = -1, max = 1),
      actionButton("predict_button", "Asess risks"),
      verbatimTextOutput("yield_output"),
      verbatimTextOutput("score_output")
    ),
    mainPanel(
      h3("ResilienceAg!"),
      h4("ResilienceAg! is an innovative initiative facilitated by AB Consultants, aimed at revolutionizing agricultural finance and agronomy services. By leveraging climate, vegetative, soil, and field data, it empowers financial institutions to make informed lending decisions to farmers. The ResilienceAg! model integrates GPS location, climate insights, historical yields, and on-field support to create a comprehensive risk profile for farmers, ultimately enabling the design of sustainable financial products. This entails developing a credit scoring engine that also offers farmers a client-facing interface to assess their credit eligibility. The project, incubated within AB Consultants, serves as a pivotal driver in promoting insurance adoption across Africa's agricultural landscape."),
      
      
      h3("How to use the Risks Assessment Platform"),
      h4(tags$b("Year Input:")),
      h4(" Enter the year for which you want to assess the risks.
          Input should be in the format YYYY (e.g., 2019)."),
      
      h4(tags$b("Rainfall Data Input:")),
      h4("Gather historical rainfall data for the chosen year.
Input the year average rainfall value from the farm based on geo-location.
Ensure the rainfall values are in a consistent unit (e.g.millimeters)."),
      
      h4(tags$b("NDVI Data Input:")),
      h4("Obtain NDVI (Normalized Difference Vegetation Index) data for the selected year.
         Input the NDVI values corresponding to the same time intervals as the rainfall data.
         NDVI values typically range from -1 to 1, representing the health and vigor of vegetation."),    
      
      h4(tags$b("Assess the Risks:")),
      h4("Click on the Assess risks button to display the assessment scores.  The ratings range from Bad to Fair, Good, and Excellent."),    
      
    ) 
  )
)

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
shinyApp(ui, server)
