# Install and load necessary packages
# install.packages("shinythemes")
library(shiny)
library(shinythemes)
library(ggplot2)
library(readr)

# Load your dataset
my_data <- read.csv("Sample_Modeling_DF.csv")
test_data <- na.omit(my_data)

# Define UI for the Shiny app
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Energy Consumption Visualization and Prediction"),
  sidebarLayout(
    sidebarPanel(
      selectInput("citySelect", "Select City:", 
                  choices = c("All Cities", unique(test_data$in.weather_file_city)), # Add "All Cities" option
                  selected = "All Cities"),  # Set "All Cities" as the default
      sliderInput("tempRange", "Temperature Range:",
                  min = min(test_data$Dry.Bulb.Temperature...C.), max = max(test_data$Dry.Bulb.Temperature...C.),
                  value = c(min(test_data$Dry.Bulb.Temperature...C.), max(test_data$Dry.Bulb.Temperature...C.))),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Energy Consumption by City", plotOutput("plot2")),
        tabPanel("Fuels Used in Each City", plotOutput("plot3")),
        tabPanel("Energy Consumed by Day of the Month", plotOutput("plot4")),
        tabPanel("Energy Consumption by Hour", plotOutput("plot5")),
        tabPanel("Energy Consumed in Relation to Temperature and Tenure Status", plotOutput("plot6")),
        tabPanel("Energy Consumption in One and Two Story Buildings", plotOutput("plot7"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Reactive expression for filtered data
  filteredData <- reactive({
    if (input$citySelect == "All Cities") {
      data <- test_data
    } else {
      data <- test_data[test_data$in.weather_file_city == input$citySelect & 
                          test_data$Dry.Bulb.Temperature...C. >= input$tempRange[1] & 
                          test_data$Dry.Bulb.Temperature...C. <= input$tempRange[2], ]
    }
    return(data)
  })
  
  
  output$plot2 <- renderPlot({
    data <- filteredData()
    ggplot(data, aes(x = in.weather_file_city, y = out.total.energy_consumption)) + 
      geom_bar(stat = "identity", fill = "maroon") + 
      labs(title = "Energy Consumption by City", x = "City", 
           y = "Total Energy Consumption") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$plot3 <- renderPlot({
    data <- filteredData()
    ggplot(data, aes(x = in.weather_file_city, fill = in.heating_fuel)) +
      geom_bar() +
      labs(title = "Types of Fuels", x = "City") +
      guides(color = guide_legend(title = "Fuel Type")) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$plot4 <- renderPlot({
    data <- filteredData()
    ggplot(data, aes(x = as.factor(Day), 
                     y = out.total.energy_consumption)) + 
      geom_line() +
      labs(title = "Energy Consumed by Day of the Month", x = "Day of the Month", 
           y = "Energy Consumed") +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  output$plot5 <- renderPlot({
    data <- filteredData()
    ggplot(data, aes(x = as.factor(Hour), 
                     y = out.total.energy_consumption)) + 
      geom_line() + 
      labs(title = "Energy Consumption by Hour", x = "Hour", 
           y = "Energy Consumption") +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  output$plot6 <- renderPlot({
    data <- filteredData()
    ggplot(data, aes(x = Dry.Bulb.Temperature...C., 
                     y = out.total.energy_consumption, 
                     color = in.tenure)) + 
      geom_point() + 
      labs(title = "Energy Consumed in Relation to Temperature and Tenure Status", 
           x = "Dry Bulb Temperatures", y = "Total Energy Consumption") + 
      guides(color = guide_legend(title = "Tenure Status")) +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  output$plot7 <- renderPlot({
    data <- filteredData()
    ggplot(data, aes(x = as.factor(in.geometry_stories), 
                     y = out.total.energy_consumption)) + 
      geom_boxplot() + 
      labs(title = "Energy Consumption in One and Two Story Buildings", 
           x = "Stories", y = "Energy Consumption") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
}

shinyApp(ui = ui, server = server)