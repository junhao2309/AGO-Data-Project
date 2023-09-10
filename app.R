# Load necessary libraries
library(shiny)
library(ggplot2)
library(readr)
library(ggthemes)

clean_data <- read_csv("AGO_Cleaned_Data.csv")
clean_data$Year <- as.numeric(format(clean_data$Date, "%Y"))

Year <- unique(clean_data$Year)
agency_list <- unique(clean_data$Agency)
Type <- unique(clean_data$`Procurement Type`)


# Shiny UI
ui <- fluidPage(
  
  titlePanel("Interactive Time Graphs"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("agencyInput", "Choose an Agency:", choices = agency_list),
      selectInput("typeInput", "Choose a Type:", choices = Type),
      selectInput("yearInput", "Choose a Year:", choices = Year),
      actionButton("plotButton", "Generate Plot")
    ),
    
    mainPanel(
      plotlyOutput("timePlot")
    )
  )
)

# Shiny Server Logic
server <- function(input, output) {
  
  output$timePlot <- renderPlotly({
    req(input$plotButton)
    
    filtered_data <- clean_data[clean_data$Agency == input$agencyInput & 
                          clean_data$`Procurement Type` == input$typeInput & 
                          clean_data$Year == input$yearInput, ]
    
    p <- ggplot(filtered_data, aes(x = Date, y = `Awarded Amount`, text = `Procurement Description`)) +
      geom_jitter(width = 0.2, height = 0, alpha = 0.6) + 
      labs(title = paste("Scatter Plot for", input$typeInput, "in", input$yearInput), x = "Date", y = "Amount") + 
      theme_economist()
    
    ggplotly(p, tooltip = c("x", "y", "text"))
  })
}

# Run Shiny App
shinyApp(ui, server)

