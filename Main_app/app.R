library(shiny)
library(readr)
library(benford.analysis)
library(dplyr)
library(lubridate)
library(DT)
library(ggthemes)
library(tidyr)
library(shinythemes)
library(shinyWidgets)
library(plotly)

clean_data <- read_csv("AGO_Cleaned_Data.csv")
clean_data$Year <- as.numeric(format(clean_data$Date, "%Y"))
Year <- unique(clean_data$Year)
agency_list <- unique(clean_data$Agency)
Type <- unique(clean_data$`Procurement Type`)





ui <- navbarPage("AGO Group",
                 theme = shinytheme('flatly'),
                 tabPanel("About Us",
                          icon = icon("university"),
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                id = "sidebar_panel",
                                style = "background-color: inherit;",
                                tags$div(style = "position: relative; text-align: left; z-index: 1000;",
                                         switchInput("mode_toggle", "Switch mode:", value = FALSE, onLabel = "Light", offLabel = "Dark")),
                                tags$br(), 
                                tags$br(),
                                
                                h2(strong("AGO Data Challenge")),
                                tags$ul(
                                  tags$li("Teo Jun Hao", style = "font-size:150%"),
                                  tags$li("Valerie Hong", style = "font-size:150%"),
                                  tags$li("Emma", style = "font-size:150%")
                                ),
                                uiOutput("custom_css"),
                              ),
                              mainPanel(
                                h2(strong("Project Objective")),
                                hr(),
                                span("The objective of this site is to collate our visualisation and turn it into a user-guided experience.",
                                     style = "font-size:150%"),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br()
                              )
                              
                            )
                          )
                 ),
                 tabPanel("Benford's Law Analysis of Awarded Amount", fluid = TRUE, icon =icon("map"),
                          sidebarLayout(position = 'left',
                                        sidebarPanel(fluid = TRUE, width = 3,
                                                     id = "sidebar_panel",
                                                     style = "background-color: inherit;",
                                                     tags$strong("Variable Inputs"),
                                                     hr(),
                                                     selectInput("agencyInput", "Choose Agency:", 
                                                                 choices = unique(clean_data$Agency), selected = NULL),
                                                     actionButton("generateBtn", "Generate Analysis")
                                        ),
                                        mainPanel(
                                          plotOutput("benfordPlot")
                                        )
                          )
                 ),
                 tabPanel("Interactive Visual", fluid = TRUE, icon =icon("map"),
                          sidebarLayout(position = 'left',
                                        sidebarPanel(fluid = TRUE, width = 3,
                                                     id = "sidebar_panel",
                                                     style = "background-color: inherit;",
                                                     tags$strong("Variable Inputs"),
                                                     hr(),
                                                     selectInput("agency_Input", "Choose an Agency:", choices = agency_list),
                                                     selectInput("typeInput", "Choose a Type:", choices = Type),
                                                     selectInput("year_Input", "Choose a Year:", choices = Year),
                                                     actionButton("plotbutton", "Generate Plot")
                                        ),
                                        mainPanel(
                                          plotlyOutput("timePlot")
                                        )
                          )
                 ),
                 tabPanel("Supplier Award by Description", fluid = TRUE, icon =icon("map"),
                          sidebarLayout(position = 'left',
                                        sidebarPanel(fluid = TRUE, width = 3,
                                                     id = "sidebar_panel",
                                                     style = "background-color: inherit;",
                                                     conditionalPanel(
                                                       'input.Supplier === "Quarterly Summary"',
                                                       tags$strong("Variable Inputs"),
                                                       hr(),
                                                       selectInput("year_S", "Select Year:", choices = 2017:2022),
                                                       selectInput("description_S", "Select Description:", choices = unique(clean_data$`Procurement Description`)),
                                                       actionButton("supplier", "Display")
                                                     ),
                                                     conditionalPanel(
                                                       'input.Supplier === "Yearly Summary"',
                                                       tags$strong("Variable Inputs"),
                                                       hr(),
                                                       selectInput("description_SY", "Select Description:", choices = unique(clean_data$`Procurement Description`)),
                                                       actionButton("supplier_Y", "Display")
                                                       
                                                     )
                                                     
                                        ),
                                        mainPanel(
                                          width = 9,
                                          tabsetPanel(
                                            id = "Supplier",
                                            tabPanel("Quarterly Summary",
                                                     DTOutput("table"),
                                                     h3("Top 3 Suppliers for Selected Year"),
                                                     DTOutput("topSuppliersTable")),
                                            tabPanel("Yearly Summary",
                                                     DTOutput("tableSY")))
                                        )
                          )
                 ),
                     
                 tabPanel("Agency Expenditure by Description", fluid = TRUE, icon =icon("map"),
                          sidebarLayout(position = 'left',
                                        sidebarPanel(fluid = TRUE, width = 3,
                                                     id = "sidebar_panel",
                                                     style = "background-color: inherit;",
                                                     conditionalPanel(
                                                       'input.Agency === "Quarterly Summary"',
                                                       tags$strong("Variable Inputs"),
                                                       hr(),
                                                       selectInput("year_A", "Select Year:", choices = 2017:2022),
                                                       selectInput("description_A", "Select Description:", choices = unique(clean_data$`Procurement Description`)),
                                                       actionButton("agency_A", "Display"),
                                                     ),
                                                     
                                                     conditionalPanel(
                                                       'input.Agency === "Yearly Summary"',
                                                       tags$strong("Variable Inputs"),
                                                       hr(),
                                                       selectInput("description_AY", "Select Description:", choices = unique(clean_data$`Procurement Description`)),
                                                       actionButton("agency_AY", "Display"),
                                                       
                                                       ),
                                                     
                                        ),
                                        mainPanel(width = 9,
                                                  tabsetPanel(
                                                    id = "Agency",
                                                    tabPanel("Quarterly Summary",
                                                             DTOutput("table_A"),
                                                             h3("Top 3 Agency for Selected Year"),
                                                             DTOutput("topAgencyTable")),
                                                    tabPanel("Yearly Summary",
                                                             DTOutput("tableAY")))
                                        )
                          )
                 ),
                 tabPanel("Outlier Analysis", fluid = TRUE, icon =icon("map"),
                          #sidebarLayout(position = 'left',
                           #             sidebarPanel(fluid = TRUE, width = 3,
                            #                         id = "sidebar_panel",
                             #                        style = "background-color: inherit;") 
                    #      conditionalPanel(
                     #       'input.Outlier === "Summary"',
                      #      tags$strong("Variable Inputs"),
                       #     hr(),
                        #    selectInput("year_A", "Select Year:", choices = 2017:2022),
                         #   selectInput("description_A", "Select Description:", choices = unique(clean_data$`Procurement Description`)),
                          #  actionButton("outlier_a", "Display"),
                          # ),
                          
                 )
)
                                                     
                                                     
                                  
                                  
server <- function(input, output) {
  mode <- reactiveVal("light")
  
  observeEvent(input$mode_toggle, {
    if (mode() == "light") {
      mode("dark")
    } else {
      mode("light")
    }
  })
  
  output$custom_css <- renderUI({
    if (mode() == "light") {
      tags$style(HTML("
      body {
        background-color: #f0f4fa;
        color: #00008B;
      }
      .dark-mode {
        background-color: #333;
        color: #fof4fa;
      }
    "))
    } else {
      tags$style(HTML("
      body {
        background-color: #333;
        color: #f0f4fa;
      }
      .dark-mode {
        background-color: #ADD8E6;
        color: #00008B;
      }
      .sidebar-panel {
        background-color: #2b2b2b;
      }
    "))
    }
  })
  
  # Benford Law
  observeEvent(input$generateBtn, {
    # Filter dataset based on input
    filtered_data <- clean_data %>%
      filter(Agency == input$agencyInput)
             
    
    # Do Benford's analysis on the filtered data
    benford_result <- benford(filtered_data$`Awarded Amount`, number.of.digits = 1)
    
    # Output the plot
    output$benfordPlot <- renderPlot({
      plot(benford_result)
    })
  })
  
  # Visual plot of transaction
  observeEvent(input$plotbutton, {
    output$timePlot <- renderPlotly({
      filtered_data <- clean_data[clean_data$Agency == input$agency_Input & 
                                    clean_data$`Procurement Type` == input$typeInput & 
                                    clean_data$Year == input$year_Input, ]
      
      p <- ggplot(filtered_data, aes(x = Date, y = `Awarded Amount`, text = `Procurement Description`)) +
        geom_jitter(width = 0.2, height = 0, alpha = 0.6) + 
        labs(title = paste("Scatter Plot for", input$typeInput, "in", input$year_Input), x = "Date", y = "Amount") + 
        theme_economist()
      
      ggplotly(p, tooltip = c("x", "y", "text"))
    })
  })
  
  # Supplier Awards
  observeEvent(input$supplier, {
    filtered_data <- clean_data %>%
      filter(Year == input$year_S, 
             `Procurement Description` == input$description_S)
    
    grouped_data <- filtered_data %>% 
      mutate(Quarter = quarter(Date)) %>%
      group_by(Supplier, Year, Quarter) %>%
      summarise(TotalAmount = sum(`Awarded Amount`, na.rm = TRUE), .groups = 'drop')
    
    # Spread data to wide format for each quarter
    wide_data <- grouped_data %>%
      pivot_wider(names_from = c(Year, Quarter), 
                  names_prefix = "Y", 
                  names_sep = "-Q", 
                  values_from = TotalAmount, 
                  values_fill = list(TotalAmount = 0))
    
    # Add a sum for each year at the end
    wide_data$Sum <- rowSums(select(wide_data, starts_with("Y")), na.rm = TRUE)
    
    output$table <- renderDT({wide_data})
    
    top_suppliers <- wide_data %>%
      arrange(-Sum) %>%
      head(3)
    output$topSuppliersTable <- renderDT({top_suppliers})
  })
  
  observeEvent(input$supplier_Y, {
    data_set <- clean_data %>%
      filter(`Procurement Description` == input$description_AY)
    
    yearly_data <- data_set %>%
      group_by(Year, Supplier) %>%
      summarise(TotalAmount = sum(`Awarded Amount`, na.rm = TRUE))
    
    table_data <- yearly_data %>%
      pivot_wider(names_from = Year, values_from = TotalAmount, values_fill = list(TotalAmount = 0))
    
    output$tableSY <- renderDT({table_data})
  })
  
  observeEvent(input$agency_A, {
    filtered_data <- clean_data %>%
      filter(Year == input$year_A, 
             `Procurement Description` == input$description_A)
    
    grouped_data <- filtered_data %>% 
      mutate(Quarter = quarter(Date)) %>%
      group_by(Agency, Year, Quarter) %>%
      summarise(TotalAmount = sum(`Awarded Amount`, na.rm = TRUE), .groups = 'drop')
    
    # Spread data to wide format for each quarter
    wide_data <- grouped_data %>%
      pivot_wider(names_from = c(Year, Quarter), 
                  names_prefix = "Y", 
                  names_sep = "-Q", 
                  values_from = TotalAmount, 
                  values_fill = list(TotalAmount = 0))
    
    # Add a sum for each year at the end
    wide_data$Sum <- rowSums(select(wide_data, starts_with("Y")), na.rm = TRUE)
    
    output$table_A <- renderDT({wide_data})
    
    top_agency <- wide_data %>%
      arrange(-Sum) %>%
      head(3)
    output$topAgencyTable <- renderDT({top_agency})
  })
  
  observeEvent(input$agency_AY, {
      
        data_set <- clean_data %>%
          filter(`Procurement Description` == input$description_AY)
        
        yearly_data <- data_set %>%
          group_by(Year, Agency) %>%
          summarise(TotalAmount = sum(`Awarded Amount`, na.rm = TRUE))
        
        table_data <- yearly_data %>%
          pivot_wider(names_from = Year, values_from = TotalAmount, values_fill = list(TotalAmount = 0))
        
        output$tableAY <- renderDT({table_data})
        
      
    })
}

shinyApp(ui = ui, server = server)
                                                    
                                                       

