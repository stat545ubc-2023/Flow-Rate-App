#
# Made by Juliana Vieira - 23-Nov-2023
#
# This is a Shiny web application. You can run the application by clicking
## Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/
#

# load useful libraries
library(shiny)
library(datateachr)
library(tidyverse)

#Define data to use

flow_sample <- datateachr::flow_sample #using dataset "flow_sample" from "datateach" package.
#   Hayley Boyce and Jordan Bourak (2020). datateachr: Data collected to
#   use for teaching at the University of British Columbia. R package
#   version 0.2.1. https://github.com/UBC-MDS/datateachr



# Instead another dataset could be used with:
# read.csv("new_dataset")

m_choices <- flow_sample %>% #created to facilitate the choice list for the following input ("id_month")
  arrange(month) %>%
  drop_na(month) %>%
  pull(month)

# Define UI for application that draws a histogram
ui <- fluidPage(


    # Application title
    titlePanel("🌊 The Flow Rate"),
    br(),
    h4("Use this tool to explore the flow rate over the years for 'station_id 05BB001'"),
    br(),
    # Sidebar with interactive options
    sidebarLayout(
        sidebarPanel(
          
            # added a options for users to select the year range to observe. May be useful to users to look at a specific period of time.
            sliderInput("id_year",  
                        "Select a year range:",
                        min = min(flow_sample$year),
                        max = max(flow_sample$year),
                        value = c(1915, 1930)),
            
            # added a options for users to select the flow extreme type. They can look at one, or both or none(🤷️) of the flow extreme types.
            h4("Check to select the extreme type:"),
            
            checkboxInput("id_max", "⬆️ Maximum extreme type"),
            checkboxInput("id_min", "⬇️ Minimum extreme type"),
            
            # added a option to investigate the flow in a specific month. Can be helpful if user, for instance, want to correlate the flow rate with the season of the year.
            selectInput("id_month", "Filter by Month", 
                        choices = c("All", unique(m_choices)))

        ),
            
        # Show a plot of the generated distribution and a table with its data
        mainPanel(
           plotOutput("FlowPlot"),
           tableOutput("FlowTable")
        )
    )
)

  # Define server logic required to filter data for the table and to draw a histogram
server <- function(input, output){

  flow_filtered <- reactive({
    flow_sample_filtered <- flow_sample %>%
      select(-station_id) %>%
      filter(year >= input$id_year[1],
             year <= input$id_year[2])
    
    if (input$id_max == TRUE) {
      flow_sample_filtered <- flow_sample_filtered %>%
        filter(extreme_type == "maximum")
    }
    
    if (input$id_min == TRUE) {
      flow_sample_filtered <- flow_sample_filtered %>%
        filter(extreme_type == "minimum")
    }
    
    if (input$id_month != "All"){ 
      flow_sample_filtered <- flow_sample_filtered %>%
      filter(month == input$id_month)
    }
      
    
    return(flow_sample_filtered)
    })
  
  #A plot showing the flow distribution with selected parameters
  output$FlowPlot <- renderPlot({
    flow_filtered() %>%
      ggplot(aes(year, flow, fill = year)) +
      geom_col() +
      labs(title = "Flow Rate",
           x = "Year",
           y = "Flow")
  })
  #A table showing the flow data with selected parameters
  output$FlowTable <- renderTable({
    flow_filtered()
      
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
