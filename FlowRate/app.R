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

# Using dataset "flow_sample" from "datateach" package.
#   Hayley Boyce and Jordan Bourak (2020). datateachr: Data collected to
#   use for teaching at the University of British Columbia. R package
#   version 0.2.1. https://github.com/UBC-MDS/datateachr
# Instead another dataset could be used with:
# read.csv("new_dataset")

# tidying the data set
month_mapping <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

n_flow_sample <- flow_sample %>%
  mutate(month_name = month_mapping[month]) %>%
  select(-month) 

#created to facilitate the choice list for the following input ("id_month")
m_choices <- n_flow_sample %>% 
  drop_na(month_name) %>%
  pull(month_name)

# Define UI for application that draws a histogram
ui <- fluidPage(

    img(src = "data:image/png;base64,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"),
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
                        min = min(n_flow_sample$year),
                        max = max(n_flow_sample$year),
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
    flow_sample_filtered <- n_flow_sample %>%
      select(-station_id) %>%
      filter(year >= input$id_year[1],
             year <= input$id_year[2])

    if (input$id_max == TRUE && input$id_min == TRUE)
      flow_sample_filtered <- flow_sample_filtered
      else if (input$id_max == TRUE) {
      flow_sample_filtered <- flow_sample_filtered %>%
        filter(extreme_type == "maximum")}
        else if (input$id_min == TRUE) {
        flow_sample_filtered <- flow_sample_filtered %>%
        filter(extreme_type == "minimum")}
    
    if (input$id_month != "All"){ 
      flow_sample_filtered <- flow_sample_filtered %>%
      filter(month_name == input$id_month)
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
