library(shiny)
library(dplyr)

# data
site_info_table <- tibble(dist_from_base = as.numeric(), 
                          near_pop = as.numeric(), 
                          far_pop = as.numeric(), 
                          team_days_fixed = as.numeric(), 
                          team_days_mobile = as.numeric()
                          )


# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Estimating cold chain needs per outbreak response vaccination strategy"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      actionButton("site_info", "Click to add a site"),
      
      br(),
      
      br(),
      
      selectInput("temp", "Ambient temperature", choices = c("below 40", "above 40"))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("all_sites")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    new_site <- reactiveValues(data = NULL)
    
    observeEvent(input$site_info, {
    showModal(modalDialog(
    numericInput('dist_from_base', 'How far is site from base?', value = 0, min = 0),
    numericInput('near_pop', 'How many people are near?', value = 0, min = 0),
    numericInput('far_pop', 'How many people are far?', value = 0, min = 0),
    numericInput('team_days_fixed', 'How long must a fixed post team spend on site?', value = 0, min = 0),
    numericInput('team_days_mobile', 'How long must a mobile team spend on site?', value = 0, min = 0),
      title = "New site information",
      fade = TRUE,
    footer = tagList(
        modalButton('Cancel'),
        actionButton('add_site', 'Add site')
    )))    })
    
    
    observeEvent(input$add_site,{
      site_info_table <- bind_rows(input$site_info, site_info_table)  
    })
output$all_sites <- renderTable(site_info_table)
}

# Run the application
shinyApp(ui = ui, server = server)