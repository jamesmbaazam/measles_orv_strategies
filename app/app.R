library(shiny)
library(dplyr)

# data
site_info_table <-  tibble(
  dist_from_base = as.numeric(),
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
      tags$b("Site information"),

      br(),

      actionButton("add_site_info", "Click to add a site"),

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
server <- function(input, output, session) {
  site_null_obj <- reactiveValues(data = NULL)
  
  observeEvent(input$add_site_info, {
      showModal(modalDialog(
      numericInput("dist_from_base_new", "How far is site from base?", value = 0, min = 0),
      numericInput("near_pop_new", "How many people are near?", value = 0, min = 0),
      numericInput("far_pop_new", "How many people are far?", value = 0, min = 0),
      numericInput("team_days_fixed_new", "How long must a fixed post team spend on site?", value = 0, min = 0),
      numericInput("team_days_mobile_new", "How long must a mobile team spend on site?", value = 0, min = 0),
      title = "New site information",
      fade = TRUE,
      footer = tagList(
        modalButton("Dismiss"),
        actionButton("add_site", "Add site")
      )
    ))
    })
  
  #When user clicks the add site button, create a new tibble with the info they'll enter and bind that to the existing data frame
  observeEvent(input$add_site, {
    site_new <- tibble(dist_from_base = input$dist_from_base_new,
                       near_pop = input$near_pop_new,
                       far_pop = input$far_pop_new,
                       team_days_fixed = input$team_days_fixed_new,
                       team_days_mobile = input$team_days_mobile_new
    )
    site_info_table <- bind_rows(site_info_table, site_new) 
  })
  
  
  
  
  #Outputs  
  output$all_sites <- renderTable(site_info_table)  
  }
  


# Run the application
shinyApp(ui = ui, server = server)