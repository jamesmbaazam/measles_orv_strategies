library(shiny)
library(dplyr)
library(DT)


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Estimating cold chain needs per outbreak response vaccination strategy"),

  # Sidebar with a slider input for number of bins
  fluidRow(
    column(4,
      tags$b("Site information"),

      br(),

      actionButton("add_site_info", "Click to add a site"),

      br(),

      br(),

      selectInput("temp", "Ambient temperature", choices = c("below 40", "above 40")),

      br(),
      numericInput("buffer_stock", "Buffer stock", value = 1, min = 1, step = 0.01)
    ),


    column(8, 
       tabsetPanel(
        tabPanel(title = "Table of sites added", DT::dataTableOutput("all_sites", width = '90%', height = '100%')),
        tabPanel(title = "Strategy comparison plots", plotOutput("plot"))
      )))
  , fluidRow(column(8, 
                    hr(), 
                    offset = 4,
        tabsetPanel(
       # title = "Individual strategy results",
        tabPanel(
          title = "Monodose FCC",
          tags$h4("Initial volume of ice required for transport"),
          wellPanel(htmlOutput("ice_vol_init_dose1_FCC")),
          tags$h4("Quantity of ice packs required"),
          wellPanel(htmlOutput("ice_packs_required_dose1_FCC")),
          tags$h4("Time needed to freeze initial ice"),
          wellPanel(htmlOutput("Init_ice_freezeTime_dose1_FCC"))
        ),
        tabPanel(
          title = "10-dose FCC",
          tags$h4("Initial volume of ice required for transport"),
          wellPanel(htmlOutput("ice_vol_init_dose10_FCC")),
          tags$h4("Quantity of ice packs required"),
          wellPanel(htmlOutput("ice_packs_required_dose10_FCC")),
          tags$h4("Time needed to freeze initial ice"),
          wellPanel(htmlOutput("Init_ice_freezeTime_dose10_FCC"))
          ),
          tabPanel(
            title = "Mixed FCC",
            tags$h4("Initial volume of ice required for transport"),
            wellPanel(htmlOutput("ice_vol_init_mixedFCC")),
            tags$h4("Quantity of ice packs required"),
            wellPanel(htmlOutput("ice_packs_required_mixedFCC")),
            tags$h4("Time needed to freeze initial ice"),
            wellPanel(htmlOutput("Init_ice_freezeTime_mixedFCC"))
        )
      )
      ))
    )




server <- function(input, output, session) {

  # empty data frame for storing all the sites added
  site_table <- reactiveValues(data = NULL)

  # empty dataframe for temporarily storing the newly added site for updating the old table
  site_new <- reactiveValues(data = NULL)




  # dialog box to collect site information
  observeEvent(input$add_site_info, {
    showModal(modalDialog(
      title = "Site information",
      numericInput("dist_from_base_new", "How far is site from base?", value = 0, min = 0, step = 0.01),
      numericInput("near_pop_new", "How many people are near?", value = 0, min = 0, step = 1),
      numericInput("far_pop_new", "How many people are far?", value = 0, min = 0, step = 1),
      numericInput("team_days_fixed_new", "How long must a fixed post team spend on site?", value = 0, min = 0, step = 0.1),
      numericInput("team_days_mobile_new", "How long must a mobile team spend on site?", value = 0, min = 0, step = 0.1),
      fade = TRUE,
      footer = tagList(
        modalButton("Dismiss"),
        actionButton("add_site", "Add site")
      )
    ))
  })


  # When user clicks the add site button, update the old data frame with the new information
  observeEvent(input$add_site, {
    site_new$data <- tibble(
      dist_from_base = input$dist_from_base_new,
      near_pop = input$near_pop_new,
      far_pop = input$far_pop_new,
      team_days_fixed = input$team_days_fixed_new,
      team_days_mobile = input$team_days_mobile_new
    )
    site_table$data <- bind_rows(site_table$data, site_new$data)
    # site_info_table <- bind_rows(site_info_table, site_new$data)
    removeModal()
  })



  # Output the table of all sites added
  output$all_sites <- DT::renderDT(site_table$data)
}



# Run the application
shinyApp(ui = ui, server = server)