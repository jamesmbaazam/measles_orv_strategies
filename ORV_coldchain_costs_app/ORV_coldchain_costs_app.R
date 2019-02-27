library(shiny)
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(shinythemes)
library(DT)

# Note: Throughout the code, FCC means Full Cold Chain, dose10 means 10-dose




# FIXED PARAMETERS

# active cold chain
mf314_quant <- 20
mf314_largepack_capacity <- 323 # large pack refers to 0.6L icepacks
mf314_smallpack_capacity <- 450 # small pack refers to 0.4L icepacks



# Passive cold chain gross volume
rcw25_grossVol <- 43735.296 / 1E3 # convert to litres
vax_carrier_grossVol <- 9504 / 1E3

# Passive cold chain net volume (after ice packs)
rcw25_netVol <- 20700 / 1E3
vax_carrier_netVol <- 2600 / 1E3

# passive cold chain ice pack capacity
rcw25_ice_capacity <- rcw25_grossVol - rcw25_netVol
vax_carrier_ice_capacity <- vax_carrier_grossVol - vax_carrier_netVol


# Vaccine vial packed volume per dose (pvd) i.e vaccine + diluent
# Vaccines are assumed to have a volume of 2.5cm^3 per dose
monodose_pvd <- 33.618 / 1E3
dose_10_pvd <- 2.42324 / 1E3 # multi-dose refers to 10 doses


# DATA
# icepack_quants <- read_excel('/data/passiveCC_icepack_quantity_data.xlsx')
# vax_storage_capacity <- read_excel('/data/passiveCC_vaccine_storage_capacity_data.xlsx')



# APP
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("spacelab"),
  # Application title
  titlePanel("Estimating cold chain needs for Outbreak Response Vaccination strategies"),

  br(),

  br(),

  # Sidebar with a slider input for number of bins
  fixedRow(
    column(
      4,
      tags$b("Site information"),

      br(),

      actionButton("add_site_info", "Click to add a site"),

      br(),

      br(),

      selectInput("temp", "Ambient temperature", choices = c("below 40", "above 40")),

      br(),
      numericInput("buffer_stock", "Buffer stock (out of 100)", value = 1, min = 1, step = 0.01),

      br(),
      selectInput("vaccine_vol_dose10", "10-dose vaccine volume (cm3)", choices = c(2.1, 3), selected = 2.1),

      br(),
      selectInput("vaccine_vol_monodose", "Monodose vaccine volume (cm3)", choices = c(21.09)),

      # br(),
      # selectInput('site_to_analyse', 'Row number of site to analyse', selected = 1, choices = c(1, nrow(site_table$added_sites))),

      br(),
      actionButton("show_results", "Display results")
    ),


    column(
      8,
      tabsetPanel(
        tabPanel(title = "Table of sites added", DT::dataTableOutput("all_sites", width = "90%", height = "100%")),
        tabPanel(title = "Strategy comparison plots", plotOutput("plot"))
      )
    )
  ), fluidRow(column(8,
    hr(),
    offset = 4,
    tags$h3("Individual strategy results"),
    tabsetPanel(
      tabPanel(
        title = "Monodose FCC",
        tags$h4("Initial volume of ice required for transport"),
        wellPanel(htmlOutput("ice_vol_init_monodose_FCC")),
        tags$h4("Quantity of ice packs required"),
        wellPanel(htmlOutput("ice_packs_required_monodose_FCC")),
        tags$h4("Time needed to freeze initial ice"),
        wellPanel(htmlOutput("Init_ice_freezeTime_monodose_FCC"))
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
  site_table <- reactiveValues(added_sites = NULL)

  # empty dataframe for temporarily storing the newly added site for updating the old table
  site_new <- reactiveValues(data = NULL)

  # empty data frame for storing results

  results <- reactiveValues(
    monodose_FCC = NULL,
    dose10_FCC = NULL,
    mixed_FCC = NULL
  ) # mixed_FCC means, in the Full Cold Chain, we'll vax near with 10-dose, and far with monodose




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
    if (input$dist_from_base_new == 0 &
      input$near_pop_new == 0 &
      input$far_pop_new == 0 &
      input$team_days_fixed_new == 0
    & input$team_days_mobile_new == 0) {
      showModal(modalDialog(title = "warning!", "No information was added. So, no site will be added")) # if user doesn't change the default values, but mistakenly clicks on add site, no site will be added.
      site_new$data <- NULL
      site_table$added_sites <- bind_rows(site_table$added_sites, site_new$data)
    } else {
      site_new$data <- tibble(
        dist_from_base = input$dist_from_base_new,
        near_pop = input$near_pop_new,
        far_pop = input$far_pop_new,
        team_days_fixed = input$team_days_fixed_new,
        team_days_mobile = input$team_days_mobile_new
      )
      site_table$added_sites <- bind_rows(site_table$added_sites, site_new$data)
      # site_info_table <- bind_rows(site_info_table, site_new$data)
      removeModal()
    }
  })

  
  
  
  # Output the table of all sites added
  output$all_sites <- DT::renderDT(site_table$added_sites)  
  
  ##########################################
  # Calculations for monodose-only FCC
  ##########################################
  
  monodose_FCC_doses <- site_table$added_sites %>%
    dplyr::slice(1) %>% # for now, we are only going to concentrate on one site. User indicates which site to analyse
    dplyr::summarise(sum(near_pop, far_pop)) # number of doses needed
  
  monodose_FCC_doses_needed <- monodose_FCC_doses * (1 + input$buffer_stock / 100) # apply buffer. This formula doesn't seem to be making any impact
  
  # number of RCW25s needed, based on the volume of the vaccine indicated (1 RCW25 can transport 1301 vials/doses and a vaccine carrier can transport 283 vials/doses)
  
  monodose_FCC_RCW25_needs <- ceiling(monodose_FCC_doses_needed / 1301) # these numbers refer to the doses along with the diluents
  monodose_vaxCarr_needs <- ceiling(dose10_FCC_doses_needed / 283) # vaccine carrier
  
  
  # Monodose FCC RCW25 icepack needs
  monodose_FCC_vaxCarr_icepack_needs_total <- vaxCarr_icepack_needs * monodose_vaxCarr_needs # total number of 0.6L ice packs = number of RCW25 needed * number of ice packs needed per RCW25
  monodose_FCC_vaxCarr_icepack_vol <- monodose_FCC_vaxCarr_icepack_needs_total * 0.4 # total volume of ice packs needed is simply the above calculation * 0.6L
  
  
  # Monodose FCC Vaccine carrier icepack needs
  monodose_FCC_RCW25_icepack_needs_total <- RCW25_icepack_needs * monodose_FCC_RCW25_needs # total number of 0.6L ice packs = number of RCW25 needed * number of ice packs needed per RCW25
  monodose_FCC_RCW25_icepack_vol <- monodose_FCC_RCW25_icepack_needs_total * 0.4 # total volume of ice packs needed is simply the above calculation * 0.6L
  
  ###
  # outputs for monodose FCC calculations
  ###
  output$ice_packs_required_monodose_FCC <- renderText({
    paste(monodose_FCC_RCW25_icepack_needs_total + monodose_FCC_vaxCarr_icepack_needs_total, "L")
  })
  
  output$Init_ice_freezeTime_monodose_FCC <- renderText({
    paste(ceiling(monodose_FCC_RCW25_icepack_needs_total / (mf314_quant * mf314_largepack_capacity)), "day(s)")
  }) # Time it takes to freeze depends on how many freezers are available and their capacity. I currently assume that we only use the MF314 freezer, which is the largest, and I specify the quantity at the beginning of this script
  
  output$ice_vol_init_monodose_FCC <- renderText({
    paste(as.numeric(monodose_FCC_RCW25_icepack_vol), "L")
  }) # we only need 0.6L ice packs to tra
  
  
  
#calculate and show results
  observeEvent(input$show_results, {
    ##########################################
    # Calculations for 10-dose only FCC
    ##########################################
    dose10_FCC_doses <- site_table$added_sites %>%
      dplyr::slice(1) %>% # for now, we are only going to concentrate on one site. User indicates which site to analyse
      dplyr::summarise(sum(near_pop, far_pop) / 10) # number of doses needed

    dose10_FCC_doses_needed <- dose10_FCC_doses * (1 + input$buffer_stock / 100) # apply buffer. This formula doesn't seem to be making any impact
    # number of RCW25s needed, based on the volume of the vaccine indicated (1 RCW25 can transport 3300 doses if vax vol = 3cm3 and 5000 doses if vax vol = 2cm3)
    if (input$vaccine_vol_dose10 == 2.1) {
      dose10_FCC_RCW25_needs <- ceiling(dose10_FCC_doses_needed / 5000) # these numbers refer to the doses along with the diluents
      dose10_FCC_vaxCarr_needs <- ceiling(dose10_FCC_doses_needed / 750) # vaccine carrier
    } else if (input$vaccine_vol_dose10 == 3) {
      dose10_FCC_RCW25_needs <- ceiling(dose10_FCC_doses_needed / 3300)
      dose10_FCC_vaxCarr_needs <- ceiling(dose10_FCC_doses_needed / 500) # vaccine carrier
    }

    # how many 0.6L ice packs will be needed for the quantity of RCW25s calculated?
    if (input$temp == "below 40") {
      RCW25_icepack_needs <- 12
    } else if (input$temp == "above 40") {
      RCW25_icepack_needs <- 18
    }


    # how many 0.4L ice packs will be needed for the quantity of vax carriers calculated?
    if (input$temp == "below 40") {
      vaxCarr_icepack_needs <- 6
    } else if (input$temp == "above 40") {
      vaxCarr_icepack_needs <- 8
    }

    # 10-dose FCC RCW25 icepack needs
    dose10_FCC_vaxCarr_icepack_needs_total <- vaxCarr_icepack_needs * dose10_FCC_vaxCarr_needs # total number of 0.6L ice packs = number of RCW25 needed * number of ice packs needed per RCW25
    dose10_FCC_vaxCarr_icepack_vol <- dose10_FCC_vaxCarr_icepack_needs_total * 0.4 # total volume of ice packs needed is simply the above calculation * 0.6L


    # 10-dose FCC Vaccine carrier icepack needs
    dose10_FCC_RCW25_icepack_needs_total <- RCW25_icepack_needs * dose10_FCC_RCW25_needs # total number of 0.6L ice packs = number of RCW25 needed * number of ice packs needed per RCW25
    dose10_FCC_RCW25_icepack_vol <- dose10_FCC_RCW25_icepack_needs_total * 0.6 # total volume of ice packs needed is simply the above calculation * 0.6L



    # outputs for 10-dose FCC calculations
    output$ice_packs_required_dose10_FCC <- renderText({
      paste(dose10_FCC_RCW25_icepack_needs_total + dose10_FCC_vaxCarr_icepack_needs_total, "L")
    })

    output$Init_ice_freezeTime_dose10_FCC <- renderText({
      paste(ceiling((dose10_FCC_RCW25_icepack_needs_total) / (mf314_quant * mf314_largepack_capacity)), "day(s)")
      }) # Time it takes to freeze depends on how many freezers are available and their capacity. I currently assume that we only use the MF314 freezer, which is the largest, and I specify the quantity at the beginning of this script

    output$ice_vol_init_dose10_FCC <- renderText({
      paste(as.numeric(dose10_FCC_RCW25_icepack_vol), "L")
    }) # we only need 0.6L ice packs to transport the vaccines in the RCW25s. The 0.4L ones don't to play here yet



  


})

}

# Run the application
shinyApp(ui = ui, server = server)