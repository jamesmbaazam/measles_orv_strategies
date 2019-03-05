library(shiny)

#source helper scripts
source('./calculation_functions.R')

# Note: Throughout the code, FCC means Full Cold Chain, dose10 means 10-dose


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
      
      br(),
      numericInput('mf314_quant', 'Number of MF314 available', value = 1, min = 1, step = 1),
      
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
                         wellPanel(htmlOutput("Init_ice_freezeTime_monodose_FCC")),
                         tags$h4("Total required team days (over all sites)"),
                         wellPanel(htmlOutput("teamDays_req_monodoseFCC"))
                       ),
                       tabPanel(
                         title = "10-dose FCC",
                         tags$h4("Initial volume of ice required for transport"),
                         wellPanel(htmlOutput("ice_vol_init_dose10_FCC")),
                         tags$h4("Quantity of ice packs required"),
                         wellPanel(htmlOutput("ice_packs_required_dose10_FCC")),
                         tags$h4("Time needed to freeze initial ice"),
                         wellPanel(htmlOutput("Init_ice_freezeTime_dose10_FCC")),
                         tags$h4("Total required team days (over all sites)"),
                         wellPanel(htmlOutput("teamDays_req_dose10_FCC"))
                       ),
                       tabPanel(
                         title = "Mixed FCC",
                         tags$h4("Initial volume of ice required for transport"),
                         wellPanel(htmlOutput("ice_vol_init_mixed_FCC")),
                         tags$h4("Quantity of ice packs required"),
                         wellPanel(htmlOutput("ice_packs_required_mixed_FCC")),
                         tags$h4("Time needed to freeze initial ice"),
                         wellPanel(htmlOutput("Init_ice_freezeTime_mixed_FCC")),
                         tags$h4("Total required team days (over all sites)"),
                         wellPanel(htmlOutput("teamDays_req_mixedFCC"))
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
      numericInput("dist_from_base_new", "Distance from field base to site (km)?", value = 0, min = 0, step = 0.01),
      numericInput("near_pop_new", "On site, how many people can be served with a fixed post?", value = 0, min = 0, step = 1),
      numericInput("far_pop_new", "On site, how many people should be served by mobile teams?", value = 0, min = 0, step = 1),
      numericInput("site_team_req", "How many teams are available for this site?", value = 0, min = 0, step = 1),
      #numericInput("team_days_fixed_new", "How long does a fixed post need to serve target near population (days)?", value = 0, min = 0, step = 0.1),
      #numericInput("team_days_mobile_new", "How long does a mobile team need to serve target far population (days)?", value = 0, min = 0, step = 0.1),
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
  
  
  
  # calculate and show results
  observeEvent(input$show_results, {
    
    
    # how many 0.6L ice packs will be needed for the quantity of RCW25s calculated?
    RCW25_icepack_needs <-  switch (input$temp,
            "below 40" = 12,
            "above 40" = 18
    )
    
    vaxCarr_icepack_needs <- switch (input$temp,
                                     "below 40" = 6,
                                     "above 40" = 8
    )
    
    
    # 
    # if (input$temp == "below 40") {
    #   RCW25_icepack_needs <- 12
    # } else if (input$temp == "above 40") {
    #   RCW25_icepack_needs <- 18
    # }
    
    
    # how many 0.4L ice packs will be needed for the quantity of vax carriers calculated?
    # if (input$temp == "below 40") {
    #   vaxCarr_icepack_needs <- 6
    # } else if (input$temp == "above 40") {
    #   vaxCarr_icepack_needs <- 8
    # }
    # 
    
    ##########################################
    # Calculations for monodose-only FCC
    ##########################################
    
    monodose_FCC_doses <- site_table$added_sites %>%
      dplyr::slice(1) %>% # for now, we are only going to concentrate on one site. User indicates which site to analyse
      dplyr::summarise(sum(near_pop, far_pop)) # number of doses needed
    
    monodose_FCC_doses_needed <- monodose_FCC_doses * (1 + input$buffer_stock / 100) # apply buffer. This formula doesn't seem to be making any impact
    
    # number of RCW25s needed, based on the volume of the vaccine indicated (1 RCW25 can transport 1301 vials/doses and a vaccine carrier can transport 283 vials/doses)
    
    monodose_FCC_RCW25_needs <- ceiling(monodose_FCC_doses_needed / 1301) # these numbers refer to the doses along with the diluents
    monodose_vaxCarr_needs <- ceiling(monodose_FCC_doses_needed / 283) # vaccine carrier
    
    
    # Monodose FCC RCW25 icepack needs
    monodose_FCC_vaxCarr_icepack_needs_total <- vaxCarr_icepack_needs * monodose_vaxCarr_needs # total number of 0.6L ice packs = number of RCW25 needed * number of ice packs needed per RCW25
    monodose_FCC_vaxCarr_icepack_vol <- monodose_FCC_vaxCarr_icepack_needs_total * 0.4 # total volume of ice packs needed is simply the above calculation * 0.6L
    
    
    # Monodose FCC Vaccine carrier icepack needs
    monodose_FCC_RCW25_icepack_needs_total <- RCW25_icepack_needs * monodose_FCC_RCW25_needs # total number of 0.6L ice packs = number of RCW25 needed * number of ice packs needed per RCW25
    monodose_FCC_RCW25_icepack_vol <- monodose_FCC_RCW25_icepack_needs_total * 0.6 # total volume of ice packs needed is simply the above calculation * 0.6L
    
    #total needs
    monodose_FCC_init_icepack_quant <- monodose_FCC_RCW25_icepack_needs_total + monodose_FCC_vaxCarr_icepack_needs_total
    ###
    # outputs for monodose FCC calculations
    ###
    output$ice_packs_required_monodose_FCC <- renderText({
      paste(monodose_FCC_RCW25_icepack_needs_total + monodose_FCC_vaxCarr_icepack_needs_total)
    })
    
    
    monodose_FCC_ft <- ceiling(monodose_FCC_init_icepack_quant / (input$mf314_quant * (mf314_largepack_fr + mf314_smallpack_fr))) # freezing time for icepacks
    
    # output
    output$Init_ice_freezeTime_monodose_FCC <- renderText({
      paste(monodose_FCC_ft, "day(s)")
    }) # Time it takes to freeze depends on how many freezers are available and their capacity. I currently assume that we only use the MF314 freezer, which is the largest, and I specify the quantity at the beginning of this script
    
    monodose_FCC_init_iceVol <- monodose_FCC_RCW25_icepack_vol + monodose_FCC_vaxCarr_icepack_vol
    output$ice_vol_init_monodose_FCC <- renderText({
      paste(as.numeric(monodose_FCC_init_iceVol), "L")
    }) # we only need 0.6L ice packs to tra
    
    
    
    
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
    
    
    # 10-dose FCC RCW25 icepack needs
    dose10_FCC_vaxCarr_icepack_needs_total <- vaxCarr_icepack_needs * dose10_FCC_vaxCarr_needs # total number of 0.6L ice packs = number of RCW25 needed * number of ice packs needed per RCW25
    dose10_FCC_vaxCarr_icepack_vol <- dose10_FCC_vaxCarr_icepack_needs_total * 0.4 # total volume of ice packs needed is simply the above calculation * 0.6L
    
    
    # 10-dose FCC Vaccine carrier icepack needs
    dose10_FCC_RCW25_icepack_needs_total <- RCW25_icepack_needs * dose10_FCC_RCW25_needs # total number of 0.6L ice packs = number of RCW25 needed * number of ice packs needed per RCW25
    dose10_FCC_RCW25_icepack_vol <- dose10_FCC_RCW25_icepack_needs_total * 0.6 # total volume of ice packs needed is simply the above calculation * 0.6L
    
    
    
    # Initial number of icepacks required
    dose10_FCC_init_icepack_quant <- dose10_FCC_RCW25_icepack_needs_total + dose10_FCC_vaxCarr_icepack_needs_total
    output$ice_packs_required_dose10_FCC <- renderText({
      paste(dose10_FCC_init_icepack_quant)
    })
    
    
    dose10_FCC_ft <- ceiling((dose10_FCC_init_icepack_quant) / (input$mf314_quant * (mf314_largepack_fr + mf314_smallpack_fr)))
    # freezing time
    output$Init_ice_freezeTime_dose10_FCC <- renderText({
      paste(dose10_FCC_ft, "day(s)")
    }) # Time it takes to freeze depends on how many freezers are available and their capacity. I currently assume that we only use the MF314 freezer, which is the largest, and I specify the quantity at the beginning of this script
    
    #Initial volume of ice required 
    dose10_FCC_init_iceVol <- dose10_FCC_RCW25_icepack_vol + dose10_FCC_vaxCarr_icepack_vol
    output$ice_vol_init_dose10_FCC <- renderText({
      paste(as.numeric(dose10_FCC_RCW25_icepack_vol), "L")
    }) # we only need 0.6L ice packs to transport the vaccines in the RCW25s. The 0.4L ones don't to play here yet
    
    
    
    
    ##########################################
    #' Calculations for mixed strategy, i.e 10-dose for near population and monodose for far population
    ##########################################
    
    mixed_FCC_dose10_quant <- site_table$added_sites %>%
      dplyr::slice(1) %>% # for now, we are only going to concentrate on one site. User indicates which site to analyse
      .$near_pop/10
    
    mixed_FCC_monodose_quant <- site_table$added_sites %>%
      dplyr::slice(1) %>% # for now, we are only going to concentrate on one site. User indicates which site to analyse
      .$far_pop 
    
    
    mixed_FCC_doses <- mixed_FCC_dose10_quant + mixed_FCC_monodose_quant
    
    mixed_FCC_dose10_final <- mixed_FCC_dose10_quant* (1 + input$buffer_stock / 100)
    
    mixed_FCC_monodose_final <- mixed_FCC_monodose_quant* (1 + input$buffer_stock / 100)
    
    mixed_FCC_doses_needed <- mixed_FCC_dose10_final + mixed_FCC_monodose_final # apply buffer. This formula doesn't seem to be making any impact
    
    
    # passive cold chain needed, based on the volume of the vaccine indicated (1 RCW25 can transport 3300 doses if vax vol = 3cm3 and 5000 doses if vax vol = 2cm3)
    if (input$vaccine_vol_dose10 == 2.1) {
      mixed_FCC_dose10_RCW25_needs <- ceiling(mixed_FCC_dose10_final / 5000) # these numbers refer to the doses along with the diluents
      mixed_FCC_dose10_vaxCarr_needs <- ceiling(mixed_FCC_dose10_final / 750) # vaccine carrier
    } else if (input$vaccine_vol_dose10 == 3) {
      mixed_FCC_dose10_RCW25_needs <- ceiling(mixed_FCC_dose10_final / 3300)
      mixed_FCC_dose10_vaxCarr_needs <- ceiling(mixed_FCC_dose10_final / 500) # vaccine carrier
    }
    
    
    #passive cold chain required for monodose vials
    mixed_FCC_monodose_RCW25_needs <- ceiling(mixed_FCC_monodose_final/1301)
    mixed_FCC_monodose_vaxCarr_needs <- ceiling(mixed_FCC_monodose_final/283)
    
    
    #total passive cold chain needs
    
    #RCW25
    mixed_FCC_RCW25_needs <- mixed_FCC_dose10_RCW25_needs + mixed_FCC_monodose_RCW25_needs
    
    #vaccine carriers
    mixed_FCC_vaxCarr_needs <- mixed_FCC_dose10_vaxCarr_needs + mixed_FCC_monodose_vaxCarr_needs
    
    # mixed FCC quantity of icepack needs
    
    #0.6L ice packs for RCW 25
    mixed_FCC_RCW25_icepack_needs <- mixed_FCC_RCW25_needs * RCW25_icepack_needs
    
    
    #0.4L ice packs for vaccine carriers
    mixed_FCC_vaxCarr_icepack_needs <- mixed_FCC_vaxCarr_needs * vaxCarr_icepack_needs
    
    #total icepack needs
    mixed_FCC_icepack_needs <- mixed_FCC_vaxCarr_icepack_needs + mixed_FCC_RCW25_icepack_needs
    
    #total volume of ice packs
    
    #0.4L
    mixed_FCC_vaxCarr_icepack_vol <- mixed_FCC_vaxCarr_icepack_needs * 0.4
    #0.6L
    mixed_FCC_RCW25_icepack_vol <- mixed_FCC_RCW25_icepack_needs * 0.6
    
    
    
    
    
    ###
    #' outputs for the mixed strategy
    ###
    
    #initial volume of ice required 
    mixed_FCC_init_iceVol <- mixed_FCC_vaxCarr_icepack_vol + mixed_FCC_RCW25_icepack_vol
    output$ice_vol_init_mixed_FCC <- renderText({
      paste(as.numeric(mixed_FCC_init_iceVol), "L")
    }) # we only need 0.6L ice packs to transport the vaccines in the RCW25s. The 0.4L ones don't to play here yet
    
    
    mixed_FCC_ft <- ceiling(mixed_FCC_icepack_needs/(input$mf314_quant * (mf314_largepack_fr + mf314_smallpack_fr)))
    # freezing time
    output$Init_ice_freezeTime_mixed_FCC <- renderText({
      paste(mixed_FCC_ft, "day(s)")
    }) # 
    
    
    # Initial number of icepacks required
    mixed_FCC_init_icepack_quant <- mixed_FCC_RCW25_icepack_needs + mixed_FCC_vaxCarr_icepack_needs
    output$ice_packs_required_mixed_FCC <- renderText({
      paste(mixed_FCC_init_icepack_quant)
    })
    
    
    
    
    
    
    
    
    
    ################################################################
    #' Plots
    #' 
    #####################################################################
    # Results of required freezing time per strategy
    freezing_time_results <- tibble(
      Strategy = c("monodose FCC", "10-dose FCC", "mixed FCC"),
      time = c(monodose_FCC_ft, dose10_FCC_ft, mixed_FCC_ft)
    )
    
    # Results of initial required volume of ice per strategy
    Init_iceVol_results <- tibble(
      Strategy = c("monodose FCC", "10-dose FCC", "mixed FCC"),
      iceVol = c(monodose_FCC_init_iceVol, dose10_FCC_init_iceVol, mixed_FCC_init_iceVol)
    )
    output$plot <- renderPlot({
      ft_plot <- ggplot(data = freezing_time_results, aes(x = Strategy, y = time)) + 
        geom_bar(stat = "identity", fill = "steelblue") + 
        ylab("Freezing time required (days)") +
        coord_flip()
      
      iceVol_plot <- ggplot(data = Init_iceVol_results, aes(x = Strategy, y = iceVol)) + 
        geom_bar(stat = "identity", fill = "steelblue") + 
        ylab("Initial volume of ice required (Litres)") + 
        coord_flip()
      
      #arrange the plots on a grid
      grid.arrange(ft_plot, iceVol_plot, ncol = 2)
    })
  })
}






# Run the application
shinyApp(ui = ui, server = server)