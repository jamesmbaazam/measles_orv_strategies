library(shiny)
library(dplyr)
library(readr)
library(readxl)
library(gridExtra)
library(ggplot2)
library(shinythemes)
library(DT)


# Note: Throughout the code, FCC means Full Cold Chain, dose10 means 10-dose,
#OCC = Out of Cold Chain
#source helper scripts
source('./params.R', local = TRUE)

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
      
      # actionButton("remove_site", "Click to remove a site"),
      # 
      # br(),
      # 
      # br(),
      
      selectInput("temp", "Ambient temperature", choices = c("below 40", "above 40")),
      
      br(),
      numericInput("buffer_stock", "Buffer stock (out of 100)", value = 1, min = 1, step = 0.01),
      
      br(),
      selectInput("vaccine_vol_dose10", "10-dose: Volume per dose (cm3)", choices = dose10_vial_vol, selected = 2.1),
      
      br(),
      selectInput("vaccine_vol_monodose", "Monodose Volume per dose (cm3)", choices = monodose_vial_vol),
      
      br(),
      numericInput('mf314_quant', 'Number of MF314 available', value = 1, min = 1, step = 1),
      
      br(),
      # sliderInput(inputId = 'sites_to_analyse', label = 'Site/range of sites to analyse', min = 1, max = 200, value = c(1, 1), step = 1),
      # 
      # br(),
      actionButton("show_results", "Display results"),   
      
      br(),
      
      br(),
      
      actionButton("clear_all", "Click to clear app!")
    ),
    
    
    column(
      8,
      tabsetPanel(
        tabPanel(title = "Table of sites added", DT::dataTableOutput("all_sites", width = "90%", height = "100%")),
        tabPanel(title = "Strategy comparison plots", plotOutput("plot")),
        tabPanel(title = 'Help', textOutput('help'))
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
                         tags$h4("Fixed team days required (over all sites)"),
                         wellPanel(htmlOutput("tdf_monodoseFCC")), #tdf = team days fixed
                         tags$h4("Mobile team days required (over all sites)"),
                         wellPanel(htmlOutput("tdm_monodoseFCC")), 
                         tags$h4("Duration of team types on site"),
                         wellPanel(htmlOutput("team_dur_monodoseFCC")) 
                       ),
                       tabPanel(
                         title = "10-dose FCC",
                         tags$h4("Initial volume of ice required for transport"),
                         wellPanel(htmlOutput("ice_vol_init_dose10_FCC")),
                         tags$h4("Quantity of ice packs required"),
                         wellPanel(htmlOutput("ice_packs_required_dose10_FCC")),
                         tags$h4("Time needed to freeze initial ice"),
                         wellPanel(htmlOutput("Init_ice_freezeTime_dose10_FCC")),
                         tags$h4("Fixed team days required (over all sites)"),
                         wellPanel(htmlOutput("tdf_dose10_FCC")), #tdf = team days fixed
                         tags$h4("Mobile team days required (over all sites)"),
                         wellPanel(htmlOutput("tdm_dose10_FCC")), #tdm = team days mobile
                         tags$h4("Duration of team types on site"),
                         wellPanel(htmlOutput("team_dur_dose10_FCC"))  
                       ),
                       tabPanel(
                         title = "Mixed FCC",
                         tags$h4("Initial volume of ice required for transport"),
                         wellPanel(htmlOutput("ice_vol_init_mixed_FCC")),
                         tags$h4("Quantity of ice packs required"),
                         wellPanel(htmlOutput("ice_packs_required_mixed_FCC")),
                         tags$h4("Time needed to freeze initial ice"),
                         wellPanel(htmlOutput("Init_ice_freezeTime_mixed_FCC")),
                         tags$h4("Fixed team days required (over all sites)"),
                         wellPanel(htmlOutput("tdf_mixed_FCC")), #tdf = team days fixed
                         tags$h4("Mobile team days required (over all sites)"),
                         wellPanel(htmlOutput("tdm_mixed_FCC")), #tdm = team days mobile
                         tags$h4("Duration of team types on site"),
                         wellPanel(htmlOutput("team_dur_mixed_FCC"))  
                       ),
                       tabPanel(
                         title = "Part OCC",
                         tags$h4("Initial volume of ice required for transport"),
                         wellPanel(htmlOutput("ice_vol_init_part_OCC")),
                         tags$h4("Quantity of ice packs required"),
                         wellPanel(htmlOutput("ice_packs_required_part_OCC")),
                         tags$h4("Time needed to freeze initial ice"),
                         wellPanel(htmlOutput("Init_ice_freezeTime_part_OCC")),
                         tags$h4("Fixed team days required (over all sites)"),
                         wellPanel(htmlOutput("tdf_part_OCC")), #tdf = team days fixed
                         tags$h4("Mobile team days required (over all sites)"),
                         wellPanel(htmlOutput("tdm_part_OCC")), #tdm = team days mobile
                         tags$h4("Duration of team types on site"),
                         wellPanel(htmlOutput("team_dur_part_OCC"))  
                       )
                     )
  ))
)

server <- function(input, output, session) {

  #source helper scripts
  source('./params.R', local = TRUE)
  source('./calculator_functions.R', local = TRUE)
  
  # empty data frame for storing all the sites added
  site_table <- reactiveValues(added_sites = NULL)
  
  # empty dataframe for temporarily storing the newly added site for updating the old table
  site_new <- reactiveValues(data = NULL)
  
  
  
  # dialog box to collect new site information
  observeEvent(input$add_site_info, {
    showModal(modalDialog(
      title = "Site information",
      numericInput("dist_from_base_new", "Distance from field base to site (km)?", value = 0, min = 0, step = 0.01),
      numericInput("near_pop_new", "On site, how many people can be served with a fixed post?", value = 0, min = 0, step = 1),
      numericInput("far_pop_new", "On site, how many people should be served by mobile teams?", value = 0, min = 0, step = 1),
      numericInput("site_team_alloc_new", "How many teams are available for this site?", value = 0, min = 0, step = 1),
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
        input$site_team_alloc_new == 0
        )
      {
      showModal(
        modalDialog(
        title = "warning!", "No information was added. So, no site will be added")
                ) # if user doesn't change the default values, but mistakenly clicks on add site, no site will be added.
    } else {
        site_new$data <- tibble(
        dist_from_base = input$dist_from_base_new,
        near_pop = input$near_pop_new,
        far_pop = input$far_pop_new,
        site_team_alloc = input$site_team_alloc_new
      )
      site_table$added_sites <- bind_rows(site_table$added_sites, site_new$data)
      removeModal()
  }
    output$all_sites <- DT::renderDataTable(site_table$added_sites, selection = num_of_sites_selectable)
  })
  
  
  
  #clear the app when the clear action button is clicked.
  # observeEvent(input$clear_all,{
  #   output$ice_packs_required_monodose_FCC <- renderText({})
  #   output$ice_packs_required_dose10_FCC<- renderText({})
  #   output$ice_packs_required_mixed_FCC<- renderText({})
  #   output$tdm_monodose_FCC <- renderText({})
  #   output$tdm_dose10_FCC <- renderText({})
  #   output$tdm_mixed_FCC <- renderText({})
  #   output$all_sites <- renderText({})
  #   output$plot <- renderText({})
  # })
  # 
  
  # dialog box to remove sites
  # observeEvent(input$remove_site, {
  #   showModal(modalDialog(
  #     title = "Enter site row number",
  #     numericInput("site_to_remove", "Which site do you want to remove?", value = 0, min = 0, step = 1),
  #     fade = TRUE,
  #     footer = tagList(
  #       modalButton("Dismiss"),
  #       actionButton("remove_site_button_click", "Remove site")
  #     )
  #   ))
  # })
  # 
  # observeEvent(input$remove_site_button_click, {
  #   if(input$site_to_remove == 0 | input$site_to_remove > dim(site_table$added_sites)[1]){
  #     showModal(
  #       modalDialog(
  #         title = "Wrong input! No site will be removed")
  #     ) 
  #   }else{
  #     showModal(
  #       modalDialog(
  #         title = paste('Site', input$site_to_remove, 'will be removed and site table will be updated'))
  #     ) 
  #     site_table$added_sites <- dplyr::slice(site_table$added_sites, -input$site_to_remove)
  #     output$all_sites <- DT::renderDT(site_table$added_sites)
  #   }
  # })
  # 
  
  # dialog box to analyse sites
  
  # calculate and show results
  observeEvent(input$show_results, {
    if(is.null(input$all_sites_rows_selected)){
      showModal(
        modalDialog(
          title = 'No site selected for analysis!', "Analyse site information by clicking the desired row in the sites table, followed by the 'Display Results' button to show the results!")
      ) 
    }else{
    # how many 0.6L ice packs will be needed for the quantity of RCW25s calculated?
    RCW25_icepack_needs <-  compute_rcw25_icepacks(input$temp)
    
    vaxCarr_icepack_needs <- compute_vaxCarr_icepacks(input$temp)
    
    
    ##########################################
    # Calculations for monodose-only FCC
    ##########################################
    
    monodose_FCC_doses_ft <- calc_doses_required(df = site_table$added_sites
                                                 ,  site_rows_selected = input$all_sites_rows_selected
                                                 , is_dose10 = F
                                                 , pop_type = 'near')
    
    monodose_FCC_doses_mt <- calc_doses_required(df = site_table$added_sites
                                                 ,  site_rows_selected = input$all_sites_rows_selected
                                                 , is_dose10 = F
                                                 , pop_type = 'far')
    
    monodose_FCC_doses <-  monodose_FCC_doses_ft + monodose_FCC_doses_mt 
    
    monodose_FCC_doses_needed <- monodose_FCC_doses * (1 + input$buffer_stock / 100) # apply buffer. This formula doesn't seem to be making any impact
    
    # number of RCW25s needed, based on the volume of the vaccine indicated (Without ice, 1 RCW25 can transport 1301 vials/doses and a vaccine carrier can transport 170 vials/doses. With ice, the numbers are 616 and 77 per our calculations)
    
    monodose_FCC_RCW25_needs <- calc_transport_equipment_needs(equip_type = 'rcw25'
                                                               , vial_type = 'monodose'
                                                               , vax_vol = input$vaccine_vol_monodose
                                                               , with_ice = T
                                                               , doses_to_transport = monodose_FCC_doses_needed
                                                               )

    monodose_FCC_vaxCarr_needs <- calc_transport_equipment_needs(equip_type = 'vaxCarr'
                                                             , vial_type = 'monodose'
                                                             , vax_vol = input$vaccine_vol_monodose
                                                             , with_ice = T
                                                             , doses_to_transport = monodose_FCC_doses_needed
                                                             )
    
    # Monodose FCC RCW25 icepack needs
    monodose_FCC_vaxCarr_icepack_needs_total <- vaxCarr_icepack_needs * monodose_FCC_vaxCarr_needs # total number of 0.6L ice packs = number of RCW25 needed * number of ice packs needed per RCW25
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
    
    
    monodose_FCC_ft <- calc_freezing_time(mf314_available = input$mf314_quant
                                          , large_icepacks_quantity = monodose_FCC_RCW25_icepack_needs_total 
                                          , small_icepacks_quantity = monodose_FCC_vaxCarr_icepack_needs_total)
    # output
    output$Init_ice_freezeTime_monodose_FCC <- renderText({
      paste(monodose_FCC_ft, "day(s)")
    }) # Time it takes to freeze depends on how many freezers are available and their capacity. I currently assume that we only use the MF314 freezer, which is the largest, and I specify the quantity at the beginning of this script
    
    monodose_FCC_init_iceVol <- monodose_FCC_RCW25_icepack_vol + monodose_FCC_vaxCarr_icepack_vol
    output$ice_vol_init_monodose_FCC <- renderText({
      paste(as.numeric(monodose_FCC_init_iceVol), "L")
    }) # we only need 0.6L ice packs to tra
    
    monodose_FCC_near_pop <- extract_near_pop(site_table$added_sites, site_rows_selected = input$all_sites_rows_selected)
    monodose_FCC_far_pop <- extract_far_pop(site_table$added_sites, site_rows_selected = input$all_sites_rows_selected)
    
    #this calculates the number of doses we can transport for the far campaign. We will then find out if we can transport more or less irrespective of how many we are expected to vaccinate, i.e, team performance/vaccination rate
    monodose_FCC_far_trip_capacity <- calc_dose_capacity(vial_type = 'monodose' 
                                                , vax_vol = 21.09
                                                , equip_type = 'vaxCarr' #we assume a mobile team uses one vaccine carrier
                                                , with_ice = T)
    
    #monodose_FCC_mt_vax_capacity <- ifelse(monodose_FCC_far_trip_capacity < tp_mobile, monodose_FCC_far_trip_capacity, tp_mobile) #if how much we can carry is less than the expected vaccination rate, then the volume constraint becomes the denominator. mt = mobile team
    
    team_days_fixed_monodose_FCC <- round(monodose_FCC_near_pop / tp_fixed, 1) #computationally, we see the number doses as the number of expected people. The "final number of doses" here have already accounted for the buffer
    team_days_mobile_monodose_FCC <- round(monodose_FCC_far_pop / monodose_FCC_far_trip_capacity, 1)
    
    
    #output for team days required for fixed teams
    output$tdf_monodoseFCC <- renderText({
      paste0(as.numeric(team_days_fixed_monodose_FCC))
    })
    
    #output for team days required for mobile teams
    output$tdm_monodoseFCC <- renderText({
      paste0(as.numeric(team_days_mobile_monodose_FCC))
    })
    
    #######
    # Team allocation calculations and output
    #######
    
    #Extract size of allocated team from the sites table
    site_teams_monodoseFCC <- extract_site_team_size(site_table$added_sites, site_rows_selected = input$all_sites_rows_selected)

    # output for the duration that each team type will spend on site
    output$team_dur_monodoseFCC <- print_site_team_dur(site_team_quant = site_teams_monodoseFCC
                                                       , td_fixed = team_days_fixed_monodose_FCC
                                                   , td_mobile = team_days_mobile_monodose_FCC
                                                   )
 
    
    ##########################################
    # Calculations for 10-dose only FCC
    ##########################################
      
    dose10_FCC_doses_near_pop <-  calc_doses_required(df = site_table$added_sites
                                                     ,  site_rows_selected = input$all_sites_rows_selected
                                                     , is_dose10 = T
                                                     , pop_type = 'near')
    
    dose10_FCC_doses_far_pop <-  calc_doses_required(df = site_table$added_sites
                                                    ,  site_rows_selected = input$all_sites_rows_selected
                                                    , is_dose10 = T
                                                    , pop_type = 'far')
    
    #doses required for near population after wastage penalty
    dose10_FCC_doses_near_pop_req <- dose10_FCC_doses_near_pop * (1 + dose10_wr_ft)
    
    #doses required for far population after wastage penalty
    dose10_FCC_doses_far_pop_req <- dose10_FCC_doses_far_pop * (1 + dose10_wr_mt)
    
    #number of doses required after buffer 
    dose10_FCC_doses_needed <- (dose10_FCC_doses_near_pop_req + dose10_FCC_doses_far_pop_req) * (1 + input$buffer_stock / 100)
    

    dose10_FCC_RCW25_needs <- calc_transport_equipment_needs(equip_type = 'rcw25'
                                   , vial_type = 'dose10'
                                   , vax_vol = input$vaccine_vol_dose10
                                   , with_ice = T
                                   , doses_to_transport = dose10_FCC_doses_needed
                                   )
    
    dose10_FCC_vaxCarr_needs <- calc_transport_equipment_needs(equip_type = 'vaxCarr'
                                                              , vial_type = 'dose10'
                                                              , vax_vol = input$vaccine_vol_dose10
                                                              , with_ice = T
                                                              , doses_to_transport = dose10_FCC_doses_needed
                                                              )
    
    
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
    
    # freezing time
    dose10_FCC_ft <- calc_freezing_time(mf314_available = input$mf314_quant
                                        , large_icepacks_quantity = dose10_FCC_RCW25_icepack_needs_total 
                                        , small_icepacks_quantity = dose10_FCC_vaxCarr_icepack_needs_total)
      
      
   
    output$Init_ice_freezeTime_dose10_FCC <- renderText({
      paste(dose10_FCC_ft, "day(s)")
    }) # Time it takes to freeze depends on how many freezers are available and their capacity. I currently assume that we only use the MF314 freezer, which is the largest, and I specify the quantity at the beginning of this script
    
    #Initial volume of ice required 
    dose10_FCC_init_iceVol <- dose10_FCC_RCW25_icepack_vol + dose10_FCC_vaxCarr_icepack_vol
    output$ice_vol_init_dose10_FCC <- renderText({
      paste(as.numeric(dose10_FCC_init_iceVol), "L")
    }) # we only need 0.6L ice packs to transport the vaccines in the RCW25s. The 0.4L ones don't to play here yet
    
    ##team days calculations
    #size of near population
    
    dose10_FCC_near_pop <- extract_near_pop(site_table$added_sites, site_rows_selected = input$all_sites_rows_selected)
    dose10_FCC_far_pop <- extract_far_pop(site_table$added_sites, site_rows_selected = input$all_sites_rows_selected)
    
    
    
    dose10_FCC_far_trip_capacity <- calc_dose_capacity(vial_type = 'dose10' 
                                                         , vax_vol = input$vaccine_vol_dose10
                                                         , equip_type = 'vaxCarr' #we assume a mobile team uses one vaccine carrier
                                                         , with_ice = T)
    
    
    dose10_FCC_far_trip_eff_doses <- dose10_FCC_far_trip_capacity * (1 - dose10_wr_mt)  #The effective number of doses a team has is the total capacity they can carry less of how many are expected to be wasted.
    
    team_days_fixed_dose10_FCC <- round(dose10_FCC_near_pop / tp_fixed, 1) #computationally, we see the number doses as the number of expected people. The "final number of doses" here have already accounted for the buffer
    team_days_mobile_dose10_FCC <- round(dose10_FCC_far_pop / dose10_FCC_far_trip_eff_doses, 1)
    
    
    #output for team days required for fixed teams
    output$tdf_dose10_FCC <- renderText({
      paste0(as.numeric(team_days_fixed_dose10_FCC))
    })
    
    #output for team days required for mobile teams
    output$tdm_dose10_FCC <- renderText({
      paste0(as.numeric(team_days_mobile_dose10_FCC))
    })
    
    #######
    # Team allocation calculations and output
    #######
    
    #Extract size of allocated team from the sites table
    site_teams_dose10_FCC <- extract_site_team_size(site_table$added_sites, site_rows_selected = input$all_sites_rows_selected)
    
    # output for the duration that each team type will spend on site
    output$team_dur_dose10_FCC <- print_site_team_dur(site_team_quant = site_teams_dose10_FCC
                                                       , td_fixed = team_days_fixed_dose10_FCC
                                                       , td_mobile = team_days_mobile_dose10_FCC
    )
    
    
    
    ##########################################
    #' Calculations for mixed strategy, i.e 10-dose for near population and monodose for far population
    ##########################################
    
    mixed_FCC_dose10_quant <- calc_doses_required(df = site_table$added_sites
                                                 ,  site_rows_selected = input$all_sites_rows_selected
                                                 , is_dose10 = T
                                                 , pop_type = 'near')
    
    mixed_FCC_monodose_quant <- calc_doses_required(df = site_table$added_sites
                                                   ,  site_rows_selected = input$all_sites_rows_selected
                                                   , is_dose10 = F
                                                   , pop_type = 'far')
    
    
    mixed_FCC_doses <- mixed_FCC_dose10_quant + mixed_FCC_monodose_quant
    
    mixed_FCC_dose10_final <- mixed_FCC_dose10_quant* (1 + input$buffer_stock / 100)
    
    mixed_FCC_monodose_final <- mixed_FCC_monodose_quant* (1 + input$buffer_stock / 100)
    
    mixed_FCC_doses_needed <- mixed_FCC_dose10_final + mixed_FCC_monodose_final # apply buffer. This formula doesn't seem to be making any impact
    
    
    # passive cold chain needed, based on the volume of the vaccine indicated (1 RCW25 can transport 3300 doses if vax vol = 3cm3 and 5000 doses if vax vol = 2cm3)
    # if (input$vaccine_vol_dose10 == 2.1) {
    #   mixed_FCC_dose10_RCW25_needs <- ceiling(mixed_FCC_dose10_final / 5000) # these numbers refer to the doses along with the diluents
    #   mixed_FCC_dose10_vaxCarr_needs <- ceiling(mixed_FCC_dose10_final / 750) # vaccine carrier
    # } else if (input$vaccine_vol_dose10 == 3) {
    #   mixed_FCC_dose10_RCW25_needs <- ceiling(mixed_FCC_dose10_final / 3300)
    #   mixed_FCC_dose10_vaxCarr_needs <- ceiling(mixed_FCC_dose10_final / 500) # vaccine carrier
    # }
    
    mixed_FCC_dose10_RCW25_needs <- calc_transport_equipment_needs(equip_type = 'rcw25'
                                   , vial_type = 'dose10'
                                   , vax_vol = input$vaccine_vol_dose10
                                   , with_ice = T
                                   , doses_to_transport = mixed_FCC_dose10_final
                                   )
    
    mixed_FCC_dose10_vaxCarr_needs <- calc_transport_equipment_needs(equip_type = 'vaxCarr'
                                                                     , vial_type = 'dose10'
                                                                     , vax_vol = input$vaccine_vol_dose10
                                                                     , with_ice = T
                                                                     , doses_to_transport = mixed_FCC_dose10_final
                                                                     )
    
    #passive cold chain required for monodose vials
    mixed_FCC_monodose_RCW25_needs <- calc_transport_equipment_needs(equip_type = 'rcw25'
                                                                     , vial_type = 'monodose'
                                                                     , vax_vol = input$vaccine_vol_monodose
                                                                     , with_ice = T
                                                                     , doses_to_transport = mixed_FCC_monodose_final
                                                                     )
      
      
    mixed_FCC_monodose_vaxCarr_needs <- calc_transport_equipment_needs(equip_type = 'vaxCarr'
                                                                       , vial_type = 'monodose'
                                                                       , vax_vol = input$vaccine_vol_monodose
                                                                       , with_ice = T
                                                                       , doses_to_transport = mixed_FCC_monodose_final
                                                                       )
    
    
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
    
    #####################################
    #' outputs for the mixed strategy
    #####################################
    
    #initial volume of ice required 
    mixed_FCC_init_iceVol <- mixed_FCC_vaxCarr_icepack_vol + mixed_FCC_RCW25_icepack_vol
    output$ice_vol_init_mixed_FCC <- renderText({
      paste(as.numeric(mixed_FCC_init_iceVol), "L")
    }) # we only need 0.6L ice packs to transport the vaccines in the RCW25s. The 0.4L ones don't to play here yet
    
    
    # freezing time
    mixed_FCC_ft <- calc_freezing_time(mf314_available = input$mf314_quant
                                       , large_icepacks_quantity = mixed_FCC_RCW25_icepack_needs 
                                       , small_icepacks_quantity = mixed_FCC_vaxCarr_icepack_needs)
    
    output$Init_ice_freezeTime_mixed_FCC <- renderText({
      paste(mixed_FCC_ft, "day(s)")
    }) # 
    
    
    # Initial number of icepacks required
    mixed_FCC_init_icepack_quant <- mixed_FCC_RCW25_icepack_needs + mixed_FCC_vaxCarr_icepack_needs
    output$ice_packs_required_mixed_FCC <- renderText({
      paste(mixed_FCC_init_icepack_quant)
    })
    
    
    
    ##team days calculations
    #size of near population 
    
    mixed_FCC_near_pop <- extract_near_pop(site_table$added_sites, site_rows_selected = input$all_sites_rows_selected)
    mixed_FCC_far_pop <- extract_far_pop(site_table$added_sites, site_rows_selected = input$all_sites_rows_selected)
    
    mixed_FCC_far_trip_capacity <- calc_dose_capacity(vial_type = 'monodose' 
                                                      , vax_vol = 21.09
                                                      , equip_type = 'vaxCarr' #we assume a mobile team uses one vaccine carrier
                                                      , with_ice = T)
    
    
    team_days_fixed_mixed_FCC<- round(mixed_FCC_near_pop / tp_fixed, 1) #computationally, we see the number doses as the number of expected people. The "final number of doses" here have already accounted for the buffer
    team_days_mobile_mixed_FCC <- round(mixed_FCC_far_pop / mixed_FCC_far_trip_capacity, 1)
    
    
    #output for team days required for fixed teams
    output$tdf_mixed_FCC <- renderText({
      paste0(as.numeric(team_days_fixed_mixed_FCC))
    })
    
    #output for team days required for mobile teams
    output$tdm_mixed_FCC <- renderText({
      paste0(as.numeric(team_days_mobile_mixed_FCC))
    })
    
    #######
    # Team allocation calculations and output
    #######
    
    #Extract size of allocated team from the sites table
    site_teams_mixed_FCC <- extract_site_team_size(site_table$added_sites, site_rows_selected = input$all_sites_rows_selected)
    
    # output for the duration that each team type will spend on site
    output$team_dur_mixed_FCC <- print_site_team_dur(site_team_quant = site_teams_mixed_FCC
                                                     , td_fixed = team_days_fixed_mixed_FCC
                                                     , td_mobile = team_days_mobile_mixed_FCC
    )
    
    
    
    ##############################################################################
    #' Calculations for partial OCC strategy, i.e 10-dose FCC for near population and monodose OCC for far population
    ##############################################################################
    
    part_OCC_dose10_quant <- calc_doses_required(df = site_table$added_sites
                                                ,  site_rows_selected = input$all_sites_rows_selected
                                                , is_dose10 = T
                                                , pop_type = 'near')
    
    part_OCC_monodose_quant <- calc_doses_required(df = site_table$added_sites
                                                   ,  site_rows_selected = input$all_sites_rows_selected
                                                   , is_dose10 = F
                                                   , pop_type = 'far')
    
    
    
   # part_OCC_doses <- part_OCC_dose10_quant + part_OCC_monodose_quant
    
    part_OCC_dose10_final <- part_OCC_dose10_quant * (1 + input$buffer_stock / 100)
    
    part_OCC_monodose_final <- part_OCC_monodose_quant * (1 + input$buffer_stock / 100)
    
    part_OCC_doses_needed <- part_OCC_dose10_final + part_OCC_monodose_final
    
    # passive cold chain needed, based on the volume of the vaccine indicated (1 RCW25 can transport 3300 doses if vax vol = 3cm3 and 5000 doses if vax vol = 2cm3)
    # if (input$vaccine_vol_dose10 == 2.1) {
    #   part_OCC_dose10_RCW25_needs <- ceiling(part_OCC_dose10_final / 5000) # these numbers refer to the doses along with the diluents. Source: Excel sheet for Appendix 23
    #   part_OCC_dose10_vaxCarr_needs <- ceiling(part_OCC_dose10_final / 750) # vaccine carrier. Source: Excel sheet for Appendix 23
    # } else if (input$vaccine_vol_dose10 == 3) {
    #   part_OCC_dose10_RCW25_needs <- ceiling(part_OCC_dose10_final / 3300) # Source: Excel sheet for Appendix 23
    #   part_OCC_dose10_vaxCarr_needs <- ceiling(part_OCC_dose10_final / 500) # vaccine carrier. Source: Excel sheet for Appendix 23
    # }
    
    part_OCC_dose10_RCW25_needs <- calc_transport_equipment_needs(equip_type = 'rcw25'
                                   , vial_type = 'dose10'
                                   , vax_vol = input$vaccine_vol_dose10
                                   , with_ice = T
                                   , doses_to_transport = part_OCC_dose10_final)
    
    
    part_OCC_dose10_vaxCarr_needs <- calc_transport_equipment_needs(equip_type = 'vaxCarr'
                                                                    , vial_type = 'dose10'
                                                                    , vax_vol = input$vaccine_vol_dose10
                                                                    , with_ice = T
                                                                    , doses_to_transport = part_OCC_dose10_final)
    
    
    #passive cold chain required for monodose vials
    part_OCC_monodose_RCW25_needs <- calc_transport_equipment_needs(equip_type = 'rcw25'
                                                                    , vial_type = 'monodose'
                                                                    , vax_vol = input$vaccine_vol_monodose
                                                                    , with_ice = F
                                                                    , doses_to_transport = part_OCC_monodose_final)
    
      
      #ceiling(part_OCC_monodose_final / 1301) #Source: Excel sheet "Cold Chain equipment"
    part_OCC_monodose_vaxCarr_needs <- calc_transport_equipment_needs(equip_type = 'vaxCarr'
                                                                      , vial_type = 'monodose'
                                                                      , vax_vol = input$vaccine_vol_monodose
                                                                      , with_ice = F
                                                                      , doses_to_transport = part_OCC_monodose_final)
    
    #total passive cold chain needs
    
    #RCW25
    part_OCC_RCW25_needs <- part_OCC_dose10_RCW25_needs + part_OCC_monodose_RCW25_needs
    
    #vaccine carriers
    part_OCC_vaxCarr_needs <- part_OCC_dose10_vaxCarr_needs + part_OCC_monodose_vaxCarr_needs
    
    # Part OCC quantity of icepack needs
    
    #0.6L ice packs for RCW 25
    part_OCC_RCW25_icepack_needs <- part_OCC_dose10_RCW25_needs * RCW25_icepack_needs #NOTE THE CHANGE HERE: WE ONLY NEED ICE PACKS FOR THE 10 DOSE VIALS, HENCE, WE DON'T CALCULATE ICE FOR THE MONODOSE FOR BOTH THE RCW 25 AND VACCINE CARRIERS
    
    #0.4L ice packs for vaccine carriers
    part_OCC_vaxCarr_icepack_needs <- part_OCC_dose10_vaxCarr_needs * vaxCarr_icepack_needs
    
    #total icepack needs
    part_OCC_icepack_needs <- part_OCC_vaxCarr_icepack_needs + part_OCC_RCW25_icepack_needs
    
    #total volume of ice packs
    
    #0.4L
    part_OCC_vaxCarr_icepack_vol <- part_OCC_vaxCarr_icepack_needs * 0.4
    #0.6L
    part_OCC_RCW25_icepack_vol <- part_OCC_RCW25_icepack_needs * 0.6
    
    
    
    
    #####################################
    #' outputs for the part OCC strategy
    #####################################
    
    #initial volume of ice required 
    part_OCC_init_iceVol <- part_OCC_vaxCarr_icepack_vol + part_OCC_RCW25_icepack_vol
    output$ice_vol_init_part_OCC <- renderText({
      paste(as.numeric(part_OCC_init_iceVol), "L")
    }) # we only need 0.6L ice packs to transport the vaccines in the RCW25s. The 0.4L ones don't to play here yet
    
    # freezing time
    part_OCC_ft <- calc_freezing_time(mf314_available = input$mf314_quant
                                      , large_icepacks_quantity = part_OCC_RCW25_icepack_needs 
                                      , small_icepacks_quantity = part_OCC_vaxCarr_icepack_needs)
      
    output$Init_ice_freezeTime_part_OCC <- renderText({
      paste(part_OCC_ft, "day(s)")
    }) # 
    
    
    # Initial number of icepacks required
    part_OCC_init_icepack_quant <- part_OCC_RCW25_icepack_needs + part_OCC_vaxCarr_icepack_needs
    output$ice_packs_required_part_OCC <- renderText({
      paste(part_OCC_init_icepack_quant)
    })
    
    
    ##team days calculations
    #size of near population 
    
    part_OCC_near_pop <- extract_near_pop(site_table$added_sites, site_rows_selected = input$all_sites_rows_selected)
    part_OCC_far_pop <- extract_far_pop(site_table$added_sites, site_rows_selected = input$all_sites_rows_selected)
    
    
    part_OCC_far_trip_capacity <- calc_dose_capacity(vial_type = 'monodose' 
                                                      , vax_vol = 21.09
                                                      , equip_type = 'vaxCarr' #we assume a mobile team uses one vaccine carrier
                                                      , with_ice = F)
    
    
    team_days_fixed_part_OCC <- round(part_OCC_near_pop / tp_fixed, 1) #computationally, we see the number doses as the number of expected people. The "final number of doses" here have already accounted for the buffer
    team_days_mobile_part_OCC <- round(part_OCC_far_pop / part_OCC_far_trip_capacity, 1)
    
    
    
    
    
    
    #output for team days required for fixed teams
    output$tdf_part_OCC<- renderText({
      paste0(as.numeric(team_days_fixed_part_OCC))
    })
    
    #output for team days required for mobile teams
    output$tdm_part_OCC <- renderText({
      paste0(as.numeric(team_days_mobile_part_OCC))
    })
    
    #######
    # Team allocation calculations and output
    #######
    
    #Extract size of allocated team from the sites table
    site_teams_part_OCC <- extract_site_team_size(site_table$added_sites, site_rows_selected = input$all_sites_rows_selected)
    
    # output for the duration that each team type will spend on site
    output$team_dur_part_OCC <- print_site_team_dur(site_team_quant = site_teams_part_OCC
                                                     , td_fixed = team_days_fixed_part_OCC
                                                     , td_mobile = team_days_mobile_part_OCC
    )

    
    
    ################################################################
    #' Combining the results and plotting
    #' 
    #####################################################################
   
    #plot theme
    plot_theme <-  theme(title = element_text(size = 12,
                                              face = 'bold'
    )
    )
    
    #Making the tibbles!!!!
    # Results of required freezing time per strategy
    freezing_time_results <- tibble(
      Strategy = Strategy_list,
      time = c(monodose_FCC_ft, dose10_FCC_ft, mixed_FCC_ft, part_OCC_ft)
    )
    
    # Results of initial required volume of ice per strategy
    Init_iceVol_results <- tibble(
      Strategy = Strategy_list,
      iceVol = c(monodose_FCC_init_iceVol, dose10_FCC_init_iceVol, mixed_FCC_init_iceVol, part_OCC_init_iceVol)
    )
    
    
    #Results of team days calculations
    td_results <- tibble(
      Strategy = rep(Strategy_list, each = 2),
      team_type = rep(team_type_list, times = length(Strategy_list)),
      team_days = c(team_days_fixed_monodose_FCC, 
                    team_days_mobile_monodose_FCC, 
                    team_days_fixed_dose10_FCC, 
                    team_days_mobile_dose10_FCC, 
                    team_days_fixed_mixed_FCC, 
                    team_days_mobile_mixed_FCC, 
                    team_days_fixed_part_OCC, 
                    team_days_mobile_part_OCC
                    )
    )
    
    

    
    
    ####################
    #Making the plots!!!
    ####################
    
    output$plot <- renderPlot({
      ft_plot <- ggplot(data = freezing_time_results, 
                        aes(x = Strategy, y = time)
                        ) + 
        geom_bar(stat = "identity", fill = "steelblue") + 
        labs(title = 'Freezing time required per strategy', 
             y = "Freezing time required (days)"
             ) + 
       plot_theme
      
      iceVol_plot <- ggplot(data = Init_iceVol_results, 
                            aes(x = Strategy, 
                                y = iceVol)
                            ) + 
        geom_bar(stat = "identity", fill = "steelblue") + 
        labs(title = 'Initial volume of ice required per strategy', 
             y = "Initial volume of ice required (Litres)"
             ) + 
        plot_theme
      
      td_plot <- ggplot(data = td_results, 
                        aes(x = Strategy, 
                            y = team_days,
                            fill = team_type
                            )
                        ) + 
        geom_bar(stat = 'identity',
                 position = 'dodge'
                 ) + 
        labs(title = 'Number of days per team type and strategy', 
             y = "Team days"
             ) +
        scale_fill_manual(values = c("royalblue4", "tomato3"), 
                          name = "Team type",
                          breaks = team_type_list) +
        plot_theme
      
      #arrange the plots on a grid
      grid.arrange(ft_plot, 
                   iceVol_plot, 
                   td_plot,
                   ncol = 3)
    })
    }
  })
}





# Run the application
shinyApp(ui = ui, server = server)