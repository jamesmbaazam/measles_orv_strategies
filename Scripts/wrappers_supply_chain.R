
#packages
source('./scripts/analyses_parameters.R')
source('./scripts/parameters.R')
source('./scripts/supply_chain_functions.R')




############################################################################
#'analyse_prep_delay(): wrapper to combine all the supply chain functions and analyse 
#'the logistical needs and time to commence a campaign for each strategy
############################################################################

analyse_prep_delay <- function(strategy_name
                               , fixed_team_with_dose10 # options = if "T", 10 dose, else monodose
                               , fixed_team_with_ice # options = if "T", ice is used, else, no ice
                               , mobile_team_with_dose10 # options = if "T", 10 dose, else monodose
                               , mobile_team_with_ice # options = if "T", ice is used, else, no ice
                               , team_dispatch #options = "parallel", "asap"
                               
                               #defaults follow: can do sensitivity analyses on them as well
                               
                               , site_details = site_data
                               , site_row = 1 #which site to analyse
                               , mf314 = sc_model_params$mf314_quant
                               , ambient_temperature = sc_model_params$ambient_temp[1]
                               , dose10_vial_volume = sc_model_params$dose10_vial_vol[1]
                               , monodose_vial_volume = sc_model_params$monodose_vial_vol
){
    
    #################
    #fixed team calculations
    ################
    
    #doses needed for near population
    doses_fixed_team <-  calc_doses_required(df = site_details
                                             , site_rows_selected = site_row
                                             , is_dose10 = fixed_team_with_dose10
                                             , pop_type = 'near'
                                             , ovwastage = ifelse(fixed_team_with_dose10, sc_model_params$dose10_ovw_fixed_team, sc_model_params$monodose_ovw_fixed_team)
                                             , buffer_size = sc_model_params$buffer_stock
                                             
    )
    
    doses_mobile_team <-  calc_doses_required(df = site_details
                                              , site_rows_selected = site_row
                                              , is_dose10 = mobile_team_with_dose10
                                              , pop_type = 'far'
                                              , ovwastage = ifelse(mobile_team_with_dose10, sc_model_params$dose10_ovw_mobile_team, sc_model_params$monodose_ovw_mobile_team)
                                              , buffer_size = sc_model_params$buffer_stock
    )
    
    #determine passive cold chain needs
    ######################################
    #Here, I am assuming that vaccine carriers are transported with ice but without vaccines to the site.
    #'The RCW25s are used to transport the vaccines to the site and then transferred into the vax carrier for 
    #'for administration.
    #'
    #'
    
    RCW25_required_fixed_team <- calc_transport_equipment_needs(equip_type = 'rcw25'
                                                                , vial_type = ifelse(fixed_team_with_dose10, 'dose10', "monodose")
                                                                , vax_vol = ifelse(fixed_team_with_dose10, dose10_vial_volume, monodose_vial_volume)
                                                                , with_ice = fixed_team_with_ice
                                                                , doses_to_transport = doses_fixed_team
    )
    
    vaxCarr_required_fixed_team <- calc_transport_equipment_needs(equip_type = 'vaxCarr'
                                                                  , vial_type = ifelse(fixed_team_with_dose10, 'dose10', "monodose")
                                                                  , vax_vol = ifelse(fixed_team_with_dose10, dose10_vial_volume, monodose_vial_volume)
                                                                  , with_ice = fixed_team_with_ice
                                                                  , doses_to_transport = doses_fixed_team
    )
    
    
    #################
    #Mobile team calculations
    ################
    
    #determine passive cold chain needs
    ######################################
    #Here, I am assuming that mobile teams only need a vaccine carrier. 
    #' vaccine carriers are transported with ice and vaccines to the site.
    RCW25_required_mobile_team <- 0
    
    vaxCarr_required_mobile_team <- calc_transport_equipment_needs(equip_type = 'vaxCarr'
                                                                   , vial_type = ifelse(mobile_team_with_dose10, 'dose10', "monodose")
                                                                   , vax_vol = ifelse(mobile_team_with_dose10, dose10_vial_volume, monodose_vial_volume)
                                                                   , with_ice = mobile_team_with_ice
                                                                   , doses_to_transport = doses_mobile_team
    )
    
    
    
    ##############################################
    #Ice pack needs # total number of 0.6L ice packs = number of RCW25 needed * number of ice packs needed per RCW25
    ###############################################
    
    #ice packs needed for each equipment type, based on the ambient temperature
    RCW25_icepack_needs <-  compute_rcw25_icepacks(sc_model_params$ambient_temp[1])
    
    vaxCarr_icepack_needs <- compute_vaxCarr_icepacks(sc_model_params$ambient_temp[1])
    
    #Fixed post
    RCW25_icepack_needs_fixed_team <- calc_icepack_tot_quant(equipment_quantity = ifelse(fixed_team_with_ice, RCW25_required_fixed_team, 0)
                                                             , icepacks_per_equipment = RCW25_icepack_needs
    ) # total number of 0.6L ice packs = number of RCW25 needed * number of ice packs needed per RCW25
    vaxCarr_icepack_needs_fixed_team <- calc_icepack_tot_quant(equipment_quantity = ifelse(fixed_team_with_ice, vaxCarr_required_fixed_team, 0)
                                                               , icepacks_per_equipment = vaxCarr_icepack_needs
    ) 
    
    #mobile teams
    RCW25_icepack_needs_mobile_team <- calc_icepack_tot_quant(equipment_quantity = ifelse(mobile_team_with_ice, RCW25_required_mobile_team, 0)
                                                              , icepacks_per_equipment = RCW25_icepack_needs
    )
    vaxCarr_icepack_needs_mobile_team <- calc_icepack_tot_quant(equipment_quantity = ifelse(mobile_team_with_ice, vaxCarr_required_mobile_team, 0)
                                                                , icepacks_per_equipment = vaxCarr_icepack_needs
    )
    
    
    #dose10_FCC_RCW25_icepack_vol <- dose10_FCC_RCW25_icepack_needs_total * 0.6 # total volume of ice packs needed is simply the above calculation * 0.6L
    
    
    
    # Initial number of icepacks required
    #dose10_FCC_init_icepack_quant <- dose10_FCC_RCW25_icepack_needs_total + dose10_FCC_vaxCarr_icepack_needs_total
    
    
    ############################################
    # freezing time: # Time it takes to freeze depends on how many freezers are available and their capacity. I currently assume that we only use the MF314 freezer, which is the largest, and I specify the quantity at the beginning of this script
    ##############################################
    
    #fixed post team
    freezing_time_fixed_team <- calc_freezing_time(mf314_available = mf314
                                                   , large_icepacks_quantity = ifelse(fixed_team_with_ice, RCW25_icepack_needs_fixed_team, 0) 
                                                   , small_icepacks_quantity = ifelse(fixed_team_with_ice, vaxCarr_icepack_needs_fixed_team, 0)
    )
    
    freezing_time_mobile_team <- calc_freezing_time(mf314_available = mf314
                                                    , large_icepacks_quantity = ifelse(mobile_team_with_ice, RCW25_icepack_needs_mobile_team, 0)
                                                    , small_icepacks_quantity = ifelse(mobile_team_with_ice, vaxCarr_icepack_needs_mobile_team, 0)
    )
    
    
    
    
    ######################################################
    #When will the campaign start?
    ######################################################
    #TODO I will modify the function so that it returns which team heads out first if the routing is 'asap"
    campaign_delay <- calc_campaign_start(fixedT_freeze_time = freezing_time_fixed_team
                                          , mobileT_freeze_time = freezing_time_mobile_team
                                          , team_routing = team_dispatch
    )
    
    out <- data.frame(strategy = strategy_name,
                      ft_vial_type = ifelse(fixed_team_with_dose10, 'dose10', 'monodose')
                      , ft_doses_required = doses_fixed_team
                      , mt_vial_type = ifelse(mobile_team_with_dose10, 'dose10', 'monodose')
                      , mt_doses_required = doses_mobile_team
                      , ft_RCW25 = RCW25_required_fixed_team
                      , mt_RCW25 = RCW25_required_mobile_team
                      , ft_vaxCarr = vaxCarr_required_fixed_team
                      , mt_vaxCarr = vaxCarr_required_mobile_team
                      , ft_icepacks_large = RCW25_icepack_needs_fixed_team
                      , mt_icepacks_large = RCW25_icepack_needs_mobile_team
                      , ft_icepacks_small = vaxCarr_icepack_needs_fixed_team
                      , mt_icepacks_small = vaxCarr_icepack_needs_mobile_team
                      , ft_freezing_time = freezing_time_fixed_team
                      , mt_freezing_time = freezing_time_mobile_team
                      , campaign_start = campaign_delay$start_day
                      , team_leaving_first = campaign_delay$which_team_first
    )
    
    return(out)
    
}



################################################################################
#' ##analyse_team_days(): a wrapper to determine what the team days is for a strategy.
#' Given tau team days, we need a single team to spend tau days, or two teams to spend 
#' tau/2 days, and for n teams, tau/n. It is to serve as the campaign duration on a site 
#' ################################################################################

#size of near population

analyse_team_days <- function(strategy_name
                              , fixed_team_with_dose10 # options = if "T", 10 dose, else monodose
                              , fixed_team_with_ice # options = if "T", ice is used, else, no ice
                              , mobile_team_with_dose10 # options = if "T", 10 dose, else monodose
                              , mobile_team_with_ice # options = if "T", ice is used, else, no ice
                              #    , team_dispatch #options = "parallel", "asap"
                              
                              #defaults follow: can do sensitivity analyses on them as well
                              , site_details = site_data
                              , site_row = 1 #analyse first row of the site data frame
                              , dose10_vial_volume = sc_model_params$dose10_vial_vol[1]
                              , monodose_vial_volume = sc_model_params$monodose_vial_vol
                              , browse = F
                              
){
    
    if (browse)browse 
    #Mobile teams only use a vaccine carrier, hence, are contrained by how much they can transport
    mobile_team_vol_capacity <- calc_dose_capacity(vial_type = ifelse(mobile_team_with_dose10, 'dose10', 'monodose')
                                                   , vax_vol = ifelse(mobile_team_with_dose10, dose10_vial_volume, monodose_vial_volume)
                                                   , equip_type = 'vaxCarr' #we assume a mobile team uses one vaccine carrier
                                                   , with_ice = mobile_team_with_ice
    )
    

    
    #team days needed by fixed teams, NOT CONSTRAINED by volume/space - we assume they can transport all they need per trip
    team_days_fixed_team <- round(site_details$near_pop / tp_fixed, 1) #computationally, we see the number doses as the number of expected people. The "final number of doses" here have already accounted for the buffer
    
    #team days needed by mobile teams, CONSTRAINED by volume/space - we assume per trip, they can only transport as much as the vaccine carrier allows; furthermore, for the 10-dose, due to open vial wastage, they end up wasting a percentage of the doses, hence they have to make more trips to account for that
    #10-dose mobile campaign team days are affected by the effective doses, which is the the total capacity they can carry less of how many are expected to be wasted.
    team_days_mobile_team <- if(mobile_team_with_dose10){
        calc_dose10_team_days(target_pop = site_details$far_pop
                              , dose10_wastage = sc_model_params$dose10_ovw_mobile_team
                              , team_performance = tp_mobile
                              , vaxCarr_capacity = mobile_team_vol_capacity
        )
    }else{
        calc_monodose_team_days(target_pop = site_details$far_pop
                                , team_performance = min(mobile_team_vol_capacity, tp_mobile)
                                , carrier_vol_capacity = mobile_team_vol_capacity
        )
    }
    
    out <- data.frame(strategy = strategy_name
                      , ft_vial_type = ifelse(fixed_team_with_dose10, 'dose10', 'monodose')
                      , ft_with_ice = ifelse(fixed_team_with_ice, 'yes', 'no')
                      , ft_team_days = team_days_fixed_team
                      , mt_vial_type = ifelse(mobile_team_with_dose10, 'dose10', 'monodose')
                      , mt_with_ice = ifelse(mobile_team_with_ice, 'yes', 'no')
                      , mt_team_days = team_days_mobile_team
    )
    
    return(out)   
}

