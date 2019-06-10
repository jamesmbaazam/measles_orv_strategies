library('ggplot2')
library('dplyr')
library('reshape2')
library('purrr')
library('gridExtra')
library('tidyr')

source('./scripts/analyses_parameters.R')
source('./scripts/parameters.R')
source('./scripts/supply_chain_functions.R')



############################################################################
#'analyse_strategy(): wrapper to combine all the supply chain functions and analyse 
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
                  fixed_team_vial_type = ifelse(fixed_team_with_dose10, 'dose10', 'monodose')
                  , ft_doses_required = doses_fixed_team
                  , mobile_team_vial_type = ifelse(mobile_team_with_dose10, 'dose10', 'monodose')
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

##########################################################
#Strategies to analyse
###########################################################
#all possible combinations
# strategy_scenarios <- expand.grid(fixed_team_with_dose10 = c(T, F)
#                         , fixed_team_with_ice = c(T, F)
#                         , mobile_team_with_dose10 = c(T, F)
#                         , mobile_team_with_ice = c(T, F)
#                         , team_dispatch = c('parallel', 'asap')
#                         )

#List of strategies
strategy_analysis_list <- list(
# 10-dose fcc
dose10_fcc_asap = data.frame(strategy_name = 'dose10_fcc_asap',
                          fixed_team_with_dose10 = T
                         , fixed_team_with_ice = T
                         , mobile_team_with_dose10 = T
                         , mobile_team_with_ice = T
                         , team_dispatch = 'asap'
                         ),
dose10_fcc_parallel = data.frame(strategy_name = 'dose10_fcc_parallel'
                             , fixed_team_with_dose10 = T
                             , fixed_team_with_ice = T
                             , mobile_team_with_dose10 = T
                             , mobile_team_with_ice = T
                             , team_dispatch = 'parallel'
                             ),
# monodose fcc
monodose_fcc_asap = data.frame(strategy_name = 'monodose_fcc_asap'
                                    , fixed_team_with_dose10 = F
                                    , fixed_team_with_ice = T
                                    , mobile_team_with_dose10 = F
                                    , mobile_team_with_ice = T
                                    , team_dispatch = 'asap'
                                    ),

monodose_fcc_parallel = data.frame(strategy_name = 'monodose_fcc_parallel'
                                        , fixed_team_with_dose10 = F
                                        , fixed_team_with_ice = T
                                        , mobile_team_with_dose10 = F
                                        , mobile_team_with_ice = T
                                        , team_dispatch = 'parallel'
                                        ),
#mixed fcc
mixed_fcc_asap = data.frame(strategy_name = 'part_occ_asap'
                                  , fixed_team_with_dose10 = T
                                  , fixed_team_with_ice = T
                                  , mobile_team_with_dose10 = F
                                  , mobile_team_with_ice = T
                                  , team_dispatch = 'asap'
                      ),

mixed_fcc_parallel = data.frame(strategy_name = 'part_occ_parallel'
                                      , fixed_team_with_dose10 = T
                                      , fixed_team_with_ice = T
                                      , mobile_team_with_dose10 = F
                                      , mobile_team_with_ice = T
                                      , team_dispatch = 'parallel'
                          ),

#part occ
part_occ_asap = data.frame(strategy_name = 'part_occ_asap'
                                      , fixed_team_with_dose10 = T
                                      , fixed_team_with_ice = T
                                      , mobile_team_with_dose10 = F
                                      , mobile_team_with_ice = F
                                      , team_dispatch = 'asap'
                     ),

part_occ_parallel = data.frame(strategy_name = 'part_occ_parallel'
                                  , fixed_team_with_dose10 = T
                                  , fixed_team_with_ice = T
                                  , mobile_team_with_dose10 = F
                                  , mobile_team_with_ice = F
                                  , team_dispatch = 'parallel'
                         )
)



#'Run the campaign delay analysis function over a number of defined scenarios
#'TODO: change the for loop to an lapply function for efficiency
#'
campaign_delay_results <- list()
strategy_names <- names(strategy_analysis_list)
for (i in seq_along(strategy_analysis_list)) {
    campaign_delay_results[[strategy_names[i]]] <- analyse_prep_delay(
        strategy_name = strategy_names[i]
        , fixed_team_with_dose10 = strategy_analysis_list[[strategy_names[i]]]$fixed_team_with_dose10
        , fixed_team_with_ice = strategy_analysis_list[[strategy_names[i]]]$fixed_team_with_ice
        , mobile_team_with_dose10 = strategy_analysis_list[[strategy_names[i]]]$mobile_team_with_dose10
        , mobile_team_with_ice = strategy_analysis_list[[strategy_names[i]]]$mobile_team_with_ice
        , team_dispatch = strategy_analysis_list[[strategy_names[i]]]$team_dispatch
        )
    }

#Convert the list of dataframes output into a single data frame
strategy_campaign_prep_delays <- do.call(rbind, args = c(campaign_delay_results, make.row.names = F))

View(strategy_campaign_prep_delays)
#' ##########################################
#' # Strategy 1: 10-dose only FCC; This will be the baseline
#' ##########################################
#' 
#' #################
#' #fixed team calculations
#' ################
#' 
#' #doses needed for near population
#' dose10_FCC_doses_fixedT <-  calc_doses_required(df = site_data
#'                                                   , site_rows_selected = 1
#'                                                   , is_dose10 = T
#'                                                   , pop_type = 'near'
#'                                                   , ovwastage = sc_model_params$dose10_wr_ft
#'                                                   , buffer_size = sc_model_params$buffer_stock)
#' 
#' 
#' # #adjustment for wastage (fixed post teams)
#' # dose10_FCC_doses_near_pop_req <- dose10_FCC_doses_near_pop * (1 + /100)
#' # 
#' # #apply buffer
#' # dose10_FCC_doses_needed_fTeam <- apply_buffer(doses = ceiling(dose10_FCC_doses_near_pop_req)
#' #                                         , buffer_size = 
#' # )  
#' 
#' #determine passive cold chain needs
#' ######################################
#' #Here, I am assuming that vaccine carriers are transported with ice but without vaccines to the site.
#' #'The RCW25s are used to transport the vaccines to the site and then transferred into the vax carrier for 
#' #'for administration.
#' #'
#' #'
#' 
#' dose10_FCC_RCW25_needs_fixedT <- calc_transport_equipment_needs(equip_type = 'rcw25'
#'                                                          , vial_type = 'dose10'
#'                                                          , vax_vol = sc_model_params$dose10_vial_vol[1]
#'                                                          , with_ice = T
#'                                                          , doses_to_transport = dose10_FCC_doses_fixedT
#' )
#' 
#' dose10_FCC_vaxCarr_needs_fixedT <- calc_transport_equipment_needs(equip_type = 'vaxCarr'
#'                                                            , vial_type = 'dose10'
#'                                                            , vax_vol = dose10_vial_vol[1]
#'                                                            , with_ice = T
#'                                                            , doses_to_transport = dose10_FCC_doses_fixedT
#' )
#' 
#' 
#' #################
#' #Mobile team calculations
#' ################
#' dose10_FCC_doses_mobileT <-  calc_doses_required(df = site_data
#'                                                  ,  site_rows_selected = 1
#'                                                  , is_dose10 = T
#'                                                  , pop_type = 'far'
#'                                                  , ovwastage = sc_model_params$dose10_wr_mt
#'                                                  , buffer_size = sc_model_params$buffer_stock
#'                                                  )
#' #determine passive cold chain needs
#' ######################################
#' #Here, I am assuming that mobile teams only need a vaccine carrier. 
#' #' vaccine carriers are transported with ice and vaccines to the site.
#' 
#' dose10_FCC_vaxCarr_needs_mobileT <- calc_transport_equipment_needs(equip_type = 'vaxCarr'
#'                                                                   , vial_type = 'dose10'
#'                                                                   , vax_vol = dose10_vial_vol[1]
#'                                                                   , with_ice = T
#'                                                                   , doses_to_transport = dose10_FCC_doses_mobileT
#' )
#' 
#' 
#' 
#' # #adjustment for wastage (mobile team)
#' # dose10_FCC_doses_far_pop_req <- dose10_FCC_doses_far_pop * (1 + sc_model_params$dose10_wr_mt/100)
#' 
#' # #apply buffer
#' # dose10_FCC_doses_needed <- apply_buffer(doses = ceiling(dose10_FCC_doses_far_pop_req)
#' #                                        , buffer_size = sc_model_params$buffer_stock
#' # )  
#' 
#' 
#' 
#' 
#' 
#' # dose10_FCC_RCW25_needs <- calc_transport_equipment_needs(equip_type = 'rcw25'
#' #                                                          , vial_type = 'dose10'
#' #                                                          , vax_vol = sc_model_params$dose10_vial_vol[1]
#' #                                                          , with_ice = T
#' #                                                          , doses_to_transport = dose10_FCC_doses_needed
#' # )
#' # 
#' # dose10_FCC_vaxCarr_needs <- calc_transport_equipment_needs(equip_type = 'vaxCarr'
#' #                                                            , vial_type = 'dose10'
#' #                                                            , vax_vol = dose10_vial_vol[1]
#' #                                                            , with_ice = T
#' #                                                            , doses_to_transport = dose10_FCC_doses_needed
#' # )
#' 
#' ##############################################
#' #Ice pack needs # total number of 0.6L ice packs = number of RCW25 needed * number of ice packs needed per RCW25
#' ###############################################
#' #Fixed post
#' dose10_FCC_RCW25_icepack_needs_fixedT <- calc_icepack_tot_quant(equipment_quantity = dose10_FCC_RCW25_needs_fixedT
#'                                                                   , icepacks_per_equipment = RCW25_icepack_needs
#'                                                                   ) # total number of 0.6L ice packs = number of RCW25 needed * number of ice packs needed per RCW25
#' dose10_FCC_vaxCarr_icepack_needs_fixedT <- calc_icepack_tot_quant(equipment_quantity = dose10_FCC_vaxCarr_needs_fixedT
#'                                                                 , icepacks_per_equipment = vaxCarr_icepack_needs
#' ) 
#' 
#' #mobile teams
#' dose10_FCC_vaxCarr_icepack_needs_mobileT <- calc_icepack_tot_quant(equipment_quantity = dose10_FCC_vaxCarr_needs_mobileT
#'                                                                   , icepacks_per_equipment = vaxCarr_icepack_needs
#'                                                                   )
#' 
#' 
#' #dose10_FCC_RCW25_icepack_vol <- dose10_FCC_RCW25_icepack_needs_total * 0.6 # total volume of ice packs needed is simply the above calculation * 0.6L
#' 
#' 
#' 
#' # Initial number of icepacks required
#' #dose10_FCC_init_icepack_quant <- dose10_FCC_RCW25_icepack_needs_total + dose10_FCC_vaxCarr_icepack_needs_total
#' 
#' 
#' ############################################
#' # freezing time: # Time it takes to freeze depends on how many freezers are available and their capacity. I currently assume that we only use the MF314 freezer, which is the largest, and I specify the quantity at the beginning of this script
#' ##############################################
#' 
#' #fixed post team
#' dose10_FCC_freezetime_fixedT <- calc_freezing_time(mf314_available = sc_model_params$mf314_quant
#'                                     , large_icepacks_quantity = dose10_FCC_RCW25_icepack_needs_fixedT 
#'                                     , small_icepacks_quantity = dose10_FCC_vaxCarr_icepack_needs_fixedT)
#' 
#' dose10_FCC_freezetime_mobileT <- calc_freezing_time(mf314_available = sc_model_params$mf314_quant
#'                                                    , large_icepacks_quantity = 0
#'                                                    , small_icepacks_quantity = dose10_FCC_vaxCarr_icepack_needs_mobileT)
#' 
#' 
#' 
#' 
#' ######################################################
#' #When will the campaign start?
#' ######################################################
#' #TODO I will modify the function so that it returns which team heads out first if the routing is 'asap"
#' dose10_FCC_delay <- calc_campaign_start(fixedT_freeze_time = dose10_FCC_freezetime_fixedT
#'                                         , mobileT_freeze_time = dose10_FCC_freezetime_mobileT
#'                                         , team_routing = 'asap')
#' 
#' 
#' 
#' 
#' 
#' #Initial volume of ice required in litres
#' #dose10_FCC_init_iceVol <- dose10_FCC_RCW25_icepack_vol + dose10_FCC_vaxCarr_icepack_vol
#' 
#' 
#' ################################################################################
#' ##team days calculations
#' ################################################################################
#' 
#' #size of near population
#' dose10_FCC_near_pop <- extract_near_pop(site_data, site_rows_selected = 1)
#' dose10_FCC_far_pop <- extract_far_pop(site_data, site_rows_selected = 1)
#' 
#' 
#' 
#' dose10_vaxCarr_cap_ice <- calc_dose_capacity(vial_type = 'dose10' 
#'                                                    , vax_vol = sc_model_params$dose10_vial_vol[1]
#'                                                    , equip_type = 'vaxCarr' #we assume a mobile team uses one vaccine carrier
#'                                                    , with_ice = T)
#' 
#' 
#' dose10_FCC_far_trip_eff_doses <- calc_effective_doses(dose_quantity = dose10_vaxCarr_cap_ice
#'                                                       , wastage = sc_model_params$dose10_wr_mt
#'                                                       )  #The effective number of doses a team has is the total capacity they can carry less of how many are expected to be wasted.
#' 
#' team_days_fixed_dose10_FCC <- round(site_data$near_pop / tp_fixed, 1) #computationally, we see the number doses as the number of expected people. The "final number of doses" here have already accounted for the buffer
#' team_days_mobile_dose10_FCC <- calc_dose10_team_days(target_pop = site_data$far_pop
#'                                                      , dose10_wastage = sc_model_params$dose10_wr_mt
#'                                                      , vaxCarr_capacity = dose10_vaxCarr_cap_ice
#'                                                      )
#' 
#' #######
#' # Team allocation calculations and output
#' #######
#' 
#' #Extract size of allocated team from the sites table
#' site_teams_dose10_FCC <- extract_site_team_size(site_data, site_rows_selected = 1)
#' 
#' 
#' 
#' ##########################################
#' # Strategy 2: Monodose-only FCC
#' ##########################################
#' 
#' monodose_FCC_doses_ft <- calc_doses_required(df = site_data
#'                                              ,  site_rows_selected = 1
#'                                              , is_dose10 = F
#'                                              , pop_type = 'near')
#' 
#' monodose_FCC_doses_mt <- calc_doses_required(df = site_data
#'                                              ,  site_rows_selected = 1
#'                                              , is_dose10 = F
#'                                              , pop_type = 'far')
#' 
#' monodose_FCC_doses <-  monodose_FCC_doses_ft + monodose_FCC_doses_mt 
#' 
#' # apply buffer
#' monodose_FCC_doses_needed <- apply_buffer(buffer_size = sc_model_params$buffer_stock
#'                                          , doses = monodose_FCC_doses
#'                                          ) 
#' 
#' # number of RCW25s needed, based on the volume of the vaccine indicated (Without ice, 1 RCW25 can transport 1301 vials/doses and a vaccine carrier can transport 170 vials/doses. With ice, the numbers are 616 and 77 per our calculations)
#' 
#' monodose_FCC_RCW25_needs <- calc_transport_equipment_needs(equip_type = 'rcw25'
#'                                                            , vial_type = 'monodose'
#'                                                            , vax_vol = sc_model_params$monodose_vial_vol
#'                                                            , with_ice = T
#'                                                            , doses_to_transport = monodose_FCC_doses_needed
#' )
#' 
#' monodose_FCC_vaxCarr_needs <- calc_transport_equipment_needs(equip_type = 'vaxCarr'
#'                                                              , vial_type = 'monodose'
#'                                                              , vax_vol = sc_model_params$monodose_vial_vol
#'                                                              , with_ice = T
#'                                                              , doses_to_transport = monodose_FCC_doses_needed
#' )
#' 
#' # Monodose FCC RCW25 icepack needs
#' monodose_FCC_vaxCarr_icepack_needs_total <- vaxCarr_icepack_needs * monodose_FCC_vaxCarr_needs # total number of 0.6L ice packs = number of RCW25 needed * number of ice packs needed per RCW25
#' monodose_FCC_vaxCarr_icepack_vol <- monodose_FCC_vaxCarr_icepack_needs_total * 0.4 # total volume of ice packs needed is simply the above calculation * 0.6L
#' 
#' 
#' # Monodose FCC Vaccine carrier icepack needs
#' monodose_FCC_RCW25_icepack_needs_total <- RCW25_icepack_needs * monodose_FCC_RCW25_needs # total number of 0.6L ice packs = number of RCW25 needed * number of ice packs needed per RCW25
#' monodose_FCC_RCW25_icepack_vol <- monodose_FCC_RCW25_icepack_needs_total * 0.6 # total volume of ice packs needed is simply the above calculation * 0.6L
#' 
#' #total needs
#' monodose_FCC_init_icepack_quant <- monodose_FCC_RCW25_icepack_needs_total + monodose_FCC_vaxCarr_icepack_needs_total
#' ###
#' # outputs for monodose FCC calculations
#' ###
#' #ice_packs_required_monodose_FCC <- monodose_FCC_RCW25_icepack_needs_total + monodose_FCC_vaxCarr_icepack_needs_total
#' 
#' 
#' monodose_FCC_ft <- calc_freezing_time(mf314_available = sc_model_params$mf314_quant
#'                                       , large_icepacks_quantity = monodose_FCC_RCW25_icepack_needs_total 
#'                                       , small_icepacks_quantity = monodose_FCC_vaxCarr_icepack_needs_total)
#' # output
#' #Init_ice_freezeTime_monodose_FCC <- monodose_FCC_ft # Time it takes to freeze depends on how many freezers are available and their capacity. I currently assume that we only use the MF314 freezer, which is the largest, and I specify the quantity at the beginning of this script
#' 
#' monodose_FCC_init_iceVol <- monodose_FCC_RCW25_icepack_vol + monodose_FCC_vaxCarr_icepack_vol# we only need 0.6L ice packs to tra
#' 
#' monodose_FCC_near_pop <- extract_near_pop(site_data, site_rows_selected = 1)
#' monodose_FCC_far_pop <- extract_far_pop(site_data, site_rows_selected = 1)
#' 
#' #this calculates the number of doses we can transport for the far campaign. We will then find out if we can transport more or less irrespective of how many we are expected to vaccinate, i.e, team performance/vaccination rate
#' monodose_FCC_far_trip_capacity <- calc_dose_capacity(vial_type = 'monodose' 
#'                                                      , vax_vol = sc_model_params$monodose_vial_vol
#'                                                      , equip_type = 'vaxCarr' #we assume a mobile team uses one vaccine carrier
#'                                                      , with_ice = T)
#' 
#' #monodose_FCC_mt_vax_capacity <- ifelse(monodose_FCC_far_trip_capacity < tp_mobile, monodose_FCC_far_trip_capacity, tp_mobile) #if how much we can carry is less than the expected vaccination rate, then the volume constraint becomes the denominator. mt = mobile team
#' 
#' team_days_fixed_monodose_FCC <- round(monodose_FCC_near_pop / sc_model_params$vax_rate['fixed_team'], 1) #computationally, we see the number doses as the number of expected people. The "final number of doses" here have already accounted for the buffer
#' team_days_mobile_monodose_FCC <- calc_monodose_team_days(target_pop = monodose_FCC_far_pop
#'                                                          , team_performance = sc_model_params$vax_rate['mobile_team']
#'                                                          , carrier_vol_capacity = monodose_FCC_far_trip_capacity
#'                                                          )
#'     
#' 
#' #######
#' # Team allocation calculations and output
#' #######
#' 
#' #Extract size of allocated team from the sites table
#' site_teams_monodoseFCC <- extract_site_team_size(site_data, site_rows_selected = 1)
#' 
#' 
#' 
#' ####################################################################################
#' #' Strategy 3: Mixed strategy A; 10-dose FCC for fixed teams and monodose FCC for mobile teams
#' ####################################################################################
#' 
#' mixed_FCC_dose10_quant <- calc_doses_required(df = site_data
#'                                               , site_rows_selected = 1
#'                                               , is_dose10 = T
#'                                               , pop_type = 'near')
#' 
#' mixed_FCC_monodose_quant <- calc_doses_required(df = site_data
#'                                                 , site_rows_selected = 1
#'                                                 , is_dose10 = F
#'                                                 , pop_type = 'far')
#' 
#' #adjust 10-doses for wastage at the fixed post
#' mixed_FCC_dose10_quant_adj <- mixed_FCC_dose10_quant*(1 + sc_model_params$dose10_wr_ft/100)
#' 
#' #apply the buffer to both doses
#' mixed_FCC_dose10_final <- apply_buffer(doses = mixed_FCC_dose10_quant_adj
#'                                       , buffer_size = sc_model_params$buffer_stock
#'                                       )
#' 
#' mixed_FCC_monodose_final <- apply_buffer(doses = mixed_FCC_monodose_quant
#'                                         , buffer_size = sc_model_params$buffer_stock
#'                                         )
#' 
#' mixed_FCC_doses_needed <- mixed_FCC_dose10_final + mixed_FCC_monodose_final 
#' 
#' 
#' ##passive cold chain required for 10-dose vials
#' mixed_FCC_dose10_RCW25_needs <- calc_transport_equipment_needs(equip_type = 'rcw25'
#'                                                                , vial_type = 'dose10'
#'                                                                , vax_vol = sc_model_params$dose10_vial_vol[1]
#'                                                                , with_ice = T
#'                                                                , doses_to_transport = mixed_FCC_dose10_final
#' )
#' 
#' mixed_FCC_dose10_vaxCarr_needs <- calc_transport_equipment_needs(equip_type = 'vaxCarr'
#'                                                                  , vial_type = 'dose10'
#'                                                                  , vax_vol = sc_model_params$dose10_vial_vol[1]
#'                                                                  , with_ice = T
#'                                                                  , doses_to_transport = mixed_FCC_dose10_final
#' )
#' 
#' #passive cold chain required for monodose vials
#' mixed_FCC_monodose_RCW25_needs <- calc_transport_equipment_needs(equip_type = 'rcw25'
#'                                                                  , vial_type = 'monodose'
#'                                                                  , vax_vol = sc_model_params$monodose_vial_vol
#'                                                                  , with_ice = T
#'                                                                  , doses_to_transport = mixed_FCC_monodose_final
#' )
#' 
#' 
#' mixed_FCC_monodose_vaxCarr_needs <- calc_transport_equipment_needs(equip_type = 'vaxCarr'
#'                                                                    , vial_type = 'monodose'
#'                                                                    , vax_vol = sc_model_params$monodose_vial_vol
#'                                                                    , with_ice = T
#'                                                                    , doses_to_transport = mixed_FCC_monodose_final
#' )
#' 
#' 
#' #total passive cold chain needs
#' 
#' #RCW25
#' mixed_FCC_RCW25_needs <- mixed_FCC_dose10_RCW25_needs + mixed_FCC_monodose_RCW25_needs
#' 
#' #vaccine carriers
#' mixed_FCC_vaxCarr_needs <- mixed_FCC_dose10_vaxCarr_needs + mixed_FCC_monodose_vaxCarr_needs
#' 
#' # mixed FCC quantity of icepack needs
#' 
#' #0.6L ice packs for RCW 25
#' mixed_FCC_RCW25_icepack_needs <- mixed_FCC_RCW25_needs * RCW25_icepack_needs
#' 
#' 
#' #0.4L ice packs for vaccine carriers
#' mixed_FCC_vaxCarr_icepack_needs <- mixed_FCC_vaxCarr_needs * vaxCarr_icepack_needs
#' 
#' #total icepack needs
#' mixed_FCC_icepack_needs <- mixed_FCC_vaxCarr_icepack_needs + mixed_FCC_RCW25_icepack_needs
#' 
#' #total volume of ice packs
#' 
#' #0.4L
#' mixed_FCC_vaxCarr_icepack_vol <- mixed_FCC_vaxCarr_icepack_needs * 0.4
#' #0.6L
#' mixed_FCC_RCW25_icepack_vol <- mixed_FCC_RCW25_icepack_needs * 0.6
#' 
#' #####################################
#' #' outputs for the mixed strategy
#' #####################################
#' 
#' #initial volume of ice required 
#' mixed_FCC_init_iceVol <- mixed_FCC_vaxCarr_icepack_vol + mixed_FCC_RCW25_icepack_vol
#' 
#' # freezing time
#' mixed_FCC_ft <- calc_freezing_time(mf314_available = sc_model_params$mf314_quant
#'                                    , large_icepacks_quantity = mixed_FCC_RCW25_icepack_needs 
#'                                    , small_icepacks_quantity = mixed_FCC_vaxCarr_icepack_needs)
#' 
#' 
#' # Initial number of icepacks required
#' mixed_FCC_init_icepack_quant <- mixed_FCC_RCW25_icepack_needs + mixed_FCC_vaxCarr_icepack_needs
#' 
#' 
#' 
#' ##team days calculations
#' 
#' #how many doses can be transported in the vaccine carrier?
#' monodose_vaxCarr_cap_ice <- calc_dose_capacity(vial_type = 'monodose' 
#'                                                   , vax_vol = sc_model_params$monodose_vial_vol
#'                                                   , equip_type = 'vaxCarr' #we assume a mobile team uses one vaccine carrier
#'                                                   , with_ice = T)
#' 
#' 
#' team_days_fixed_mixed_FCC <- round(site_data$near_pop / tp_fixed, 1) #computationally, we see the number doses as the number of expected people. The "final number of doses" here have already accounted for the buffer
#' 
#' team_days_mobile_mixed_FCC <- calc_monodose_team_days(target_pop = site_data$far_pop
#'                                                       , team_performance = sc_model_params$vax_rate['mobile_team']
#'                                                       ,  carrier_vol_capacity = monodose_vaxCarr_cap_ice
#'                                                       )
#'     
#'     
#'     
#' #round(site_data$far_pop / monodose_vaxCarr_cap_ice, 1)
#' 
#' 
#' #######
#' # Team allocation calculations and output
#' #######
#' 
#' #Extract size of allocated team from the sites table
#' site_teams_mixed_FCC <- extract_site_team_size(site_data, site_rows_selected = 1)
#' 
#' ##############################################################################
#' #' Strategy 4a: Mixed strategy B (Partial OCC strategy); 10-dose FCC for fixed teams, Monodose OCC for mobile teams
#' #' # In this sub-strategy, we assume that both teams have to be dispatched at the same time. So, the fact that the mobile
#' #' teams do not need ice does not cut the delay time. Maybe, it does because you only have to freeze ice for the fixed teams
#' #' This strategy doesn't seem logical but the following might be the justifications:
#' #' 1. The available transport can be maximised if it does only one trip and saves fuel
#' #' 2. Access road nature makes it impossible to do two trips
#' #' 3. Mobile teams cannot work autonomously as people may not be aware they are coming and hence, they
#' #' have to wait for the fixed teams so the people realise that the campaign has been sanctioned.
#' ##############################################################################
#' 
#' part_OCC_dose10_quant <- calc_doses_required(df = site_data
#'                                              , site_rows_selected = 1
#'                                              , is_dose10 = T
#'                                              , pop_type = 'near')
#' 
#' part_OCC_monodose_quant <- calc_doses_required(df = site_data
#'                                                ,  site_rows_selected = 1
#'                                                , is_dose10 = F
#'                                                , pop_type = 'far')
#' 
#' 
#' part_OCC_dose10_quant_adj <- part_OCC_dose10_quant*(1 + sc_model_params$dose10_wr_ft/100)
#' # part_OCC_doses <- part_OCC_dose10_quant + part_OCC_monodose_quant
#' 
#' part_OCC_dose10_final <- apply_buffer(doses = part_OCC_dose10_quant_adj
#'                                      , buffer_size = sc_model_params$buffer_stock
#'                                      )
#' 
#' part_OCC_monodose_final <- apply_buffer(doses = part_OCC_monodose_quant
#'                                        , buffer_size = sc_model_params$buffer_stock
#'                                        )
#' 
#' part_OCC_doses_needed <- part_OCC_dose10_final + part_OCC_monodose_final
#' 
#' # passive cold chain needed, based on the volume of the vaccine indicated (1 RCW25 can transport 3300 doses if vax vol = 3cm3 and 5000 doses if vax vol = 2cm3)
#' # if (input$vaccine_vol_dose10 == 2.1) {
#' #   part_OCC_dose10_RCW25_needs <- ceiling(part_OCC_dose10_final / 5000) # these numbers refer to the doses along with the diluents. Source: Excel sheet for Appendix 23
#' #   part_OCC_dose10_vaxCarr_needs <- ceiling(part_OCC_dose10_final / 750) # vaccine carrier. Source: Excel sheet for Appendix 23
#' # } else if (input$vaccine_vol_dose10 == 3) {
#' #   part_OCC_dose10_RCW25_needs <- ceiling(part_OCC_dose10_final / 3300) # Source: Excel sheet for Appendix 23
#' #   part_OCC_dose10_vaxCarr_needs <- ceiling(part_OCC_dose10_final / 500) # vaccine carrier. Source: Excel sheet for Appendix 23
#' # }
#' 
#' 
#' #determine passive cold chain needs
#' part_OCC_dose10_RCW25_needs <- calc_transport_equipment_needs(equip_type = 'rcw25'
#'                                                               , vial_type = 'dose10'
#'                                                               , vax_vol = sc_model_params$dose10_vial_vol[1]
#'                                                               , with_ice = T
#'                                                               , doses_to_transport = part_OCC_dose10_final)
#' 
#' 
#' part_OCC_dose10_vaxCarr_needs <- calc_transport_equipment_needs(equip_type = 'vaxCarr'
#'                                                                 , vial_type = 'dose10'
#'                                                                 , vax_vol = sc_model_params$dose10_vial_vol[1]
#'                                                                 , with_ice = T
#'                                                                 , doses_to_transport = part_OCC_dose10_final)
#' 
#' 
#' #passive cold chain required for monodose vials
#' part_OCC_monodose_RCW25_needs <- calc_transport_equipment_needs(equip_type = 'rcw25'
#'                                                                 , vial_type = 'monodose'
#'                                                                 , vax_vol = sc_model_params$monodose_vial_vol
#'                                                                 , with_ice = F
#'                                                                 , doses_to_transport = part_OCC_monodose_final)
#' 
#' 
#' #ceiling(part_OCC_monodose_final / 1301) #Source: Excel sheet "Cold Chain equipment"
#' part_OCC_monodose_vaxCarr_needs <- calc_transport_equipment_needs(equip_type = 'vaxCarr'
#'                                                                   , vial_type = 'monodose'
#'                                                                   , vax_vol = sc_model_params$monodose_vial_vol
#'                                                                   , with_ice = F
#'                                                                   , doses_to_transport = part_OCC_monodose_final)
#' 
#' #' Question: If we need more than 1 vaccine carrier for the doses, how do we translate that? Does that translate into
#' #' how many teams we'll need or how many trips should be undertaken by a single team? The latter will draw in the 
#' #' need for a rule for how the distance of the site from the base translates to trips in days and how that will affect
#' #' the team days and campaign duration.
#' 
#' #total passive cold chain needs
#' 
#' #RCW25
#' part_OCC_RCW25_needs <- part_OCC_dose10_RCW25_needs + part_OCC_monodose_RCW25_needs
#' 
#' #vaccine carriers
#' part_OCC_vaxCarr_needs <- part_OCC_dose10_vaxCarr_needs + part_OCC_monodose_vaxCarr_needs
#' 
#' # Part OCC quantity of icepack needs
#' 
#' #0.6L ice packs for RCW 25
#' part_OCC_RCW25_icepack_needs <- part_OCC_dose10_RCW25_needs * RCW25_icepack_needs #NOTE THE CHANGE HERE: WE ONLY NEED ICE PACKS FOR THE 10 DOSE VIALS, HENCE, WE DON'T CALCULATE ICE FOR THE MONODOSE FOR BOTH THE RCW 25 AND VACCINE CARRIERS
#' 
#' #0.4L ice packs for vaccine carriers
#' part_OCC_vaxCarr_icepack_needs <- part_OCC_dose10_vaxCarr_needs * vaxCarr_icepack_needs
#' 
#' #total icepack needs
#' part_OCC_icepack_needs <- part_OCC_vaxCarr_icepack_needs + part_OCC_RCW25_icepack_needs
#' 
#' #total volume of ice packs
#' 
#' #0.4L
#' part_OCC_vaxCarr_icepack_vol <- part_OCC_vaxCarr_icepack_needs * 0.4
#' #0.6L
#' part_OCC_RCW25_icepack_vol <- part_OCC_RCW25_icepack_needs * 0.6
#' 
#' 
#' 
#' 
#' #####################################
#' #' outputs for the part OCC strategy
#' #####################################
#' 
#' #initial volume of ice required 
#' part_OCC_init_iceVol <- part_OCC_vaxCarr_icepack_vol + part_OCC_RCW25_icepack_vol
#' # we only need 0.6L ice packs to transport the vaccines in the RCW25s. The 0.4L ones don't to play here yet
#' 
#' # freezing time
#' part_OCC_ft <- calc_freezing_time(mf314_available = sc_model_params$mf314_quant
#'                                   , large_icepacks_quantity = part_OCC_RCW25_icepack_needs 
#'                                   , small_icepacks_quantity = part_OCC_vaxCarr_icepack_needs)
#' 
#' 
#' 
#' # Initial number of icepacks required
#' part_OCC_init_icepack_quant <- part_OCC_RCW25_icepack_needs + part_OCC_vaxCarr_icepack_needs
#' 
#' 
#' ##team days calculations
#' #size of near population 
#' 
#' # part_OCC_near_pop <- extract_near_pop(site_data, site_rows_selected = 1)
#' # part_OCC_far_pop <- extract_far_pop(site_data, site_rows_selected = 1)
#' 
#' 
#' monodose_vaxCarr_cap_noIce <- calc_dose_capacity(vial_type = 'monodose' 
#'                                                  , vax_vol = sc_model_params$monodose_vial_vol
#'                                                  , equip_type = 'vaxCarr' #we assume a mobile team uses one vaccine carrier
#'                                                  , with_ice = F)
#' 
#' 
#' team_days_fixed_part_OCC <- round(site_data$near_pop / tp_fixed, 1) #computationally, we see the number doses as the number of expected people. The "final number of doses" here have already accounted for the buffer
#' team_days_mobile_part_OCC <- calc_monodose_team_days(target_pop = site_data$far_pop,
#'                                                      team_performance = sc_model_params$vax_rate['mobile_team']
#'                                                      ,  carrier_vol_capacity = monodose_vaxCarr_cap_noIce
#'                                                      )
#' 
#' #round(site_data$far_pop / monodose_vaxCarr_cap_noIce, 1)
#' 
#' 
#' #######
#' # Team allocation calculations and output
#' #######
#' 
#' #Extract size of allocated team from the sites table
#' site_teams_part_OCC <- extract_site_team_size(site_data, site_rows_selected = 1)
#' 
#' 
#' ################################################################
#' #' Combining the results and plotting
#' #' 
#' #####################################################################
#' 
#' 
#' #Making the tibbles!!!!
#' # Results of required freezing time per strategy
#' freezing_time_results <- tibble(
#'     strategy = strategy_list,
#'     time = c(monodose_FCC_ft, dose10_FCC_ft, mixed_FCC_ft, part_OCC_ft)
#' )
#' 
#' # Results of initial required volume of ice per strategy
#' Init_iceVol_results <- tibble(
#'     strategy = strategy_list,
#'     iceVol = c(monodose_FCC_init_iceVol, dose10_FCC_init_iceVol, mixed_FCC_init_iceVol, part_OCC_init_iceVol)
#' )
#' 
#' 
#' passive_cold_chain_needs <- tibble(strategy = strategy_list,
#'                                    rcw25 = c(monodose_FCC_RCW25_needs
#'                                              , dose10_FCC_RCW25_needs
#'                                              , mixed_FCC_RCW25_needs
#'                                              , part_OCC_RCW25_needs
#'                                              ),
#'                                    vaxCarr = c(monodose_FCC_vaxCarr_needs
#'                                                , dose10_FCC_vaxCarr_needs
#'                                                , mixed_FCC_vaxCarr_needs
#'                                                , part_OCC_vaxCarr_needs
#'                                                )
#'                                    )
#'                                              
#' 
#' #Results of team days calculations
#' td_results <- tibble(
#'     strategy = rep(strategy_list, each = 2),
#'     team_type = rep(team_type_list, times = length(strategy_list)),
#'     team_days = c(team_days_fixed_monodose_FCC, 
#'                   team_days_mobile_monodose_FCC, 
#'                   team_days_fixed_dose10_FCC, 
#'                   team_days_mobile_dose10_FCC, 
#'                   team_days_fixed_mixed_FCC, 
#'                   team_days_mobile_mixed_FCC, 
#'                   team_days_fixed_part_OCC, 
#'                   team_days_mobile_part_OCC
#'     )
#' )
#' 
#' 
#' 
#' 
#' 
#' ####################
#' #Making the plots!!!
#' ####################
#' 
#'     ft_plot <- ggplot(data = freezing_time_results, 
#'                       aes(x = strategy, y = time)
#'     ) + 
#'         geom_bar(stat = "identity", fill = "steelblue") + 
#'         labs(title = 'Freezing time required per strategy', 
#'              y = "Freezing time required (days)"
#'         ) + 
#'         shiny_plot_theme
#'     
#'     iceVol_plot <- ggplot(data = Init_iceVol_results, 
#'                           aes(x = strategy, 
#'                               y = iceVol)
#'     ) + 
#'         geom_bar(stat = "identity", fill = "steelblue") + 
#'         labs(title = 'Initial volume of ice required per strategy', 
#'              y = "Initial volume of ice required (Litres)"
#'         ) + 
#'         shiny_plot_theme
#'     
#'     td_plot <- ggplot(data = td_results, 
#'                       aes(x = strategy, 
#'                           y = team_days,
#'                           fill = team_type
#'                       )
#'     ) + 
#'         geom_bar(stat = 'identity',
#'                  position = 'dodge'
#'         ) + 
#'         labs(title = 'Number of days per team type and strategy', 
#'              y = "Team days"
#'         ) +
#'         scale_fill_manual(values = c("royalblue4", "tomato3"), 
#'                           name = "Team type",
#'                           breaks = team_type_list) +
#'         shiny_plot_theme
#'     
#'     # ft_presentation_plot <- ft_plot + presentation_plot_theme
#'     # iceVol_presentation_plot <- iceVol_plot + presentation_plot_theme
#'     # td_presentation_plot <- td_plot + presentation_plot_theme
#'     # 
#'     #arrange the plots on a grid
#' 
#'     
#'     if(display_sc_plots){
#'         sc_results_barplot <- grid.arrange(ft_plot, 
#'                                            # iceVol_plot, 
#'                                            td_plot,
#'                                            ncol = 2)
#'     }
#' 
#' #'Towards the isocline!! We have determined that the mixed FCC and monodose FCC
#' #'are out of question now. Also, we know that vaccine wastage is a function of the landscape
#' #' hence, is non-linear. Additionally, we know the vaccine carrier design is optimised for transporting ice.
#' #' The question then arises that if the vaccine carriers could be manufactured in larger capacities to transport monodose,
#' #' would that solve the problem? Again, that the fixed post teams have all they need to conduct the campaign at 
#' #'the site, hence, we will narrow in on the campaign targetting far children. Here, we have the choice 
#' #'between using 10-dose vials in full cold chain or the monodose out of the cold chain. We want to investigate
#' #'under what combinations of 10-dose wastage and higher storage capacity for monodose vials will 
#' #'the use of monodose outside of the cold chain be better than the 10-dose vaccine?
#' #'We will answer this using an isocline formulation.
#' 
#' #####################
#' #Far campaigns: monodose Outside of Cold Chain versus 10-dose in Full Cold Chain
#' #########################
#' 
#' 
#' ###############################
#' 
#' #how many doses of the 10-dose vials can we transport in a vaccine carrier?
#' # monodose_OCC_far_trip_capacity <- calc_dose_capacity(vial_type = 'monodose' 
#' #                                                      , vax_vol = 21.09
#' #                                                      , equip_type = 'vaxCarr' #we assume a mobile team uses one vaccine carrier
#' #                                                      , with_ice = F)
#' # #how many doses of the 10-dose vials can we transport in a vaccine carrier?
#' # dose10_vaxCarr_cap_ice <- calc_dose_capacity(vial_type = 'dose10' 
#' #                                                    , vax_vol = dose10_vial_vol[1]
#' #                                                    , equip_type = 'vaxCarr' #we assume a mobile team uses one vaccine carrier
#' #                                                    , with_ice = T)
#' 
#' 
#' monodose_OCC_far_trip_capacity_expanded <- seq(monodose_vaxCarr_cap_noIce, dose10_vaxCarr_cap_ice, 5) #just a vector of possible increasing volume capacities to consider. The idea is to increase it to as high as the capacity for 10-dose carriage per trip.
#' 
#' 
#' #mobile team days using monodose OCC
#' 
#' team_days_mobile_monodose_noIce <- unlist(purrr::map(monodose_OCC_far_trip_capacity_expanded, calc_monodose_team_days, target_pop = site_data$far_pop))
#' 
#' ###############################
#' #10 dose FCC for far campaigns
#' ###############################
#' 
#' #Considering wastage ranging from 0% to 100%, with 0% meaning no wastage and 100%, wastage of whole vial. MSF considers an average of 15% wastage rate in field operations. 
#' wastage_dose10_mt_vect <- round(seq(0, 1, length.out = length(team_days_mobile_monodose_noIce)), 3) 
#' 
#' #dose10_FCC_far_trip_eff_doses_vect <- ceiling(dose10_vaxCarr_cap_ice * (1 - wastage_dose10_mt_vect)) #effectively, how many vaccinations is a mobile team actually undertaking?
#' 
#' team_days_mobile_dose10_Ice <-  purrr::map_dbl(.x = wastage_dose10_mt_vect,
#'                                                   .f = calc_dose10_team_days, 
#'                                                   target_pop = site_data$far_pop, 
#'                                                   vaxCarr_capacity = dose10_vaxCarr_cap_ice,
#'                                                   team_performance = tp_mobile)
#'                                         #there's a division by zero here, returning an infinity, so we'll replace it with NA to allow for better manipulation
#' 
#' 
#' team_days_mobile_dose10_Ice <- ifelse(!is.infinite(team_days_mobile_dose10_Ice), team_days_mobile_dose10_Ice, NA) #I do a correction here to avoid division by zero.
#' 
#' #data.frame(wastage = wastage_dose10_mt_vect, team_days = team_days_mobile_dose10_Ice)
#' 
#' #x-axis of plot: ratio of increasing vaccine carrier volume capacity for monodose vs fixed for 10-dose
#' ratio_monodose_dose10_vaxCarr_capacity <- round(monodose_OCC_far_trip_capacity_expanded / dose10_vaxCarr_cap_ice, 3)
#' 
#' 
#' team_days_output <- tibble(
#'     dose10_wastage = wastage_dose10_mt_vect,
#'     vaxCarr_dose10_capacity = dose10_vaxCarr_cap_ice,
#'     vaxCarr_monodose_capacity = monodose_OCC_far_trip_capacity_expanded,
#'     vaxCarr_capacity_ratio = ratio_monodose_dose10_vaxCarr_capacity,
#'     team_days_dose10_far_FCC = team_days_mobile_dose10_Ice,
#'     team_days_monodose_far_OCC = team_days_mobile_monodose_noIce
#' )
#' 
#' #Plots
#' 
#' 
#' 
#' 
#' #range of points for formating axis labels
#' wastage_vs_team_days_axis_lim <- range(as.numeric(c(team_days_output$team_days_dose10_far_FCC, team_days_output$team_days_monodose_far_OCC)), na.rm = T)
#' 
#' #formating monodose data for comparison
#' monodose_wastage_comparison_dat <- dplyr::filter(team_days_output, dose10_wastage == 0) %>% 
#'     select(dose10_wastage, team_days_monodose_far_OCC) %>% 
#'     bind_rows(data.frame(dose10_wastage = max(team_days_output$dose10_wastage, na.rm = T), 
#'                                                         team_days_monodose_far_OCC = .$team_days_monodose_far_OCC
#'                                                         )
#'                                              )
#' 
#' #plot of 10 dose mobile team days against increasing wastage
#' wastage_vs_team_days <- ggplot(data = team_days_output) + 
#'     geom_point(aes(x = dose10_wastage, y = team_days_dose10_far_FCC)) + 
#'     geom_line(aes(x = dose10_wastage, y = team_days_dose10_far_FCC))  + 
#'     geom_point(data = slice(monodose_wastage_comparison_dat, 1), 
#'               aes(x = dose10_wastage, y = team_days_monodose_far_OCC), 
#'               color = 'red', 
#'               size = 2
#'               ) + 
#'     scale_y_continuous(breaks = round(seq(wastage_vs_team_days_axis_lim[1], 
#'                                     wastage_vs_team_days_axis_lim[2], 
#'                                     length.out = 10), 2
#'                                     ), 
#'                        labels = round(seq(wastage_vs_team_days_axis_lim[1], 
#'                                     wastage_vs_team_days_axis_lim[2], 
#'                                     length.out = 10), 2
#'                                     )
#'                        ) + 
#'     labs(x = 'Open vial wastage' 
#'          , y = 'Mobile team days' 
#'          #, title = '10 dose for far campaigns in full cold chain (monodose value shown in red)'
#'          )
#' 
#' 
#' 
#' #plot of monodose mobile team days against increasing dose storage capacity
#' storage_vs_team_days_lim <- range(c(team_days_output$team_days_monodose_far_OCC, 
#'                                     team_days_output$team_days_dose10_far_FCC[1]), 
#'                                   na.rm = T
#'                                   )
#' 
#' #formating 10-dose data for comparison
#' dose10_storage_comparison_dat <- dplyr::filter(team_days_output, dose10_wastage == 0.155) %>% 
#'     select(vaxCarr_dose10_capacity, vaxCarr_capacity_ratio, team_days_dose10_far_FCC) %>% 
#'     bind_rows(data.frame(vaxCarr_dose10_capacity = min(team_days_output$vaxCarr_monodose_capacity, na.rm = T),
#'                          vaxCarr_capacity_ratio = min(team_days_output$vaxCarr_capacity_ratio, na.rm = T),
#'                                                         team_days_dose10_far_FCC = .$team_days_dose10_far_FCC
#'                                              ))
#' 
#' 
#' storage_vs_team_days <- ggplot(data = team_days_output) + 
#'     geom_point(aes(x = vaxCarr_capacity_ratio, y = team_days_monodose_far_OCC)) + 
#'     geom_line(aes(x = vaxCarr_capacity_ratio, y = team_days_monodose_far_OCC))  + 
#'     geom_point(data = slice(dose10_storage_comparison_dat, 1), 
#'                aes(x = vaxCarr_capacity_ratio, y = team_days_dose10_far_FCC), 
#'                color = 'red', 
#'                size = 2
#'     ) + 
#'     scale_x_continuous(breaks = round(seq(range(team_days_output$vaxCarr_capacity_ratio)[1], 
#'             range(team_days_output$vaxCarr_capacity_ratio)[2], 
#'             length.out = 8), 2),
#'             labels = round(seq(range(team_days_output$vaxCarr_capacity_ratio)[1], 
#'                          range(team_days_output$vaxCarr_capacity_ratio)[2], 
#'                          length.out = 8), 2)
#'                        ) + 
#'     scale_y_continuous(breaks = round(
#'         seq(storage_vs_team_days_lim[1], 
#'             storage_vs_team_days_lim[2], 
#'             length.out = 10
#'         ),
#'         2
#'     ), 
#'     labels = round(seq(storage_vs_team_days_lim[1], 
#'                        storage_vs_team_days_lim[2], 
#'                        length.out = 10
#'     ), 2
#'     )
#'     ) + 
#'     labs(x = 'Dose storage capacity ratio (monodose vs 10-dose)'
#'          , y = 'Mobile team days'
#'          #, title = 'Monodose for far campaigns out of cold chain (10 dose value shown in red)'
#'          )
#' 
#'  
#' 
#' if(display_sc_plots){
#'     sc_results_wastage_storage <-  grid.arrange(wastage_vs_team_days, storage_vs_team_days, ncol = 1)
#' }
#' 
#' 
#' #Reshaping the results and assigning zero wastage to the monodose strategy
#' 
#' team_days_output_melted <- team_days_output %>% 
#'     select(-vaxCarr_dose10_capacity, -vaxCarr_monodose_capacity) %>% 
#'     gather(key = 'strategy'
#'            , value = 'team_days'
#'            , c('team_days_dose10_far_FCC', 'team_days_monodose_far_OCC')
#'            , factor_key = T
#'            )
#' 
#' team_days_output_melted_mod <- dplyr::mutate(team_days_output_melted
#'                                              , dose10_wastage = if_else(strategy == 'team_days_dose10_far_FCC'
#'                                                                         , team_days_output_melted$dose10_wastage
#'                                                                         , 0
#'                                                                         )
#'                                              )
#' #I do some further reshaping of the results for 10-dose so I can plot the team days wrt constant wastage rates across the monodose plot
#' team_days_output_melted_dose10 <- dplyr::filter(team_days_output_melted_mod
#'                                                 , strategy == 'team_days_dose10_far_FCC') %>% 
#'     dplyr::mutate(vaxCarr_capacity_ratio = paste(vaxCarr_capacity_ratio, collapse = ',')) %>% 
#'     separate_rows(vaxCarr_capacity_ratio, convert = T)
#' 
#' 
#' 
#' 
#' #' plot to illustrate how the 10-dose and monodose team days intersect at some 
#' #' points and how that leads to an isocline for decision-making
#' #' 
#' 
#' team_days_intersection_plot <- ggplot(data = team_days_output) + 
#'     geom_point(aes(x = vaxCarr_capacity_ratio, 
#'                    y = team_days_monodose_far_OCC), 
#'                color = 'black'
#'                ) +
#'     geom_line(aes(x = vaxCarr_capacity_ratio, 
#'                    y = team_days_monodose_far_OCC), 
#'                color = 'black'
#'     ) +
#'     geom_point(data = team_days_output_melted_dose10 %>% 
#'                    dplyr::filter(between(team_days, min(team_days_output$team_days_monodose_far_OCC) + 0.4, max(team_days_output$team_days_monodose_far_OCC))
#'                                  ), 
#'                aes(x = vaxCarr_capacity_ratio, 
#'                    y = team_days, 
#'                    color = factor(dose10_wastage)
#'                    )
#'                ) +
#'     scale_x_continuous(breaks = seq(0.2, 1, 0.05),
#'                        labels  = every_nth(seq(0.2, 1, 0.05), 2, inverse = T)
#'                        ) +
#'     labs(x = 'Vaccine carrier capacity ratio (monodose vs 10-dose)',
#'          y = 'Mobile team days',
#'          color = 'Wastage (10-dose)') 
#' 
#' if(display_sc_plots){
#'    plot(team_days_intersection_plot) 
#' }
#' 
#' 
#' if(save_plots){
#'  ggsave(filename = 'team_days_intersection_plot.png'
#'         #, plot = team_days_intersection_plot + presentation_plot_theme #uncomment this line to save a powerpoint version
#'         , team_days_intersection_plot
#'         , path = './figures/'
#'         , width = 9
#'         , height = 5)
#' }
#' 
#' 
#' #####################################
#' #Isocline plot
#' #####################################
#' 
#' #'The isocline represents the point on the intersection between the monodose and 10-dose 
#' #'strategies where there is no difference in team days. The areas above and below the line
#' #'correspond to a switch in decision.
#' 
#' isocline_df <- team_days_output %>% mutate(wastage_isocline = 1 - (site_data$far_pop/(vaxCarr_dose10_capacity*team_days_monodose_far_OCC)))
#' 
#' #View(isocline_df)
#' 
#' isocline_plot <- ggplot(isocline_df %>% filter(wastage_isocline > 0.6666667)) +
#'     geom_point(aes(x = vaxCarr_capacity_ratio,
#'                    y = wastage_isocline)
#'                )+
#'     geom_line(aes(x = vaxCarr_capacity_ratio,
#'                    y = wastage_isocline)
#'     ) +
#'     labs(x = 'Vaccine carrier capacity ratio (monodose vs 10-dose)',
#'          y = 'Open vial wastage (10-dose)')
#' 
#' if(display_sc_plots){
#' plot(isocline_plot)
#' }
#' 
#' 
#' if(save_plots){
#'     ggsave(filename = 'mobile_team_days_isocline.png'
#'           # , plot = isocline_plot + presentation_plot_theme #uncomment this line to save a powerpoint version
#'           , plot = isocline_plot
#'           , path = './figures/'
#'            , width = 9
#'            , height = 5)
#' }

