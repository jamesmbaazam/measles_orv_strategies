
#packages
source('./scripts/deterministic_framework_analysis/global_scripts/analyses_parameters.R')
source('./scripts/deterministic_framework_analysis/global_scripts/parameters.R')
source('./scripts/deterministic_framework_analysis/global_scripts/supply_chain_functions.R')


#NB: Create code sections by following a comment with 4 or more dashes (----), 
# subsections by following a comment with 4 or more equal signs (====),
# and subsubsections following a comment with 4 or more hashes (####)



# analyse_prep_delay() ---- 
#' wrapper to combine all the supply chain functions and analyse 
#'the logistical needs and time to commence a campaign for each strategy, based on
#' the first transportation assumption. (Refer to dissertation writeup but basically,
#' we assume that you transport the vaccines to the field base in cold boxes with ice
#' and when you arrive, you transfer the vaccines into fridges, then you freeze 
#' fresh ice depending on the number of teams being dispatched. This strategy
#' requires much less ice and cold boxes as it depends on the number of teams being
#' dispatched)
#'
#' @param strategy_name 
#' @param fixed_team_with_dose10 #' options = if "T", 10 dose, else monodose
#' @param fixed_team_with_ice #' options = if "T", ice is used, else, no ice
#' @param mobile_team_with_dose10 #' options = if "T", 10 dose, else monodose
#' @param mobile_team_with_ice #' options = if "T", ice is used, else, no ice
#' @param team_dispatch #' options = "parallel", "asap"
#' @param site_details 
#' @param fixed_team_equip_type # fixed teams can use one of 
#' options = c('rcw25', 'vaxCarr', 'both') for now, we assume fixed teams use both
#' @param mobile_team_equip_type # options = c('rcw25', 'vaxCarr', 'both') for 
#' now, we assume mobile teams use vaxCarr
#' @param n_teams_fixed 
#' @param n_teams_mobile 
#' @param mf314 
#' @param rcw25_ice_replacement_days 
#' @param ambient_temperature 
#' @param dose10_vial_volume 
#' @param res_type #c('simple', 'detailed'): 'simple' returns only the team days 
#' per team type. 'detailed' returns a detailed output
#' @param n_fixed_teams_per_site #number of fixed post teams per vax site (options = 1 or 2 based on MSF policy)
#' @param browse 
#' @param monodose_vial_volume
#'
#' @example analyse_prep_delay(strategy_name = 'dose10_occ_parallel', fixed_team_with_dose10 = T, fixed_team_with_ice = F, mobile_team_with_dose10 = T, mobile_team_with_ice = F, team_dispatch = 'parallel', site_details = data.frame(location_id = 1, near_pop = 10000, far_pop = 20000), fixed_team_equip_type = 'both', mobile_team_equip_type = 'vaxCarr', n_teams_fixed = 20, n_teams_mobile = 20, mf314 = 1, rcw25_ice_replacement_days = 2, ambient_temperature = sc_model_params$ambient_temp[1], dose10_vial_volume = sc_model_params$dose10_vial_vol[1], monodose_vial_volume = sc_model_params$monodose_vial_vol, res_type = 'simple') 

analyse_prep_delay <- function(strategy_name,
                               fixed_team_with_dose10, 
                               fixed_team_with_ice, 
                               mobile_team_with_dose10, 
                               mobile_team_with_ice, 
                               team_dispatch,
                               site_details,
                               fixed_team_equip_type = "both", 
                               mobile_team_equip_type, 
                               n_teams_fixed,
                               n_fixed_teams_per_site,
                               n_teams_mobile,
                               mf314,
                               rcw25_ice_replacement_days,
                               ambient_temperature, 
                               dose10_vial_volume, 
                               monodose_vial_volume,
                               res_type,
                               browse = F
                               ) {

  #' Equipment rules: if n_fixed_teams_per_site = 1, then each fixed team will require 1 RCW25 and 1 vaccine carrier,
  #' else, the fixed teams at the same post will require 1 RCW25 and 2 vaccine carriers
  #'
  if(browse) browser()
    # fixed team rules
   fixed_teams_rcw25 <- if (n_fixed_teams_per_site == 2 & n_teams_fixed %% 2 == 0) {
    n_teams_fixed / 2
  } else if (n_fixed_teams_per_site == 2 & n_teams_fixed %% 2 == 1) {
    floor(n_teams_fixed / 2) + 1
  } else if (n_fixed_teams_per_site == 1) {
    n_teams_fixed
  } else {
    stop("Check inputs: n_fixed_teams_per_site must be 1 or 2")
  }

  fixed_teams_vaxCarr <- n_teams_fixed

  # mobile team rules
  mobile_teams_rcw25 <- if (mobile_team_equip_type == "rcw25") {
    1
  }
  else if (mobile_team_equip_type == "both") {
    1
  }
  else if (mobile_team_equip_type == "vaxCarr") {
    0
  }
  else {
    stop("unknown fixed team equipment scenario")
  }

  mobile_teams_vaxCarr <- if (mobile_team_equip_type == "vaxCarr") {
    1
  }
  else if (mobile_team_equip_type == "both") {
    1
  }
  else if (mobile_team_equip_type == "rcw25") {
    0
  }
  else {
    stop("unknown mobile team equipment scenario")
  }


  ## Doses and equipment calculations ====

  ### Fixed post - number of doses ####
  n_doses_fixed_team <- calc_doses_required(
    df = site_details,
    is_dose10 = fixed_team_with_dose10,
    pop_type = "near",
    ovwastage = ifelse(fixed_team_with_dose10,
      sc_model_params$dose10_ovw_fixed_team,
      sc_model_params$monodose_ovw_fixed_team
    ), 
    buffer_size = sc_model_params$buffer_stock
  )

  ### Fixed post - Final number of passive cold chain required, based on the number of teams specified ####
  RCW25_required_fixed_team <- fixed_teams_rcw25

  vaxCarr_required_fixed_team <- fixed_teams_vaxCarr


  ### Mobile teams - number of doses ####
  n_doses_mobile_team <- calc_doses_required(
    df = site_details, 
    is_dose10 = mobile_team_with_dose10,
    pop_type = "far",
    ovwastage = ifelse(mobile_team_with_dose10,
      sc_model_params$dose10_ovw_mobile_team,
      sc_model_params$monodose_ovw_mobile_team
    ),
    buffer_size = sc_model_params$buffer_stock
  )

  ### Mobile teams - passive cold chain needs ####

  # Here, I am assuming that mobile teams only need a vaccine carrier.
  #' vaccine carriers are transported with ice and vaccines to the site.
  RCW25_required_mobile_team <- n_teams_mobile * mobile_teams_rcw25

  vaxCarr_required_mobile_team <- n_teams_mobile * mobile_teams_vaxCarr



  ## Ice pack needs calculations ====
  #' total number of 0.6L ice packs = number of RCW25 needed * number of ice packs
  #' needed per RCW25

  ### RCW25 and vaccince carrier needs ####
  # ice packs needed for each equipment type, based on the ambient temperature
  RCW25_icepack_needs <- compute_rcw25_icepacks(sc_model_params$ambient_temp[1],
    replacement_days = rcw25_ice_replacement_days
  )

  vaxCarr_icepack_needs <- compute_vaxCarr_icepacks(sc_model_params$ambient_temp[1])

  #### Fixed post - Ice packs required ####
  #' total number of 0.6L ice packs = number of RCW25 needed * number of ice packs needed per RCW25
  RCW25_icepack_needs_fixed_team <- calc_icepack_tot_quant(
    equipment_quantity = ifelse(fixed_team_with_ice,
      RCW25_required_fixed_team,
      0
    ),
    icepacks_per_equipment = RCW25_icepack_needs
  )
  
  vaxCarr_icepack_needs_fixed_team <- calc_icepack_tot_quant(
    equipment_quantity = ifelse(fixed_team_with_ice,
      vaxCarr_required_fixed_team,
      0
    ),
    icepacks_per_equipment = vaxCarr_icepack_needs
  )

  ### Mobile team - Ice packs required ####
  RCW25_icepack_needs_mobile_team <- calc_icepack_tot_quant(
    equipment_quantity = ifelse(mobile_team_with_ice,
      RCW25_required_mobile_team,
      0
    ),
    icepacks_per_equipment = RCW25_icepack_needs
  )
  vaxCarr_icepack_needs_mobile_team <- calc_icepack_tot_quant(
    equipment_quantity = ifelse(mobile_team_with_ice,
      vaxCarr_required_mobile_team,
      0
    ),
    icepacks_per_equipment = vaxCarr_icepack_needs
  )

  ## Freezing time calculations ====
  #' Time it takes to freeze depends on how many freezers are available and their
  #' capacity. I currently assume that we only use the MF314 freezer, which is the
  #' largest, and I specify the quantity at the beginning of this script


  ### Fixed post team  - freezing time ####
  freezing_time_fixed_team <- calc_freezing_time(
    mf314_available = mf314,
    large_icepacks_quantity = ifelse(fixed_team_with_ice,
      RCW25_icepack_needs_fixed_team,
      0
    ),
    small_icepacks_quantity = ifelse(fixed_team_with_ice,
      vaxCarr_icepack_needs_fixed_team,
      0
    )
  )


  ### Mobile team  - freezing time ####
  freezing_time_mobile_team <- calc_freezing_time(
    mf314_available = mf314,
    large_icepacks_quantity = ifelse(mobile_team_with_ice,
      RCW25_icepack_needs_mobile_team,
      0
    ),
    small_icepacks_quantity = ifelse(mobile_team_with_ice,
      vaxCarr_icepack_needs_mobile_team,
      0
    )
  )


  ## Campaign delay calculation ====
  #' this function uses the results of mobile and fixed post team freezing times
  #' and more importantly, the routing decision to determine how long a strategy
  #' will be delayed before a campaign can start
  campaign_delay <- calc_campaign_start(
    ft_freeze_time = freezing_time_fixed_team,
    mt_freeze_time = freezing_time_mobile_team,
    team_routing = team_dispatch
  )


  ## Results - Campaign delays ====
  
  if(res_type == 'detailed'){
  res <- data.frame(
    strategy = as_factor(strategy_name),
    location_id = site_details$location_id,
    near_pop = site_details$near_pop,
    far_pop = site_details$far_pop,
    n_teams_fixed = n_teams_fixed,
    n_teams_mobile = n_teams_mobile,
    mt_equip_type = as_factor(mobile_team_equip_type),
    ft_equip_type = as_factor(fixed_team_equip_type),
    ft_vial_type = as_factor(ifelse(fixed_team_with_dose10, "dose10", "monodose")),
    ft_doses_required = n_doses_fixed_team,
    mt_vial_type = as_factor(ifelse(mobile_team_with_dose10, "dose10", "monodose")),
    mt_doses_required = n_doses_mobile_team,
    ft_RCW25 = RCW25_required_fixed_team,
    mt_RCW25 = RCW25_required_mobile_team,
    ft_vaxCarr = vaxCarr_required_fixed_team,
    mt_vaxCarr = vaxCarr_required_mobile_team,
    ft_icepacks_large = RCW25_icepack_needs_fixed_team,
    mt_icepacks_large = RCW25_icepack_needs_mobile_team,
    ft_icepacks_small = vaxCarr_icepack_needs_fixed_team,
    mt_icepacks_small = vaxCarr_icepack_needs_mobile_team,
    ft_freezing_time = freezing_time_fixed_team,
    mt_freezing_time = freezing_time_mobile_team,
    campaign_start = campaign_delay$start_day,
    team_leaving_first = campaign_delay$which_team_first
  )
  }else if (res_type == 'simple'){
  res <- data.frame(
    strategy = as_factor(strategy_name),
    ft_freezing_time = freezing_time_fixed_team,
    mt_freezing_time = freezing_time_mobile_team
  )
  } else{
    stop('Please specify the correct type of output in c(simple, detailed)')
  }
  return(res)
}


# analyse_team_days() ----
#' a wrapper to determine what the team days is for a strategy.
#' Given tau team days, we need a single team to spend tau days, or two teams to spend 
#' tau/2 days, and for n teams, tau/n. It is to serve as the campaign duration on a site 
#'
#' @param strategy_name 
#' @param fixed_team_with_dose10 #' options = if "T", 10 dose, else monodose
#' @param fixed_team_with_ice #' options = if "T", ice is used, else, no ice
#' @param mobile_team_with_dose10 #' options = if "T", 10 dose, else monodose
#' @param mobile_team_with_ice #' options = if "T", ice is used, else, no ice
#' @param site_details 
#' @param site_row 
#' @param dose10_vial_volume 
#' @param monodose_vial_volume 
#' @param mobile_team_equip_type 
#' @param browse 


#' estim_campaign_metrics
#'
#' @param strategy_name 
#' @param fixed_team_with_dose10 #' options = if "T", 10 dose, else monodose
#' @param fixed_team_with_ice #' options = if "T", ice is used, else, no ice
#' @param mobile_team_with_dose10 '# options = if "T", 10 dose, else monodose
#' @param mobile_team_with_ice #' options = if "T", ice is used, else, no ice
#' @param site_details 
#' @param site_campaign_dur 
#' @param n_teams_fixed 
#' @param ft_team_performance 
#' @param n_teams_mobile 
#' @param mt_team_performance 
#' @param dose10_vial_volume 
#' @param monodose_vial_volume 
#' @param mobile_team_equip_type 
#' @param browse 
#'
#' @return
#' @export
#'
#' @examples estim_campaign_metrics(strategy_name = 'toy_strategy', 
# fixed_team_with_dose10 = T, 
# fixed_team_with_ice = T, 
# mobile_team_with_dose10 = T, 
# mobile_team_with_ice = T, 
# site_details = data.frame(
#   near_pop = 50000,
#   far_pop =50000,
#   location_id = 1
# ), 
# site_campaign_dur_constraint = 10, 
# n_teams_fixed = 10, 
# ft_team_performance = 450, 
# n_teams_mobile = 10, 
# mt_team_performance = 250, 
# dose10_vial_volume = sc_model_params$dose10_vial_vol[1], 
# monodose_vial_volume = sc_model_params$monodose_vial_vol[1], 
# mobile_team_equip_type = 'vaxCarr', 
# dose10_ovwr_mt = 100, 
# monodose_ovwr_mt = 100, 
# browse = F
# )


 
estim_campaign_metrics <- function(strategy_name,
                               fixed_team_with_dose10,
                               fixed_team_with_ice,
                               mobile_team_with_dose10,
                               mobile_team_with_ice,
                               site_details,
                               site_campaign_dur_constraint,
                               n_teams_fixed,
                               ft_team_performance,
                               n_teams_mobile,
                               mt_team_performance,
                               dose10_vial_volume,
                               monodose_vial_volume,
                               mobile_team_equip_type,
                               dose10_ovwr_ft,
                               monodose_ovwr_ft,
                               dose10_ovwr_mt,
                               monodose_ovwr_mt,
                               browse = F) {
  if (browse) browser()

  ## Fixed post team days ====
#  team_days_fixed_team <- round(site_details$near_pop / ft_team_performance, 1)   #' Team days needed by fixed teams, NOT CONSTRAINED by volume/space - we assume
  #' they can transport all they need per trip and they use both an RCW25 and vaccine
  #' carrier at the vaccination site. Also, any losses due to wastage can be replenished the same day unlike mobile teams

#fixed team vaccine carrier volume  
fixed_team_vaxCarr_vol <- calc_dose_capacity(
  ifelse(fixed_team_with_dose10,
         "dose10",
         "monodose"
  ),
  vax_vol = ifelse(fixed_team_with_dose10,
                   dose10_vial_volume,
                   monodose_vial_volume
  ),
  equip_type = 'vaxCarr',
  with_ice = fixed_team_with_ice
)

#fixed team RCW25 volume
fixed_team_rcw25_vol <- calc_dose_capacity(
  ifelse(fixed_team_with_dose10,
         "dose10",
         "monodose"
  ),
  vax_vol = ifelse(fixed_team_with_dose10,
                   dose10_vial_volume,
                   monodose_vial_volume
  ),
  equip_type = 'rcw25',
  with_ice = fixed_team_with_ice
)


#'total transport capacity for fixed teams is the sum of the capacity for the vaccine
#'carrier and rcw25 because we assume that fixed post teams use both equipment

ft_total_transport_capacity <- fixed_team_vaxCarr_vol + fixed_team_rcw25_vol


  ## Mobile teams team days ====
  #' Mobile teams are constrained by how much they can transport, hence, the equipment scenario at play
  mobile_team_vol_capacity <- calc_dose_capacity(
    vial_type = ifelse(mobile_team_with_dose10,
      "dose10",
      "monodose"
    ),
    vax_vol = ifelse(mobile_team_with_dose10,
      dose10_vial_volume,
      monodose_vial_volume
    ),
    equip_type = mobile_team_equip_type,
    with_ice = mobile_team_with_ice
  )



  #' team days needed by mobile teams, CONSTRAINED by volume/space - we assume 
  #' per trip, they can only transport as much as the equipment allows; 
  #' furthermore, for the 10-dose, due to open vial wastage, they end up wasting 
  #' a percentage of the doses, hence they have to make more trips to account 
  #' for that 10-dose mobile campaign team days are affected by the effective 
  #' doses, which is the the total capacity they can carry less of how many 
  #' are expected to be wasted.
 
  #mobile wastage rate  
  mt_ovwr <- ifelse(mobile_team_with_dose10, dose10_ovwr_mt, monodose_ovwr_mt) #this line determines which open vial wastage rate (ovwr) to use for the next line
 
  #calculate mobile team days with the wastage above
  team_days_mobile_team <-  calc_team_days(
    target_pop = site_details$far_pop,
    ovwastage = mt_ovwr,
    team_performance = mt_team_performance,
    carrier_vol_capacity = mobile_team_vol_capacity
  )
  
  
  
  #fixed team wastage rate
  ft_ovwr <- ifelse(fixed_team_with_dose10, dose10_ovwr_ft, monodose_ovwr_ft)
   
  #calculate fixed team days with the wastage above
  team_days_fixed_team <-  calc_team_days(
    target_pop = site_details$near_pop,
    ovwastage = ft_ovwr,
    team_performance = ft_team_performance,
    carrier_vol_capacity = ft_total_transport_capacity
  )

  #' assuming one team type can start immediately e.g., using occ
  #' BUT I need to think hard about this: is it plus or minus in the numerator?
  per_ft_campaign_dur <- team_days_fixed_team/n_teams_fixed
  
  per_mt_campaign_dur <- team_days_mobile_team/n_teams_mobile

  ft_campaign_dur_constrained <- min(per_ft_campaign_dur, site_campaign_dur_constraint)
  mt_campaign_dur_constrained <- min(per_mt_campaign_dur, site_campaign_dur_constraint)
  
  #' Fixed post vaccination coverage
  ft_vax_capacity <- min(ft_total_transport_capacity*(1 - ft_ovwr/100), ft_team_performance) #the vax capacity is the amount of vaccinations possible for fixed teams during a team day. This is either constrained by the effective doses resulting after applying coverage, or the team days
  
  fixed_team_coverage <- (ft_campaign_dur_constrained*n_teams_fixed*ft_vax_capacity)/site_details$near_pop
  
  #' Mobile team vaccination coverage
  mt_vax_capacity <- min(mobile_team_vol_capacity*(1 - mt_ovwr/100), mt_team_performance) #the vax capacity is the amount of vaccinations possible for mobile teams during a team day. This is either constrained by the effective doses resulting after applying coverage, or the team days
  
  mobile_team_coverage <- (mt_campaign_dur_constrained*n_teams_mobile*mt_vax_capacity)/site_details$far_pop 
  
  
  total_site_coverage <- (fixed_team_coverage * site_details$near_pop + 
                            mobile_team_coverage * site_details$far_pop)/ 
    (site_details$near_pop + site_details$far_pop)
  
  #' campaign duration calculations
  #' 1. The constrained campaign duration calculation assumes the two team types move to new sites together so that
  #' the total duration depends on the slower team type
  campaign_dur_constrained <- max(ft_campaign_dur_constrained, mt_campaign_dur_constrained)
  
  #' 2. The unconstrained assumes that they two team types move to new sites independently
  campaign_dur_uncontrained <- max(per_ft_campaign_dur, per_mt_campaign_dur)
  
  #' calculate campaign duration gains or deficits to track coverage especially
  #' for the deficit case. Gains can be harnessed in the next campaign
  dur_diff <- site_campaign_dur_constraint - campaign_dur_uncontrained
  if(dur_diff <= 0){
    dur_deficit <- abs(dur_diff)
    dur_gain <- 0
  }else{
    dur_gain <- abs(dur_diff)
    dur_deficit <- 0
  }
  
  ## Results 
  out <- data.frame(
    strategy = strategy_name,
    location_id = site_details$location_id,
    near_pop = site_details$near_pop,
    far_pop = site_details$far_pop,
    fixed_teams = n_teams_fixed,
    mobile_teams = n_teams_mobile,
    mt_equip_type = mobile_team_equip_type,
    ft_dur_constrained = round(ft_campaign_dur_constrained, 1),
    mt_dur_constrained = round(mt_campaign_dur_constrained, 1),
    site_campaign_dur_constrained = round(campaign_dur_constrained, 1),
    site_campaign_dur_unconstrained = round(campaign_dur_uncontrained, 1),
    site_campaign_dur_gain = round(dur_gain, 1),
    site_campaign_dur_deficit = round(dur_deficit, 1),
    ft_cov = round(fixed_team_coverage, 3),
    mt_cov = round(mobile_team_coverage, 3),
    site_cov_total = round(total_site_coverage, 3)
  )

  return(out)
}


#' summarise_campaign_metrics
#'
#' @param sc_analysis_res a data.frame of the final results from the 
#' supply chain analysis 
#'
#' @return a condensed summary data.frame of the full supply chain analysis
#' @export
#'
#' @examples summarise_campaign_metrics(df)
#' 
# summarise_campaign_metrics <- function(sc_analysis_res, browse = F){
#   if(browse) browser()
#   coverage_mean <- mean(sc_analysis_res$site_cov_total)
#   campaign_delay_total <- calc_compounded_delays(delays = sc_analysis_res$total_op_time,
#                                                  team_days = sc_analysis_res$total_op_time)
#   
#   campaign_summary <- data.frame(strategy = sc_analysis_res$strategy[1],
#                                  mt_equip = sc_analysis_res$mt_equip_type[1],
#                                  n_sites = length(sc_analysis_res$location_id),
#                                  n_fixed_teams = sc_analysis_res$fixed_teams[1],
#                                  n_mobile_teams = sc_analysis_res$mobile_teams[1],
#                                  average_coverage = coverage_mean,
#                                  campaign_duration = tail(campaign_delay_total, 1)
#   )
#   return(campaign_summary)
# }

#' analyse_team_days <- function(strategy_name,
#'                               fixed_team_with_dose10,
#'                               fixed_team_with_ice,
#'                               mobile_team_with_dose10,
#'                               mobile_team_with_ice,
#'                               site_details,
#'                               dose10_vial_volume = sc_model_params$dose10_vial_vol[1],
#'                               monodose_vial_volume = sc_model_params$monodose_vial_vol,
#'                               mobile_team_equip_type,
#'                               browse = F) {
#'   if (browse) browser()
#' 
#'   ## Fixed post team days ====
#'   #' team days needed by fixed teams, NOT CONSTRAINED by volume/space - we assume
#'   #' they can transport all they need per trip
#'   #' #computationally, we see the number doses as the number of expected people.
#'   #' The "final number of doses" here have already accounted for the buffer
#'   team_days_fixed_team <- round(site_details$near_pop / tp_fixed, 1)
#' 
#' 
#' 
#'   ## Mobile teams team days ====
#'   # Mobile teams only use a vaccine carrier, hence, are contrained by how much they can transport
#'   mobile_team_vol_capacity <- calc_dose_capacity(
#'     vial_type = ifelse(mobile_team_with_dose10,
#'       "dose10",
#'       "monodose"
#'     ),
#'     vax_vol = ifelse(mobile_team_with_dose10,
#'       dose10_vial_volume,
#'       monodose_vial_volume
#'     ),
#'     equip_type = mobile_team_equip_type,
#'     with_ice = mobile_team_with_ice
#'   )
#' 
#' 
#' 
#'   #' team days needed by mobile teams, CONSTRAINED by volume/space - we assume
#'   #' per trip, they can only transport as much as the vaccine carrier allows;
#'   #' furthermore, for the 10-dose, due to open vial wastage, they end up wasting
#'   #' a percentage of the doses, hence they have to make more trips to account
#'   #' for that 10-dose mobile campaign team days are affected by the effective doses,
#'   #' which is the the total capacity they can carry less of how many are
#'   #' expected to be wasted.
#'   
#'   mt_ovwr <- ifelse(mobile_team_with_dose10, 
#'                     sc_model_params$dose10_ovw_mobile_team, 
#'                     sc_model_params$monodose_ovw_mobile_team
#'                     )
#'   
#'  
#'   #this line determines which open vial wastage rate (ovwr) to use for the next line
#'   
#'   
#'   team_days_mobile_team <-  calc_team_days(
#'     target_pop = site_details$far_pop,
#'     ovwastage = mt_ovwr,
#'     team_performance = mt_team_performance,
#'     carrier_vol_capacity = mobile_team_vol_capacity
#'   )
#'   
#'   ## Results - Team days  ====
#'   out <- data.frame(
#'     strategy = strategy_name,
#'     location_id = site_details$location_id,
#'     near_pop = site_details$near_pop,
#'     far_pop = site_details$far_pop,
#'     mt_equip_type = mobile_team_equip_type,
#'     ft_vial_type = ifelse(fixed_team_with_dose10, "dose10", "monodose"),
#'     ft_with_ice = ifelse(fixed_team_with_ice, "yes", "no"),
#'     ft_team_days = team_days_fixed_team,
#'     mt_vial_type = ifelse(mobile_team_with_dose10, "dose10", "monodose"),
#'     mt_with_ice = ifelse(mobile_team_with_ice, "yes", "no"),
#'     mt_team_days = team_days_mobile_team
#'   )
#' 
#'   return(out)
#' }


# estimate_prior_logistical_needs() ---- 
#' wrapper to combine all the necessary supply chain functions to estimate 
#'the logistical needs to convey the vaccines to the field base for each strategy. 
#' we assume that you transport the vaccines to the field base in cold boxes with ice
#' and when you arrive, you freeze to replace what's in the cold boxes. The essential
#' point here is that, throughout the campaign, you don't take out the vaccines 
#' in the cold boxes. This assumption requires a lot more ice)
#'
#' @param strategy_name 
#' @param fixed_team_with_dose10 options = if "T", 10 dose, else monodose
#' @param fixed_team_with_ice options = if "T", ice is used, else, no ice
#' @param mobile_team_with_dose10  options = if "T", 10 dose, else monodose
#' @param mobile_team_with_ice options = if "T", ice is used, else, no ice
#' @param team_dispatch 
#' @param site_details 
#' @param site_row 
#' @param mf314 
#' @param rcw25_ice_replacement_days 
#' @param ambient_temperature 
#' @param dose10_vial_volume 
#' @param monodose_vial_volume 
#' @example estimate_prior_logistical_needs(strategy_name = 'bla', 
#' fixed_team_with_dose10 = T, 
#' fixed_team_with_ice = T, mobile_team_with_dose10 = T, mobile_team_with_ice = T, 
#' site_details = make_site_data(near_pop_size = 1000, far_pop_size = 1000), 
#' mf314 = 1, rcw25_ice_replacement_days = 2, ambient_temperature = sc_model_params$ambient_temp[1], 
#' dose10_vial_volume = sc_model_params$dose10_vial_vol[1], 
#' monodose_vial_volume = sc_model_params$monodose_vial_vol[1]
#' )
#' estimate_prior_logistical_needs <- function(strategy_name,
#'                                             fixed_team_with_dose10,
#'                                             fixed_team_with_ice,
#'                                             mobile_team_with_dose10,
#'                                             mobile_team_with_ice,
#'                                             site_details,
#'                                             mf314 = sc_model_params$mf314_quant,
#'                                             rcw25_ice_replacement_days,
#'                                             ambient_temperature,
#'                                             dose10_vial_volume,
#'                                             monodose_vial_volume,
#'                                             browse = F
#' ) {
#'   
#'   if (browse) browser()
#'   ## Doses and equipment calculations ====
#'   
#'   ### Fixed post - number of doses ####
#'   n_doses_fixed_team <- calc_doses_required(
#'     df = site_details,
#'     is_dose10 = fixed_team_with_dose10,
#'     pop_type = "near",
#'     ovwastage = ifelse(fixed_team_with_dose10,
#'                        sc_model_params$dose10_ovw_fixed_team,
#'                        sc_model_params$monodose_ovw_fixed_team
#'     ),
#'     buffer_size = sc_model_params$buffer_stock
#'   )
#'   
#'   ### Fixed post - passive cold chain ####
#'   #' Assumptions:
#'   #' 1). RCW25 cold boxes are used to transport the vaccines to the site and then
#'   #' transferred into the referigerators. The transported ice is thrown out and
#'   #' new ones frozen.
#'   #' (2). Whether a strategy is in the cold chain or not doesn't matter,
#'   #' the vaccines will be transported in the cold chain to the field base.
#'   
#'   RCW25_required_fixed_team <- calc_transport_equipment_needs(
#'     equip_type = "rcw25",
#'     vial_type = ifelse(fixed_team_with_dose10,
#'                        "dose10",
#'                        "monodose"
#'     ),
#'     vax_vol = ifelse(fixed_team_with_dose10,
#'                      dose10_vial_volume,
#'                      monodose_vial_volume
#'     ),
#'     with_ice = T,
#'     doses_to_transport = n_doses_fixed_team
#'   )
#'   
#'   vaxCarr_required_fixed_team <- calc_transport_equipment_needs(
#'     equip_type = "vaxCarr",
#'     vial_type = ifelse(fixed_team_with_dose10,
#'                        "dose10",
#'                        "monodose"
#'     ),
#'     vax_vol = ifelse(fixed_team_with_dose10,
#'                      dose10_vial_volume,
#'                      monodose_vial_volume
#'     ),
#'     with_ice = fixed_team_with_ice,
#'     doses_to_transport = n_doses_fixed_team
#'   )
#'   
#'   
#'   ### Mobile teams - number of doses ####
#'   n_doses_mobile_team <- calc_doses_required(
#'     df = site_details,
#'     is_dose10 = mobile_team_with_dose10,
#'     pop_type = "far",
#'     ovwastage = ifelse(mobile_team_with_dose10,
#'                        sc_model_params$dose10_ovw_mobile_team,
#'                        sc_model_params$monodose_ovw_mobile_team
#'     ),
#'     buffer_size = sc_model_params$buffer_stock
#'   )
#'   
#'   ### Mobile teams - passive cold chain needs ####
#'   
#'   RCW25_required_mobile_team <- calc_transport_equipment_needs(
#'     equip_type = "rcw25",
#'     vial_type = ifelse(mobile_team_with_dose10,
#'                        "dose10",
#'                        "monodose"
#'     ),
#'     vax_vol = ifelse(mobile_team_with_dose10,
#'                      dose10_vial_volume,
#'                      monodose_vial_volume
#'     ),
#'     with_ice = T,
#'     doses_to_transport = n_doses_mobile_team
#'   )
#'   
#'   vaxCarr_required_mobile_team <- calc_transport_equipment_needs(
#'     equip_type = "vaxCarr",
#'     vial_type = ifelse(mobile_team_with_dose10,
#'                        "dose10",
#'                        "monodose"
#'     ),
#'     vax_vol = ifelse(mobile_team_with_dose10,
#'                      dose10_vial_volume,
#'                      monodose_vial_volume
#'     ),
#'     with_ice = mobile_team_with_ice,
#'     doses_to_transport = n_doses_mobile_team
#'   )
#'   
#'   ## Ice pack needs calculations ====
#'   #' total number of 0.6L ice packs = number of RCW25 needed * number of ice 
#'   #' packs needed per RCW25
#'   
#'   ### RCW25 and vaccine carrier needs ####
#'   # ice packs needed for each equipment type, based on the ambient temperature
#'   RCW25_icepack_needs <- compute_rcw25_icepacks(ambient_temperature,
#'                                                 replacement_days = rcw25_ice_replacement_days
#'   )
#'   
#'   vaxCarr_icepack_needs <- compute_vaxCarr_icepacks(ambient_temperature)
#'   
#'   #### Fixed post - Ice packs required ####
#'   #' total number of 0.6L ice packs = number of RCW25 needed * number of ice 
#'   #' packs needed per RCW25
#'   RCW25_icepack_needs_fixed_team <- calc_icepack_tot_quant(
#'     equipment_quantity = ifelse(fixed_team_with_ice,
#'                                 RCW25_required_fixed_team,
#'                                 0
#'     ),
#'     icepacks_per_equipment = RCW25_icepack_needs
#'   )
#'   vaxCarr_icepack_needs_fixed_team <- calc_icepack_tot_quant(
#'     equipment_quantity = ifelse(fixed_team_with_ice,
#'                                 vaxCarr_required_fixed_team,
#'                                 0
#'     ),
#'     icepacks_per_equipment = vaxCarr_icepack_needs
#'   )
#'   
#'   ### Mobile team - Ice packs required ####
#'   RCW25_icepack_needs_mobile_team <- calc_icepack_tot_quant(
#'     equipment_quantity = ifelse(mobile_team_with_ice,
#'                                 RCW25_required_mobile_team,
#'                                 0
#'     ),
#'     icepacks_per_equipment = RCW25_icepack_needs
#'   )
#'   vaxCarr_icepack_needs_mobile_team <- calc_icepack_tot_quant(
#'     equipment_quantity = ifelse(mobile_team_with_ice,
#'                                 vaxCarr_required_mobile_team,
#'                                 0
#'     ),
#'     icepacks_per_equipment = vaxCarr_icepack_needs
#'   )
#'   
#'   
#'   # results of logistical needs estimation
#'   out <- data.frame(
#'     strategy = strategy_name,
#'     near_pop = site_details$near_pop,
#'     far_pop = site_details$far_pop,
#'     ft_vial_type = ifelse(fixed_team_with_dose10,
#'                           "dose10",
#'                           "monodose"
#'     ),
#'     ft_doses_required = n_doses_fixed_team,
#'     mt_vial_type = ifelse(mobile_team_with_dose10,
#'                           "dose10",
#'                           "monodose"
#'     ),
#'     mt_doses_required = n_doses_mobile_team,
#'     ft_RCW25 = RCW25_required_fixed_team,
#'     mt_RCW25 = RCW25_required_mobile_team,
#'     ft_vaxCarr = vaxCarr_required_fixed_team,
#'     mt_vaxCarr = vaxCarr_required_mobile_team,
#'     ft_icepacks_large = RCW25_icepack_needs_fixed_team,
#'     mt_icepacks_large = RCW25_icepack_needs_mobile_team,
#'     ft_icepacks_small = vaxCarr_icepack_needs_fixed_team,
#'     mt_icepacks_small = vaxCarr_icepack_needs_mobile_team
#'   )
#'   
#'   return(out)
#' }
