
# compute_rcw25_icepacks() & compute_varCarr_icepacks(): ----
#' Calculate ice pack needs for each vax carrier and RCW25s, based on ambient
#' temperature and number of days to replace ice, which is daily for the 
#' vaccine carrier ice packs and 1, 2, or 3 days for the RCW25s


source('./scripts/parameters.R')

compute_rcw25_icepacks <- function(amb_temp, replacement_days) { # replacement_days = c(1, 2, 3)
  if (amb_temp == "below 40" & (replacement_days == 1 | replacement_days == 2)) {
    12
  } else if (amb_temp == "below 40" & replacement_days == 3) {
    14
  } else if (amb_temp == "above 40" & replacement_days == 1) {
    12
  } else if (amb_temp == "above 40" & replacement_days == 2) {
    18
  } else if (amb_temp == "above 40" & replacement_days == 3) {
    24
  } else {
    stop("wrong inputs provided")
  }
}


compute_vaxCarr_icepacks <- function(amb_temp, ...) {
  replacement_days <- 1
  if (amb_temp == "below 40" & replacement_days == 1) {
    6
  } else if (amb_temp == "above 40" & replacement_days == 1) {
    8
  } else {
    stop("wrong inputs provided")
  }
}

#function to convert specified near and far populations into a data frame

make_site_data <- function(near_pop_size, far_pop_size){
   df <- data.frame(near_pop = near_pop_size, far_pop = far_pop_size)
   df <- df %>% mutate(location_id = 1:nrow(df))
   return(df)
}



# extract_near_pop() & extract_far_pop(): ----
#' Extract population sizes for near and far
#' kids for further calculations

extract_near_pop <- function(df,
                             site_rows_selected) {
  df %>%
    dplyr::slice(site_rows_selected) %>% # for now, we are only going to concentrate on one site. User indicates which site to analyse
    .$near_pop
}


extract_far_pop <- function(df,
                            site_rows_selected) {
  df %>%
    dplyr::slice(site_rows_selected) %>% # for now, we are only going to concentrate on one site. User indicates which site to analyse
    .$far_pop
}



# extract_site_team_size(): ----
#' #Extract size of allocated teams for a site from the sites table


extract_site_team_size <- function(df,
                                   site_rows_selected) { # for now, we are only going to concentrate on one site. User indicates which site to analyse
  teams <- df %>%
    dplyr::slice(site_rows_selected) %>%
    .$site_team_alloc # number of teams allocated to site
  return(teams)
}




# calc_effective_doses(): ----
#' adjusts for wastage and determines the actual number of doses we may be transporting

calc_effective_doses <- function(dose_quantity,
                                 ovwastage) {
  eff_doses <- dose_quantity * (1 - ovwastage / 100)
  return(eff_doses)
}




# team days calculations ----
#' Functions to calculate the team days for strategies using either monodose or
#' 10-dose vials

calc_team_days <- function(target_pop, 
                           ovwastage, 
                           carrier_vol_capacity, 
                           team_performance, # how many you are expected to vaccinate on average per day
                           browse = F
                            ) {
  if(browse) browser()
  effective_doses <- ceiling(carrier_vol_capacity * (1 - ovwastage / 100)) # effectively, how many vaccinations is a team actually undertaking?
  if ((target_pop <= team_performance) & (team_performance <= effective_doses)) {
    return(1) # we can carry more doses than the target population size and are within the maximum kids we are expected to vaccinate, so it'll take us 1 day
  }
  else if ((team_performance <= target_pop) & (team_performance <= effective_doses)) {
    return(round(target_pop / team_performance, 3)) # we can carry more than we are expected to vaccinate but the target population is more than how many kids we are expected to vaccinate, so the team performance becomes the constraint
  }
  else if ((effective_doses <= target_pop) & (effective_doses <= team_performance)) {
    return(round(target_pop / effective_doses, 3)) # we can't carry enough doses to even vaccinate the expected number of kids per day, so the volume capacity becomes a constrainst
  }
}



# calculate passive cold chain dose capacity ----

calc_dose_capacity <- function(vial_type,
                               vax_vol,
                               equip_type,
                               with_ice) # vial_type = monodose/dose10 and vax_vol depends on monodose_vialVol/dose10_vialVol #equip_type = c('rcw25','vaxCarr')
{
  if (vial_type == "dose10" & vax_vol == 2.1 & equip_type == "rcw25" & with_ice == T) {
   # 5000
   as.numeric(round(rcw25_netVol/dose_10_pvd["2.1"]))
  }
  else if (vial_type == "dose10" & vax_vol == 2.1 & equip_type == "rcw25" & with_ice == F) {
    as.numeric(round(rcw25_grossVol / dose_10_pvd["2.1"]))
  }
  else if (vial_type == "dose10" & vax_vol == 3 & equip_type == "rcw25" & with_ice == T) {
   # 3300
  as.numeric(round(rcw25_netVol/dose_10_pvd["3"]))
  } else if (vial_type == "dose10" & vax_vol == 3 & equip_type == "rcw25" & with_ice == F) {
    as.numeric(round(rcw25_grossVol / dose_10_pvd["3"]))
  }
  else if (vial_type == "dose10" & vax_vol == 2.1 & equip_type == "vaxCarr" & with_ice == T) {
   # 750
    as.numeric(round(vax_carrier_netVol / dose_10_pvd["2.1"]))
  }
  else if (vial_type == "dose10" & vax_vol == 2.1 & equip_type == "vaxCarr" & with_ice == F) {
    as.numeric(round(vax_carrier_grossVol / dose_10_pvd["2.1"]))
  }
  else if (vial_type == "dose10" & vax_vol == 3 & equip_type == "vaxCarr" & with_ice == T) {
    #500
    as.numeric(round(vax_carrier_netVol / dose_10_pvd["3"]))
  }
  else if (vial_type == "dose10" & vax_vol == 3 & equip_type == "vaxCarr" & with_ice == F) {
    as.numeric(round(vax_carrier_grossVol / dose_10_pvd["3"]))
  }
  else if (vial_type == "monodose" & vax_vol == 21.09 & equip_type == "rcw25" & with_ice == T) {
   # 616
    as.numeric(round(rcw25_netVol / monodose_pvd))
  }
  else if (vial_type == "monodose" & vax_vol == 21.09 & equip_type == "rcw25" & with_ice == F) {
   # 1301
    as.numeric(round(rcw25_grossVol / monodose_pvd))
  }
  else if (vial_type == "monodose" & vax_vol == 21.09 & equip_type == "vaxCarr" & with_ice == T) {
  #  77
    as.numeric(round(vax_carrier_netVol / monodose_pvd))
  }
  else if (vial_type == "monodose" & vax_vol == 21.09 & equip_type == "vaxCarr" & with_ice == F) {
   # 170
    as.numeric(round(vax_carrier_grossVol / monodose_pvd))
  }
  else {
    stop("Wrong input entered")
  }
}


## calc_transport_equipment_needs ----

calc_transport_equipment_needs <- function(equip_type,
                                           vial_type,
                                           vax_vol,
                                           with_ice,
                                           doses_to_transport) {
  # NB: numbers here are doses and are based on the packed volume per dose, i.e, how much volume is required to pack one dose.
  if (vial_type == "dose10" & vax_vol == 2.1 & equip_type == "rcw25" & with_ice == T) {
    ceiling(doses_to_transport / 5000)
  }
  else if (vial_type == "dose10" & vax_vol == 3 & equip_type == "rcw25" & with_ice == T) {
    ceiling(doses_to_transport / 3300)
  }
  else if (vial_type == "dose10" & vax_vol == 2.1 & equip_type == "vaxCarr" & with_ice == T) # I've not yet thrown in the numbers for 10 dose OCC because it's not operational and even being considered but might be good for a counterfactual.
    {
      ceiling(doses_to_transport / 750)
    }
  else if (vial_type == "dose10" & vax_vol == 3 & equip_type == "vaxCarr" & with_ice == T) {
    ceiling(doses_to_transport / 500)
  }
  else if (vial_type == "monodose" & vax_vol == 21.09 & equip_type == "rcw25" & with_ice == T) {
    ceiling(doses_to_transport / 616)
  }
  else if (vial_type == "monodose" & vax_vol == 21.09 & equip_type == "rcw25" & with_ice == F) {
    ceiling(doses_to_transport / 1301)
  }
  else if (vial_type == "monodose" & vax_vol == 21.09 & equip_type == "vaxCarr" & with_ice == T) {
    ceiling(doses_to_transport / 77)
  }
  else if (vial_type == "monodose" & vax_vol == 21.09 & equip_type == "vaxCarr" & with_ice == F) {
    ceiling(doses_to_transport / 170)
  }
  else {
    stop("Check inputs")
  }
}


# Full Cold Chain (FCC) calculations ----

#' It is worth noting that the number of doses for a target population is the same
#' for both 10-dose and monodose.
#' However, the number of vials or vaccines for the same number of doses changes,
#' i.e, 10 doses per vaccine vial for
#' 10-dose and 1 vaccine vial per monodose

# calc_doses_required(): ====
#' calculates number of doses needed based on the target population size; indicate
#' if you need doses for the near or far people; the type of vial you need calcalations
#' for; and the row numbers of the sites you want to analyse

calc_doses_required <- function(df,
                                # site_rows_selected = 1,
                                pop_type,
                                ovwastage,
                                buffer_size,
                                 is_dose10) { # df is the dataframe of added sites; site_row = row numbers to analyse; is.dose10, if false, means monodose; #pop_type = c(near, far), ovwastage = open vial wastage, buffer = safety stock to cushion uncertainty
  # selected_sites <- df %>%
  #   dplyr::slice(site_rows_selected)
  selected_sites <- df 
  if (pop_type == "near" & is_dose10 == T) {
    dose_quant <- selected_sites %>%
      dplyr::summarise(sum(.$near_pop))

    doses_required <- dose_quant * (1 + buffer_size / 100) * (1 + ovwastage / 100) # apply buffer and wastage cushion

    return(ceiling(as.numeric(doses_required)))
  } else if (pop_type == "near" & is_dose10 == F) {
    dose_quant <- selected_sites %>%
      dplyr::summarise(sum(.$near_pop))

    doses_required <- dose_quant * (1 + buffer_size / 100) * (1 + ovwastage / 100)

    return(as.numeric(doses_required))
  } else if (pop_type == "far" & is_dose10 == T) {
    dose_quant <- selected_sites %>%
      dplyr::summarise(sum(.$far_pop))

    doses_required <- dose_quant * (1 + buffer_size / 100) * (1 + ovwastage / 100)

    return(as.numeric(ceiling(doses_required)))
  } else if (pop_type == "far" & is_dose10 == F) {
    dose_quant <- selected_sites %>%
      dplyr::summarise(sum(.$far_pop))

    doses_required <- dose_quant * (1 + buffer_size / 100) * (1 + ovwastage / 100)

    return(as.numeric(doses_required))
  } else {
    stop("Error in dose calculation function; check your inputs")
  }
}



# Ice pack quantity needed ----

calc_icepack_tot_quant <- function(equipment_quantity # options: rcw25, vaxCarr
                                   , icepacks_per_equipment) {
  return(equipment_quantity * icepacks_per_equipment)
}


# Ice pack volume needed ----

calc_icepack_tot_vol <- function(equipment_type # options: large = 0.6L, small = 0.4L
                                 , icepack_quantity) {
  if (equipment_type == "rcw25") {
    return(0.6 * icepack_quantity)
  } else if (equipment_type == "vaxCarr") {
    return(0.4 * icepack_quantity)
  } else {
    stop("Check inputs")
  }
}


# calc_freezing_time() ----

calc_freezing_time <- function(mf314_available,
                               large_icepacks_quantity,
                               small_icepacks_quantity) {
  ceiling(
    (1 / mf314_available) * ((large_icepacks_quantity / mf314_largepack_fr) + 
                               (small_icepacks_quantity / mf314_smallpack_fr)
                             )
    )
}

# calc_teams_and_campaign_days() ----
#' This function takes in the team days and provides the possible number of teams
#' and campaign days it would take, assuming teams are either routed in parallel
#' or sequence. If in sequence, it'll take the same number of team days no matter
#' the number of teams
calc_teams_and_campaign_days <- function(team_days) {
  team_quantity <- seq(1, team_days, by = 1)
  campaign_days <- team_days / team_quantity
  out <- data.frame(teams = team_quantity, campaign_duration = campaign_days)
  return(out)
}



# calc_logistical_needs() ----
#' This function takes in the possible number of teams, the equipment type
#' the team will use, and how many per team. It returns the number of
#' the logisticals needs in terms of RCW25 and vaccine carriers using the rule
#' that 1 fixed team needs an RCW25 and a vaccine carrier and a mobile team either
#' can use 1 RCW25 or 1 vaccine carrier, hence, two scenarios.
#' NOTE: If we want to use the MSF policy where two teams on a site share one RCW25s
#' but one vaccine carrier per team, then the number of RCW25 per team input will
#' be 0.5 and vaxCarr will be 1.

calc_logistical_needs <- function(num_of_teams,
                                  team_equip_type,
                                  rcw25_per_team = NA,
                                  vaxCarr_per_team = NA) { #' team_equip_type = 'rcw25', 'vaxCarr', 'both'
  if (team_equip_type == "rcw25" & !is.na(rcw25_per_team)) {
    out <- data.frame(rcw25 = num_of_teams * rcw25_per_team)
    return(out)
  } else if (team_equip_type == "vaxCarr" & !is.na(vaxCarr_per_team)) {
    out <- data.frame(vaxCarr = num_of_teams * vaxCarr_per_team)
    return(out)
  } else if (
    (team_equip_type == "both" & vaxCarr_per_team) & (team_equip_type == "both" &
      !is.na(vaxCarr_per_team)
    )
  ) {
    out <- data.frame(rcw25 = num_of_teams * rcw25_per_team, 
                      vaxCarr = num_of_teams * vaxCarr_per_team
                      )
    return(out)
  } else {
    stop("check team_equip_type input")
  }
}


# calc_campaign_start(): ----
# Calculates the delay to the start of a campaign based on routing rules: sequential or parallel

calc_campaign_start <- function(ft_freeze_time,
                                mt_freeze_time,
                                team_routing # routing: "asap" or "parallel"
) {
  team_prep_delays <- c(fixed_team = ft_freeze_time, mobile_team = mt_freeze_time)
  if (team_routing == "asap") {
    asap_campaign_day <- min(team_prep_delays)
    faster_team <- ifelse(length(which(team_prep_delays == asap_campaign_day)) == 1, 
                          names(which(team_prep_delays == asap_campaign_day)), "both"
                          )
    return(list(
      start_day = asap_campaign_day,
      which_team_first = faster_team
    ))
  } else if (team_routing == "parallel") {
    parallel_campaign_day <- sum(ft_freeze_time, mt_freeze_time)
    return(list(
      start_day = parallel_campaign_day,
      which_team_first = "both"
    ))
  }
}



calc_next_campaign_start <- function(ft_campaign_dur,
                                mt_campaign_dur,
                                team_movement # routing: "asap" or "parallel"
) {
  team_campaign_durations <- c(fixed_team = ft_campaign_dur, 
                               mobile_team = mt_campaign_dur
                               )
  if (team_movement == "asap") {
    next_campaign_time <- min(ft_campaign_dur, mt_campaign_dur)
    faster_team <- ifelse(length(which(team_campaign_durations == next_campaign_time)) == 1, 
                          names(which(team_campaign_durations == next_campaign_time)), 
                          "both_asap"
                          )
    return(data.frame(
      time_to_next_loc = next_campaign_time,
      which_team_first = faster_team
    ))
  } else if (team_movement == "parallel") {
    next_campaign_time <- max(ft_campaign_dur, ft_campaign_dur)
    return(data.frame(
      time_to_next_loc = next_campaign_time,
      which_team_first = "both_parallel"
    ))
  }
}




# compound_delays(): ----
#' Calculates the compounded delays from a sequential campaign based on the 
#' strategy commencement delay and campaign durations from previous locations.
calc_compounded_delays <- function(delays, team_days){
  cpd_delays <- rep(NA, length(delays))
  cpd_delays[1] <- delays[1] 
  
  for (i in 2: length(delays)) {
    cpd_delays[i] <- delays[1] + sum(team_days[1:i-1]) 
  }
  return(cpd_delays) 
} 
