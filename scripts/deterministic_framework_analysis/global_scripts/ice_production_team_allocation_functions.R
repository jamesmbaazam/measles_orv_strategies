# packages
library(dplyr)
library(tibble)
library(gridExtra)
library(ggthemes)
library(ggplot2)

# scripts
source("scripts/deterministic_framework_analysis/global_scripts/parameters.R")
source('scripts/deterministic_framework_analysis/global_scripts/analyses_parameters.R')
source("scripts/deterministic_framework_analysis/global_scripts/supply_chain_functions.R")



# calc_daily_icepack_needs() ----

#' Title
#'
#' @param campaign_duration 
#' @param fixed_teams_per_vax_unit #' How many fixed post will be set up at a vaccination site. This is usually 1 or 2 and not more.
#' @param mobile_teams_per_site 
#' @param ambient_temperature 
#' @param rcw25_ice_replacement_days 
#' @param mobile_team_equip_type 
#' @param num_of_freezers 
#' @param small_icepacks_fr 
#' @param large_icepacks_fr 
#'
#' @return
#' @export
#'
#' @examples
calc_daily_icepack_needs <- function(campaign_duration,
                                     n_fixed_teams,
                                     n_mobile_teams,
                                     fixed_teams_per_vax_unit, # c(1, 2)
                                     ambient_temperature,
                                     rcw25_ice_replacement_days,
                                     mobile_team_equip_type = 'vaxCarr', #options = c('vaxCarr', 'rcw25', 'both')
                                     num_of_freezers,
                                     small_icepacks_fr, #MF314 freezing rate for small icepacks
                                     large_icepacks_fr #MF314 freezing rate for large icepacks
                                     ) {
  if (fixed_teams_per_vax_unit > 2 | fixed_teams_per_vax_unit < 0) {
    stop("Fixed teams per site cannot be greater than 2 or negative")
  }

    per_ft_vaxCarr <- 1
    per_ft_rcw25 <- ifelse(fixed_teams_per_vax_unit == 0, 0, 1)
  
    per_mt_rcw25 <- if (mobile_team_equip_type == "rcw25") {
      1
    } else if (mobile_team_equip_type == "both") {
      1
    } else if (mobile_team_equip_type == "vaxCarr") {
      0
    } else {
      stop("unknown mobile team equipment type")
    }      
    
  rcw25_icepacks <- compute_rcw25_icepacks(amb_temp = ambient_temperature, 
                                           replacement_days = rcw25_ice_replacement_days
                                           )
  
  vaxCarr_icepacks <- compute_vaxCarr_icepacks(amb_temp = ambient_temperature)
  
  
  
  rcw25_needed <- sum(n_fixed_teams*per_ft_rcw25, n_mobile_teams*per_mt_rcw25)
  
  vaxCarr_needed <- sum(n_fixed_teams*per_ft_vaxCarr, n_mobile_teams*per_mt_vaxCarr)

  
  
  per_replacement_large_packs <- ifelse(fixed_teams_per_vax_unit == 0, 
                                        0, 
                                        rcw25_icepacks
                                        ) * fixed_teams_per_vax_unit * fixed_teams
  
  per_replacement_small_packs <- vaxCarr_icepacks * mobile_teams_per_site

  daily_small_packs_need <- rep(vaxCarr_icepacks, times = campaign_duration)

  large_packs_replacement_days <- seq(1, campaign_duration, rcw25_ice_replacement_days)
  daily_large_packs_needed <- rep(0, campaign_duration)
  daily_large_packs_needed[large_packs_replacement_days] <- rcw25_icepacks

  #final processing
  
  #vaccine carrier icepacks
  small_icepacks_ft <- daily_small_packs_need * per_ft_vaxCarr * fixed_teams_per_vax_unit
  small_icepacks_mt <- daily_small_packs_need * per_mt_vaxCarr * mobile_teams_per_site
  
  #rcw25 icepacks
  large_icepacks_ft <- daily_large_packs_needed * per_ft_rcw25
  large_icepacks_mt <- daily_large_packs_needed * per_mt_rcw25

  
  # output
  icepack_needs <- tibble(
    campaign_day = 1:campaign_duration,
    small_icepacks_mt_total = cumsum(small_icepacks_mt),
    small_icepacks_ft_total = cumsum(small_icepacks_ft),
    small_icepacks_overall_total = rowSums(cbind(small_icepacks_ft = small_icepacks_ft, small_icepacks_mt = small_icepacks_mt)),
    days_to_freeze_small_packs = ceiling(small_icepacks_overall_total/small_icepacks_fr),
    large_icepacks_mt_total = cumsum(large_icepacks_mt),
    large_icepacks_ft_total = cumsum(large_icepacks_ft),
    large_icepacks_overall_total = rowSums(cbind(large_icepacks_ft = large_icepacks_ft, large_icepacks_mt = large_icepacks_mt)),
    days_to_freeze_large_packs =  ceiling(large_icepacks_overall_total/large_icepacks_fr),
    icepacks_daily_total = rowSums(cbind(small_icepacks_overall_total = small_icepacks_overall_total, large_icepacks_overall_total = large_icepacks_overall_total)),
    icepacks_overall_total = cumsum(icepacks_daily_total),
    days_to_freeze_all_ice = calc_freezing_time(mf314_available = num_of_freezers, large_icepacks_quantity = large_icepacks_overall_total, small_icepacks_quantity = small_icepacks_overall_total)
  )

  return(icepack_needs)
}

# calc_team_equipment_needs() ----

calc_team_equipment_needs <- function(campaign_duration,
                                     fixed_teams_per_vax_unit, # c(1, 2)
                                     mobile_teams_per_site,
                                     ambient_temperature,
                                     rcw25_ice_replacement_days,
                                     mobile_team_equip_type = 'vaxCarr', #options = c('vaxCarr', 'rcw25', 'both')
                                     num_of_freezers,
                                     small_icepacks_fr, #MF314 freezing rate for small icepacks
                                     large_icepacks_fr #MF314 freezing rate for large icepacks
) {
    if (fixed_teams_per_vax_unit > 2 | fixed_teams_per_vax_unit < 0) {
        stop("Fixed teams per site cannot be greater than 2 or negative")
    }
    
    per_ft_vaxCarr <- 1
    per_ft_rcw25 <- ifelse(fixed_teams_per_vax_unit == 0, 0, 1)
    per_mt_vaxCarr <- ifelse(mobile_teams_per_site == 0, 0, 1)
    per_mt_rcw25 <- if (mobile_team_equip_type == "rcw25") {
        1
    } else if (mobile_team_equip_type == "both") {
        1
    } else if (mobile_team_equip_type == "vaxCarr") {
        0
    } else {
        stop("unknown mobile team equipment type")
    }      
    
    
    # output
    
    
    team_equipment <- tibble(fixed_teams_per_vax_unit = fixed_teams_per_vax_unit,
                             mobile_teams_on_site = mobile_teams_per_site,
                             rcw25_required_ft = per_ft_rcw25,
                             rcw25_required_mt = per_mt_rcw25,
                             vaxCarr_required_ft = per_ft_vaxCarr * fixed_teams_per_vax_unit,
                             vaxCarr_required_mt = per_mt_vaxCarr * mobile_teams_per_site
    )
    
    
    return(team_equipment)
}

# calc_icepack_production() ----
calc_icepack_production <- function(campaign_duration,
                                    fixed_teams_per_vax_unit, # c(1, 2)
                                    mobile_teams_per_site,
                                    ambient_temperature,
                                    rcw25_ice_replacement_days,
                                    mobile_team_equip_type = 'vaxCarr', #options = c('vaxCarr', 'rcw25', 'both')
                                    num_of_freezers,
                                    small_icepacks_fr, #MF314 freezing rate for small icepacks
                                    large_icepacks_fr, #MF314 freezing rate for large icepacks
                                    icepack_fr_large,
                                    icepack_fr_small,
                                    num_of_teams
                                    ) {
    if (fixed_teams_per_vax_unit > 2 | fixed_teams_per_vax_unit < 0) {
        stop("Fixed teams per site cannot be greater than 2 or negative")
    }
    
    per_ft_vaxCarr <- 1
    per_ft_rcw25 <- ifelse(fixed_teams_per_vax_unit == 0, 0, 1)
    per_mt_vaxCarr <- ifelse(mobile_teams_per_site == 0, 0, 1)
    per_mt_rcw25 <- if (mobile_team_equip_type == "rcw25") {
        1
    } else if (mobile_team_equip_type == "both") {
        1
    } else if (mobile_team_equip_type == "vaxCarr") {
        0
    } else {
        stop("unknown mobile team equipment type")
    }      
    
    rcw25_icepacks <- compute_rcw25_icepacks(amb_temp = ambient_temperature, replacement_days = rcw25_ice_replacement_days)
    vaxCarr_icepacks <- compute_vaxCarr_icepacks(amb_temp = ambient_temperature)
    
    # output
    
    ice_production_capacity <- tibble(campaign_day = 1:campaign_duration,
                                      small_icepacks = 0.5 * num_of_freezers * campaign_day * small_icepacks_fr,
                                      vaxCarr_ready = floor(small_icepacks/vaxCarr_icepacks),
                                      large_icepacks = 0.5 * num_of_freezers * campaign_day * large_icepacks_fr,
                                      rcw25_ready = floor(large_icepacks/rcw25_icepacks),
                                      ft_possible = fixed_teams_per_vax_unit * rcw25_ready, #I use the number of rcw25's as a proxy to determine the number of fixed teams since they require rcw25s and vax carriers
                                      mt_possible = (vaxCarr_ready * fixed_teams_per_vax_unit) - rcw25_ready, #I assume the remaining vaccine carriers will be used by the mobile teams
                                    #  ice_surplus = 0.5*(mf314_largepack_capacity + mf314_smallpack_capacity) - cumsum(large_icepacks + small_icepacks)
    )
    
    #ice demand and supply
    return(ice_production_capacity)
}



#trial runs
msf_appendix23_reproduced <- calc_daily_icepack_needs(
  campaign_duration = 10,
  fixed_teams_per_vax_unit = 2,
  mobile_teams_per_site = ,
  ambient_temperature = "above 40",
  rcw25_ice_replacement_days = 3,
  num_of_freezers = sc_model_params$mf314_quant,
  small_icepacks_fr = mf314_smallpack_fr,
  large_icepacks_fr = mf314_largepack_fr
)

glimpse(msf_appendix23_reproduced)

ice_and_team_allocation <- calc_icepack_production(
    campaign_duration = 10,
    fixed_teams_per_vax_unit = 2,
    mobile_teams_per_site = 0,
    ambient_temperature = "below 40",
    rcw25_ice_replacement_days = 2,
    num_of_freezers = sc_model_params$mf314_quant,
    small_icepacks_fr = mf314_smallpack_fr,
    large_icepacks_fr = mf314_largepack_fr
)

ice_and_team_allocation

# calc_team_equipment_needs(
#     campaign_duration = 4,
#     fixed_teams_per_vax_unit = 2,
#     mobile_teams_per_site = 0,
#     ambient_temperature = "above 40",
#     rcw25_ice_replacement_days = 3,
#     num_of_freezers = sc_model_params$mf314_quant,
#     small_icepacks_fr = mf314_smallpack_fr,
#     large_icepacks_fr = mf314_largepack_fr
# )

# team_vax_carrier_needs <- tibble(
#     small_packs_frozen = 0.5 * mf314_smallpack_fr * (1:floor(mf314_smallpack_capacity / mf314_smallpack_fr)),
#     vax_carriers_below40 = floor(small_packs_frozen / compute_vaxCarr_icepacks("below 40")),
#     teams_for_below40 = vax_carriers_below40,
#     vax_carriers_above40 = floor(small_packs_frozen / compute_vaxCarr_icepacks("above 40")),
#     teams_for_above40 = vax_carriers_above40,
#     days_to_freeze = seq_along(small_packs_frozen)
# )
# 
# 
# team_vaxCarr_needs_plot <- ggplot(data = team_vax_carrier_needs, aes(x = paste0(days_to_freeze, " (", small_packs_frozen, ")"), y = teams_for_below40)) +
#     geom_bar(stat = "identity", aes(fill = "forestgreen")) +
#     geom_bar(data = team_vax_carrier_needs, aes(x = days_to_freeze, y = teams_for_above40, fill = "tomato3"), stat = "identity") +
#     scale_y_continuous(
#         breaks = seq(0, max(team_vax_carrier_needs$teams_for_below40) + 10, 5),
#         labels = seq(0, max(team_vax_carrier_needs$teams_for_below40) + 10, 5)
#     ) +
#     scale_fill_manual(name = "Ambient Temperature", values = c("forestgreen", "tomato3"), labels = c("< 40", "> 40")) +
#     labs(
#         title = "Quantity of ready vax carriers as a function of freezing rate (MF314 freezer)",
#         x = "Time (Days) and 0.4L ice packs frozen",
#         y = "Vax carriers"
#     ) +
#     theme(legend.position = "top")
# 
# plot(team_vaxCarr_needs_plot)
# 
# 
# team_rcw25_needs <- tibble(
#     large_packs_frozen = 0.5 * mf314_largepack_fr * (1:floor(mf314_largepack_capacity / mf314_largepack_fr)), # the 0.5 means that the other half space will be occupied by the other ice pack type
#     rcw25_below40 = floor(large_packs_frozen / compute_rcw25_icepacks("below 40")),
#     teams_for_below40 = rcw25_below40,
#     rcw25_above40 = floor(large_packs_frozen / compute_rcw25_icepacks("above 40")),
#     teams_for_above40 = rcw25_above40,
#     days_to_freeze = seq_along(large_packs_frozen)
# )
# 
# team_rcw25_needs_plot <- ggplot(data = team_rcw25_needs, aes(x = paste0(days_to_freeze, " (", large_packs_frozen, ")"), y = teams_for_below40)) +
#     geom_bar(stat = "identity", aes(fill = "forestgreen")) +
#     geom_bar(data = team_rcw25_needs, aes(x = days_to_freeze, y = teams_for_above40, fill = "tomato3"), stat = "identity") +
#     scale_y_continuous(
#         breaks = seq(0, max(team_rcw25_needs$teams_for_below40) + 15, 2),
#         labels = seq(0, max(team_rcw25_needs$teams_for_below40) + 15, 2)
#     ) +
#     scale_fill_manual(name = "Ambient Temperature", values = c("forestgreen", "tomato3"), labels = c("< 40", "> 40")) +
#     labs(
#         title = "Quantity of ready RCW25 cold boxes as a function of freezing rate (MF314 freezer)",
#         x = "Time (Days) and 0.6L ice packs frozen",
#         y = "RCW25 cold boxes"
#     ) +
#     theme(legend.position = "none")
# 
# plot(team_rcw25_needs_plot)
# 
# 
# all_equipment <- grid.arrange(team_vaxCarr_needs_plot, team_rcw25_needs_plot, nrow = 2)
# 
# ggsave(filename = "figures/all_equipment.png", plot = all_equipment)
# 
# 


#complex function
# calc_daily_icepack_needs <- function(campaign_duration,
#                                      fixed_teams_per_vax_unit, # c(1, 2)
#                                      mobile_teams_per_site,
#                                      ambient_temperature,
#                                      rcw25_ice_replacement_days,
#                                      mobile_team_equip_type = 'vaxCarr', #options = c('vaxCarr', 'rcw25', 'both')
#                                      num_of_freezers,
#                                      small_icepacks_fr, #MF314 freezing rate for small icepacks
#                                      large_icepacks_fr #MF314 freezing rate for large icepacks
# ) {
#     if (fixed_teams_per_vax_unit > 2 | fixed_teams_per_vax_unit < 0) {
#         stop("Fixed teams per site cannot be greater than 2 or negative")
#     }
#     
#     per_ft_vaxCarr <- 1
#     per_ft_rcw25 <- ifelse(fixed_teams_per_vax_unit == 0, 0, 1)
#     per_mt_vaxCarr <- ifelse(mobile_teams_per_site == 0, 0, 1)
#     per_mt_rcw25 <- if (mobile_team_equip_type == "rcw25") {
#         1
#     } else if (mobile_team_equip_type == "both") {
#         1
#     } else if (mobile_team_equip_type == "vaxCarr") {
#         0
#     } else {
#         stop("unknown mobile team equipment type")
#     }      
#     
#     rcw25_icepacks <- compute_rcw25_icepacks(amb_temp = ambient_temperature, replacement_days = rcw25_ice_replacement_days)
#     vaxCarr_icepacks <- compute_vaxCarr_icepacks(amb_temp = ambient_temperature)
#     per_replacement_large_packs <- ifelse(fixed_teams_per_vax_unit == 0, 0, rcw25_icepacks)
#     per_replacement_small_packs <- vaxCarr_icepacks * mobile_teams_per_site
#     
#     daily_small_packs_need <- rep(vaxCarr_icepacks, times = campaign_duration)
#     
#     large_packs_replacement_days <- seq(1, campaign_duration, rcw25_ice_replacement_days)
#     daily_large_packs_needed <- rep(0, campaign_duration)
#     daily_large_packs_needed[large_packs_replacement_days] <- rcw25_icepacks
#     
#     # output
#     icepack_needs <- tibble(
#         campaign_day = 1:campaign_duration,
#         small_icepacks_ft = daily_small_packs_need * per_ft_vaxCarr * fixed_teams_per_vax_unit,
#         small_icepacks_mt = daily_small_packs_need * per_mt_vaxCarr * mobile_teams_per_site,
#         small_icepacks_mt_total = cumsum(small_icepacks_mt),
#         small_icepacks_ft_total = cumsum(small_icepacks_ft),
#         small_icepacks_overall_total = rowSums(cbind(small_icepacks_ft = small_icepacks_ft, small_icepacks_mt = small_icepacks_mt)),
#         days_to_freeze_small_packs = ceiling(small_icepacks_overall_total/small_icepacks_fr),
#         large_icepacks_ft = daily_large_packs_needed * per_ft_rcw25,
#         large_icepacks_ft_total = cumsum(large_icepacks_ft),
#         large_icepacks_mt = daily_large_packs_needed * per_mt_rcw25,
#         large_icepacks_mt_total = cumsum(large_icepacks_mt),
#         large_icepacks_overall_total = rowSums(cbind(large_icepacks_ft = large_icepacks_ft, large_icepacks_mt = large_icepacks_mt)),
#         days_to_freeze_large_packs =  ceiling(large_icepacks_overall_total/large_icepacks_fr),
#         icepacks_total = rowSums(cbind(small_icepacks_overall_total = small_icepacks_overall_total, large_icepacks_overall_total = large_icepacks_overall_total)),
#         days_to_freeze_all_ice = calc_freezing_time(mf314_available = num_of_freezers, large_icepacks_quantity = large_icepacks_overall_total, small_icepacks_quantity = small_icepacks_overall_total)
#     )
#     
#     team_equipment <- tibble(fixed_teams_per_vax_unit = fixed_teams_per_vax_unit,
#                              mobile_teams_on_site = mobile_teams_per_site,
#                              rcw25_required_ft = per_ft_rcw25,
#                              rcw25_required_mt = per_mt_rcw25,
#                              vaxCarr_required_ft = per_ft_vaxCarr * fixed_teams_per_vax_unit,
#                              vaxCarr_required_mt = per_mt_vaxCarr * mobile_teams_per_site
#     )
#     
#     ice_production_capacity <- tibble(campaign_day = 1:campaign_duration,
#                                       small_icepacks = 0.5 * num_of_freezers * campaign_day * small_icepacks_fr,
#                                       vaxCarr_ready = floor(small_icepacks/vaxCarr_icepacks),
#                                       large_icepacks = 0.5 * num_of_freezers * campaign_day * large_icepacks_fr,
#                                       rcw25_ready = floor(large_icepacks/rcw25_icepacks),
#                                       ft_possible = rcw25_ready, #I use the number of rcw25's as a proxy to determine the number of fixed teams since they require rcw25s and vax carriers
#                                       mt_possible = vaxCarr_ready - rcw25_ready #I assume the remaining vaccine carriers will be used by the mobile teams
#     )
#     
#     return(list(icepack_needs_table = select(icepack_needs, -c('small_icepacks_ft', 'small_icepacks_mt', 'large_icepacks_ft', 'large_icepacks_mt')),
#                 team_equipment_table = team_equipment,
#                 ice_production_rate = ice_production_capacity
#     )
#     )
# }