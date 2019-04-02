library('ggplot2')
library('dplyr')
library('reshape2')
library('purrr')

source('./ORV_coldchain_costs_app/params.R')
source('./ORV_coldchain_costs_app/calculator_functions.R')

#params
far_pop <- 250

###############################

#how many doses of the 10-dose vials can we transport in a vaccine carrier?
monodose_OCC_far_trip_capacity <- calc_dose_capacity(vial_type = 'monodose' 
                                                     , vax_vol = 21.09
                                                     , equip_type = 'vaxCarr' #we assume a mobile team uses one vaccine carrier
                                                     , with_ice = F)
#how many doses of the 10-dose vials can we transport in a vaccine carrier?
dose10_FCC_far_trip_capacity <- calc_dose_capacity(vial_type = 'dose10' 
                                                   , vax_vol = dose10_vial_vol[1]
                                                   , equip_type = 'vaxCarr' #we assume a mobile team uses one vaccine carrier
                                                   , with_ice = T)


monodose_OCC_far_trip_capacity_expanded <- seq(monodose_OCC_far_trip_capacity, dose10_FCC_far_trip_capacity, 5) #just a vector of possible increasing volume capacities to consider. The idea is to increase it to as high as the capacity for 10-dose carriage per trip.


#mobile team days using monodose OCC

team_days_mobile_monodose_noIce <- unlist(purrr::map(monodose_OCC_far_trip_capacity_expanded, calc_monodose_team_days, target_pop = far_pop))

###############################
#10 dose FCC for far campaigns
###############################

#Considering wastage ranging from 0% to 100%, with 0% meaning no wastage and 100%, wastage of whole vial. MSF considers an average of 15% wastage rate in field operations. 
wastage_dose10_mt_vect <- round(seq(0, 1, length.out = length(team_days_mobile_monodose_noIce)), 2) 


dose10_FCC_far_trip_eff_doses_vect <- ceiling(dose10_FCC_far_trip_capacity * (1 - wastage_dose10_mt_vect)) #effectively, how many vaccinations is a mobile team actually undertaking?

dose10_FCC_far_trip_eff_doses_vect <- ifelse(dose10_FCC_far_trip_eff_doses_vect > 0, dose10_FCC_far_trip_eff_doses_vect, NA) #I do a correction here to avoid division by zero.

team_days_mobile_dose10_Ice <- round(far_pop / dose10_FCC_far_trip_eff_doses_vect, 2)  #there's a division by zero here, returning an infinity, so we'll replace it with NA to allow for better manipulation


#x-axis of plot: ratio of increasing vaccine carrier volume capacity for monodose vs fixed for 10-dose
ratio_monodose_dose10_vaxCarr_capacity <- round(monodose_OCC_far_trip_capacity_expanded / dose10_FCC_far_trip_capacity, 2)


team_days_output <- tibble(
    dose10_wastage = wastage_dose10_mt_vect,
    vaxCarr_dose10_capacity = dose10_FCC_far_trip_capacity,
    vaxCarr_monodose_capacity = monodose_OCC_far_trip_capacity_expanded,
    vaxCarr_capacity_ratio = ratio_monodose_dose10_vaxCarr_capacity,
    team_days_dose10_far_FCC = team_days_mobile_dose10_Ice,
    team_days_monodose_far_OCC = team_days_mobile_monodose_noIce
)

#Plots

team_days_output_melted <- team_days_output %>% melt(id.vars = c('dose10_wastage', 'vaxCarr_capacity_ratio'), variable.name = 'strategy', value.name = 'team_days_mt')

#assign zero wastage to the monodose strategy
team_days_output_melted_mod <- dplyr::mutate(team_days_output_melted, dose10_wastage = if_else(strategy == 'team_days_dose10_far_FCC', team_days_output_melted$dose10_wastage, 0))

team_days_axis_lim <- range(team_days_output_melted_mod$team_days_mt, na.rm = T)

ggplot(data = team_days_output_melted_mod) + 
 #   geom_point(aes(x = vaxCarr_capacity_ratio, y = team_days_mt, shape = strategy, size = dose10_wastage )) + 
    geom_line(aes(x = dose10_wastage , y = team_days_mt, color = strategy), size = 2)  + 
    geom_point(data = dplyr::filter(team_days_output_melted_mod, dose10_wastage ==0.15), aes(x = vaxCarr_capacity_ratio, y = team_days_mt), color = 'yellow') + 
    scale_y_continuous(breaks = seq(team_days_axis_lim[1], team_days_axis_lim[2], 2), labels = seq(team_days_axis_lim[1], team_days_axis_lim[2], 2)) + 
    scale_shape(name = 'Strategy', labels = c('10-dose FCC', 'Monodose OCC')) +
    scale_size(name = '10 dose open vial wastage') + 
    labs(x = 'Ratio of increasing monodose volume capacity to fixed 10-dose capacity', y = 'Mobile team days')
