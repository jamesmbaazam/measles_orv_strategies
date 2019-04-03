library('ggplot2')
library('dplyr')
library('reshape2')
library('purrr')
library('gridExtra')
library('tidyr')

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

#team_days_output_melted <- team_days_output %>% melt(id.vars = c('dose10_wastage', 'vaxCarr_capacity_ratio'), variable.name = 'strategy', value.name = 'team_days_mt')

#assign zero wastage to the monodose strategy
#team_days_output_melted_mod <- dplyr::mutate(team_days_output_melted, dose10_wastage = if_else(strategy == 'team_days_dose10_far_FCC', team_days_output_melted$dose10_wastage, 0))

wastage_vs_team_days_axis_lim <- range(as.numeric(c(team_days_output$team_days_dose10_far_FCC, team_days_output$team_days_monodose_far_OCC)), na.rm = T)
#monodose_lineplot_df <- purrr::map_df(length(team_days_output), bind_rows(), dplyr::filter(team_days_output, dose10_wastage == 0), dplyr::filter(team_days_output, dose10_wastage == 0)) 


#plot of 10 dose mobile team days against increasing wastage
wastage_vs_team_days <- ggplot(data = team_days_output) + 
    geom_point(aes(x = dose10_wastage, y = team_days_dose10_far_FCC)) + 
    geom_line(aes(x = dose10_wastage, y = team_days_dose10_far_FCC))  + 
    geom_point(data = dplyr::filter(team_days_output, dose10_wastage == 0), 
              aes(x = dose10_wastage, y = team_days_monodose_far_OCC), 
              color = 'red', 
              size = 4
              ) + 
    scale_y_continuous(breaks = round(seq(wastage_vs_team_days_axis_lim[1], 
                                    wastage_vs_team_days_axis_lim[2], 
                                    length.out = 10), 2
                                    ), 
                       labels = round(seq(wastage_vs_team_days_axis_lim[1], 
                                    wastage_vs_team_days_axis_lim[2], 
                                    length.out = 10), 2
                                    )
                       ) + 
    labs(x = 'Open vial wastage', y = 'Mobile team days', title = '10 dose for far campaigns in full cold chain (monodose value shown in red)')



#plot of monodose mobile team days against increasing dose storage capacity
storage_vs_team_days_lim <- range(c(team_days_output$team_days_monodose_far_OCC, 
                                    team_days_output$team_days_dose10_far_FCC[1]), 
                                  na.rm = T
                                  )

storage_vs_team_days <- ggplot(data = team_days_output) + 
    geom_point(aes(x = vaxCarr_monodose_capacity, y = team_days_monodose_far_OCC)) + 
    geom_line(aes(x = vaxCarr_monodose_capacity, y = team_days_monodose_far_OCC))  + 
    geom_point(data = dplyr::filter(team_days_output, dose10_wastage == 0.15), 
               aes(x = vaxCarr_dose10_capacity, y = team_days_dose10_far_FCC), 
               color = 'red', 
               size = 4
    ) + 
    scale_x_continuous(breaks = seq(range(team_days_output$vaxCarr_monodose_capacity)[1], 
            range(team_days_output$vaxCarr_monodose_capacity)[2], 
            44),
            labels = seq(range(team_days_output$vaxCarr_monodose_capacity)[1], 
                         range(team_days_output$vaxCarr_monodose_capacity)[2], 
                         44)
                       ) + 
    scale_y_continuous(breaks = round(
        seq(storage_vs_team_days_lim[1], 
            storage_vs_team_days_lim[2], 
            length.out = 10
        ),
        2
    ), 
    labels = round(seq(storage_vs_team_days_lim[1], 
                       storage_vs_team_days_lim[2], 
                       length.out = 10
    ), 2
    )
    ) + 
    labs(x = 'Dose storage capacity', y = 'Mobile team days', title = 'Monodose for far campaigns out of cold chain (10 dose value shown in red)')


grid.arrange(wastage_vs_team_days, storage_vs_team_days, ncol = 1)
