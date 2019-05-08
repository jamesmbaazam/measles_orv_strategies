library('ggplot2')
library('dplyr')
library('reshape2')
library('purrr')
library('gridExtra')
library('tidyr')

source('./ORV_coldchain_costs_app/params.R')
source('./ORV_coldchain_costs_app/calculator_functions.R')

#####################
#Far campaigns: monodose Outside of Cold Chain versus 10-dose in Full Cold Chain
#########################



#params
far_pop <- 500

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
wastage_dose10_mt_vect <- round(seq(0, 1, length.out = length(team_days_mobile_monodose_noIce)), 3) 

#dose10_FCC_far_trip_eff_doses_vect <- ceiling(dose10_FCC_far_trip_capacity * (1 - wastage_dose10_mt_vect)) #effectively, how many vaccinations is a mobile team actually undertaking?

team_days_mobile_dose10_Ice <-  purrr::map_dbl(.x = wastage_dose10_mt_vect,
                                                  .f = calc_dose10_team_days, 
                                                  target_pop = far_pop, 
                                                  vaxCarr_capacity = dose10_FCC_far_trip_capacity,
                                                  team_performance = tp_mobile)
                                        #there's a division by zero here, returning an infinity, so we'll replace it with NA to allow for better manipulation


team_days_mobile_dose10_Ice <- ifelse(!is.infinite(team_days_mobile_dose10_Ice), team_days_mobile_dose10_Ice, NA) #I do a correction here to avoid division by zero.

#data.frame(wastage = wastage_dose10_mt_vect, team_days = team_days_mobile_dose10_Ice)

#x-axis of plot: ratio of increasing vaccine carrier volume capacity for monodose vs fixed for 10-dose
ratio_monodose_dose10_vaxCarr_capacity <- round(monodose_OCC_far_trip_capacity_expanded / dose10_FCC_far_trip_capacity, 3)


team_days_output <- tibble(
    dose10_wastage = wastage_dose10_mt_vect,
    vaxCarr_dose10_capacity = dose10_FCC_far_trip_capacity,
    vaxCarr_monodose_capacity = monodose_OCC_far_trip_capacity_expanded,
    vaxCarr_capacity_ratio = ratio_monodose_dose10_vaxCarr_capacity,
    team_days_dose10_far_FCC = team_days_mobile_dose10_Ice,
    team_days_monodose_far_OCC = team_days_mobile_monodose_noIce
)

#Plots




#range of points for formating axis labels
wastage_vs_team_days_axis_lim <- range(as.numeric(c(team_days_output$team_days_dose10_far_FCC, team_days_output$team_days_monodose_far_OCC)), na.rm = T)

#formating monodose data for comparison
monodose_wastage_comparison_dat <- dplyr::filter(team_days_output, dose10_wastage == 0) %>% 
    select(dose10_wastage, team_days_monodose_far_OCC) %>% 
    bind_rows(data.frame(dose10_wastage = max(team_days_output$dose10_wastage, na.rm = T), 
                                                        team_days_monodose_far_OCC = .$team_days_monodose_far_OCC
                                                        )
                                             )

#plot of 10 dose mobile team days against increasing wastage
wastage_vs_team_days <- ggplot(data = team_days_output) + 
    geom_point(aes(x = dose10_wastage, y = team_days_dose10_far_FCC)) + 
    geom_line(aes(x = dose10_wastage, y = team_days_dose10_far_FCC))  + 
    geom_point(data = slice(monodose_wastage_comparison_dat, 1), 
              aes(x = dose10_wastage, y = team_days_monodose_far_OCC), 
              color = 'red', 
              size = 2
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
    labs(x = 'Open vial wastage' 
         , y = 'Mobile team days' 
         #, title = '10 dose for far campaigns in full cold chain (monodose value shown in red)'
         )



#plot of monodose mobile team days against increasing dose storage capacity
storage_vs_team_days_lim <- range(c(team_days_output$team_days_monodose_far_OCC, 
                                    team_days_output$team_days_dose10_far_FCC[1]), 
                                  na.rm = T
                                  )

#formating 10-dose data for comparison
dose10_storage_comparison_dat <- dplyr::filter(team_days_output, dose10_wastage == 0.155) %>% 
    select(vaxCarr_dose10_capacity, vaxCarr_capacity_ratio, team_days_dose10_far_FCC) %>% 
    bind_rows(data.frame(vaxCarr_dose10_capacity = min(team_days_output$vaxCarr_monodose_capacity, na.rm = T),
                         vaxCarr_capacity_ratio = min(team_days_output$vaxCarr_capacity_ratio, na.rm = T),
                                                        team_days_dose10_far_FCC = .$team_days_dose10_far_FCC
                                             ))


storage_vs_team_days <- ggplot(data = team_days_output) + 
    geom_point(aes(x = vaxCarr_capacity_ratio, y = team_days_monodose_far_OCC)) + 
    geom_line(aes(x = vaxCarr_capacity_ratio, y = team_days_monodose_far_OCC))  + 
    geom_point(data = slice(dose10_storage_comparison_dat, 1), 
               aes(x = vaxCarr_capacity_ratio, y = team_days_dose10_far_FCC), 
               color = 'red', 
               size = 2
    ) + 
    scale_x_continuous(breaks = round(seq(range(team_days_output$vaxCarr_capacity_ratio)[1], 
            range(team_days_output$vaxCarr_capacity_ratio)[2], 
            length.out = 8), 2),
            labels = round(seq(range(team_days_output$vaxCarr_capacity_ratio)[1], 
                         range(team_days_output$vaxCarr_capacity_ratio)[2], 
                         length.out = 8), 2)
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
    labs(x = 'Dose storage capacity ratio (monodose vs 10-dose)'
         , y = 'Mobile team days'
         #, title = 'Monodose for far campaigns out of cold chain (10 dose value shown in red)'
         )

grid.arrange(wastage_vs_team_days, storage_vs_team_days, ncol = 1)



#Reshaping the results and assigning zero wastage to the monodose strategy

team_days_output_melted <- team_days_output %>% 
    select(-vaxCarr_dose10_capacity, -vaxCarr_monodose_capacity) %>% 
    gather(key = 'strategy'
           , value = 'team_days'
           , c('team_days_dose10_far_FCC', 'team_days_monodose_far_OCC')
           , factor_key = T
           )

team_days_output_melted_mod <- dplyr::mutate(team_days_output_melted
                                             , dose10_wastage = if_else(strategy == 'team_days_dose10_far_FCC'
                                                                        , team_days_output_melted$dose10_wastage
                                                                        , 0
                                                                        )
                                             )
#I do some further reshaping of the results for 10-dose so I can plot the team days wrt constant wastage rates across the monodose plot
team_days_output_melted_dose10 <- dplyr::filter(team_days_output_melted_mod
                                                , strategy == 'team_days_dose10_far_FCC') %>% 
    dplyr::mutate(vaxCarr_capacity_ratio = paste(vaxCarr_capacity_ratio, collapse = ',')) %>% 
    separate_rows(vaxCarr_capacity_ratio, convert = T)




#' plot to illustrate how the 10-dose and monodose team days intersect at some 
#' points and how that leads to an isocline for decision-making
#' 

team_days_intersection_plot <- ggplot(data = team_days_output) + 
    geom_point(aes(x = vaxCarr_capacity_ratio, 
                   y = team_days_monodose_far_OCC), 
               color = 'black'
               ) +
    geom_line(aes(x = vaxCarr_capacity_ratio, 
                   y = team_days_monodose_far_OCC), 
               color = 'black'
    ) +
    geom_point(data = team_days_output_melted_dose10 %>% 
                   dplyr::filter(between(team_days, min(team_days_output$team_days_monodose_far_OCC) + 0.4, max(team_days_output$team_days_monodose_far_OCC))
                                 ), 
               aes(x = vaxCarr_capacity_ratio, 
                   y = team_days, 
                   color = factor(dose10_wastage)
                   )
               ) +
    scale_x_continuous(breaks = seq(0.2, 1, 0.05),
                       labels  = every_nth(seq(0.2, 1, 0.05), 2, inverse = T)
                       ) +
    labs(x = 'Vaccine carrier capacity ratio (monodose vs 10-dose)',
         y = 'Mobile team days',
         color = 'Wastage (10-dose)') 

team_days_intersection_plot

if(save_plots){
 ggsave(filename = 'team_days_intersection_plot.png'
        #, plot = team_days_intersection_plot + presentation_plot_theme #uncomment this line to save a powerpoint version
        , team_days_intersection_plot
        , width = 9
        , height = 5)
}


#####################################
#Isocline plot
#####################################

#'The isocline represents the point on the intersection between the monodose and 10-dose 
#'strategies where there is no difference in team days. The areas above and below the line
#'correspond to a switch in decision.

isocline_df <- team_days_output %>% mutate(wastage_isocline = 1 - (far_pop/(vaxCarr_dose10_capacity*team_days_monodose_far_OCC)))

#View(isocline_df)

isocline_plot <- ggplot(isocline_df %>% filter(wastage_isocline > 0.6666667)) +
    geom_point(aes(x = vaxCarr_capacity_ratio,
                   y = wastage_isocline)
               )+
    geom_line(aes(x = vaxCarr_capacity_ratio,
                   y = wastage_isocline)
    ) +
    labs(x = 'Vaccine carrier capacity ratio (monodose vs 10-dose)',
         y = 'Open vial wastage (10-dose)')

plot(isocline_plot)

if(save_plots){
    ggsave(filename = 'mobile_team_days_isocline.png'
          # , plot = isocline_plot + presentation_plot_theme #uncomment this line to save a powerpoint version
          , plot = isocline_plot
           , width = 9
           , height = 5)
}