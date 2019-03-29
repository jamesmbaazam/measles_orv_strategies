library('ggplot2')
library('dplyr')

source('./params.R', local = TRUE)
source('./calculator_functions.R', local = TRUE)

#params
far_pop <- 250

#monodose mobile team days
monodose_OCC_far_trip_capacity <- calc_dose_capacity(vial_type = 'monodose' 
                                                     , vax_vol = 21.09
                                                     , equip_type = 'vaxCarr' #we assume a mobile team uses one vaccine carrier
                                                     , with_ice = F)


monodose_OCC_far_trip_capacity_expanded <- seq(1, 4.5, 0.5) * monodose_FCC_far_trip_capacity 

team_days_mobile_monodose_noIce <- round(far_pop / monodose_OCC_far_trip_capacity_expanded, 1)


#10 dose mobile team days
wastage_dose10_mt_vect <- seq(1, 100/15, length.out = length(monodose_FCC_far_trip_capacity_expanded)) * dose10_wr_ft #wastage rates from the starting one for fixed teams 15% all the way to 100%


dose10_FCC_far_trip_capacity <- calc_dose_capacity(vial_type = 'dose10' 
                                                   , vax_vol = dose10_vial_vol[1]
                                                   , equip_type = 'vaxCarr' #we assume a mobile team uses one vaccine carrier
                                                   , with_ice = T)

dose10_FCC_far_trip_eff_doses_vect <- dose10_FCC_far_trip_capacity * (1 - wastage_dose10_mt_vect)

team_days_mobile_dose10_Ice <- round(far_pop / dose10_FCC_far_trip_eff_doses_vect, 1)  #there's a division by zero here, returning an infinity, so we'll replace it with NA to allow for better manipulation

team_days_mobile_dose10_Ice[which(!is.finite(team_days_mobile_dose10_Ice))] <- NA 

ratio_monodose_dose10_trip_capacity <- monodose_FCC_far_trip_capacity_expanded / dose10_FCC_far_trip_capacity


team_days_comparison_df <- tibble(
    dose10_wastage = rev(wastage_dose10_mt_vect),
    trip_capacity_ratio = rev(ratio_monodose_dose10_trip_capacity),
    team_days_dose10_far_FCC = rev(team_days_mobile_dose10_Ice),
    team_days_monodose_far_OCC = rev(team_days_mobile_monodose_noIce)
)

#####
#current situation
#####
team_days_comparison_current <- tibble(dose10_wastage= dose10_wr_ft
                       , trip_capacity_ratio = monodose_FCC_far_trip_capacity_expanded[1] / dose10_FCC_far_trip_capacity
                       , strategy = c('team_days_dose10_far_FCC', 'team_days_monodose_far_OCC')
                       , team_days_mt = c(team_days_mobile_dose10_Ice[1], team_days_mobile_monodose_noIce[1]))


df <- team_days_comparison_df %>% melt(id.vars = c('dose10_wastage', 'trip_capacity_ratio'), variable.name = 'strategy', value.name = 'team_days_mt')

ggplot(data = df) + 
    geom_point(aes(x = trip_capacity_ratio, y = dose10_wastage, color = strategy, size = team_days_mt)) + 
    geom_point(data = team_days_comparison_current, aes(x = trip_capacity_ratio, y = dose10_wastage), color = 'yellow') +
    labs(x = 'Fraction of volume capacity of monodose versus 10-dose', y = 'wastage rate (10-dose)') + 
    scale_color_manual(values = c('team_days_dose10_far_FCC' = 'green', 'team_days_monodose_far_OCC' = 'tomato3')
                       , labels = c('10-dose FCC', 'Monodose OCC'), name = 'Strategy') + 
    scale_size(name = 'Mobile team days')
