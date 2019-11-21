#packages
library(ggplot2)
library(tibble)

#scripts
source('scripts/Supply_chain_functions.R')
source('scripts/analyses_parameters.R')
source('scripts/Parameters.R')
source('scripts/plotting_functions.R')



################################################################################
#' #'STORAGE VOLUME AND WASTAGE ISOCLINE!! 
#' ################################################################################
#' 
#' 
#' We know that vaccine carrier capacity is a constraint affecting monodose 
#' and vaccine wastage constrains the 10-dose presentation, and is a function 
#' of vaccine demand and supply during an outbreak. The wastage of 10-dose during
#' outbreak response mass vaccination is largely affected by the clustering of
#' encountered populations and the time until a vaccine should not be used post
#' reconstitution. 
#' 
#' The question then arises: can we formulate a framework, which incorporates these
#' two constraints, considering the worst and best scenarios in determining 
#' the team days required to achieve 100% vaccination coverage? 
#' 
#' We answer this question with an isocline derived below.
#' 
#' Assumption: this isocline neglects the speed of commencement
#' of the strategy and only relies on the apriori knowledge that the best strategy
#' will commence and finish in the shortest possible time
#' 
#how many doses of the monodose vials can we transport in a vaccine carrier and rcw25?
monodose_occ_capacity_lower_bound <- calc_dose_capacity(vial_type = 'monodose'
                                            , vax_vol = 21.09
                                            , equip_type = 'vaxCarr' #we assume a mobile team uses one vaccine carrier
                                            , with_ice = F)


monodose_occ_capacity_upper_bound <- calc_dose_capacity(vial_type = 'monodose'
                                                  , vax_vol = 21.09
                                                  , equip_type = 'rcw25' #we assume a mobile team uses one vaccine carrier
                                                  , with_ice = F)



#how many doses of the 10-dose vials can we transport in a vaccine carrier and rcw25?
dose10_fcc_lower_bound <- calc_dose_capacity(vial_type = 'dose10'
                                          , vax_vol = dose10_vial_vol[1]
                                          , equip_type = 'vaxCarr' #we assume a mobile team uses one vaccine carrier
                                          , with_ice = T)


dose10_fcc_upper_bound <- calc_dose_capacity(vial_type = 'dose10'
                                             , vax_vol = dose10_vial_vol[1]
                                             , equip_type = 'rcw25' #we assume a mobile team uses one vaccine carrier
                                             , with_ice = T)


#' We are going to increase the storage volume for monodose occ, and in so doing, 
#' accomodate the best case scenario for monodose storage volume
monodose_occ_larger_capacity <- seq(monodose_occ_capacity_lower_bound, dose10_fcc_upper_bound, 5) #just a vector of possible increasing volume capacities to consider. The idea is to increase it to as high as the capacity for 10-dose carriage per trip.


#' We are going to consider the full spectrum of 10-dose wastage on the open 
#' interval (0, 1). In so doing, we are accomodating the best and worst case
#' scenarios for the 10-dose in fcc. 


dose10_fcc_perc_ovw <- seq(0, 100, length.out = length(monodose_occ_larger_capacity)) #ovw = open vial wastage

#x-axis of plot: ratio of increasing vaccine carrier volume capacity for monodose vs fixed for 10-dose
monodose_to_dose10_capacity_ratio <- round(monodose_occ_larger_capacity / dose10_fcc_upper_bound, 3)


volume_capacity_and_wastage_df <- tibble(
    dose10_perc_ovw = dose10_fcc_perc_ovw,
    dose10_capacity = dose10_fcc_lower_bound,
    monodose_capacity = monodose_occ_larger_capacity,
    storage_capacity_ratio = monodose_to_dose10_capacity_ratio
)


#' #####################################
#' #Isocline plot
#' #####################################
#' 
#'The isocline represents the point on the intersection between the monodose and 10-dose
#'strategies where there is no difference in team days. The areas above and below the line
#'correspond to an optimality in favour of the indicated vial type. This isocline
#' eliminates any dependence on the population sizes
#'and indicates the wastage (10-dose) and storage (monodose) pairs that give the 
#'same team days between monodose occ and 10 dose fcc
#'

#' We assume that the monodose is worse when the volume capacity < team performance,
#' and the 10 dose is worse when the effective doses < team performance.
#' The relationship that equates these two leads to a single equation in
#' two variables - wastage and storage, that is, wastage of 10-dose = 1 - storage 
#' capacity for monodose / storage  capacity for 10 dose. 
#' Solving the above in the regions where storage capacity for monodose < mobile team 
#' performance will yield the following
#' 
isocline_df <- volume_capacity_and_wastage_df %>%
    filter(monodose_capacity < tp_mobile) %>% 
    mutate(dose10_ovw = 1 - storage_capacity_ratio
           , dose10_effec_dose = 750 * (1 - dose10_ovw / 100)
    )


isocline_plot <- ggplot(data = isocline_df) + 
    geom_point(aes(x = storage_capacity_ratio, y = dose10_ovw), size = 2) + 
    geom_line(aes(x = storage_capacity_ratio, y = dose10_ovw), size = 1) + 
    scale_y_continuous(breaks = seq(0, 1, 0.05), labels = seq(0, 1, 0.05)) 

isocline_plot <- isocline_plot +
    labs(x = 'Storage capacity ratio (monodose over 10-dose)', y = 'Open vial wastage (10-dose)') + 
    annotate('text', label = '10-dose', x = 0.25, y = 0.45, size = 7) +
    annotate('text', label = 'Monodose', x = 0.75, y = 0.75, size = 7) 

isocline_plot <- isocline_plot + presentation_plot_theme

if (display_sc_plots) {
    plot(isocline_plot)  
}   

if(save_sc_plots){
    ggsave(filename = 'figures/isocline_plot.pdf')
}

#' Interpretation: Irrespective of population size, so far as the storage 
#' capacity for monodose is (monodose storage/750) > 32.5% , monodose will lead to 
#' a shorter team days than 10-dose fcc. On the other hand, anytime the 10-dose 
#' wastage is below 77% in any setting, it will be better to use the 10-dose since
#' it will lead to shorter team days. As indicated earlier, this isocline neglects 
#' the speed of commencement of the strategy and only relies on the apriori knowledge 
#' that the best strategy will commence and finish in the shortest possible time.
#' It also assumes that the open vial wastage is known but it is not, and depends
#' on the team encounters per day. The next model considers what happens in a team
#' day in a bid to understand how population clustering and team encounters impact
#' vaccine usage and wastage.
#' 
#' 
#' 
#' 