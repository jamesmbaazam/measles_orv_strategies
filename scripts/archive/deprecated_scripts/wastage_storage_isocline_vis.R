#packages
library(ggplot2)
library(ggthemes)
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
monodose_occ_larger_capacity <- seq(monodose_occ_capacity_lower_bound, dose10_fcc_lower_bound, 5) #just a vector of possible increasing volume capacities to consider. The idea is to increase it to as high as the capacity for 10-dose carriage per trip.


#' We are going to consider the full spectrum of 10-dose wastage on the open 
#' interval (0, 1). In so doing, we are accomodating the best and worst case
#' scenarios for the 10-dose in fcc. 


dose10_fcc_percent_ovw <- seq(0, 100, length.out = length(monodose_occ_larger_capacity)) #ovw = open vial wastage

#x-axis of plot: ratio of increasing vaccine carrier volume capacity for monodose vs fixed for 10-dose
monodose_to_dose10_capacity_ratio <- round(monodose_occ_larger_capacity / dose10_fcc_lower_bound, 3)


volume_capacity_and_wastage_df <- tibble(
    dose10_fcc_percent_ovw = dose10_fcc_percent_ovw,
    dose10_vaxCarr_capacity = dose10_fcc_lower_bound,
    monodose_ideal_capacity = monodose_occ_larger_capacity,
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
isocline_data_df <- volume_capacity_and_wastage_df %>%
    filter(monodose_ideal_capacity <= tp_mobile) %>% 
    mutate(dose10_ovw = (1 - storage_capacity_ratio)
           , dose10_effec_dose = 750 * (1 - dose10_ovw)
    )


isocline_data <- tibble(monodose_capacity = seq(min(isocline_data_df$storage_capacity_ratio), max(isocline_data_df$storage_capacity_ratio), 0.01),
                            dose10_ovw = rev(seq(0, max(isocline_data_df$dose10_ovw), length.out = length(monodose_capacity)))
                        )



equip_region <- tibble(dose_10_ovw_component = c(isocline_data$dose10_ovw, seq(max(isocline_data$dose10_ovw), 1, 0.02)), 
                       monodose_capacity_cut_off = as.numeric(rep(isocline_data[nrow(isocline_data), 'monodose_capacity'], 
                                                                  times = length(dose_10_ovw_component))
                                                              )
                       )


# baseline_constraints <- tibble(dose10_ovw_baseline = seq(0, max(isocline_data$dose10_ovw), 0.01),
#                                monodose_capacity  = rep(min(isocline_data$monodose_capacity), times = length(dose10_ovw_baseline)),
#                                monodose_capacity_baseline = rep(max(isocline_data$dose10_ovw), times = length())
#                                )
# 
# p1 <- plot(1, 
#            xlab = "transport capacity", 
#            ylab = "open vial wastage", 
#            ylim = range(volume_capacity_and_wastage_df$dose10_fcc_percent_ovw), 
#            xlim = range(volume_capacity_and_wastage_df$monodose_ideal_capacity),
#            add = TRUE
#            )
# 
# p1 <- lines(isocline_data$monodose_capacity, isocline_data$dose10_ovw)
# 
# plot(p1)

isocline_plot_empty <- ggplot(volume_capacity_and_wastage_df, aes(x = storage_capacity_ratio, y = dose10_fcc_percent_ovw/100)) + geom_blank()

isocline_plot <- isocline_plot_empty + 
   # geom_point(data = isocline_data, aes(x = monodose_capacity, y = dose10_ovw), size = 3, color = 'navy') + 
    geom_line(data = isocline_data, aes(x = monodose_capacity, y = dose10_ovw), size = 2, color = 'navy') + 
     scale_y_continuous(breaks = seq(0, 1, 0.25), labels = seq(0, 1, 0.25), expand = c(0,0)) #+
    # scale_x_continuous(limits = c(min(isocline_data$monodose_capacity), max(volume_capacity_and_wastage_df$storage_capacity_ratio)),
    #                    expand = c(0, 0),
    #                    breaks = round(seq(min(isocline_data$monodose_capacity), max(volume_capacity_and_wastage_df$storage_capacity_ratio), length.out = 7), 2),
    #                    labels = round(seq(min(isocline_data$monodose_capacity), max(volume_capacity_and_wastage_df$storage_capacity_ratio), length.out = 7), 2))
    # # scale_x_continuous(breaks = round(seq(min(isocline_data$monodose_capacity), max(volume_capacity_and_wastage_df$storage_capacity_ratio), length.out = 7), 2),
    #                    labels = round(seq(min(isocline_data$monodose_capacity), max(volume_capacity_and_wastage_df$storage_capacity_ratio), length.out = 7), 2))

isocline_plot <- isocline_plot + geom_line(data = equip_region, aes(x = monodose_capacity_cut_off, y = dose_10_ovw_component), size = 2, color = 'navy')

isocline_plot <- isocline_plot + labs(x = 'Transport capacity ratio (Monodose:10-dose)', y = 'Open vial wastage (10-dose)') 

    
isocline_plot <- isocline_plot + 
    annotate('text', label = '10-dose', x = 0.27, y = 0.10, size = 5, color = 'navy') +
    annotate('text', label = '1-dose', x = 0.28, y = 0.75, size = 5, color = 'navy') +
    theme(axis.line.x = element_line(color="black", size = 1.5),
          axis.line.y = element_line(color="black", size = 1.5)
          )


isocline_plot <- isocline_plot + scale_x_continuous(trans = squish_trans(0.36, 1, 100), breaks = seq(0, 1, by = 2), labels = seq(0, 1, by = 2))
                     


isocline_plot <- isocline_plot + presentation_plot_theme 

if (display_sc_plots) {
    plot(isocline_plot)  
}   

if(save_sc_plots){
    ggsave(filename = 'figures/isocline_plot.pdf')
}




#Gap x-axis plot with plotrix

# gap.plot(x, y, gap=c(from,to), type="b", xlab="index", ylab="value")
# axis.break(2, from, breakcol="snow", style="gap")
# axis.break(2, from*(1+0.02), breakcol="black", style="slash")
# axis.break(4, from*(1+0.02), breakcol="black", style="slash")
# axis(2, at=from)
# 
# 
# 
# 
# gap.plot(volume_capacity_and_wastage_df$storage_capacity_ratio, 
#          volume_capacity_and_wastage_df$dose10_fcc_percent_ovw/100, 
#          gap = c(0.36,0.9),
#          gap.axis = 'x',
#          type = 'n',
#          xlab = 'Transport capacity ratio (Monodose:10-dose)',
#          ylab = 'Open vial wastage (10-dose)'
#          )
# 
# axis.break(1, 0.36, breakcol="red", style="slash")
# 
# plot(isocline_data_df$storage_capacity_ratio, 
#      isocline_data_df$dose10_ovw,
#      type = "n"
# )













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