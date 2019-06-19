
#packages
library('ggplot2')
library('dplyr')
library('reshape2')
library('purrr')
library('gridExtra')
library('tidyr')

#scripts
source('./scripts/parameters.R') #global parameter list
source('./scripts/analyses_parameters.R') #specific parameter list for this analysis
source('./scripts/supply_chain_functions.R') #functions for performing individual supply chain calculations
source('./scripts/wrappers_supply_chain.R') #functions for running the supply chain model
source('scripts/measlesFunctions.R') #functions for running the orv model


##########################################################
#SUPPLY CHAIN ANALYSIS
###########################################################
#all possible combinations
# strategy_scenarios <- expand.grid(fixed_team_with_dose10 = c(T, F)
#                         , fixed_team_with_ice = c(T, F)
#                         , mobile_team_with_dose10 = c(T, F)
#                         , mobile_team_with_ice = c(T, F)
#                         , team_dispatch = c('parallel', 'asap')
#                         )

#Full list of strategies
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


##########################################################################
#'STRATEGY-SPECIFIC CAMPAIGN DELAY ANALYSIS 
#'TODO: change the for loop to an lapply function for efficiency
##########################################################################
campaign_delay_results <- list()
strategy_names_subset <- names(strategy_analysis_list)[c(2, 4, 6, 7)]
for (i in seq_along(strategy_names_subset)) {
    campaign_delay_results[[strategy_names_subset[i]]] <- analyse_prep_delay(
        strategy_name = strategy_names_subset[i]
        , fixed_team_with_dose10 = strategy_analysis_list[[strategy_names_subset[i]]]$fixed_team_with_dose10
        , fixed_team_with_ice = strategy_analysis_list[[strategy_names_subset[i]]]$fixed_team_with_ice
        , mobile_team_with_dose10 = strategy_analysis_list[[strategy_names_subset[i]]]$mobile_team_with_dose10
        , mobile_team_with_ice = strategy_analysis_list[[strategy_names_subset[i]]]$mobile_team_with_ice
        , team_dispatch = strategy_analysis_list[[strategy_names_subset[i]]]$team_dispatch
    )
}

#Convert the list of dataframes output into a single data frame
strategy_campaign_prep_delays <- do.call(rbind, args = c(campaign_delay_results, make.row.names = F))

#View(strategy_campaign_prep_delays)



##########################################################################
#'STRATEGY-SPECIFIC TEAM DAYS ANALYSIS 
#'TODO: change the for loop to an lapply function for efficiency
##########################################################################

team_days_results <- list()
#strategy_names <- names(strategy_analysis_list)
for (i in seq_along(strategy_names_subset)) {
    team_days_results[[strategy_names_subset[i]]] <- analyse_team_days(
        strategy_name = strategy_names_subset[i]
        , fixed_team_with_dose10 = strategy_analysis_list[[strategy_names_subset[i]]]$fixed_team_with_dose10
        , fixed_team_with_ice = strategy_analysis_list[[strategy_names_subset[i]]]$fixed_team_with_ice
        , mobile_team_with_dose10 = strategy_analysis_list[[strategy_names_subset[i]]]$mobile_team_with_dose10
        , mobile_team_with_ice = strategy_analysis_list[[strategy_names_subset[i]]]$mobile_team_with_ice
    )
}

#Convert the list of dataframes output into a single data frame
strategy_team_days <- do.call(rbind, args = c(team_days_results, make.row.names = F))

#View(strategy_team_days)

sc_analysis_output <- left_join(strategy_campaign_prep_delays, strategy_team_days, by = 'strategy')
#View(sc_analysis_output)


##########################################################################
#'SUPPLY CHAIN PLOTS
#'TODO: change the for loop to an lapply function for efficiency
##########################################################################



#Data wrangling for plots: convert the wide table to long
strategy_team_days_long <- strategy_team_days %>%
    select(-c(ft_vial_type, mt_vial_type)) %>%
    filter(strategy == strategy_names_subset) %>% 
    dplyr::rename(strategy_name = strategy, fixed_team = ft_team_days, mobile_team = mt_team_days) %>% 
    gather('team_type', 'team_days', c(fixed_team, mobile_team)) %>% 
    mutate(team_type = factor(team_type))

strategy_logistical_needs_long <- sc_analysis_output %>% 
    select(strategy, ft_RCW25, ft_vaxCarr, mt_RCW25, mt_vaxCarr) %>%
    gather('equip_name', 'equip_quantity', 2:5) %>% 
    separate(col = 'equip_name', into = c('team_type', 'equip_name'), sep = '_') %>% 
    mutate(team_type = factor(if_else(team_type == 'ft', 'fixed_team', 'mobile_team')))


#' #' Question: If we need more than 1 vaccine carrier for the doses, how do we translate that? Does that translate into
#' #' how many teams we'll need or how many trips should be undertaken by a single team? The latter will draw in the 
#' #' need for a rule for how the distance of the site from the base translates to trips in days and how that will affect
#' #' the team days and campaign duration.
#' 



x_axis_labels <- c('10 dose FCC (parallel)', 'Monodose FCC (parallel)', 'Mixed FCC (parallel)', 'Part OCC (asap)')

#Plot 1: Delay before a campaign can commence
campaign_delay_plot <- ggplot(data = strategy_campaign_prep_delays,
                              aes(x = strategy, y = mt_freezing_time)) +
    geom_bar(aes(fill = 'tomato3'), stat = "identity", width = 0.25) +
    labs(#title = 'Freezing time required per strategy',
        x = '',
        y = "Campaign delay (days)"
    ) + 
    scale_fill_manual(name = "Team type",
                      values = c("tomato3")
                      , labels = c('Mobile teams')) +
    scale_x_discrete(labels = c('','','','')) + 
    theme(legend.position = 'none', axis.ticks.x = element_blank()) + 
    # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    shiny_plot_theme

# if(save_sc_plots){
#     ggsave(filename = 'figures/campaign_commencement_delay.pdf', plot = campaign_delay_plot, device = 'pdf')
# }
#Plot 2: Number of days required by teams to complete the campaign

#team days plot
team_days_plot <- ggplot(data = strategy_team_days_long,
                         aes(x = strategy_name,
                             y = team_days,
                             fill = team_type
                         )
) +
    geom_bar(stat = 'identity', position = 'dodge', width = 0.25) +
    # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(#title = 'Number of days per team type and strategy',
        x = 'Strategy'
        ,  y = "Team days"
    ) +
    scale_x_discrete(labels = x_axis_labels) +
    scale_fill_manual(values = c("royalblue4", "tomato3"),
                      name = "Team type",
                      labels = c('Fixed team', 'Mobile team')) +
    theme(legend.position = 'bottom') + 
    shiny_plot_theme

# if(save_sc_plots){
#     ggsave(filename = 'figures/team_days.pdf', plot = team_days_plot, device = 'pdf')
# }


#Plot logistical needs of each strategy
#rcw25 and vaccine carrier needs
logistical_needs <- ggplot(data = strategy_logistical_needs_long,
                           aes(x = strategy,
                               y = equip_quantity,
                               fill = equip_name
                           )
) +
    geom_bar(stat = 'identity',
             position = 'dodge'
    ) + 
    facet_wrap('team_type', labeller = as_labeller(c('fixed_team' = 'Fixed team', 'mobile_team' = 'Mobile team'))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(#title = 'Number of days per team type and strategy',
        x = 'Strategy'
        ,  y = "Quantity"
    ) +
    scale_x_discrete(labels = x_axis_labels) +
    scale_y_continuous(breaks = seq(min(strategy_logistical_needs_long$equip_quantity), max(strategy_logistical_needs_long$equip_quantity), 10),
                       labels = every_nth(seq(min(strategy_logistical_needs_long$equip_quantity), max(strategy_logistical_needs_long$equip_quantity), 10), 2)
    ) +
    scale_fill_manual(values = c("forestgreen", "grey27"),
                      name = "Equipment",
                      labels = c('RCW 25', 'Vaccine carrier')) +
    shiny_plot_theme

if(save_sc_plots){
    ggsave(filename = 'figures/logistical_needs.pdf', plot = logistical_needs, device = 'pdf')
}


#Combine all into one plot
if(display_sc_plots){
    plot(logistical_needs)
    campaign_delay_and_duration_plot <- grid.arrange(campaign_delay_plot,
                                       # iceVol_plot,
                                       team_days_plot,
                                       nrow = 2)
}

if(save_sc_plots){
    ggsave(filename = 'figures/campaign_delay_and_duration.pdf', plot = campaign_delay_and_duration_plot, device = 'pdf')
}
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
#' if(save_sc_plots){
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
#' if(save_sc_plots){
#'     ggsave(filename = 'mobile_team_days_isocline.png'
#'           # , plot = isocline_plot + presentation_plot_theme #uncomment this line to save a powerpoint version
#'           , plot = isocline_plot
#'           , path = './figures/'
#'            , width = 9
#'            , height = 5)
#' }

##########################################################
#EPIDEMIOLOGICAL ANALYSIS
###########################################################





###############################################################################
#Running the simulations for each strategy
###############################################################################

strategy_names_subset <- names(strategy_analysis_list)[c(2, 4, 6, 7)]
orv_far_strategy_results <- list()
for (i in 1:length(strategy_names_subset)) {
    orv_far_strategy_results[[strategy_names_subset[i]]] <- runSimulations(
        R0 = orv_model_params$R0 # transmission coeficient
        , run_time = orv_model_params$model_time # 1 yr!
        , pop = initializePop(N = site_data$far_pop, initPropImmune = 0.25, I0 = 1)
        , strategy_name = strategy_names_subset[i]
        , vaxDay = as.numeric(subset(strategy_campaign_prep_delays, strategy == strategy_names_subset[i])['mt_freezing_time'])
        , orv_duration = as.numeric(subset(strategy_team_days_long, strategy_name == strategy_names_subset[i] & team_type == 'mobile_team')[ ,'team_days']) #for now we're only looking at the far campaigns 
        , vax_eff = orv_model_params$vaccine_efficacy
        , team_performance = ifelse(strategy_analysis_list[[strategy_names_subset[i]]][["mobile_team_with_dose10"]], as.numeric(sc_model_params$vax_rate['mobile_team']), ifelse(strategy_analysis_list[[strategy_names_subset[i]]][["mobile_team_with_ice"]], 77, 170))
        , time_to_immunity = orv_model_params$immune_response_timing
        , browse = F
    ) 
}

orv_near_strategy_results <- list()
for (i in 1:length(strategy_names_subset)) {
    orv_near_strategy_results[[strategy_names_subset[i]]] <- runSimulations(
        R0 = orv_model_params$R0 # transmission coeficient
        , run_time = orv_model_params$model_time # 1 yr!
        , pop = initializePop(N = site_data$near_pop, initPropImmune = 0.25, I0 = 1)
        , strategy_name = strategy_names_subset[i]
        , vaxDay = as.numeric(subset(strategy_campaign_prep_delays, strategy == strategy_names_subset[i])['ft_freezing_time'])
        , orv_duration = as.numeric(subset(strategy_team_days_long, strategy_name == strategy_names_subset[i] & team_type == 'fixed_team')[ ,'team_days']) #for now we're only looking at the far campaigns 
        , vax_eff = orv_model_params$vaccine_efficacy
        , team_performance = ifelse(strategy_analysis_list[[strategy_names_subset[i]]][["fixed_team_with_dose10"]], as.numeric(sc_model_params$vax_rate['fixed_team']), ifelse(strategy_analysis_list[[strategy_names_subset[i]]][["fixed_team_with_ice"]], 77, 170))
        , time_to_immunity = orv_model_params$immune_response_timing
        , browse = F
    ) 
}

################################################################################
#Plotting the SC decision's consequence on the epidemic

################################################################################

#pre-processing the orv model output
#1: far campaign
#1A. Extract the detailed dynamics and bind them into one df
far_orv_results_detailed <- orv_far_strategy_results %>% 
    purrr::map('Detailed')
#data for plotting
far_orv_epi_dyn_detailed <- do.call("rbind", args = c(far_orv_results_detailed, make.row.names = F)) %>% 
    mutate(strategy = factor(strategy))

#1B. Extract the sub-summed results and bind them into one df
far_orv_results_collapsed <- orv_far_strategy_results %>% 
    purrr::map('Collapsed')
#data for plotting
far_orv_epi_dyn_summed <- do.call("rbind", args = c(far_orv_results_collapsed, make.row.names = F)) %>% 
    mutate(strategy = factor(strategy))


#2: near campaign
#2A. Extract the detailed dynamics and bind them into one df
near_orv_results_detailed <- orv_near_strategy_results %>% 
    purrr::map('Detailed')
#data for plotting
near_orv_epi_dyn_detailed <- do.call("rbind", args = c(near_orv_results_detailed, make.row.names = F)) %>% 
    mutate(strategy = factor(strategy))

#2B. Extract the sub-summed results and bind them into one df
near_orv_results_collapsed <- orv_near_strategy_results %>% 
    purrr::map('Collapsed')
#data for plotting
near_orv_epi_dyn_summed <- do.call("rbind", args = c(near_orv_results_collapsed, make.row.names = F)) %>% 
    mutate(strategy = factor(strategy))

################################################################################
#Plots
################################################################################
campaign_indicators_df <- select(sc_analysis_output, strategy, ft_freezing_time, mt_freezing_time, ft_team_days, mt_team_days)%>% 
    group_by(strategy)
#1. far orv: total cases
far_orv_dynamics <- far_orv_epi_dyn_detailed %>% 
    group_by(strategy) %>% 
    mutate(cases_cumulative = cumsum(Inf4)) %>% 
    left_join(. , campaign_indicators_df, by = 'strategy') 
# %>% 
#     filter(Inf >  0.1) 

#find the total cases at the end of the epidemic

#far campaign
far_orv_total_cases <- dplyr::filter(far_orv_dynamics, time == max(time)) %>% select(time, strategy, cases_cumulative)
#print a table of total cases
far_orv_total_cases_table <- knitr::kable(select(far_orv_total_cases, 'epidemic duration' = 'time', 'strategy', 'total cases' = 'cases_cumulative'), caption = 'Total cases and epidemic duration per strategy')
far_orv_total_cases_table



far_orv_total_cases_plot <- ggplot(data = far_orv_dynamics %>% filter(Inf4 >= 0.1)) + 
    geom_point(aes(x = time, y = Inf4, color = strategy), size = 2) + 
    geom_line(aes(x = time, y = Inf4, color = strategy), size = 1) 


far_orv_total_cases_plot <- far_orv_total_cases_plot + 
    geom_line(data = far_orv_dynamics %>% filter(time <= 10), 
              aes(x = mt_freezing_time, y = Inf4, color = strategy), size = 1) 

far_orv_total_cases_plot <- far_orv_total_cases_plot +
    labs(title = paste0('Far population (size = ', site_data$far_pop, ')'), x = 'Time (days)', y = 'Total cases') + 
    guides(color = guide_legend(ncol = 2, nrow = 2, byrow = TRUE)) + 
    theme(legend.position = 'bottom') +
    scale_color_manual(name = "Strategy"
                       , values = c('forestgreen', 'blue', 'black', 'red', 'orange')
                       , labels = x_axis_labels
                       , breaks = strategy_names_subset
                       ) + scale_x_continuous(breaks = seq(min(far_orv_dynamics$time), max(far_orv_dynamics$time), 5)
                                              , labels = every_nth(seq(min(far_orv_dynamics$time), max(far_orv_dynamics$time), 5), 2, inverse = T)
                                              ) 

far_orv_total_cases_plot <- far_orv_total_cases_plot + annotation_custom(tableGrob(select(far_orv_total_cases, 'strategy', 'total cases' = 'cases_cumulative'), rows = NULL), xmin = 100, xmax = 130, ymin = 20, ymax = 30)



#1. Near orv: total cases
near_orv_dynamics <- near_orv_epi_dyn_detailed %>% 
    group_by(strategy) %>%
    mutate(cases_cumulative = cumsum(Inf4)) %>% 
    left_join(. , campaign_indicators_df, by = 'strategy')  
# %>% 
#     filter(totalInf >  0.1) 

##find the total cases at the end of the epidemic
near_orv_total_cases <- dplyr::filter(near_orv_dynamics, time == max(time)) %>% select(time, strategy, cases_cumulative) 
#print a table of total cases
near_orv_total_cases_table <- knitr::kable(select(near_orv_total_cases, 'epidemic duration' = 'time', 'strategy', 'total cases' = 'cases_cumulative'), caption = 'Total cases and epidemic duration per strategy')
near_orv_total_cases_table


near_orv_total_cases_plot <- ggplot(data = near_orv_dynamics %>% filter(Inf4 >= 0.1)) + 
    geom_point(aes(x = time, y = Inf4, color = strategy), size = 2) + 
    geom_line(aes(x = time, y = Inf4, color = strategy), size = 1) 

near_orv_total_cases_plot <- near_orv_total_cases_plot + geom_line(data = near_orv_dynamics %>% filter(Inf4 <= 100), 
                                                                   aes(x = ft_freezing_time, y = Inf4, color = strategy)
                                                                   ) 

near_orv_total_cases_plot <- near_orv_total_cases_plot + labs(title = paste0('Near population (size = ', site_data$near_pop, ')'), x = 'Time', y = 'Total cases') + 
    guides(color = guide_legend(ncol = 2, nrow = 2, byrow = TRUE)) + 
    theme(legend.position = 'bottom') +
    scale_color_manual(name = "Strategy"
                       , values = c('forestgreen', 'blue', 'black', 'red', 'orange')
                       , labels = x_axis_labels
                       , breaks = strategy_names_subset
    ) + scale_x_continuous(breaks = seq(min(near_orv_dynamics$time), max(near_orv_dynamics$time), 5)
                           , labels = every_nth(seq(min(near_orv_dynamics$time), max(near_orv_dynamics$time), 5), 2, inverse = T)
    )

near_orv_total_cases_plot <- near_orv_total_cases_plot + annotation_custom(tableGrob(select(near_orv_total_cases, 'strategy', 'total cases' = 'cases_cumulative'), rows = NULL), xmin = 100, xmax = 130, ymin = 200, ymax = 350)


###############
#complete orv dynamics
###############

orv_complete <- grid.arrange(near_orv_total_cases_plot, far_orv_total_cases_plot, nrow = 2)


#display and save if indicated
#display near dynamics
if (display_epi_plots) {
    plot(near_orv_total_cases_plot)  
}
#save near dynamics
if(save_epi_plots){
    ggsave(file = 'figures/near_orv_total_cases_plot.pdf', plot = near_orv_total_cases_plot)
}

#display far dynamics
if (display_epi_plots) {
    plot(far_orv_total_cases_plot)  
}
#save far dynamics
if(save_epi_plots){
    ggsave(file = 'figures/far_orv_total_cases_plot.pdf', plot = far_orv_total_cases_plot)
}


#display complete dynamics
if (display_epi_plots) {
    plot(orv_complete)  
}
#save complete dynamics
if(save_epi_plots){
    ggsave(file = 'figures/orv_complete_campaign_total_cases_plot.pdf', orv_complete)
}







#View(head(orv_plot_dat %>% filter(strategy == 'monodose_fcc'), n = 20))

#ggplot(epi_dyn_detailed, aes(x = time, y = epi_dyn_detailed$Inf5)) + stat_density() 

#2. Incidence: This plot is just a backward translation of the final size plot because the model is deterministic

# incidence_plot <- ggplot(data = epi_dyn_detailed) + 
#     geom_point(aes(x = time, y = Exp1, color = strategy), size = 2) + 
#     geom_line(aes(x = time, y = Exp1, color = strategy), size = 1) +
#     labs(x = 'Time (days)', y = 'New cases (exposed)') + 
#     scale_color_manual(name = "Strategy"
#                        , values = c('green', 'blue', 'black', 'red')
#                        , labels = c("10-dose FCC", "Monodose FCC", "Mixed FCC", 'Part OCC')
#                        , breaks = c("dose10_fcc", "monodose_fcc", "mixed_fcc", 'part_occ')
#     )
# 
# if (display_epi_plots) {
#     plot(incidence_plot)  
# }

#################################################################################
#Sensitivity on number of freezers
#################################################################################

#mf314_quant <- 1:10 #we currently run the sc model on only one freezer. What if the base has more than 1?
