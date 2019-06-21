
#packages
library('ggplot2')
library('ggpubr')
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



x_axis_labels <- c('10-dose FCC', 'Monodose FCC', 'Mixed FCC', 'Part OCC')

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
far_orv_total_cases_table_console <- knitr::kable(select(far_orv_total_cases, 'epidemic duration' = 'time', 'strategy', 'total cases' = 'cases_cumulative'), caption = 'Total cases and epidemic duration per strategy')
far_orv_total_cases_table_console

far_orv_total_cases_table_grob <- select(far_orv_total_cases, 'strategy', 'total cases' = 'cases_cumulative')
far_orv_total_cases_table_grob$strategy <- c('10-dose FCC', 'monodose FCC', 'mixed FCC', 'part OCC')
far_orv_total_cases_table_grob

#I create a dataframe here which I use to draw line segments to indicate the campaign period
far_campaign_period_df <- data.frame(campaign_indicators_df, cases_peak = max(far_orv_dynamics$Inf4))
far_campaign_period_df <- mutate(far_campaign_period_df, cases_peak = cases_peak - c(0, 2, 4, 6))


#incidence (in this model, we assume infectious people are only diagnosed on their 4th day of infectiousness, i.e, Inf4)    
far_orv_incidence_plot <- ggplot(data = far_orv_dynamics %>% filter(Inf4 >= 0.1)) + 
    geom_point(aes(x = time, y = Inf4, color = strategy), size = 2) + 
    geom_line(aes(x = time, y = Inf4, color = strategy), size = 1) 


far_orv_incidence_plot <- far_orv_incidence_plot + 
    geom_segment(data = far_campaign_period_df, 
                 aes(x = mt_freezing_time
                     , xend = mt_team_days + mt_freezing_time
                     , y = cases_peak
                     , yend = cases_peak
                     , color = strategy
                     )
               #  , lineend = 'round'
                 , size = 1
                 , arrow = arrow(length = unit(0.01, "npc"), ends = 'both', type = 'closed')
                 )

far_orv_incidence_plot <- far_orv_incidence_plot +
    labs(title = paste0('Far population (size = ', site_data$far_pop, ')'), x = 'Time (days)', y = 'Incidence') + 
    guides(color = guide_legend(ncol = 2, nrow = 2, byrow = TRUE)) + 
    theme(legend.position = 'bottom') +
    scale_color_manual(name = "Strategy"
                       , values = c('forestgreen', 'blue', 'black', 'red', 'orange')
                       , labels = x_axis_labels
                       , breaks = strategy_names_subset
                       ) + scale_x_continuous(breaks = seq(min(far_orv_dynamics$time), max(far_orv_dynamics$time), 5)
                                              , labels = every_nth(seq(min(far_orv_dynamics$time), max(far_orv_dynamics$time), 5), 2, inverse = T)
                                              ) 
far_orv_incidence_plot <- far_orv_incidence_plot + theme_pubr(legend = 'bottom')
    
far_orv_incidence_plot <- far_orv_incidence_plot + 
    annotation_custom(grob = tableGrob(far_orv_total_cases_table_grob, rows = NULL, theme = ttheme(base_style = "lBlackWhite", tbody.style = tbody_style(hjust = 0)))
                      , xmin = 90
                      , xmax = 120
                      , ymin = 20
                      , ymax = 30
                      )

far_orv_incidence_plot

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
near_orv_total_cases_table_console <- knitr::kable(select(near_orv_total_cases, 'epidemic duration' = 'time', 'strategy', 'total cases' = 'cases_cumulative'), caption = 'Total cases and epidemic duration per strategy')
near_orv_total_cases_table_console

near_orv_total_cases_table_grob <- select(near_orv_total_cases, 'strategy', 'total cases' = 'cases_cumulative')
near_orv_total_cases_table_grob$strategy <- c('10-dose FCC', 'monodose FCC', 'mixed FCC', 'part OCC')
near_orv_total_cases_table_grob

#I create a dataframe here which I use to draw line segments to indicate the campaign period
near_campaign_period_df <- data.frame(campaign_indicators_df, cases_peak = max(near_orv_dynamics$Inf4))
near_campaign_period_df <- mutate(near_campaign_period_df, cases_peak = cases_peak - c(0, 10, 20, 30))


near_orv_incidence_plot <- ggplot(data = near_orv_dynamics %>% filter(Inf4 >= 0.1)) + 
    geom_point(aes(x = time, y = Inf4, color = strategy), size = 2) + 
    geom_line(aes(x = time, y = Inf4, color = strategy), size = 1) 

near_orv_incidence_plot <- near_orv_incidence_plot + 
    geom_segment(data = near_campaign_period_df, 
                 aes(x = ft_freezing_time
                     , xend = ft_team_days + ft_freezing_time
                     , y = cases_peak
                     , yend = cases_peak
                     , color = strategy) 
                 ,lineend = 'round'
                 , size = 1
                 , arrow = arrow(length = unit(0.01, "npc"), ends = 'both', type = 'closed')
    )

near_orv_incidence_plot <- near_orv_incidence_plot + 
    labs(title = paste0('Near population (size = ', site_data$near_pop, ')'), x = 'Time', y = 'Incidence') + 
    guides(color = guide_legend(ncol = 2, nrow = 2, byrow = TRUE)) + 
    theme(legend.position = 'bottom') +
    scale_color_manual(name = "Strategy"
                       , values = c('forestgreen', 'blue', 'black', 'red', 'orange')
                       , labels = x_axis_labels
                       , breaks = strategy_names_subset
    ) + scale_x_continuous(breaks = seq(min(near_orv_dynamics$time), max(near_orv_dynamics$time), 5)
                           , labels = every_nth(seq(min(near_orv_dynamics$time), max(near_orv_dynamics$time), 5), 2, inverse = T)
    )
near_orv_incidence_plot <- near_orv_incidence_plot + theme_pubr(legend = 'bottom')

near_orv_incidence_plot <- near_orv_incidence_plot + 
    annotation_custom(tableGrob(near_orv_total_cases_table_grob, 
                                rows = NULL
                                , theme = ttheme(base_style = "lBlackWhite", tbody.style = tbody_style(hjust = 0))
                                )
                      , xmin = 100
                      , xmax = 130
                      , ymin = 200
                      , ymax = 350
                      )

near_orv_incidence_plot

######
#cumulative incidence
#######
#I create a dataframe here which I use to draw line segments to indicate the campaign period
far_campaign_period_cum_cases <- data.frame(campaign_indicators_df, cases_peak = max(far_orv_dynamics$cases_cumulative))
far_campaign_period_cum_cases <- mutate(far_campaign_period_cum_cases, cases_peak = cases_peak - c(0, 20, 40, 60))


#far
far_orv_cum_incidence_plot <- ggplot(data = far_orv_dynamics %>% filter(Inf4 >= 0.1)) + 
    geom_point(aes(x = time, y = cases_cumulative, color = strategy), size = 2) + 
    geom_line(aes(x = time, y = cases_cumulative, color = strategy), size = 1) 


far_orv_cum_incidence_plot <- far_orv_cum_incidence_plot + 
    geom_segment(data = far_campaign_period_cum_cases, 
                 aes(x = mt_freezing_time
                     , xend = mt_team_days + mt_freezing_time
                     , y = cases_peak
                     , yend = cases_peak
                     , color = strategy
                 )
                 #  , lineend = 'round'
                 , size = 1
                 , arrow = arrow(length = unit(0.01, "npc"), ends = 'both', type = 'closed')
    )

far_orv_cum_incidence_plot <- far_orv_cum_incidence_plot +
    labs(title = paste0('Far population (size = ', site_data$far_pop, ')'), x = 'Time (days)', y = 'Cumulative incidence') + 
    guides(color = guide_legend(ncol = 2, nrow = 2, byrow = TRUE)) + 
    theme(legend.position = 'bottom') +
    scale_color_manual(name = "Strategy"
                       , values = c('forestgreen', 'blue', 'black', 'red', 'orange')
                       , labels = x_axis_labels
                       , breaks = strategy_names_subset
    ) + scale_x_continuous(breaks = seq(min(far_orv_dynamics$time), max(far_orv_dynamics$time), 5)
                           , labels = every_nth(seq(min(far_orv_dynamics$time), max(far_orv_dynamics$time), 5), 2, inverse = T)
    ) 

far_orv_cum_incidence_plot <- far_orv_cum_incidence_plot + theme_pubr(legend = 'bottom')

far_orv_cum_incidence_plot <- far_orv_cum_incidence_plot + 
    annotation_custom(grob = tableGrob(far_orv_total_cases_table_grob, rows = NULL, theme = ttheme(base_style = "lBlackWhite", tbody.style = tbody_style(hjust = 0)))
                      , xmin = 90
                      , xmax = 125
                      , ymin = 20
                      , ymax = 200
    )

far_orv_cum_incidence_plot

#Near orv
#I create a dataframe here which I use to draw line segments to indicate the campaign period
near_campaign_period_cum_cases <- data.frame(campaign_indicators_df, cases_peak = max(near_orv_dynamics$cases_cumulative))
near_campaign_period_cum_cases <- mutate(near_campaign_period_cum_cases, cases_peak = cases_peak - c(0, 200, 400, 600))


near_orv_cum_incidence_plot <- ggplot(data = near_orv_dynamics %>% filter(Inf4 >= 0.1)) + 
    geom_point(aes(x = time, y = cases_cumulative, color = strategy), size = 2) + 
    geom_line(aes(x = time, y = cases_cumulative, color = strategy), size = 1) 

near_orv_cum_incidence_plot <- near_orv_cum_incidence_plot + 
    geom_segment(data = near_campaign_period_cum_cases, 
                 aes(x = ft_freezing_time
                     , xend = ft_team_days + ft_freezing_time
                     , y = cases_peak
                     , yend = cases_peak
                     , color = strategy) 
                 ,lineend = 'round'
                 , size = 1
                 , arrow = arrow(length = unit(0.01, "npc"), ends = 'both', type = 'closed')
    )

near_orv_cum_incidence_plot <- near_orv_cum_incidence_plot + 
    labs(title = paste0('Near population (size = ', site_data$near_pop, ')'), x = 'Time', y = 'Cumulative incidence') + 
    guides(color = guide_legend(ncol = 2, nrow = 2, byrow = TRUE)) + 
    theme(legend.position = 'bottom') +
    scale_color_manual(name = "Strategy"
                       , values = c('forestgreen', 'blue', 'black', 'red', 'orange')
                       , labels = x_axis_labels
                       , breaks = strategy_names_subset
    ) + scale_x_continuous(breaks = seq(min(near_orv_dynamics$time), max(near_orv_dynamics$time), 5)
                           , labels = every_nth(seq(min(near_orv_dynamics$time), max(near_orv_dynamics$time), 5), 2, inverse = T)
    )

near_orv_cum_incidence_plot <- near_orv_cum_incidence_plot + theme_pubr(legend = 'bottom')

near_orv_cum_incidence_plot <- near_orv_cum_incidence_plot + 
    annotation_custom(tableGrob(near_orv_total_cases_table_grob, 
                                rows = NULL
                                , theme = ttheme(base_style = "lBlackWhite", tbody.style = tbody_style(hjust = 0))
                                )
                      , xmin = 100
                      , xmax = 140
                      , ymin = 4000
                      , ymax = 6000
                      )

near_orv_cum_incidence_plot
###############
#complete orv dynamics
###############

#incidence
orv_complete_incidence_plot <- grid.arrange(near_orv_incidence_plot, far_orv_incidence_plot, nrow = 2)

#cumulative incidence
orv_complete_cum_incidence_plot <- grid.arrange(near_orv_cum_incidence_plot, far_orv_cum_incidence_plot, nrow = 2)



#display and save if indicated

#near orv dynamics

#display near orv incidence
if (display_epi_plots) {
    plot(near_orv_incidence_plot)  
}

#save near orv incidence plot
if(save_epi_plots){
    ggsave(file = 'figures/near_orv_incidence_plot.pdf', plot = near_orv_incidence_plot)
}

#display near orv cumulative incidence
if (display_epi_plots) {
    plot(near_orv_cum_incidence_plot)  
}

#save near orv cumulative incidence
if(save_epi_plots){
    ggsave(file = 'figures/near_orv_cum_incidence_plot.pdf', plot = near_orv_cum_incidence_plot)
}

#far orv dynamics

#display incidence plot
if (display_epi_plots) {
    plot(far_orv_incidence_plot)  
}
##save incidence plot
if(save_epi_plots){
    ggsave(file = 'figures/far_orv_incidence_plot.pdf', plot = far_orv_incidence_plot)
}

#display cumulative incidence plot
if (display_epi_plots) {
    plot(far_orv_cum_incidence_plot)  
}

#display cumulative incidence plot
if(save_epi_plots){
    ggsave(file = 'figures/far_orv_cum_incidence_plot.pdf', plot = far_orv_cum_incidence_plot)
}


#display complete dynamics
if (display_epi_plots) {
    plot(orv_complete_cum_incidence_plot)  
}

if (display_epi_plots) {
    plot(orv_complete_incidence_plot)  
}

#save complete dynamics
# if(save_epi_plots){
#     ggsave(file = 'figures/orv_complete_campaign_total_cases_plot.pdf', orv_complete)
# }



#################################################################################
#Sensitivity on number of freezers
#################################################################################

#mf314_quant <- 1:10 #we currently run the sc model on only one freezer. What if the base has more than 1?
