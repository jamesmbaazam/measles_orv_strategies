
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
    # monodose occ
    monodose_occ_asap = data.frame(strategy_name = 'monodose_occ_asap'
                                   , fixed_team_with_dose10 = F
                                   , fixed_team_with_ice = F
                                   , mobile_team_with_dose10 = F
                                   , mobile_team_with_ice = F
                                   , team_dispatch = 'asap'
    ),
    
    monodose_occ_parallel = data.frame(strategy_name = 'monodose_occ_parallel'
                                       , fixed_team_with_dose10 = F
                                       , fixed_team_with_ice = F
                                       , mobile_team_with_dose10 = F
                                       , mobile_team_with_ice = F
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
strategy_names_subset <- c("dose10_fcc_parallel", "monodose_fcc_parallel", "monodose_occ_asap", "mixed_fcc_parallel", "part_occ_asap")


campaign_delay_results <- list()
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









################################################################################

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
   # theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'top') +
    theme(legend.position = 'top') +
    labs(#title = 'Number of days per team type and strategy',
        x = 'Strategy'
        ,  y = "Quantity"
    ) +
    scale_x_discrete(labels = x_axis_labels) +
    scale_y_continuous(breaks = seq(0, (max(strategy_logistical_needs_long$equip_quantity) + 10), 10),
                       labels = every_nth(seq(0, max(strategy_logistical_needs_long$equip_quantity) + 10, 10), 2)) +
    scale_fill_manual(values = c("forestgreen", "grey27"),
                      name = "Equipment",
                      labels = c('RCW 25', 'Vaccine carrier')) +
    shiny_plot_theme

if(save_sc_plots){
    ggsave(filename = 'figures/logistical_needs_plot.pdf', plot = logistical_needs, device = 'pdf', width = 10, height = 4.3) #probably not a good scale to save it for a publication but for a presentation
}

#applying theme_economist() from the ggthemes package for trial
library('ggthemes')
#control whether or not to save the economist_theme plots
save_economist_theme_plots <- TRUE

logistical_needs_econ_theme_white <- logistical_needs + theme_economist_white() + scale_color_economist()
logistical_needs_econ_theme_default <- logistical_needs + theme_economist()

#display the economist_theme plots
if(display_sc_plots){
    plot(logistical_needs_econ_theme_white)
    plot(logistical_needs_econ_theme_default)
}

if(save_economist_theme_plots){
    ggsave(filename = 'figures/logistical_needs_econ_theme_white.pdf', plot = logistical_needs_econ_theme_white, device = 'pdf', width = 10, height = 4.3) 
    ggsave(filename = 'figures/logistical_needs_econ_theme_default.pdf', plot = logistical_needs_econ_theme_default, device = 'pdf', width = 10, height = 4.3)
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
        , n_team_type = 2
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
        , n_team_type = 1
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

################################################################################
#' #'TOWARDS THE ISOCLINE!! 
#' 
#' We have determined that the mixed FCC and monodose FCC
#' are out of the question now. Also, we know that vaccine wastage is a function 
#' of the demand during an outbreak hence, is non-linear. Additionally, we know 
#' the vaccine carrier design is optimised for transporting ice but not so for
#' transporting enough monodose vials as 10-dose.
#' 
#' Q: The question then arises: if the vaccine carriers could be optimised to carry
#' more monodose, would that solve there be a point in the parameter space where
#' the partial OCC strategy would match the 10-dose strategy or even surpass it? 
#' We consider that the fixed post team requirements are the same for the two 
#' strategies and the difference is in the mobile teams. Therefore, we will zoom 
#' in on the mobile campaign. 
#' 
#' Here, we have the choice 
#' between using 10-dose vials in full cold chain or the monodose out of the 
#' cold chain. We want to investigate
#' under what combinations of 10-dose wastage and higher storage capacity for 
#' monodose vials will 
#' the use of monodose outside of the cold chain be better than the 10-dose 
#' vaccine?
#' We will answer this using an isocline formulation.
#' ################################################################################
#' 
#' #####################
#' #Far campaigns: monodose Outside of Cold Chain versus 10-dose in Full Cold Chain
#' #########################
#' 
#' 
#' ###############################
#' 
#how many doses of the 10-dose vials can we transport in a vaccine carrier?

monodose_occ_capacity <- calc_dose_capacity(vial_type = 'monodose'
                                                     , vax_vol = 21.09
                                                     , equip_type = 'vaxCarr' #we assume a mobile team uses one vaccine carrier
                                                     , with_ice = F)

#how many doses of the 10-dose vials can we transport in a vaccine carrier?
dose10_fcc_capacity <- calc_dose_capacity(vial_type = 'dose10'
                                                   , vax_vol = dose10_vial_vol[1]
                                                   , equip_type = 'vaxCarr' #we assume a mobile team uses one vaccine carrier
                                                   , with_ice = T)


monodose_occ_capacity_larger <- seq(monodose_occ_capacity, dose10_fcc_capacity, 5) #just a vector of possible increasing volume capacities to consider. The idea is to increase it to as high as the capacity for 10-dose carriage per trip.


#mobile team days for monodose OCC with increasing storage/transport volume
monodose_occ_team_days_increasing_vol <- vector()
team_days_mobile_monodose_occ <- for (storage_capacity_index in 1:length(monodose_occ_capacity_larger)){
    monodose_occ_team_days_increasing_vol[storage_capacity_index] <- calc_monodose_team_days(target_pop = site_data$far_pop
                                                                                             , team_performance = min(monodose_occ_capacity_larger[storage_capacity_index], tp_mobile)
                                                                                             , carrier_vol_capacity = monodose_occ_capacity_larger[storage_capacity_index]
                                                                                             )
    }

###############################
#10 dose FCC 
###############################

#Considering wastage ranging from 0% to 100%, with 0% meaning no wastage and 100%, wastage of whole vial. MSF considers an average of 15% wastage rate in field operations.

dose10_fcc_ovw_increasing <- seq(0, 100, length.out = length(monodose_occ_team_days_increasing_vol)) #ovw = open vial wastage

#dose10_FCC_far_trip_eff_doses_vect <- ceiling(dose10_vaxCarr_cap_ice * (1 - wastage_dose10_mt_vect)) #effectively, how many vaccinations is a mobile team actually undertaking?

team_days_mobile_dose10_fcc <- vector()
for(ovw_index in 1: length(dose10_fcc_ovw_increasing)){
    team_days_mobile_dose10_fcc[ovw_index] <- calc_dose10_team_days(target_pop = site_data$far_pop
                                                                    , dose10_wastage = dose10_fcc_ovw_increasing[ovw_index]
                                                                    , vaxCarr_capacity = dose10_fcc_capacity
                                                                    , team_performance = tp_mobile
    )
    }

#there's a division by zero here, returning an infinity, so we'll replace it with NA to allow for better manipulation


team_days_mobile_dose10_fcc <- ifelse(!is.infinite(team_days_mobile_dose10_fcc), team_days_mobile_dose10_fcc, NA) #I do a correction here to avoid division by zero.

#data.frame(wastage = wastage_dose10_mt_vect, team_days = team_days_mobile_dose10_Ice)

#x-axis of plot: ratio of increasing vaccine carrier volume capacity for monodose vs fixed for 10-dose
monodose_to_dose10_capacity_ratio <- round(monodose_occ_capacity_larger / dose10_fcc_capacity, 3)


team_days_monodose_occ_dose10_fcc <- tibble(
    dose10_ovw = dose10_fcc_ovw_increasing,
    dose10_capacity = dose10_fcc_capacity,
    monodose_capacity = monodose_occ_capacity_larger,
    storage_capacity_ratio = monodose_to_dose10_capacity_ratio,
    team_days_dose10_fcc = team_days_mobile_dose10_fcc,
    team_days_monodose_occ = monodose_occ_team_days_increasing_vol
)


#' #####################################
#' #Isocline plot
#' #####################################
#' 
#'The isocline represents the point on the intersection between the monodose and 10-dose
#'strategies where there is no difference in team days. The areas above and below the line
#'correspond to a switch in decision.


#'I've labelled this as sketchy because it depended on the monodose team days but the plot that
#'follows is irrespective of team days and only depends on the parameters of choice, i.e wastage (dose10)
#'and storage. I think that is a much desirable result
# isocline_plot_sketchy_data <- team_days_monodose_occ_dose10_fcc %>% 
#     mutate(wastage_diag = 1 - (site_data$far_pop/(dose10_capacity * team_days_monodose_occ)))
# 
# #View(isocline_df)
# 
# isocline_plot_sketchy <- ggplot(isocline_plot_data %>% filter(wastage_diag > 1 - sc_model_params$dose10_ovw_mobile_team/100)) +
#     geom_point(aes(x = storage_capacity_ratio,
#                    y = wastage_diag)
#                , size = 2
#                ) +
#     geom_line(aes(x = storage_capacity_ratio,
#                    y = wastage_diag)
#               , size = 1
#     ) 
# 
# isocline_plot_sketchy <- isocline_plot_sketchy + 
#     annotate('text', label = '10-dose', x = 0.25, y = 0.7, size = 4) +
#     annotate('text', label = 'Monodose', x = 0.3, y = 0.75, size = 4)
# 
# isocline_plot_sketchy <- isocline_plot_sketchy + 
#     labs(x = 'Storage capacity ratio (monodose vs 10-dose)',
#          y = 'Open vial wastage (10-dose)'
#          )
# isocline_plot_sketchy <- isocline_plot_sketchy + theme_pubr()
#     
# if(display_sc_plots){
# plot(isocline_plot_sketchy)
# }
# 
# 
# if(save_sc_plots){
#     ggsave(filename = 'mobile_team_days_isocline.png'
#           # , plot = isocline_plot + presentation_plot_theme #uncomment this line to save a powerpoint version
#           , plot = isocline_plot_sketchy
#           , path = './figures/'
#            , width = 9
#            , height = 5)
# }


#'I believe this is the result I've been seeking: The plot eliminates anything relating to the population size
#'and indicates the wastage (10-dose) and storage (monodose) pairs that give the same team days
#'

#' Monodose is worse when the volume capacity < 250
#' 10 dose is worse when the effective doses < 250
#' The relationship that links these two ideas leads to a single equation in
#' two variables - wastage and storage, that is, wastage (10-dose) = 1 - storage 
#' (monodose) / storage (10 dose). 
#' Solving the above in the regions where storage (monodose) < mobile team 
#' performance will yield the following
#' 
isocline_data <- team_days_monodose_occ_dose10_fcc %>%
    filter(monodose_capacity < tp_mobile) %>% 
    mutate(dose10_ovw = 1 - storage_capacity_ratio
           , dose10_effec_dose = 750 * (1 - dose10_ovw / 100)
           )
    

isocline_plot <- ggplot(data = isocline_data) + 
    geom_point(aes(x = storage_capacity_ratio, y = dose10_ovw)) + 
    geom_line(aes(x = storage_capacity_ratio, y = dose10_ovw)) + 
    scale_y_continuous(breaks = round(isocline_data$dose10_ovw, 2), labels = round(isocline_data$dose10_ovw, 2)) 

isocline_plot <- isocline_plot +
    labs(x = 'Storage capacity ratio (monodose over 10-dose)', y = 'Open vial wastage (10-dose)') + 
    annotate('text', label = '10-dose', x = 0.25, y = 0.70, size = 4) +
    annotate('text', label = 'Monodose', x = 0.3, y = 0.75, size = 4)

isocline_plot <- isocline_plot + theme_pubr()

if (display_sc_plots) {
   plot(isocline_plot)  
}   

if(save_sc_plots){
    ggsave(filename = 'figures/isocline_plot.pdf')
}
#' #Other Plots
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
#Reshaping the results and assigning zero wastage to the monodose strategy

team_days_monodose_occ_dose10_fcc_long <- team_days_monodose_occ_dose10_fcc %>%
    select(-dose10_capacity, -monodose_capacity) %>%
    gather(key = 'strategy'
           , value = 'team_days'
           , c('team_days_dose10_fcc', 'team_days_monodose_occ')
           , factor_key = T
           ) %>%  
    dplyr::mutate(dose10_ovw = if_else(strategy == 'team_days_dose10_fcc', dose10_ovw, 0))

#' #I do some further reshaping of the results for 10-dose so I can plot the team days wrt constant wastage rates across the monodose plot
team_days_output_dose10_long <- dplyr::filter(team_days_monodose_occ_dose10_fcc_long
                                                , strategy == 'team_days_dose10_fcc') %>%
    dplyr::mutate(storage_capacity_ratio = paste(storage_capacity_ratio, collapse = ',')) %>%
    separate_rows(storage_capacity_ratio, convert = T)




#' plot to illustrate how the 10-dose and monodose team days intersect at some
#' points and how that leads to an isocline for decision-making
#'

team_days_intersection_plot <- ggplot(data = team_days_monodose_occ_dose10_fcc) +
    geom_point(aes(x = storage_capacity_ratio,
                   y = team_days_monodose_occ),
               color = 'black'
               ) +
    geom_line(aes(x = storage_capacity_ratio,
                   y = team_days_monodose_occ),
               color = 'black'
    ) +
    geom_point(data = team_days_output_dose10_long %>%
                   dplyr::filter(between(team_days
                                         , min(team_days_monodose_occ_dose10_fcc$team_days_monodose_occ) + 0.4
                                         , max(team_days_monodose_occ_dose10_fcc$team_days_monodose_occ)
                                         )
                                 ),
               aes(x = storage_capacity_ratio,
                   y = team_days,
                   color = factor(round(dose10_ovw, 2))
                   )
               ) +
    scale_x_continuous(breaks = seq(0.2, 1, 0.05),
                       labels  = every_nth(seq(0.2, 1, 0.05), 2, inverse = T)
                       ) +
    labs(x = 'Storage capacity ratio (monodose vs 10-dose)',
         y = 'Mobile team days',
         color = 'Open vial wastage (10-dose)')

if(display_sc_plots){
   plot(team_days_intersection_plot)
}


if(save_sc_plots){
 ggsave(filename = 'figures/team_days_intersection_plot.pdf')
}


#################################################################################
# Research days talk plots ----
x_axis_labels_rd_ppt <- c('Full Cold Chain', 'Outside Cold Chain')

campaign_delay_df <- strategy_campaign_prep_delays %>% 
    select(strategy, ft_freezing_time, mt_freezing_time) %>% 
    melt(id = 'strategy', value.name = 'freezing_time')

campaign_delay_df$variable <- ifelse(campaign_delay_df$variable == 'ft_freezing_time', 'fixed_team', 'mobile_team') 
campaign_delay_df

campaign_delay_plot_rd_ppt <- ggplot(data = campaign_delay_df[c(1,4,5,8), ],
                                     aes(x = strategy, y = freezing_time)) +
    geom_bar(aes(fill = variable), 
             color = 'black', 
             stat = "identity", 
             width = 0.25, 
             position = 'dodge') +
    labs(#title = 'Freezing time required per strategy',
        x = '',
        y = "Campaign delay (days)"
    ) + 
    scale_fill_manual(name = "Team type",
                      values = c("royalblue4", "tomato3"),
                      #values = c("aquamarine4", "goldenrod4"),
                      labels = c('Fixed team', 'Mobile team')
                      ) +
    scale_x_discrete(labels = c('','')) + 
    theme_economist() + 
    theme(legend.position = 'none', axis.ticks.x = element_blank()) + 
    presentation_plot_theme

team_days_plot_rd_ppt<- ggplot(data = strategy_team_days_long[c(1,4,5,8), ],
                               aes(x = strategy_name,
                                   y = team_days,
                                   fill = team_type
                               )
) +
    geom_bar(color = 'black', stat = 'identity', position = 'dodge', width = 0.25) +
    # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(#title = 'Number of days per team type and strategy',
        x = 'Strategy'
        ,  y = "Campaign duration"
    ) +
    scale_x_discrete(labels = x_axis_labels_rd_ppt) +
    scale_fill_manual(name = "Team type",
                      #values = c("aquamarine4", "goldenrod4"),
                      values = c("royalblue4", "tomato3"),
                      labels = c('Fixed team', 'Mobile team')
                      ) + theme_economist() + 
    theme(legend.position = 'bottom') + 
    presentation_plot_theme

#arrange on a grid
delays_plot_rd_ppt <- grid.arrange(campaign_delay_plot_rd_ppt,
             # iceVol_plot,
             team_days_plot_rd_ppt,
             nrow = 2)

plot(delays_plot_rd_ppt)

logistical_needs_subset <- strategy_logistical_needs_long[c(1,4,5,8,9,16), ]

#logistical needs between 10-dose FCC and monodose OCC
logistical_needs_subset_plot <- ggplot(data = logistical_needs_subset, aes(x = strategy, y = equip_quantity, fill = equip_name)) +
    geom_bar(stat = 'identity', position = 'dodge') + 
    facet_wrap('team_type', labeller = as_labeller(c('fixed_team' = 'Fixed team', 'mobile_team' = 'Mobile team'))) +
    scale_color_manual(name = "Strategy",
                       # breaks = c('dose10_fcc_parallel', 'part_occ_asap'),
                       values = c("royalblue4", "tomato3"),
                       labels = x_axis_labels_rd_ppt
    ) +
    scale_x_discrete(labels = x_axis_labels_rd_ppt) +
    scale_fill_manual(values = c("forestgreen", "grey27"),
                      name = "Equipment",
                      labels = c('RCW 25', 'Vaccine carrier')) + 
    labs(x = 'Strategy', y = 'Equipment Quantity') +
    theme(legend.position = 'bottom') + 
    presentation_plot_theme 


#arrow plots
#fixed team
near_campaign_period_plot_rd_ppt <- ggplot() + 
    geom_segment(data = near_campaign_period_df[c(1,4), ], 
                 aes(x = ft_freezing_time
                     , xend = ft_team_days + ft_freezing_time
                     , y = 1
                     , yend = 1
                     , color = strategy
                 )
                 , size = 1
                 , arrow = arrow(length = unit(0.06, "npc"), ends = 'both', type = 'closed')
    ) + 
    geom_segment(data = near_campaign_period_df[c(1,4), ], 
                 aes(x = ft_freezing_time
                     , xend = ft_team_days + ft_freezing_time
                     , y = 1.01
                     , yend = 1.01
                     
                 ), color = 'blue'
                 , size = 1
                 , arrow = arrow(length = unit(0.06, "npc"), ends = 'both', type = 'closed')
    ) + scale_x_continuous(breaks = seq(0, 25, 5), labels = seq(0, 25, 5)) + 
    scale_color_manual(name = "Strategy",
                       # breaks = c('dose10_fcc_parallel', 'part_occ_asap'),
                       values = c("royalblue4", "tomato3"),
                       labels = x_axis_labels_rd_ppt
    ) +
    labs(x = 'Campaign period (Fixed team)', y = '') + 
    theme_economist() +
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank()
    ) +
    presentation_plot_theme


near_campaign_period_plot_rd_ppt

#mobile team
far_campaign_period_plot_rd_ppt <- ggplot() + 
    geom_segment(data = far_campaign_period_df[c(1,4), ], 
             aes(x = mt_freezing_time
                 , xend = mt_team_days + mt_freezing_time
                 , y = 1
                 , yend = 1
                 , color = strategy
             )
             , size = 1
             , arrow = arrow(length = unit(0.06, "npc"), ends = 'both', type = 'closed')
) + scale_x_continuous(breaks = seq(0, 15, 3), labels = seq(0, 15, 3)) + 
    scale_color_manual(name = "Strategy",
                     # breaks = c('dose10_fcc_parallel', 'part_occ_asap'),
                      values = c("royalblue4", "tomato3"),
                      labels = x_axis_labels_rd_ppt
                      ) +
    labs(x = 'Campaign period (days)', y = '') + 
    theme_economist() +
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank()
          ) +
    presentation_plot_theme


far_campaign_period_plot_rd_ppt









#total cases bar plot
near_orv_total_cases_mod <- near_orv_total_cases %>% 
    mutate(cases_cumulative_near = cases_cumulative) %>% 
    select(-cases_cumulative)

far_orv_total_cases_mod <- far_orv_total_cases %>% 
    mutate(cases_cumulative_far = cases_cumulative) %>% 
    select(-cases_cumulative)


total_cases_df <- left_join(near_orv_total_cases_mod, far_orv_total_cases_mod, by = 'strategy') %>% 
    mutate(total_cases = sum(cases_cumulative_near, cases_cumulative_far))
    
total_cases_plot_rd_ppt <- ggplot(data = total_cases_df[c(1, 4), ], 
                                  aes(x = strategy, y = total_cases,
                                      fill = x_axis_labels_rd_ppt
                                      )
                                  ) +
    geom_bar(color = 'black', stat = "identity", width = 0.25) +
    scale_fill_manual(name = "Strategy",
                       values = c("Full Cold Chain" = "royalblue4", "Outside Cold Chain" = "tomato3"),
    ) +
    scale_x_discrete(labels = x_axis_labels_rd_ppt) + 
    labs(x = 'Strategy',  
         y = "Total cases"
    ) +
    theme_economist() + presentation_plot_theme

total_cases_plot_rd_ppt

#the region where the two team days intersect are:
# isocline_with_monodose_td <- ggplot(data = team_days_monodose_occ_dose10_fcc %>% filter(team_days_monodose_occ > min(team_days_monodose_occ))) +
#     geom_point(aes(x = storage_capacity_ratio,
#                    y = team_days_monodose_occ),
#                color = 'black'
#     ) +
#     geom_line(aes(x = storage_capacity_ratio,
#                   y = team_days_monodose_occ),
#               color = 'black'
#     )
# 
# isocline_with_dose10_td <- ggplot(data = team_days_monodose_occ_dose10_fcc %>% filter(team_days_dose10_fcc > min(team_days_dose10_fcc, na.rm = T))) +
#     geom_point(aes(x = dose10_ovw,
#                    y = team_days_dose10_fcc),
#                color = 'black'
#     ) +
#     geom_line(aes(x = dose10_ovw,
#                   y = team_days_dose10_fcc),
#               color = 'black'
#     )

#################################################################################
#Sensitivity on number of freezers
#################################################################################

#mf314_quant <- 1:10 #we currently run the sc model on only one freezer. What if the base has more than 1?
