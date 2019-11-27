
#packages

library('dplyr')
library('reshape2')
library('purrr')
library('tidyr')

#scripts
source('./scripts/parameters.R') #global parameter list
source('./scripts/analyses_parameters.R') #specific parameter list for this analysis
source('./scripts/supply_chain_functions.R') #functions for performing individual supply chain calculations
source('./scripts/wrappers_supply_chain.R') #functions for running the supply chain model
source('scripts/measlesFunctions.R') #functions for running the orv model
source('scripts/strategy_list_complete.R')
source('scripts/plotting_functions.R')

##########################################################
#SUPPLY CHAIN ANALYSIS
###########################################################


##########################################################################
#'STRATEGY-SPECIFIC CAMPAIGN DELAY ANALYSIS 
#'TODO: change the for loop to an lapply function for efficiency
##########################################################################
strategy_names_subset <- c("dose10_fcc_asap", "monodose_fcc_asap", "monodose_occ_asap")
strategy_names_subset_plot_labels <- c('10-dose FCC', '1-dose FCC', '1-dose OCC')

#Location characteristics
far_pop_sizes <- rep(2500, times = 5)
near_pop_sizes <- rep(10000, times = length(far_pop_sizes))
site_pops_df <- make_site_data(near_pop_sizes, far_pop_sizes)
site_pops_df <- site_pops_df %>% mutate(location = 1:length(far_pop_sizes))
site_pops_df    


# Analyse the campaign delays
campaign_delay_results_assump2 <- list()
delay_results_rcw25 <- vector('list', length = length(strategy_names_subset))

# Campaign delay: Mobile team equipment scenario analyses ----
# Scenario 1: rcw25 ####
    for (strategy in seq_along(strategy_names_subset)) {
        for (loc in 1:nrow(site_pops_df)){
        campaign_delay_results_assump2[[loc]] <- analyse_prep_delay_assump2(
            strategy_name = strategy_names_subset[strategy]
            , fixed_team_with_dose10 = strategy_analysis_list[[strategy_names_subset[strategy]]]$fixed_team_with_dose10
            , fixed_team_with_ice = strategy_analysis_list[[strategy_names_subset[strategy]]]$fixed_team_with_ice
            , mobile_team_with_dose10 = strategy_analysis_list[[strategy_names_subset[strategy]]]$mobile_team_with_dose10
            , mobile_team_with_ice = strategy_analysis_list[[strategy_names_subset[strategy]]]$mobile_team_with_ice
            , team_dispatch = strategy_analysis_list[[strategy_names_subset[strategy]]]$team_dispatch
            , site_details = site_pops_df[loc, ]
          #  , site_row = loc
            , fixed_team_equip_type = 'both'
            , mobile_team_equip_type = 'rcw25'
            , rcw25_ice_replacement_days = 2
            , n_teams_fixed = 1
            , n_teams_mobile = 1
        )
    }
    
    delay_results_rcw25[[strategy]] = do.call(rbind, args = campaign_delay_results_assump2) %>% 
        mutate(location = site_pops_df$location)
}



# Scenario 2: vaccine carrier ####
delay_results_vaxCarr <- vector('list', length = length(strategy_names_subset))

for (strategy in seq_along(strategy_names_subset)) {
for (loc in 1:nrow(site_pops_df)){
        campaign_delay_results_assump2[[loc]] <- analyse_prep_delay_assump2(
            strategy_name = strategy_names_subset[strategy]
            , fixed_team_with_dose10 = strategy_analysis_list[[strategy_names_subset[strategy]]]$fixed_team_with_dose10
            , fixed_team_with_ice = strategy_analysis_list[[strategy_names_subset[strategy]]]$fixed_team_with_ice
            , mobile_team_with_dose10 = strategy_analysis_list[[strategy_names_subset[strategy]]]$mobile_team_with_dose10
            , mobile_team_with_ice = strategy_analysis_list[[strategy_names_subset[strategy]]]$mobile_team_with_ice
            , team_dispatch = strategy_analysis_list[[strategy_names_subset[strategy]]]$team_dispatch
            , site_details = site_pops_df[loc, ]
            #  , site_row = loc
            , fixed_team_equip_type = 'both'
            , mobile_team_equip_type = 'vaxCarr'
            , rcw25_ice_replacement_days = 2
            , n_teams_fixed = 1
            , n_teams_mobile = 1
        )
    }
    
    delay_results_vaxCarr[[strategy]] = do.call(rbind, args = campaign_delay_results_assump2) %>% 
        mutate(location = site_pops_df$location)
}




# Combine the results from the two into one dataframe

campaign_delay_equipment_scenarios <- do.call(rbind, args = c(delay_results_rcw25, delay_results_vaxCarr)) 

View(campaign_delay_equipment_scenarios)



##########################################################################
#'STRATEGY-SPECIFIC TEAM DAYS ANALYSIS 
#'TODO: change the for loop to an lapply function for efficiency
##########################################################################


# Team days: Mobile team equipment scenario analyses ----
# Scenario 1: rcw25 ####
team_days_rcw25_scenario <- vector('list', length(strategy_names_subset))
team_days_results_tmp <- vector('list', length(strategy_names_subset))

for (strategy in seq_along(strategy_names_subset)) {
for (loc in 1:nrow(site_pops_df)){
        team_days_results_tmp[[loc]] <- analyse_team_days(
            strategy_name = strategy_names_subset[strategy]
            , site_details = site_pops_df[loc, ]
            , mobile_team_equip_type = 'rcw25'
            , fixed_team_with_dose10 = strategy_analysis_list[[strategy_names_subset[strategy]]]$fixed_team_with_dose10
            , fixed_team_with_ice = strategy_analysis_list[[strategy_names_subset[strategy]]]$fixed_team_with_ice
            , mobile_team_with_dose10 = strategy_analysis_list[[strategy_names_subset[strategy]]]$mobile_team_with_dose10
            , mobile_team_with_ice = strategy_analysis_list[[strategy_names_subset[strategy]]]$mobile_team_with_ice
        )
    }
    team_days_rcw25_scenario[[strategy]] <- do.call(rbind, args = team_days_results_tmp) 
}

team_days_rcw25_scenario_df <- do.call(rbind, args = team_days_rcw25_scenario) %>% 
    mutate(location = rep(site_pops_df$location, times = length(strategy_names_subset))) %>% 
    as_tibble()

team_days_rcw25_scenario_df    
    
# Team days: Mobile team equipment scenario analyses ----
# Scenario 2: vaccine carrier ####
team_days_vaxCarr_scenario <- vector('list', length(strategy_names_subset))
team_days_results_tmp <- vector('list', length(strategy_names_subset))

for (strategy in seq_along(strategy_names_subset)) {
    for (loc in 1:nrow(site_pops_df)){
        team_days_results_tmp[[loc]] <- analyse_team_days(
            strategy_name = strategy_names_subset[strategy]
            , site_details = site_pops_df[loc, ]
            , mobile_team_equip_type = 'vaxCarr'
            , fixed_team_with_dose10 = strategy_analysis_list[[strategy_names_subset[strategy]]]$fixed_team_with_dose10
            , fixed_team_with_ice = strategy_analysis_list[[strategy_names_subset[strategy]]]$fixed_team_with_ice
            , mobile_team_with_dose10 = strategy_analysis_list[[strategy_names_subset[strategy]]]$mobile_team_with_dose10
            , mobile_team_with_ice = strategy_analysis_list[[strategy_names_subset[strategy]]]$mobile_team_with_ice
        )
    }
    team_days_vaxCarr_scenario[[strategy]] <- do.call(rbind, args = team_days_results_tmp) 
}

team_days_vaxCarr_scenario_df <- do.call(rbind, args = team_days_vaxCarr_scenario) %>% 
    mutate(location = rep(site_pops_df$location, times = length(strategy_names_subset))) %>% 
    as_tibble()

team_days_vaxCarr_scenario_df

# combine the two equipment scenarios' results of team days into one dataframe
team_days_equipment_scenarios_df <- rbind(team_days_rcw25_scenario_df, team_days_vaxCarr_scenario_df)

View(team_days_equipment_scenarios_df)


#a quick look at the results 
ggplot(data = team_days_rcw25_scenario_df, aes(x = strategy, y = mt_team_days, fill = strategy)) + 
    geom_bar(stat = 'identity') + 
    facet_wrap( ~ location) +
    labs(x = 'Strategy', y = 'Mobile team days', title = 'Team days (Mobile teams use RCW25)') +
    scale_x_discrete(labels = c('10-dose FCC' , 'Monodose FCC', 'Monodose OCC', 'Mixed FCC', 'Part OCC')) +
    theme(legend.position = 'none')

ggplot(data = team_days_vaxCarr_scenario_df, aes(x = strategy, y = mt_team_days, fill = strategy)) + 
    geom_bar(stat = 'identity') + 
    labs(x = 'Strategy', y = 'Mobile team days', title = 'Team days (Mobile teams use vaccine carriers)') +
    scale_x_discrete(labels = c('10-dose FCC' , 'Monodose FCC', 'Monodose OCC', 'Mixed FCC', 'Part OCC')) +
    theme(legend.position = 'none')




#all supply chain results combined
sc_result <- left_join(campaign_delay_equipment_scenarios, team_days_equipment_scenarios_df)

sc_result


################################################################################
#Data wrangling for epi analyses and plots: convert the wide table to long
################################################################################

team_days_analysis_long <- team_days_equipment_scenarios_df %>%
    select(-c(ft_vial_type, mt_vial_type)) %>%
    filter(strategy == strategy_names_subset) %>%
    dplyr::rename(strategy_name = strategy, fixed_team = ft_team_days, mobile_team = mt_team_days) %>%
    gather('team_type', 'team_days', c(fixed_team, mobile_team)) %>%
    mutate(team_type = factor(team_type))

team_days_analysis_long




#campaign delay for rcw25 results
# strategy_delays <- delay_results_rcw25_scenario_df %>%
#     select(strategy, mt_freezing_time, campaign_start) %>%
#     mutate(location = rep(site_pops_df$location, times = length(strategy_names_subset))) %>% 
#     mutate(campaign_start = campaign_start + c(c(0,0,0), campaign_start[4:15]))



campaign_delay_analysis_long <- campaign_delay_equipment_scenarios %>%
     select(strategy, ft_RCW25, ft_vaxCarr, mt_RCW25, mt_vaxCarr) %>%
     gather('equip_name', 'equip_quantity', 2:5) %>%
     separate(col = 'equip_name', into = c('team_type', 'equip_name'), sep = '_') %>%
     mutate(team_type = factor(if_else(team_type == 'ft', 'fixed_team', 'mobile_team')))

campaign_delay_analysis_long

campaign_delay_rcw25_scenario <- filter(campaign_delay_equipment_scenarios, mt_equip_type == 'rcw25') %>% 
    select(strategy, near_pop, far_pop, mt_freezing_time, campaign_start, mt_equip_type)

team_days_rcw25_scenario <- filter(team_days_equipment_scenarios_df, mt_equip_type == 'rcw25') %>% 
    select(strategy, near_pop, far_pop, mt_team_days)



sc_results <- cbind(campaign_delay_rcw25_scenario, select(team_days_rcw25_scenario, -c(strategy, near_pop, far_pop))) %>% 
    mutate(location = rep(site_pops_df$location, times = length(strategy_names_subset)))



#' split the supply chain results based on strategy and apply a function to calculate
#' the compounded delays based on sequential campaigns using one team.
sc_results_split_modified <- sc_results %>% 
    group_split(strategy) %>% 
    map(function(dat){mutate(dat, campaign_start_compounded = calc_compounded_delays(campaign_start, mt_team_days))})


sc_results_final <- do.call(rbind, args = sc_results_split_modified)

sc_results_final

#' #' Question: If we need more than 1 vaccine carrier for the doses, how do we 
#' translate that? Does that translate into how many teams we'll need or how many 
#' trips should be undertaken by a single team? The latter will draw in the need 
#' for a rule for how the distance of the site from the base translates to trips 
#' in days and how that will affect the team days and campaign duration.
#' 




##########################################################
#EPIDEMIOLOGICAL ANALYSIS
###########################################################


###############################################################################
#Running the simulations for each strategy
###############################################################################

#far/remote location

orv_far_pop_dynamics <- vector('list', nrow(sc_results))

#strategy_name <- vector('list', nrow(sc_results))

for (sc_result_row in 1: nrow(sc_results_final)) {
    
   # tp <- ifelse(strategy_analysis_list[[as.character(sc_results$strategy[sc_result_row])]][["mobile_team_with_dose10"]], as.numeric(sc_model_params$vax_rate['mobile_team']), ifelse(strategy_analysis_list[[as.character(sc_results$strategy[sc_result_row])]][["mobile_team_with_ice"]], 616, 1301)) #mobile teams are using rcw25 for monodose campaigns, either with or without ice
    tp <- as.numeric(sc_model_params$vax_rate['mobile_team']) #if all mobile teams can exceed the doses needed to achieve the average daily performance, then it's safe to assume all teams can hit the average team performance
    orv_far_pop_dynamics[[sc_result_row]] <- runSimulations(
            R0 = orv_model_params$far_pop_R0 # transmission coeficient
            , run_time = orv_model_params$model_time # 1 yr!
            , pop = initializePop(N = as.numeric(sc_results_final[sc_result_row, 'far_pop']), initPropImmune = 0.25, I0 = 1)
            , strategy_name = sc_results_final[sc_result_row, 'strategy']
            , vaxDay = as.numeric(sc_results_final[sc_result_row, 'campaign_start_compounded'])
            , orv_duration = as.numeric(sc_results_final[sc_result_row ,'mt_team_days']) #for now we're only looking at the far campaigns 
            , n_team_type = 1
            , vax_eff = orv_model_params$vaccine_efficacy
            , team_performance = tp
            , time_to_immunity = orv_model_params$immune_response_timing
            , browse = F
        ) 
                                               
    }

far_orv_dynamics_unlist <- unlist(orv_far_pop_dynamics, recursive = F)


#Exploring the list elements
listviewer::jsonedit(orv_far_pop_dynamics)


# Outbreak size (far dynamics) ----

#extract the detailed dynamics
orv_far_pop_detailed_dynamics_list <- unlist(rlist::list.select(orv_far_pop_dynamics, Detailed), recursive = F)
listviewer::jsonedit(orv_far_pop_detailed_dynamics_list)

#extract the collapsed dynamics
orv_far_pop_collapsed_dynamics_list <- unlist(rlist::list.select(orv_far_pop_dynamics, Collapsed), recursive = F)
listviewer::jsonedit(orv_far_pop_collapsed_dynamics_list)


#create a column to calculate the cumulative sums of infected cases, to determine the outbreak size
far_orv_outbreak_size <- round(as.numeric(unlist(rlist::list.select(orv_far_pop_dynamics, epiTotal))))

strategy_outbreak_size_df <- data.frame(strategy = sc_results_final$strategy, outbreak_size = far_orv_outbreak_size)
strategy_outbreak_size_df


strategy_outbreak_size_baseline <- filter(strategy_outbreak_size_df, strategy == 'dose10_fcc_asap') %>% 
    select(outbreak_size) %>% unlist(use.names = F)


strategy_outbreak_size_baseline_vec <- rep(strategy_outbreak_size_baseline, times = length(strategy_names_subset))

strategy_cases_averted <- mutate(strategy_outbreak_size_df, 
                                 cases_averted = strategy_outbreak_size_baseline_vec - outbreak_size,
                                 location = rep(site_pops_df$location, times = length(strategy_names_subset)))
strategy_cases_averted

# Plots ----


# Transmission dynamics from the first lcoation of each strategy
location_1_dynamics <- rbind(orv_far_pop_collapsed_dynamics_list[[1]], 
                             orv_far_pop_collapsed_dynamics_list[[6]], 
                             orv_far_pop_collapsed_dynamics_list[[11]]
)


#Time delays and campaign durations
loc_1_delays_df <- filter(sc_results_final, location == 1) %>% 
    select(strategy, campaign_start_compounded, mt_team_days)

loc_1_campaign_period_df <- loc_1_delays_df %>% 
    group_by(strategy) %>% 
    mutate(cases_peak = round(max(location_1_dynamics$totalInf))) %>% 
    ungroup() %>% 
    mutate(cases_peak = round(cases_peak  + c(0, 5, 10)))



#1. Transmission dynamics from Location one

location_1_dynamics_plot <- ggplot(data = filter(location_1_dynamics, time <= 150), aes(x = time, y = totalInf, color = strategy)) +
    geom_line(size = 2) 


location_1_dynamics_plot <- location_1_dynamics_plot +
    geom_segment(data = loc_1_campaign_period_df, 
                                  aes(x = campaign_start_compounded
                                      , xend = campaign_start_compounded + mt_team_days
                                      , y = cases_peak
                                      , yend = cases_peak
                                      , color = strategy
                                      )
                                  , size = 2
                                  , arrow = arrow(length = unit(0.01, "npc"), ends = 'both', type = 'closed')
                                  )


location_1_dynamics_plot <- location_1_dynamics_plot + 
    scale_color_manual(values = cbbPalette[c(2, 6, 4)], labels = strategy_names_subset_plot_labels) 
 
    
    
location_1_dynamics_plot <- location_1_dynamics_plot + labs(x = 'Time (days)', y = 'Total infected') +
    theme(legend.position = 'top') +
    presentation_plot_theme


plot(location_1_dynamics_plot)



# 2. Cases averted ####
strategy_cases_averted_plot <- ggplot(strategy_cases_averted, aes(x = location, y = round(cases_averted), fill = strategy)) + 
    geom_bar(stat = 'identity', position = 'dodge') + 
    scale_fill_manual(values = cbbPalette[c(2, 6, 4)], labels = strategy_names_subset_plot_labels) +
    scale_y_continuous(breaks = seq(0, max(strategy_cases_averted$cases_averted), length.out = 5), labels = seq(0, max(strategy_cases_averted$cases_averted), length.out = 5)) +
    labs(x = 'Location', y = 'Cases averted', fill = 'Strategy') +
    theme(legend.position = 'none') +
    presentation_plot_theme

plot(strategy_cases_averted_plot)


strategy_outbreak_size_split <- strategy_cases_averted %>% 
    group_split(strategy) 


strategy_aggregated_outbreak_size <- map(strategy_outbreak_size_split, mutate, aggregated_outbreak_size = cumsum(outbreak_size))

strategy_aggregated_outbreak_size_df <- do.call(rbind, args = strategy_aggregated_outbreak_size)
strategy_aggregated_outbreak_size_df

final_outbreak_sizes <- strategy_aggregated_outbreak_size_df[c(5, 10, 15), c('strategy', 'aggregated_outbreak_size')]
final_outbreak_sizes

# 3. Outbreak sizes ####
location_outbreak_sizes_plot <- ggplot(data = strategy_cases_averted, aes(x = location, y = outbreak_size, fill = strategy), color = 'black') + 
    geom_bar(stat = 'identity', position = 'dodge') + 
    scale_fill_manual(values = cbbPalette[c(2, 6, 4)], labels = strategy_names_subset_plot_labels) +
  #  geom_smooth(method = "auto") +
    labs(x = 'Location', y = 'Outbreak size', fill = 'Strategy') +
    theme(legend.position = 'top') +
    presentation_plot_theme

plot(location_outbreak_sizes_plot)
