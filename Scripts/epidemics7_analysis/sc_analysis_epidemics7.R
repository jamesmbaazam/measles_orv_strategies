##########################################################
#SUPPLY CHAIN ANALYSIS
###########################################################

#packages

library('dplyr')
library('reshape2')
library('tidyr')

#scripts
source('./scripts/parameters.R') #global parameter list
source('./scripts/analyses_parameters.R') #specific parameter list for this analysis
source('./scripts/supply_chain_functions.R') #functions for performing individual supply chain calculations
source('./scripts/wrappers_supply_chain.R') #functions for running the supply chain model
source('scripts/strategy_list_complete.R')

##########################################################################
#'STRATEGY-SPECIFIC CAMPAIGN DELAY ANALYSIS 
#'TODO: change the for loop to an lapply function for efficiency
##########################################################################
strategy_subset <- c("dose10_fcc_asap", "monodose_fcc_asap", "monodose_occ_asap")
strategy_plot_labels <- c('10-dose FCC', '1-dose FCC', '1-dose OCC')

# Analyse the campaign delays
campaign_delay_results_assump2 <- list()
delay_results_rcw25 <- vector('list', length = length(strategy_subset))

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

#View(campaign_delay_equipment_scenarios)



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

#View(team_days_equipment_scenarios_df)

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




