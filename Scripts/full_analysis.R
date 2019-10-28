
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

##########################################################
#SUPPLY CHAIN ANALYSIS
###########################################################


##########################################################################
#'STRATEGY-SPECIFIC CAMPAIGN DELAY ANALYSIS 
#'TODO: change the for loop to an lapply function for efficiency
##########################################################################
strategy_names_subset <- c("dose10_fcc_asap", "monodose_fcc_asap", "monodose_occ_asap", "mixed_fcc_asap", "part_occ_asap")


#campaign delay - assumption 1
# campaign_delay_results_assump1 <- list()
# for (i in seq_along(strategy_names_subset)) {
#     campaign_delay_results_assump1[[strategy_names_subset[i]]] <- analyse_prep_delay_assump1(
#         strategy_name = strategy_names_subset[i]
#         , fixed_team_with_dose10 = strategy_analysis_list[[strategy_names_subset[i]]]$fixed_team_with_dose10
#         , fixed_team_with_ice = strategy_analysis_list[[strategy_names_subset[i]]]$fixed_team_with_ice
#         , mobile_team_with_dose10 = strategy_analysis_list[[strategy_names_subset[i]]]$mobile_team_with_dose10
#         , mobile_team_with_ice = strategy_analysis_list[[strategy_names_subset[i]]]$mobile_team_with_ice
#         , team_dispatch = strategy_analysis_list[[strategy_names_subset[i]]]$team_dispatch
#     )
# }

#save to file
# saveRDS(campaign_delay_results_assump1, file = 'model_output/campaign_delay_results_assump1.rds')

far_pop_sizes <- seq(50, 250, length.out = 5)
near_pop_sizes <- rep(1000, times = length(far_pop_sizes))
site_pops_df <- make_site_data(near_pop_sizes, far_pop_sizes)
    

campaign_delay_results_assump2 <- list()
delay_results <- vector('list', length = nrow(site_pops_df))


for (pop_index in 1:nrow(site_pops_df)){
    for (strategy in seq_along(strategy_names_subset)) {
        campaign_delay_results_assump2[[strategy_names_subset[strategy]]] <- analyse_prep_delay_assump2(
            strategy_name = strategy_names_subset[strategy]
            , fixed_team_with_dose10 = strategy_analysis_list[[strategy_names_subset[strategy]]]$fixed_team_with_dose10
            , fixed_team_with_ice = strategy_analysis_list[[strategy_names_subset[strategy]]]$fixed_team_with_ice
            , mobile_team_with_dose10 = strategy_analysis_list[[strategy_names_subset[strategy]]]$mobile_team_with_dose10
            , mobile_team_with_ice = strategy_analysis_list[[strategy_names_subset[strategy]]]$mobile_team_with_ice
            , team_dispatch = strategy_analysis_list[[strategy_names_subset[strategy]]]$team_dispatch
            , site_details = site_pops_df[pop_index, ]
            , site_row = pop_index
            , fixed_team_equip_type = 'both'
            , mobile_team_equip_type = 'vaxCarr'
            , n_teams_fixed = 1
            , n_teams_mobile = 1
        )
    }
    
    delay_results[[pop_index]] = campaign_delay_results_assump2
}

#save to file
saveRDS(campaign_delay_results_assump2, file = 'model_output/campaign_delay_results_assump2.rds')


#Convert the list of dataframes output into a single data frame
strategy_campaign_prep_delays_1 <- do.call(rbind, args = c(delay_results[[1]], make.row.names = F))
strategy_campaign_prep_delays_2 <- do.call(rbind, args = c(delay_results[[2]], make.row.names = F))
strategy_campaign_prep_delays_3 <- do.call(rbind, args = c(delay_results[[3]], make.row.names = F))




strategy_campaign_prep_delays_assump2 <- do.call(rbind, args = c(campaign_delay_results_assump2, make.row.names = F))

#save to file
# saveRDS(strategy_campaign_prep_delays_assump1, file = 'model_output/strategy_campaign_prep_delays_assump1.rds')
saveRDS(strategy_campaign_prep_delays_assump2, file = 'model_output/strategy_campaign_prep_delays_assump2.rds')


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

#save to file
saveRDS(team_days_results, file = 'model_output/team_days_results.rds')

#Convert the list of dataframes output into a single data frame 
strategy_team_days <- do.call(rbind, args = c(team_days_results, make.row.names = F))

#save to file
saveRDS(strategy_team_days, file = 'model_output/strategy_team_days.rds')


#all supply chain results combined
# sc_assump1_analysis_output <- left_join(strategy_campaign_prep_delays_assump1, strategy_team_days, by = 'strategy')
sc_assump2_analysis_output <- left_join(strategy_campaign_prep_delays_assump2, strategy_team_days, by = 'strategy')

#save to file
# saveRDS(sc_assump1_analysis_output, file = 'model_output/sc_assump1_analysis_output.rds')
saveRDS(sc_assump2_analysis_output, file = 'model_output/sc_assump2_analysis_output.rds')

################################################################################
#Data wrangling for epi analyses and plots: convert the wide table to long
################################################################################

#team days results
strategy_team_days_long <- strategy_team_days %>%
    select(-c(ft_vial_type, mt_vial_type)) %>%
    filter(strategy == strategy_names_subset) %>% 
    dplyr::rename(strategy_name = strategy, fixed_team = ft_team_days, mobile_team = mt_team_days) %>% 
    gather('team_type', 'team_days', c(fixed_team, mobile_team)) %>% 
    mutate(team_type = factor(team_type))

#save to file
saveRDS(strategy_team_days_long, file = 'model_output/strategy_team_days_long.rds')



#logistical needs
# strategy_assump1_logistical_needs_long <- sc_assump1_analysis_output %>% 
#     select(strategy, ft_RCW25, ft_vaxCarr, mt_RCW25, mt_vaxCarr) %>%
#     gather('equip_name', 'equip_quantity', 2:5) %>% 
#     separate(col = 'equip_name', into = c('team_type', 'equip_name'), sep = '_') %>% 
#     mutate(team_type = factor(if_else(team_type == 'ft', 'fixed_team', 'mobile_team')))

strategy_assump2_logistical_needs_long <- sc_assump2_analysis_output %>% 
    select(strategy, ft_RCW25, ft_vaxCarr, mt_RCW25, mt_vaxCarr) %>%
    gather('equip_name', 'equip_quantity', 2:5) %>% 
    separate(col = 'equip_name', into = c('team_type', 'equip_name'), sep = '_') %>% 
    mutate(team_type = factor(if_else(team_type == 'ft', 'fixed_team', 'mobile_team')))


#save to file
# saveRDS(strategy_assump1_logistical_needs_long, file = 'model_output/strategy_assump1_logistical_needs_long.rds')
saveRDS(strategy_assump2_logistical_needs_long, file = 'model_output/strategy_assump2_logistical_needs_long.rds')


#' #' Question: If we need more than 1 vaccine carrier for the doses, how do we translate that? Does that translate into
#' #' how many teams we'll need or how many trips should be undertaken by a single team? The latter will draw in the 
#' #' need for a rule for how the distance of the site from the base translates to trips in days and how that will affect
#' #' the team days and campaign duration.
#' 




##########################################################
#EPIDEMIOLOGICAL ANALYSIS
###########################################################


###############################################################################
#Running the simulations for each strategy
###############################################################################

#far/remote location



#running it for different far population sizes
orv_far_strategy_multiple_pops_results <- list()
simulation_pop <- list()
for (pop_size in 1: length(far_pop_sizes)) {
    simulation_pop[[pop_size]] <- far_pop_sizes[pop_size] 
    for (i in 1:length(strategy_names_subset)) {
        orv_far_strategy_multiple_pops_results[[strategy_names_subset[i]]][[far_pop = far_pop]] <- runSimulations(
            R0 = orv_model_params$far_pop_R0 # transmission coeficient
            , run_time = orv_model_params$model_time # 1 yr!
            , pop = initializePop(N = far_pop_sizes[pop_size], initPropImmune = 0.25, I0 = 1)
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
    }

orv_far_strategy_results <- list()
for (i in 1:length(strategy_names_subset)) {
    orv_far_strategy_results[[strategy_names_subset[i]]] <- runSimulations(
        R0 = orv_model_params$far_pop_R0 # transmission coeficient
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

#save to file
saveRDS(orv_far_strategy_results, file = 'model_output/orv_far_strategy_results.rds')


#near/urban location
orv_near_strategy_results <- list()
for (i in 1:length(strategy_names_subset)) {
    orv_near_strategy_results[[strategy_names_subset[i]]] <- runSimulations(
        R0 = orv_model_params$near_pop_R0 # transmission coeficient
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

#save to file
saveRDS(orv_near_strategy_results, file = 'model_output/orv_near_strategy_results.rds')

#################################################################################
#Sensitivity on number of freezers
#################################################################################

#mf314_quant <- 1:10 #we currently run the sc model on only one freezer. What if the base has more than 1?
