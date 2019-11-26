
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
strategy_names_subset <- c("dose10_fcc_asap", "monodose_fcc_asap", "monodose_occ_asap")
strategy_names_subset_plot_labels <- c('10 dose FCC', 'Monodose FCC', 'Monodose OCC')

#Location characteristics
far_pop_sizes <- rep(1000, times = 5)
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



compound_delays <- function(delays, team_days){
    cpd_delays <- rep(NA, length(delays))
    cpd_delays[1] <- delays[1] 
    
    for (i in 2: length(delays)) {
        cpd_delays[i] <- delays[1] + sum(team_days[1:i-1]) 
    }
    return(cpd_delays) 
} 


sc_results_split_modified <- sc_results %>% 
    group_split(strategy) %>% 
    map(function(dat){mutate(dat, campaign_start_compounded = compound_delays(campaign_start, mt_team_days))})


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



#running it for different far population sizes
#orv_far_pop_dynamics <- vector('list', nrow(sc_results))

#far_pop_size <- vector('list', nrow(site_pops_df))

# for (fp in 1: nrow(sc_results)) {
#   #  simulation_pop[[pop_size]] <- far_pop_sizes[pop_size] 
#     for (strategy in 1:length(strategy_names_subset)) {
#         orv_far_pop_dynamics[[strategy_names_subset[strategy]]][[far_pop = sc_results$far_pop[fp]]] <- runSimulations(
#             R0 = orv_model_params$far_pop_R0 # transmission coeficient
#             , run_time = orv_model_params$model_time # 1 yr!
#             , pop = initializePop(N = far_pop_sizes[pop_size], initPropImmune = 0.25, I0 = 1)
#             , strategy_name = strategy_names_subset[strategy]
#             , vaxDay = as.numeric(subset(strategy_campaign_prep_delays, strategy == strategy_names_subset[strategy])['mt_freezing_time'])
#             , orv_duration = as.numeric(subset(strategy_team_days_long, strategy_name == strategy_names_subset[strategy] & team_type == 'mobile_team')[ ,'team_days']) #for now we're only looking at the far campaigns 
#             , n_team_type = 2
#             , vax_eff = orv_model_params$vaccine_efficacy
#             , team_performance = ifelse(strategy_analysis_list[[strategy_names_subset[strategy]]][["mobile_team_with_dose10"]], as.numeric(sc_model_params$vax_rate['mobile_team']), ifelse(strategy_analysis_list[[strategy_names_subset[strategy]]][["mobile_team_with_ice"]], 77, 170))
#             , time_to_immunity = orv_model_params$immune_response_timing
#             , browse = F
#         ) 
#     }
#     }

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

#create a column to calculate the cumulative sums of infected cases, to determine the outbreak size
far_orv_add_outbreak_size <- round(as.numeric(unlist(rlist::list.select(orv_far_pop_dynamics, epiTotal))))

#listviewer::jsonedit(far_orv_add_outbreak_size)


orv_far_pop_detailed_dynamics_df <- do.call(rbind, args = c(far_orv_add_outbreak_size, make.row.names = F))

#head(orv_far_pop_detailed_dynamics_df)
#tail(orv_far_pop_detailed_dynamics_df)

#extract the outbreak sizes

orv_far_outbreak_size <- map(orv_far_pop_dynamics, function(x){x[['dynamics']]$epiTotal})

orv_far_results <- sc_result %>% mutate(outbreak_size = round(as.numeric(orv_far_outbreak_size)))

View(orv_far_results)




ggplot(orv_far_results, aes(x = strategy, y = outbreak_size, fill = mt_equip_type)) + 
    geom_bar(stat = 'identity', position = 'dodge') + 
    facet_grid( ~ far_pop) + 
    coord_flip()





# orv_far_strategy_results <- list()
# for (i in 1:length(strategy_names_subset)) {
#     orv_far_strategy_results[[strategy_names_subset[i]]] <- runSimulations(
#         R0 = orv_model_params$far_pop_R0 # transmission coeficient
#         , run_time = orv_model_params$model_time # 1 yr!
#         , pop = initializePop(N = site_data$far_pop, initPropImmune = 0.25, I0 = 1)
#         , strategy_name = strategy_names_subset[i]
#         , vaxDay = as.numeric(subset(strategy_campaign_prep_delays, strategy == strategy_names_subset[i])['mt_freezing_time'])
#         , orv_duration = as.numeric(subset(strategy_team_days_long, strategy_name == strategy_names_subset[i] & team_type == 'mobile_team')[ ,'team_days']) #for now we're only looking at the far campaigns 
#         , n_team_type = 2
#         , vax_eff = orv_model_params$vaccine_efficacy
#         , team_performance = ifelse(strategy_analysis_list[[strategy_names_subset[i]]][["mobile_team_with_dose10"]], as.numeric(sc_model_params$vax_rate['mobile_team']), ifelse(strategy_analysis_list[[strategy_names_subset[i]]][["mobile_team_with_ice"]], 77, 170))
#         , time_to_immunity = orv_model_params$immune_response_timing
#         , browse = F
#     ) 
# }

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
