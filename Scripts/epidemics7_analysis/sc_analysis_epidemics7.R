##########################################################
#SUPPLY CHAIN ANALYSIS
###########################################################

#packages
library('conflicted')
library('dplyr')
library('reshape2')
library('tidyr')


#resolve conflicts
conflict_prefer('filter', 'dplyr') #anytime I call the function 'filter', I mean dplyr::filter
conflict_prefer('select', 'dplyr')
conflict_prefer('merge', 'base')

#helper functions and parameters
source('./scripts/parameters.R') #global parameter list
source('./scripts/supply_chain_functions.R') #functions for performing individual supply chain calculations
source('./scripts/wrappers_supply_chain.R') #functions for running the supply chain model
source('./scripts/strategy_list_complete.R')
source('./scripts/analyses_parameters.R') #specific parameter list for this analysis
source('./scripts/epidemics7_analysis/scenarios.R') 
source('./scripts/epidemics7_analysis/simulation_params.R')


##########################################################################
#'STRATEGY-SPECIFIC CAMPAIGN DELAY ANALYSIS 
##########################################################################


campaign_delay_results <- sim_params_table %>%
  rowwise() %>%
  do({
    with(
      .,
      analyse_prep_delay_assump2(
        strategy,
        ft_with_dose10,
        ft_with_ice,
        mt_with_dose10,
        mt_with_ice,
        dispatch,
        data.frame(near_pop = near_pop, far_pop = far_pop),
        "both",
        equip_type,
        1,
        1,
        rcw25_ice_replacement_days = 2
      )
    )
  })

#remove columns I don't need
campaign_delay_results_actual <- campaign_delay_results %>% 
    select(-c(near_pop, far_pop, ft_vial_type, 
              mt_vial_type, ft_doses_required, mt_doses_required, ft_RCW25, 
              mt_RCW25, ft_vaxCarr, mt_vaxCarr, ft_icepacks_large, 
              mt_icepacks_large, ft_icepacks_small, mt_icepacks_small,
              )
           )




##########################################################################
#'STRATEGY-SPECIFIC TEAM DAYS ANALYSIS 
##########################################################################

team_days_results <- sim_params_table %>%
    rowwise() %>%
    do({
        with(.,
             analyse_team_days(
                 strategy,
                 ft_with_dose10,
                 ft_with_ice,
                 mt_with_dose10,
                 mt_with_ice,
                 data.frame(near_pop = near_pop, far_pop = far_pop),
                 mobile_team_equip_type = equip_type
             )
        )
    })


team_days_results_actual <- team_days_results %>% 
    select(-c(near_pop, far_pop, ft_with_ice, mt_with_ice, ft_vial_type, mt_vial_type))


#combine the campaign delays and team days results into one dataframe
sc_analysis_full <- left_join(campaign_delay_results, 
                          team_days_results
                          )

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
    mutate(location_id = rep(site_pops_df$location, times = length(strategy_names_subset)))



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




