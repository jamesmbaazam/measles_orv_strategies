
#Supply chain analysis

## Packages ####
library('conflicted')
library('dplyr')
library('reshape2')
library('tidyr')
library('purrr')


## Resolve package conflicts ####
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


# Campaign commencement delay ----

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
        data.frame(location_id = location_id, near_pop = near_pop, far_pop = far_pop),
        "both",
        equip_type,
        1,
        1,
        rcw25_ice_replacement_days = 2
      )
    )
  })

## Remove some columns ==== 
campaign_delay_results_actual <- campaign_delay_results %>% 
    select(-c(near_pop, far_pop, ft_vial_type, 
              mt_vial_type, ft_doses_required, mt_doses_required, ft_RCW25, 
              mt_RCW25, ft_vaxCarr, mt_vaxCarr, ft_icepacks_large, 
              mt_icepacks_large, ft_icepacks_small, mt_icepacks_small
              )
           )



## Team days analysis ----


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
                 data.frame(location_id = location_id, near_pop = near_pop, far_pop = far_pop),
                 mobile_team_equip_type = equip_type
             )
        )
    })


team_days_results_actual <- team_days_results %>% 
    select(-c(ft_with_ice, mt_with_ice, ft_vial_type, mt_vial_type))


## Combine campaign delays and team days results ----
sc_analysis <- left_join(campaign_delay_results_actual, 
                          team_days_results_actual
                          ) %>% ungroup()


## Compounded delays ----

#' split-apply-combine
#' 
sc_results_full <- sc_analysis %>% 
    group_split(strategy, mt_equip_type) %>% 
    map_df(function(dat){mutate(dat, mt_compounded_delay = calc_compounded_delays(campaign_start, mt_team_days))}) 


saveRDS(sc_results_full, file = './model_output/sc_results_full.rds')

rm(list = ls())

#' #' Question: If we need more than 1 vaccine carrier for the doses, how do we 
#' translate that? Does that translate into how many teams we'll need or how many 
#' trips should be undertaken by a single team? The latter will draw in the need 
#' for a rule for how the distance of the site from the base translates to trips 
#' in days and how that will affect the team days and campaign duration.
#' 




