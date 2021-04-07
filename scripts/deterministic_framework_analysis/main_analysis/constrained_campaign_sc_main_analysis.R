options(nwarnings = 10000) #print this many messages if they exist or occur

#helper scripts
source("./scripts/deterministic_framework_analysis/global_scripts/wrappers_supply_chain.R")
source("./scripts/deterministic_framework_analysis/main_analysis/simulation_params_main_analysis.R")


#packages
library(xlsx)
library(purrr)
library(dplyr)

#calculate delays prior to campaign start
campaign_delay_results_main_analysis <- sim_params_table %>%
  rowwise() %>%
  do({
    with(
      .,
      analyse_prep_delay(
        strategy_name = strategy,
        fixed_team_with_dose10 = ft_with_dose10,
        fixed_team_with_ice = ft_with_ice,
        mobile_team_with_dose10 = mt_with_dose10,
        mobile_team_with_ice = mt_with_ice,
        team_dispatch = dispatch,
        site_details = data.frame(location_id = location_id, 
                                  near_pop = near_pop, 
                                  far_pop = far_pop
                                  ),
        fixed_team_equip_type = "both",
        mobile_team_equip_type = mt_equip_type,
        n_teams_fixed = n_ft,
        n_teams_mobile = n_mt, 
        n_fixed_teams_per_site = 2,
        rcw25_ice_replacement_days = sc_model_params$rcw25_ice_replacement_days[1],
        mf314 = sc_model_params$mf314_quant, 
        ambient_temperature = sc_model_params$ambient_temp[1], 
        dose10_vial_volume = sc_model_params$dose10_vial_vol[1], 
        monodose_vial_volume = sc_model_params$monodose_vial_vol[1], 
        res_type = 'detailed',
        browse = F
      )
    )
  }) %>% 
  ungroup() %>% 
  as_tibble()

#' save the results
# saveRDS(campaign_delay_results_msf_params, file = './model_output/deterministic_framework_analysis_output/main_analysis/campaign_delay_results_msf_params.rds')


## Remove some columns ==== 
campaign_delay_results_reduced_main_analysis <- campaign_delay_results_main_analysis %>% 
  select(-c(near_pop, far_pop, ft_vial_type, ft_equip_type, 
            mt_vial_type, ft_doses_required, mt_doses_required, 
            ft_RCW25, mt_RCW25, ft_vaxCarr, 
            mt_vaxCarr, ft_icepacks_large, mt_icepacks_large, 
            ft_icepacks_small, mt_icepacks_small, team_leaving_first
            )
         )


#View(campaign_delay_results_cropped_msf_params)

campaign_metrics_main_analysis <- sim_params_table %>%
  rowwise() %>%
  do({
    with(
      .,
      estim_campaign_metrics(
        strategy_name = strategy,
        fixed_team_with_dose10 = ft_with_dose10,
        fixed_team_with_ice = ft_with_ice,
        mobile_team_with_dose10 = mt_with_dose10,
        mobile_team_with_ice = mt_with_ice,
        site_details = data.frame(
          location_id = location_id,
          near_pop = near_pop,
          far_pop = far_pop
        ),
        mobile_team_equip_type = mt_equip_type,
        n_teams_fixed = n_ft, 
        n_teams_mobile = n_mt, 
        dose10_vial_volume = sc_model_params$dose10_vial_vol[1],
        monodose_vial_volume = sc_model_params$monodose_vial_vol[1],
        site_campaign_dur_constraint = sc_model_params$site_campaign_dur_constraint,
        ft_team_performance = sc_model_params$vax_rate[['fixed_team']],
        mt_team_performance = sc_model_params$vax_rate[['mobile_team']],
        dose10_ovwr_ft = sc_model_params$dose10_ovw_fixed_team,
        monodose_ovwr_ft = sc_model_params$monodose_ovw_fixed_team,
        dose10_ovwr_mt = sc_model_params$dose10_ovw_mobile_team,
        monodose_ovwr_mt = sc_model_params$monodose_ovw_mobile_team,
        browse = F
      )
    )
  }) %>% 
  ungroup() %>% 
  as_tibble()

#' 
#' #' save the results
#' saveRDS(campaign_metrics_msf_params, file = './model_output/deterministic_framework_analysis_output/main_analysis/campaign_metrics_msf_params.rds')


#Combine the two supply chain analyses
sc_main_analysis_results_full <- left_join(
  campaign_delay_results_main_analysis,
  campaign_metrics_main_analysis, 
  by = c("strategy", "location_id", "mt_equip_type", 'near_pop', 'far_pop')
) 


saveRDS(sc_main_analysis_results_full, file = "./model_output/deterministic_framework_analysis_output/main_analysis/sc_main_analysis_results_full.rds")




#' Supply chain outcomes (final): 1. Campaign duration (commencement delay + total time to complete campaign in all locations)
#' 2. Average coverage = mean of the coverage from all locations.
sc_main_analysis_results_summarized <- sc_main_analysis_results_full %>%
  group_by(strategy, mt_equip_type) %>% 
  summarise(strategy = strategy[1],
            mt_equip_type = mt_equip_type[1],
            n_locations = n_distinct(location_id),
            n_fixed_teams = fixed_teams[1],
            n_mobile_teams = mobile_teams[1],
            average_coverage = mean(site_cov_total),
            campaign_duration = campaign_start[1] + sum(site_campaign_dur_constrained),
            .groups = 'drop'
            ) 

saveRDS(sc_main_analysis_results_summarized, 
        file = "./model_output/deterministic_framework_analysis_output/main_analysis/sc_main_analysis_results_summarized.rds"
        )


