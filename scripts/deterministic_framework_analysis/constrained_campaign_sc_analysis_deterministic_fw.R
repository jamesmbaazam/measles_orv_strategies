options(nwarnings = 10000) #print this many messages if they exist or occur

#helper scripts
source("./scripts/wrappers_supply_chain.R")
source("./scripts/deterministic_framework_analysis/simulation_params.R")


#packages
library(xlsx)
library(purrr)
library(dplyr)

#calculate delays prior to campaign start
campaign_delay_results_10_teams <- sim_params_table %>%
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
        n_teams_fixed = 15,#teams$n_ft[1], #10 fixed teams
        n_teams_mobile = 5, #teams$n_mt[1], #10 mobile teams
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
saveRDS(campaign_delay_results_10_teams, file = './model_output/deterministic_framework_analysis_output/campaign_delay_results_10_teams.rds')


## Remove some columns ==== 
campaign_delay_results_cropped_10_teams <- campaign_delay_results_10_teams %>% 
  select(-c(near_pop, far_pop, ft_vial_type, ft_equip_type, 
            mt_vial_type, ft_doses_required, mt_doses_required, 
            ft_RCW25, mt_RCW25, ft_vaxCarr, 
            mt_vaxCarr, ft_icepacks_large, mt_icepacks_large, 
            ft_icepacks_small, mt_icepacks_small, team_leaving_first
            )
         )


#View(campaign_delay_results_cropped_10_teams)

campaign_metrics_10_teams <- sim_params_table %>%
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
        n_teams_fixed = 15, #teams$n_ft[1],
        n_teams_mobile = 5, #teams$n_mt[1],
        dose10_vial_volume = sc_model_params$dose10_vial_vol[1],
        monodose_vial_volume = sc_model_params$monodose_vial_vol[1],
        site_campaign_dur_constraint = sc_model_params$site_campaign_dur_constraint,
        ft_team_performance = sc_model_params$vax_rate[['fixed_team']],
        mt_team_performance = sc_model_params$vax_rate[['mobile_team']],
        dose10_ovwr_mt = sc_model_params$dose10_ovw_mobile_team,
        monodose_ovwr_mt = sc_model_params$monodose_ovw_mobile_team,
        browse = F
      )
    )
  }) %>% 
  ungroup() %>% 
  as_tibble()


#' save the results
saveRDS(campaign_metrics_10_teams, file = './model_output/deterministic_framework_analysis_output/campaign_metrics_10_teams.rds')
#View(campaign_metrics_10_teams)


sc_analysis_10_teams_merged <- left_join(
  campaign_delay_results_cropped_10_teams,
  campaign_metrics_10_teams
) 


#' calculate the total operational time per strategy = time to start a strategy +
#' time to complete a strategy across all locations
#' 
sc_analysis_full_10_teams <- sc_analysis_10_teams_merged %>%
  mutate(total_op_time = campaign_start + site_campaign_dur_constrained) %>%
  as_tibble() 

#View(sc_analysis_full_10_teams)

saveRDS(sc_analysis_full_10_teams, file = "./model_output/deterministic_framework_analysis_output/sc_analysis_full_10_teams.rds")

sc_results_summary_10_teams <- sc_analysis_full_10_teams %>%
  group_split(strategy, mt_equip_type) %>%
  map_df(function(dat) {
    summarise_campaign_metrics(dat)
  })

View(sc_results_summary_10_teams)
saveRDS(sc_results_summary_10_teams, file = "./model_output/deterministic_framework_analysis_output/sc_results_summary_10_teams.rds")
#' write the results to file
#' full supply chain analysis
#' write.xlsx(x = sc_analysis_full_10_teams, 
#'            file = './model_output/sc_analysis_full_10_teams_iid_pops.xlsx'
#'            )
#' 
#' #' summary of the full supply chain analysis
#' write.xlsx(x = sc_results_summary_10_teams, 
#'            file = './model_output/sc_results_summary_10_teams_iid_pops.xlsx'
#'            )
#' 

