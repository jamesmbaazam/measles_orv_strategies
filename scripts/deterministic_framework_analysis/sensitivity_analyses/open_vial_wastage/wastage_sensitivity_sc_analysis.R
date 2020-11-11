options(nwarnings = 10000) #print this many messages if they exist or occur

#helper scripts
source("./scripts/wrappers_supply_chain.R")
source("./scripts/deterministic_framework_analysis/simulation_params.R")


#packages
library(xlsx)
library(purrr)
library(dplyr)

#calculate delays prior to campaign start
campaign_delay_ovw_sensitivity <- sim_params_table_ovw_sensitivity %>% 
#   slice_tail(n = 50) %>% 
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
                n_teams_fixed = 10,#teams$n_ft[1], #10 fixed teams
                n_teams_mobile = 10, #teams$n_mt[1], #10 mobile teams
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
saveRDS(campaign_delay_ovw_sensitivity, 
        file = './model_output/deterministic_framework_analysis_output/sensitivity_analysis/wastage/campaign_delay_ovw_sensitivity.rds')

## Remove some columns ==== 
campaign_delay_ovw_sensitivity_trunc <- campaign_delay_ovw_sensitivity %>% 
    select(-c(near_pop, far_pop, ft_vial_type, ft_equip_type, 
              mt_vial_type, ft_doses_required, mt_doses_required, 
              ft_RCW25, mt_RCW25, ft_vaxCarr, 
              mt_vaxCarr, ft_icepacks_large, mt_icepacks_large, 
              ft_icepacks_small, mt_icepacks_small, team_leaving_first
              )
           )

#View(campaign_delay_ovw_sensitivity_trunc)

campaign_metrics_ovw_sensitivity <- sim_params_table_ovw_sensitivity %>%
#    slice_tail(n = 50) %>% 
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
                n_teams_fixed = 10, #teams$n_ft[1],
                n_teams_mobile = 10, #teams$n_mt[1],
                dose10_vial_volume = sc_model_params$dose10_vial_vol[1],
                monodose_vial_volume = sc_model_params$monodose_vial_vol[1],
                site_campaign_dur_constraint = sc_model_params$site_campaign_dur_constraint,
                ft_team_performance = sc_model_params$vax_rate[['fixed_team']],
                mt_team_performance = sc_model_params$vax_rate[['mobile_team']],
                dose10_ovwr_mt = sc_model_params$dose10_ovw_mobile_team,
                monodose_ovwr_mt = sc_model_params$monodose_ovw_mobile_team,
                dose10_ovwr_ft = sc_model_params$dose10_ovw_fixed_team,
                monodose_ovwr_ft = sc_model_params$monodose_ovw_fixed_team,
                browse = F
            )
        )
    }) %>% 
    ungroup() %>% 
    as_tibble()


#' save the results
saveRDS(campaign_metrics_ovw_sensitivity, 
        file = './model_output/deterministic_framework_analysis_output/sensitivity_analysis/wastage/campaign_metrics_ovw_sensitivity.rds')
#View(campaign_metrics_10_teams)


campaign_metrics_ovw_sensitivity_trunc <- campaign_metrics_ovw_sensitivity %>% 
                                            select(-c(strategy, 
                                                    location_id, 
                                                    mt_equip_type
                                                    )
                                                 )





ovw_sensitivity_sc_analysis_full <- bind_cols(
    campaign_delay_ovw_sensitivity_trunc,
    select(sim_params_table_ovw_sensitivity, dose10_ovw_ft, dose10_ovw_mt, monodose_ovw_ft, monodose_ovw_mt),
    campaign_metrics_ovw_sensitivity_trunc
) 

#View(ovw_sensitivity_sc_analysis_full)

#' calculate the total operational time per strategy = time to start a strategy +
#' time to complete a strategy across all locations
#' 
sc_analysis_ovw_sensitivity <- ovw_sensitivity_sc_analysis_full %>%
    mutate(total_op_time = campaign_start + site_campaign_dur_constrained) %>%
    as_tibble() 

#View(sc_analysis_ovw_sensitivity)

saveRDS(sc_analysis_ovw_sensitivity, 
        file = "./model_output/deterministic_framework_analysis_output/sensitivity_analysis/wastage/sc_analysis_ovw_sensitivity.rds")

