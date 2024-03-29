options(nwarnings = 10000) #print this many messages if they exist or occur

#helper scripts
source("./scripts/deterministic_framework_analysis/global_scripts/wrappers_supply_chain.R")
source("./scripts/deterministic_framework_analysis/sensitivity_analyses/pop_size/sim_params_pop_size_equal_teams_sensitivity.R")


#packages
library(xlsx)
library(purrr)
library(dplyr)
library(stringr)

#calculate the delay before starting the campaign
campaign_delay_pop_size_equal_teams_sensitivity <- sim_params_pop_size_equal_teams_sensitivity %>% 
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
                n_teams_fixed = n_teams_fixed,
                n_teams_mobile = n_teams_mobile, 
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




## Remove some columns ==== 
campaign_delay_pop_size_equal_teams_sensitivity_trunc <- campaign_delay_pop_size_equal_teams_sensitivity %>% 
    select(-c(near_pop, far_pop, ft_vial_type, ft_equip_type, 
              mt_vial_type, ft_doses_required, mt_doses_required, 
              ft_RCW25, mt_RCW25, ft_vaxCarr, 
              mt_vaxCarr, ft_icepacks_large, mt_icepacks_large, 
              ft_icepacks_small, mt_icepacks_small, team_leaving_first
    )
    )


#calculate expected coverage and the campaign duration per location
campaign_metrics_pop_size_equal_teams_sensitivity <- sim_params_pop_size_equal_teams_sensitivity %>%
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
                n_teams_fixed = n_teams_fixed,
                n_teams_mobile = n_teams_mobile, 
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


## Remove some columns ==== 
campaign_metrics_pop_size_equal_teams_sensitivity_trunc <- campaign_metrics_pop_size_equal_teams_sensitivity %>% 
    select(-c(strategy, 
              location_id, 
              mt_equip_type
    )
    )




#Combine the results from the two analyses
pop_size_equal_teams_sensitivity_sc_analysis_full <- bind_cols(
    campaign_delay_pop_size_equal_teams_sensitivity_trunc,
    campaign_metrics_pop_size_equal_teams_sensitivity_trunc
    ) %>% 
    mutate(
        predeployment_delay = rep(21, times = nrow(campaign_delay_pop_size_equal_teams_sensitivity_trunc
        )
        )
    )


#' Epi input: compounded delays
sc_analysis_pop_size_equal_teams_sensitivity_w_cpd_delays <- pop_size_equal_teams_sensitivity_sc_analysis_full %>%
    group_by(strategy, mt_equip_type, near_pop, far_pop, n_teams_fixed, n_teams_mobile) %>%
        mutate(
            mt_compounded_delay = calc_compounded_delays(
                predeployment_delay,
                   campaign_start,
                   mt_dur_constrained
               ),
               ft_compounded_delay = calc_compounded_delays(
                   predeployment_delay,
                   campaign_start,
                   ft_dur_constrained
               ), 
            strategy = stringr::str_replace(strategy, '_parallel', '')
            )



#Supply chain outcomes: 1. total operational time, 2. Average coverage
sc_analysis_pop_size_equal_teams_sensitivity_summary <- sc_analysis_pop_size_equal_teams_sensitivity_w_cpd_delays %>%
    group_by(strategy, mt_equip_type, near_pop, far_pop, n_teams_fixed, n_teams_mobile) %>%
    summarise(n_locations = length(location_id),
              average_coverage = mean(site_cov_total),
              campaign_duration = sum(site_campaign_dur_constrained) + campaign_start[1],
              near_pop = sum(near_pop),
              far_pop = sum(far_pop),
              .groups = 'drop'
              ) %>%
    mutate(
        cold_chain = as_factor(ifelse(str_detect(strategy, "_fcc"),
                                      "cc",
                                      ifelse(str_detect(strategy, "mixed_"),
                                             "part_cc",
                                             "no_cc"
                                      )
        )),
        vial_type = as_factor(ifelse(str_detect(strategy, "dose10_"),
                                     "dose10",
                                     ifelse(str_detect(strategy, "mixed_"),
                                            "dose10 + monodose",
                                            "monodose"
                                     )
        ))
    )



 #Save the final analysis
saveRDS(sc_analysis_pop_size_equal_teams_sensitivity_w_cpd_delays, 
        file = "./model_output/deterministic_framework_analysis_output/sensitivity_analysis/pop_sizes/sc_analysis_pop_size_equal_teams_sensitivity_full.rds")


saveRDS(sc_analysis_pop_size_equal_teams_sensitivity_summary, 
        file = "./model_output/deterministic_framework_analysis_output/sensitivity_analysis/pop_sizes/sc_analysis_pop_size_equal_teams_sensitivity_summary.rds")

