source("./scripts/wrappers_supply_chain.R")
source("./scripts/epidemics7_analysis/simulation_params.R")


#calculate delays prior to campaign start
campaign_delay_results <- sim_params_table %>%
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
        mobile_team_equip_type = equip_type,
        n_teams_fixed = n_ft,
        n_teams_mobile = n_mt,
        rcw25_ice_replacement_days = 2,
        mf314 = 1, 
        ambient_temperature = sc_model_params$ambient_temp[1], 
        dose10_vial_volume = sc_model_params$dose10_vial_vol[1], 
        monodose_vial_volume = sc_model_params$monodose_vial_vol[1], 
        res_type = 'detailed',
        browse = F
      )
    )
  })


## Remove some columns ==== 
campaign_delay_results_actual <- campaign_delay_results %>% 
  select(-c(near_pop, far_pop, ft_vial_type, ft_equip_type, 
            mt_vial_type, ft_doses_required, mt_doses_required, 
            ft_RCW25, mt_RCW25, ft_vaxCarr, 
            mt_vaxCarr, ft_icepacks_large, mt_icepacks_large, 
            ft_icepacks_small, mt_icepacks_small, team_leaving_first
            )
         )


View(campaign_delay_results_actual)

campaign_metrics <- sim_params_table %>%
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
        mobile_team_equip_type = equip_type,
        n_teams_fixed = n_ft,
        n_teams_mobile = n_mt,
        dose10_vial_volume = sc_model_params$dose10_vial_vol[1],
        monodose_vial_volume = sc_model_params$monodose_vial_vol,
        site_campaign_dur_constraint = 10,
        ft_team_performance = 450,
        mt_team_performance = 250,
        browse = F
      )
    )
  })

View(campaign_metrics)
#library(xlsx)
#write.xlsx(x = campaign_metrics, file = './model_output/campaign_metrics.xlsx')

sc_analysis_merged <- bind_cols(campaign_delay_results_actual, campaign_metrics) %>% 
  select(-c(strategy1, location_id1, mt_equip_type1))

sc_analysis_full <- sc_analysis_merged %>% 
  mutate(total_op_time = sum(campaign_start, site_campaign_dur_constrained))

dim(campaign_metrics)
dim(campaign_delay_results_actual)

dim(sc_analysis_full)
View(sc_analysis_full)

# visualisations
