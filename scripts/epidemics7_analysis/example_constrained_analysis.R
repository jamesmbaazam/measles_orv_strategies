source("./scripts/wrappers_supply_chain.R")
source("./scripts/epidemics7_analysis/simulation_params.R")

#teams <- data.frame(ft = 30, mt = 30)

# dose10_occ_campaign_metrics <- estim_campaign_metrics(
#   strategy_name = "dose10_occ_parallel",
#   fixed_team_with_dose10 = T,
#   fixed_team_with_ice = F,
#   mobile_team_with_dose10 = T,
#   mobile_team_with_ice = F,
#   site_details = data.frame(
#     location_id = 1,
#     near_pop = 100000,
#     far_pop = 50000
#   ),
#   mobile_team_equip_type = "vaxCarr",
#   n_teams_fixed = teams$ft,
#   n_teams_mobile = teams$mt,
#   dose10_vial_volume = sc_model_params$dose10_vial_vol[1],
#   monodose_vial_volume = sc_model_params$monodose_vial_vol,
#   site_campaign_dur_constraint = 10,
#   ft_team_performance = 450,
#   mt_team_performance = 250,
#   browse = F
# )
# 
# print(dose10_occ_campaign_metrics)




# estim_campaign_metrics(
#   strategy_name = "dose10_occ_parallel",
#   fixed_team_with_dose10 = T,
#   fixed_team_with_ice = F,
#   mobile_team_with_dose10 = T,
#   mobile_team_with_ice = F,
#   site_details = data.frame(
#     location_id = 1,
#     near_pop = 100000,
#     far_pop = 50000
#   ),
#   mobile_team_equip_type = "vaxCarr",
#   n_teams_fixed = teams$ft,
#   n_teams_mobile = teams$mt,
#   dose10_vial_volume = sc_model_params$dose10_vial_vol[1],
#   monodose_vial_volume = sc_model_params$monodose_vial_vol,
#   site_campaign_dur_constraint = 10,
#   ft_team_performance = 450,
#   mt_team_performance = 250,
#   browse = F
# )



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


# visualisations
time_gained_plot <- campaign_metrics %>% ggplot(aes(near_pop, far_pop)) +
  geom_raster(aes(fill = site_campaign_dur_gain)) +
  facet_wrap(~ strategy + mt_equip_type)

plot(time_gained_plot)