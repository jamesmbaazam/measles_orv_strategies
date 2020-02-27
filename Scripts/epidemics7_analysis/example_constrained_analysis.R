source('./scripts/wrappers_supply_chain.R')

teams <- data.frame(ft = 30, mt = 30)


dose10_occ_parallel_prep_delay <- analyse_prep_delay(
  strategy_name = "dose10_occ_parallel",
  fixed_team_with_dose10 = T,
  fixed_team_with_ice = F,
  mobile_team_with_dose10 = T,
  mobile_team_with_ice = F,
  team_dispatch = "parallel",
  site_details = data.frame(
    location_id = 1,
    near_pop = 100000,
    far_pop = 50000
  ),
  fixed_team_equip_type = "both",
  mobile_team_equip_type = "vaxCarr",
  n_teams_fixed = teams$ft,
  n_teams_mobile = teams$mt,
  mf314 = 1,
  rcw25_ice_replacement_days = 2,
  ambient_temperature = sc_model_params$ambient_temp[1],
  dose10_vial_volume = sc_model_params$dose10_vial_vol[1],
  monodose_vial_volume = sc_model_params$monodose_vial_vol,
  res_type = "simple"
)

print(dose10_occ_parallel_prep_delay)

dose10_occ_campaign_metrics <- estim_campaign_metrics(
  strategy_name = "dose10_occ_parallel",
  fixed_team_with_dose10 = T,
  fixed_team_with_ice = F,
  mobile_team_with_dose10 = T,
  mobile_team_with_ice = F,
  site_details = data.frame(
    location_id = 1,
    near_pop = 100000,
    far_pop = 50000
  ),
  mobile_team_equip_type = "vaxCarr",
  n_teams_fixed = teams$ft,
  n_teams_mobile = teams$mt,
  dose10_vial_volume = sc_model_params$dose10_vial_vol[1],
  monodose_vial_volume = sc_model_params$monodose_vial_vol,
  site_campaign_dur = 10,
  ft_team_performance = 450,
  mt_team_performance = 250,
  browse = F
)

print(dose10_occ_campaign_metrics)
