source("./scripts/wrappers_supply_chain.R")
source("./scripts/epidemics7_analysis/simulation_params.R")


#packages
library(xlsx)
library(patchwork)
library(purrr)

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
        mobile_team_equip_type = equip_type,
        n_teams_fixed = teams$n_ft[1],
        n_teams_mobile = teams$n_mt[1],
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
        mobile_team_equip_type = equip_type,
        n_teams_fixed = teams$n_ft[1],
        n_teams_mobile = teams$n_mt[1],
        dose10_vial_volume = sc_model_params$dose10_vial_vol[1],
        monodose_vial_volume = sc_model_params$monodose_vial_vol,
        site_campaign_dur_constraint = 10,
        ft_team_performance = 450,
        mt_team_performance = 250,
        browse = F
      )
    )
  })

#View(campaign_metrics_10_teams)


sc_analysis_10_teams_merged <- bind_cols(campaign_delay_results_cropped_10_teams, 
                                         campaign_metrics_10_teams
                                         ) %>% 
  select(-c(strategy1, location_id1, mt_equip_type1))

sc_analysis_full_10_teams <- sc_analysis_10_teams_merged %>% 
  mutate(total_op_time = sum(campaign_start, site_campaign_dur_constrained)) %>% 
  as_tibble()

#View(sc_analysis_full_10_teams)


sc_results_summary_10_teams <- sc_analysis_full_10_teams %>% 
  group_split(strategy, mt_equip_type) %>% 
  map_df(function(dat){summarise_campaign_metrics(dat)}) 

View(sc_results_summary_10_teams)

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

#' visualisations
ggplot(data = sc_results_summary_10_teams) + 
  geom_jitter(aes(x = campaign_duration, 
                  y = average_coverage, 
                  color = mt_equip,
                  shape = strategy), 
              width = 0.15, 
              height = 0.015
              ) +
  labs(title = 'Campaign duration and average vaccination \n coverage for 10 fixed post and 10 mobile teams',
       x = 'Campaign duration', 
       y = 'Overall vaccination coverage'
       )



