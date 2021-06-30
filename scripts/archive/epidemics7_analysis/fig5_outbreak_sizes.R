
# 3. Outbreak sizes ####

# Outbreak size (far dynamics) ----

#extract the detailed dynamics
orv_far_pop_detailed_dynamics_list <- unlist(rlist::list.select(orv_far_pop_dynamics, Detailed), recursive = F)
#listviewer::jsonedit(orv_far_pop_detailed_dynamics_list)

#extract the collapsed dynamics
orv_far_pop_collapsed_dynamics_list <- unlist(rlist::list.select(orv_far_pop_dynamics, Collapsed), recursive = F)
#listviewer::jsonedit(orv_far_pop_collapsed_dynamics_list)


#create a column to calculate the cumulative sums of infected cases, to determine the outbreak size
far_orv_outbreak_size <- round(as.numeric(unlist(rlist::list.select(orv_far_pop_dynamics, epiTotal))))

strategy_outbreak_size_df <- data.frame(strategy = sc_results_final$strategy, outbreak_size = far_orv_outbreak_size)
strategy_outbreak_size_df


strategy_outbreak_size_baseline <- filter(strategy_outbreak_size_df, strategy == 'dose10_fcc_asap') %>% 
  select(outbreak_size) %>% unlist(use.names = F)


strategy_outbreak_size_baseline_vec <- rep(strategy_outbreak_size_baseline, times = length(strategy_names_subset))

strategy_cases_averted <- mutate(strategy_outbreak_size_df, 
                                 cases_averted = strategy_outbreak_size_baseline_vec - outbreak_size,
                                 location = rep(site_pops_df$location, times = length(strategy_names_subset)))
strategy_cases_averted

# Plots ----


# Transmission dynamics from the first lcoation of each strategy
location_1_dynamics <- rbind(orv_far_pop_collapsed_dynamics_list[[1]], 
                             orv_far_pop_collapsed_dynamics_list[[6]], 
                             orv_far_pop_collapsed_dynamics_list[[11]]
)


#Time delays and campaign durations
loc_1_delays_df <- filter(sc_results_final, location == 1) %>% 
  select(strategy, campaign_start_compounded, mt_team_days)

loc_1_campaign_period_df <- loc_1_delays_df %>% 
  group_by(strategy) %>% 
  mutate(cases_peak = round(max(location_1_dynamics$totalInf))) %>% 
  ungroup() %>% 
  mutate(cases_peak = round(cases_peak  + c(0, 5, 10)))

strategy_outbreak_size_split <- strategy_cases_averted %>% 
  group_split(strategy) 


strategy_aggregated_outbreak_size <- map(strategy_outbreak_size_split, mutate, aggregated_outbreak_size = cumsum(outbreak_size))

strategy_aggregated_outbreak_size_df <- do.call(rbind, args = strategy_aggregated_outbreak_size)
strategy_aggregated_outbreak_size_df

final_outbreak_sizes <- strategy_aggregated_outbreak_size_df[c(5, 10, 15), c('strategy', 'aggregated_outbreak_size')]
final_outbreak_sizes

location_outbreak_sizes_plot <- ggplot(data = strategy_cases_averted, aes(x = location, y = outbreak_size, fill = strategy), color = 'black') + 
    geom_bar(stat = 'identity', position = 'dodge') + 
    scale_fill_manual(values = cbbPalette[c(2, 6, 4)], labels = strategy_names_subset_plot_labels) +
  #  geom_smooth(method = "auto") +
    labs(x = 'Location', y = 'Outbreak size', fill = 'Strategy') +
    theme(legend.position = 'top') +
    presentation_plot_theme

plot(location_outbreak_sizes_plot)

ggsave(filename = 'figures/location_outbreak_sizes_plot.png', plot = location_outbreak_sizes_plot, device = 'png')
