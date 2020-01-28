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
# 2. Cases averted ####
strategy_cases_averted_plot <- ggplot(strategy_cases_averted, aes(x = location, y = round(cases_averted), fill = strategy)) + 
    geom_bar(stat = 'identity', position = 'dodge') + 
    scale_fill_manual(values = cbbPalette[c(2, 6, 4)], labels = strategy_names_subset_plot_labels) +
    scale_y_continuous(breaks = seq(0, max(strategy_cases_averted$cases_averted), length.out = 5), labels = seq(0, max(strategy_cases_averted$cases_averted), length.out = 5)) +
    labs(x = 'Location', y = 'Cases averted', fill = 'Strategy') +
    theme(legend.position = 'none') #+
#  presentation_plot_theme

plot(strategy_cases_averted_plot)

ggsave(filename = 'figures/strategy_cases_averted_plot.png', plot = strategy_cases_averted_plot, device = 'png')

