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



#1. Transmission dynamics from Location one

location_1_dynamics_plot <- ggplot(data = filter(location_1_dynamics, time <= 150), aes(x = time, y = totalInf, color = strategy)) +
    geom_line(size = 2) 


location_1_dynamics_plot <- location_1_dynamics_plot +
    geom_segment(data = loc_1_campaign_period_df, 
                 aes(x = campaign_start_compounded
                     , xend = campaign_start_compounded + mt_team_days
                     , y = cases_peak
                     , yend = cases_peak
                     , color = strategy
                 )
                 , size = 2
                 , arrow = arrow(length = unit(0.01, "npc"), ends = 'both', type = 'closed')
    )


location_1_dynamics_plot <- location_1_dynamics_plot + 
    scale_color_manual(values = cbbPalette[c(2, 6, 4)], labels = strategy_names_subset_plot_labels) 



location_1_dynamics_plot <- location_1_dynamics_plot + labs(x = 'Time (days)', y = 'Total infected', color = 'Strategy') +
    theme(legend.position = 'top') #+
# presentation_plot_theme


plot(location_1_dynamics_plot)

ggsave(filename = 'figures/transmission_dynamics_location1.png', plot = location_1_dynamics_plot, device = 'png')

