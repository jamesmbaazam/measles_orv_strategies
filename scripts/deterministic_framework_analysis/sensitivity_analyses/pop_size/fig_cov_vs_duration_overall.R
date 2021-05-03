library('tidyverse')
library('conflicted')

#resolve conflicts
conflict_prefer('filter', winner = 'dplyr')



#load the model output
prop_team_alloc_summary <- readRDS('./model_output/deterministic_framework_analysis_output/sensitivity_analysis/pop_sizes/sc_analysis_pop_size_prop_teams_sensitivity_summary.rds')
equal_team_alloc_summary <- readRDS('./model_output/deterministic_framework_analysis_output/sensitivity_analysis/pop_sizes/sc_analysis_pop_size_equal_teams_sensitivity_summary.rds')


#rename some of the columns for easy joining

prop_team_alloc_summary_renamed_cols <- prop_team_alloc_summary %>% 
    select(-c('cold_chain', 'vial_type')) %>% 
    rename('average_coverage_prop_alloc' = 'average_coverage',
           'campaign_duration_prop_alloc' = 'campaign_duration'
    )

equal_team_alloc_summary_renamed_cols <- equal_team_alloc_summary %>% 
    select(-c('cold_chain', 'vial_type')) %>% 
    rename('average_coverage_equal_alloc' = 'average_coverage',
           'campaign_duration_equal_alloc' = 'campaign_duration'
    )

prop_team_alloc_summary_coverage <- prop_team_alloc_summary_renamed_cols %>% 
    group_by(near_pop, far_pop) %>% 
    summarise(overall_coverage_prop_alloc = mean(average_coverage_prop_alloc),
              .groups = 'drop'
              )

equal_team_alloc_summary_coverage <- equal_team_alloc_summary_renamed_cols %>% 
    group_by(near_pop, far_pop) %>% 
    summarise(overall_coverage_equal_alloc = mean(average_coverage_equal_alloc),
              .groups = 'drop'
    )


#combine the data
team_alloc_sensitivity_results <- left_join(prop_team_alloc_summary_coverage, 
                                            equal_team_alloc_summary_coverage,
                                            by = c("near_pop", "far_pop")
                                            )


options("scipen" = 10000, "digits" = 4) #remove scientific notation in plots 

#overall coverage (mean over team allocation scheme and population structure) across the strategies
average_coverage_team_allocation_plot <- ggplot(data = team_alloc_sensitivity_results) + 
geom_point(
    aes(x = near_pop, 
        y = overall_coverage_prop_alloc*100)
           ) + 
geom_point(data = team_alloc_sensitivity_results,
    aes(x = near_pop, 
        y = overall_coverage_equal_alloc*100
        )
    ) +
    geom_line(data = team_alloc_sensitivity_results,
               aes(x = near_pop, 
                   y = overall_coverage_prop_alloc*100,
                   linetype = 'dashed'
                   )
              ) +
    geom_line(data = team_alloc_sensitivity_results,
              aes(x = near_pop, 
                  y = overall_coverage_equal_alloc*100,
                  linetype = 'solid'
                  )
              ) +
    scale_y_continuous(limits = c(0, 100),
                       labels = scales::label_percent(scale = 1)
                       ) +
    scale_linetype_discrete(labels = c('Proportional', 'Equal')) +
    labs(x = 'Near population size (out of 500 000)', 
         y = 'Average vaccination coverage',
         linetype = 'Team allocation'
         ) +
    theme_minimal(base_size = 16)


plot(average_coverage_team_allocation_plot)


ggsave(plot = average_coverage_team_allocation_plot, 
       filename = './figures/deterministic_framework_analysis_figures/sensitivity_analysis/pop_sizes/average_coverage_team_allocation_plot.tiff', 
       width = 23.76, 
       height = 17.86, 
       units = 'cm'
       )
    