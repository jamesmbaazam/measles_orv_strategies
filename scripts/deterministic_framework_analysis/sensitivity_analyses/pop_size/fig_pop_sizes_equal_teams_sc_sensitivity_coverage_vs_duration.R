#A quick plot
#beeswarm plot, a better option than jitter plots
library(ggbeeswarm)
library(scales)

options("scipen" = 10000, "digits" = 4) #remove scientific notation in plots 

#Read in the model output data
sc_analysis_pop_size_equal_teams_sensitivity_summary <- readRDS(file = "./model_output/deterministic_framework_analysis_output/sensitivity_analysis/pop_sizes/sc_analysis_pop_size_equal_teams_sensitivity_summary.rds")


coverage_vs_duration_equal_team_alloc_plot <- ggplot(data = sc_analysis_pop_size_equal_teams_sensitivity_summary, 
                                                       aes(x = campaign_duration, 
                                                           y = average_coverage)) + 
    geom_beeswarm(groupOnX = T,
                  aes(shape = mt_equip_type, 
                      fill = vial_type,
                      color = cold_chain
                  ),
                  size = 2,
                  cex = 5, 
                  stroke = 2
    ) +
    scale_y_continuous(breaks = seq(min(sc_analysis_pop_size_equal_teams_sensitivity_summary$average_coverage), 
                                    max(sc_analysis_pop_size_equal_teams_sensitivity_summary$average_coverage), 
                                    length.out = 5),
                       labels = percent(seq(min(sc_analysis_pop_size_equal_teams_sensitivity_summary$average_coverage), 
                                            max(sc_analysis_pop_size_equal_teams_sensitivity_summary$average_coverage), 
                                            length.out = 5)
                       )
    ) +
    scale_x_continuous(breaks = seq(min(sc_analysis_pop_size_equal_teams_sensitivity_summary$campaign_duration), 
                                    max(sc_analysis_pop_size_equal_teams_sensitivity_summary$campaign_duration), 
                                    length.out = 5
    ),
    labels = seq(min(sc_analysis_pop_size_equal_teams_sensitivity_summary$campaign_duration), 
                 max(sc_analysis_pop_size_equal_teams_sensitivity_summary$campaign_duration), 
                 length.out = 5)
    ) +
    scale_shape_manual(name = 'Mobile team equipment', 
                       values = c(21, 24), 
                       labels = c('rcw25' = 'RCW25', 
                                  'vaxCarr' = 'Vaccine carrier')
    ) +
    scale_color_manual(name = 'Cold chain option', 
                       breaks = c('cc', 
                                  'no_cc', 
                                  'part_cc'),
                       labels = c('Cold chain' , 
                                  'Outside cold chain', 
                                  'Partial cold chain' ),
                       values = c('cc' = '#00AFBB', 
                                  'no_cc' = '#FC4E07', 
                                  'part_cc' = '#E7B800')
    ) +
    scale_fill_manual(name = 'Vial type',
                      breaks = c('dose10', 
                                 'monodose',
                                 'dose10 + monodose'),
                      values = c('dose10' = NA, 
                                 'monodose' = NA,
                                 'dose10 + monodose' = NA),
                      labels = c('dose10' = '10-dose', 
                                 'monodose' = 'Monodose',
                                 'dose10 + monodose' = '10-dose & Monodose')
    ) +
    guides(shape = guide_legend(override.aes = list(size = 6, 
                                                    stroke = 1.2
    ), 
    order = 1
    ), 
    fill = guide_legend(override.aes = list(size = 6, 
                                            color = 'black', 
                                            fill = 'white', 
                                            shape = 22)
    )
    ) +
    labs(
     #   title = 'Strategy ranking by vaccination coverage and campaign duration (equal team type allocation)',
        x = "Campaign duration (days)",
        y = "Vaccination coverage"
    ) +
    facet_wrap(n_teams_fixed + near_pop ~ n_teams_mobile + far_pop) +
    theme_minimal(base_size = 16) +
    NULL

plot(coverage_vs_duration_equal_team_alloc_plot)

#save plot
ggsave(plot = coverage_vs_duration_equal_team_alloc_plot,
       filename = './figures/deterministic_framework_analysis_figures/sensitivity_analysis/pop_sizes/coverage_vs_duration_equal_team_allocation_varying_pop_sensitivity_plot.jpg',
       width = 23.76,
       height = 17.86,
       units = 'cm'
)
