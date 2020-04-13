library(ggplot2)
library(ggthemes)

#laod plotting data
cases_averted_plot_df <- readRDS('./model_output/sc_epi_analysis_summary_10_teams.rds')

#' visualisations
fig4a_cases_averted <- ggplot(data = cases_averted_plot_df) + 
    geom_text(aes(x = campaign_duration,
                  y = average_coverage,
                  label = round(cases_averted, 2)
                  ),
               check_overlap = TRUE,
              nudge_y = 0.01
              ) +
    geom_jitter(aes(x = campaign_duration, 
                    y = average_coverage, 
                    color = strategy,
                    size = cases_averted), 
                width = 0.15#, 
               # height = 0.025
                ) +
    labs(title = 'Ranking of scenarios by cases averted, expected vaccination coverage, and total operational duration',
         x = 'Campaign duration', 
         y = 'Vaccination coverage') +
    theme_minimal()

plot(fig4a_cases_averted)

# ggsave(filename = './figures/fig4a_cases_averted.pdf', 
#        plot = fig4a_cases_averted, 
#        device = 'pdf'
#        )





