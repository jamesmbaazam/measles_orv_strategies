library(ggplot2)
library(ggthemes)

#laod plotting data
cases_averted_df <- readRDS('./model_output/sc_epi_analysis_summary_10_teams.rds')

#' visualisations
fig4a_cases_averted <- ggplot(data = cases_averted_df) + 
    geom_jitter(aes(x = campaign_duration, 
                    y = average_coverage, 
                    color = mt_equip_type,
                    size = cases_averted), 
                width = 0.15, 
                height = 0.025
    ) +
    labs(title = 'Campaign duration and average vaccination coverage for 10 fixed post and 10 mobile teams',
         x = 'Campaign duration', 
         y = 'Vaccination coverage') +
    theme_minimal()

plot(fig4a_cases_averted)

# ggsave(filename = './figures/fig4a_cases_averted.pdf', 
#        plot = fig4a_cases_averted, 
#        device = 'pdf'
#        )





