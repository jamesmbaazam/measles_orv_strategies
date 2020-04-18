library(ggplot2)
library(ggthemes)
library(stringr)
library(forcats)
library(dplyr)

#load plotting data
results_summary_df <- readRDS("./model_output/deterministic_framework_analysis_output/sc_epi_analysis_summary_10_teams.rds") %>%
  mutate(
    cold_chain = as_factor(ifelse(str_detect(strategy, "_fcc"),
      "cc",
      ifelse(str_detect(strategy, "mixed_"),
        "part_cc",
        "no_cc"
      )
    )),
    vial_type = as_factor(ifelse(str_detect(strategy, "dose10_"),
      "dose10",
      ifelse(str_detect(strategy, "mixed_"),
        "dose10 + monodose",
        "monodose"
      )
    ))
  )


#' visualisations
coverage_duration_plot <- ggplot(data = results_summary_df, 
                                 aes(x = campaign_duration, 
                                     y = average_coverage)
                                 ) + 
    geom_jitter(aes(fill = cold_chain,
                    shape = mt_equip_type, 
                    color = vial_type
                    ),
                size = 5,
                stroke = 2,
                height = 0.005,
                width = 0.25) +
  scale_shape_manual(values = c(21, 24)) + 
  scale_fill_brewer(palette = 'Set3') + 
  scale_color_brewer(palette = 'Dark2') + 
  guides(fill = guide_legend(override.aes = list(shape = c(21), col = NA)),
         colour = guide_legend(override.aes = list(shape = c(22), fill = NA))) +
    labs(title = 'Ranking of scenarios by vaccination coverage and campaign duration',
         x = 'Campaign duration', 
         y = 'Vaccination coverage', 
         shape = 'Mobile team equipment',
         color = 'Vial type',
         fill = 'Cold chain use'
         ) +
    theme_minimal()

plot(coverage_duration_plot)

# ggsave(filename = './figures/coverage_duration_plot.pdf', 
#        plot = coverage_duration_plot, 
#        device = 'pdf'
#        )
