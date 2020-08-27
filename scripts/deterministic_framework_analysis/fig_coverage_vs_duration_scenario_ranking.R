library(ggplot2)
library(scales)
library(ggthemes)
library(ggpubr)
library(stringr)
library(forcats)
library(dplyr)
library(extrafont)
library(conflicted)

# resolve conflicts
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

# set up for publication-ready plots
# font_import() only do this one time - it takes a while
loadfonts(device = "win")
windowsFonts(Arial = windowsFont("Arial"))

# save plots to this directory
plot_path <- "./figures/deterministic_framework_analysis_figures/"

# load plotting data
results_summary_model_output <- readRDS("./model_output/deterministic_framework_analysis_output/sc_epi_analysis_summary_10_teams.rds")

results_summary_df <- results_summary_model_output %>%
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



#' Illustrator version - plain shapes

set.seed(1234)
coverage_duration_plot_plain_shapes <- ggplot(data = results_summary_df, 
                                          aes(x = campaign_duration, 
                                              y = average_coverage
                                              )
                                          ) + 
  geom_jitter(aes(shape = mt_equip_type, 
                  fill = vial_type,
                  color = cold_chain
                  ), 
              size = 6.5, 
              stroke = 2, 
              height = 0.006, 
              width = 0.25
              ) +
  scale_y_continuous(breaks = seq(min(sc_epi_analysis_summary_10_teams$average_coverage), 
                                  max(sc_epi_analysis_summary_10_teams$average_coverage), 
                                  0.02
  ),
  labels = percent(seq(min(sc_epi_analysis_summary_10_teams$average_coverage), 
                       max(sc_epi_analysis_summary_10_teams$average_coverage), 
                       0.02)
                   )
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
  labs( # title = 'Ranking of scenarios by vaccination coverage and campaign duration',
    x = "Campaign duration (days)",
    y = "Vaccination coverage"
    ) +
  ggpubr::font('xy.text', face = 'plain') +
  ggpubr::font('xy.title', face = 'plain') +
  ggpubr::theme_pubr(legend = 'right', 
                     base_size = 16,
                     border = T) +
  NULL

plot(coverage_duration_plot_plain_shapes)

# ggsave(filename = './final_plots/coverage_duration_plot_plain_shapes.eps', 
#        plot = coverage_duration_plot_plain_shapes, 
#        device = "eps", 
#        path = plot_path,
#        width = 25,
#        height = 18.54,
#        units = 'cm',
#        dpi = 300
# )
# 
# ggsave(filename = './final_plots/coverage_duration_plot_plain_shapes.tiff', 
#        plot = coverage_duration_plot_plain_shapes, 
#        device = "tiff", 
#        path = plot_path,
#        width = 25,
#        height = 18.54,
#        units = 'cm',
#        dpi = 300
# )


#beeswarm plot, a better option than jitter plots
library(ggbeeswarm)

coverage_duration_plot_plain_shapes_beeswarm <- ggplot(data = results_summary_df, 
                                                       aes(x = campaign_duration, 
                                                           y = average_coverage
                                                       )) + 
  geom_beeswarm(groupOnX = F,
                aes(shape = mt_equip_type, 
                  fill = vial_type,
                  color = cold_chain
                  ),
                size = 8,
                cex = 4.5, 
                stroke = 2
                ) +
  scale_y_continuous(breaks = seq(min(sc_epi_analysis_summary_10_teams$average_coverage), 
                                  max(sc_epi_analysis_summary_10_teams$average_coverage), 
                                  length.out = 5),
  labels = percent(seq(min(sc_epi_analysis_summary_10_teams$average_coverage), 
                       max(sc_epi_analysis_summary_10_teams$average_coverage), 
                       length.out = 5)
                   )
  ) +
  scale_x_continuous(breaks = seq(min(sc_epi_analysis_summary_10_teams$campaign_duration), 
                                  max(sc_epi_analysis_summary_10_teams$campaign_duration), 
                                  5
  ),
  labels = seq(min(sc_epi_analysis_summary_10_teams$campaign_duration), 
                       max(sc_epi_analysis_summary_10_teams$campaign_duration), 
                       5)
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
  labs( # title = 'Ranking of scenarios by vaccination coverage and campaign duration',
    x = "Campaign duration (days)",
    y = "Vaccination coverage"
  ) +
  ggpubr::font('xy.text', face = 'plain') +
  ggpubr::font('xy.title', face = 'plain') +
  ggpubr::theme_pubr(legend = 'right', 
                     base_size = 16,
                     border = T) +
  NULL

plot(coverage_duration_plot_plain_shapes_beeswarm)



#' visualisations
# coverage_duration_plot <- ggplot(data = results_summary_df,
#                                  aes(x = campaign_duration,
#                                      y = average_coverage)
# ) +
#   geom_jitter(aes(fill = cold_chain,
#                   shape = mt_equip_type,
#                   color = vial_type
#   ),
#   size = 5,
#   stroke = 2,
#   height = 0.0085,
#   width = 0.25) +
#   scale_shape_manual(values = c(21, 24)) +
#   scale_fill_brewer(palette = 'Set3') +
#   scale_color_brewer(palette = 'Dark2') +
#   guides(fill = guide_legend(override.aes = list(shape = c(21), col = NA)),
#          colour = guide_legend(override.aes = list(shape = c(22), fill = NA))) +
#   labs(title = 'Ranking of scenarios by vaccination coverage and campaign duration',
#        x = 'Campaign duration',
#        y = 'Vaccination coverage',
#        shape = 'Mobile team equipment',
#        color = 'Vial type',
#        fill = 'Cold chain use'
#   ) +
#   theme_minimal()
#
# plot(coverage_duration_plot)
