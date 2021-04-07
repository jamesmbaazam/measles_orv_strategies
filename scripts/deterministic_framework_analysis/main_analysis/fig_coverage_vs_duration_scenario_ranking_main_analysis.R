library(ggplot2)
library(scales)
library(ggthemes)
library(ggpubr)
library(stringr)
library(forcats)
library(dplyr)
library(extrafont)
library(conflicted)


# set up for publication-ready plots
# font_import() only do this one time - it takes a while
loadfonts(device = "win")
windowsFonts(Arial = windowsFont("Arial"))

# save plots to this directory
plot_path <- "./figures/deterministic_framework_analysis_figures/main_analysis/"

# load plotting data
main_analysis_outcomes_summarized <- readRDS("./model_output/deterministic_framework_analysis_output/main_analysis/sc_main_analysis_results_summarized.rds")

main_analysis_outcomes_summarized_mod <- main_analysis_outcomes_summarized %>%
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
#' 
#' 
#beeswarm plot, a better option than jitter plots
library(ggbeeswarm)

coverage_duration_plot_plain_shapes_beeswarm <- ggplot(data = main_analysis_outcomes_summarized_mod, 
                                                       aes(x = campaign_duration, 
                                                           y = average_coverage
                                                       )) + 
  geom_beeswarm(groupOnX = F,
                aes(shape = mt_equip_type, 
                    fill = vial_type,
                    color = cold_chain
                ),
                size = 4,
                cex = 4.5, 
                stroke = 2
  ) +
  scale_y_continuous(breaks = seq(round(min(main_analysis_outcomes_summarized_mod$average_coverage), 1), 
                                  max(main_analysis_outcomes_summarized_mod$average_coverage), 
                                  0.02
  ),
  labels = percent(seq(round(min(main_analysis_outcomes_summarized_mod$average_coverage), 1), 
                       max(main_analysis_outcomes_summarized_mod$average_coverage), 
                       0.02)
  )
  ) +
  scale_x_continuous(breaks = seq(min(main_analysis_outcomes_summarized_mod$campaign_duration), 
                                  max(main_analysis_outcomes_summarized_mod$campaign_duration), 
                                  length.out = 5
  ),
  labels = seq(min(main_analysis_outcomes_summarized_mod$campaign_duration), 
               max(main_analysis_outcomes_summarized_mod$campaign_duration), 
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
  labs( # title = 'Ranking of scenarios by vaccination coverage and campaign duration',
    x = "Campaign duration (days)",
    y = "Vaccination coverage"
  ) + 
  theme_minimal(base_size = 16) +
  NULL

plot(coverage_duration_plot_plain_shapes_beeswarm)

ggsave(plot = coverage_duration_plot_plain_shapes_beeswarm,
filename = './figures/deterministic_framework_analysis_figures/main_analysis/fig4_cov_vs_duration_main_analysis_shapes.jpg',
width = 23.76,
height = 17.86,
units = 'cm'
)






# set.seed(1234)
# coverage_duration_plot_plain_shapes <- ggplot(data = main_analysis_outcomes_summarized_mod, 
#                                           aes(x = campaign_duration, 
#                                               y = average_coverage
#                                               )
#                                           ) + 
#   geom_jitter(aes(shape = mt_equip_type, 
#                   fill = vial_type,
#                   color = cold_chain
#                   ), 
#               size = 4, 
#               stroke = 2, 
#               height = 0.006, 
#               width = 0.25
#               ) +
#   scale_y_continuous(breaks = seq(round(min(main_analysis_outcomes_summarized_mod$average_coverage), 1), 
#                                   max(main_analysis_outcomes_summarized_mod$average_coverage), 
#                                   0.02
#                                   ),
#   labels = percent(seq(round(min(main_analysis_outcomes_summarized_mod$average_coverage), 1), 
#                        max(main_analysis_outcomes_summarized_mod$average_coverage), 
#                        0.02)
#                    )
#   ) +
#   scale_shape_manual(name = 'Mobile team equipment', 
#                      values = c(21, 24), 
#                      labels = c('rcw25' = 'RCW25', 
#                                 'vaxCarr' = 'Vaccine carrier')
#                      ) +
#   scale_color_manual(name = 'Cold chain option', 
#                      breaks = c('cc', 
#                                 'no_cc', 
#                                 'part_cc'),
#                      labels = c('Cold chain' , 
#                                 'Outside cold chain', 
#                                 'Partial cold chain' ),
#                      values = c('cc' = '#00AFBB', 
#                                 'no_cc' = '#FC4E07', 
#                                 'part_cc' = '#E7B800')
#   ) +
#   scale_fill_manual(name = 'Vial type',
#                     breaks = c('dose10', 
#                                'monodose',
#                                'dose10 + monodose'),
#                     values = c('dose10' = NA, 
#                                'monodose' = NA,
#                                'dose10 + monodose' = NA),
#                     labels = c('dose10' = '10-dose', 
#                                'monodose' = 'Monodose',
#                                'dose10 + monodose' = '10-dose & Monodose')
#                     ) +
#   guides(shape = guide_legend(override.aes = list(size = 6, 
#                                                   stroke = 1.2
#                                                   ), 
#                               order = 1
#                               ), 
#          fill = guide_legend(override.aes = list(size = 6, 
#                                           color = 'black', 
#                                           fill = 'white', 
#                                           shape = 22)
#                              )
#          ) +
#   labs( # title = 'Ranking of scenarios by vaccination coverage and campaign duration',
#     x = "Campaign duration (days)",
#     y = "Vaccination coverage"
#     ) +
#   ggpubr::font('xy.text', face = 'plain') +
#   ggpubr::font('xy.title', face = 'plain') +
#   ggpubr::theme_pubr(legend = 'right', 
#                      base_size = 16,
#                      border = T) +
#   NULL
# 
# plot(coverage_duration_plot_plain_shapes)

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




