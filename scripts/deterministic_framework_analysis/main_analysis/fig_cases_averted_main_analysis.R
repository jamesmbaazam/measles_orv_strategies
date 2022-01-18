library(ggplot2)
library(ggthemes)
library(ggpubr)
library(stringr)
library(forcats)
library(dplyr)
library(tidyr)
#library(ggalt) #the geom_lollipop function comes from this package
library(extrafont)
library(conflicted)

#resolve conflicts
conflict_prefer('select', 'dplyr')
conflict_prefer('filter', 'dplyr')

#set up for publication-ready plots
#font_import() only do this one time - it takes a while
# loadfonts(device = "win")
 #windowsFonts(Arial = windowsFont("Arial"))
# 
# theme_set(theme_bw(base_size = 18,
#                    base_family = 'Arial'
#                    ) +
#             theme(panel.grid.major = element_blank(),
#                   panel.grid.minor = element_blank(),
#                   axis.line = element_line(color = "black", size = 2)
#                   )
#           )

#save plots to this directory
plot_path <- './figures/deterministic_framework_analysis_figures/'

#load plotting data
cases_averted_results <- readRDS("./model_output/deterministic_framework_analysis_output/main_analysis/cases_averted_main_analysis.rds") 


#create new columns and remove the old ones we don't need
cases_averted_df <- cases_averted_results %>% mutate(
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
    ) %>% 
  select(strategy, mt_equip_type, cold_chain, vial_type, predeployment_delay, cases_averted, relative_cases_averted)  

#' I want to rearrange the bars so I have to reduce the dimensions
#' by combining some variables
#' 
cases_averted_df_mod_strategy <- cases_averted_df %>%
  unite(col = 'strategy', 
        c(strategy, mt_equip_type), 
        remove = F
  )

#' #' modify the cold chain factor level order
cases_averted_df_cc_mod <- cases_averted_df_mod_strategy

cases_averted_df_cc_mod$cold_chain <- factor(cases_averted_df_cc_mod$cold_chain,
                                             levels = c('cc', 'part_cc', 'no_cc')
                                             )

################################################################################
#' visualisations
################################################################################

#' Relative cases averted (here, the 10-dose in full cold chain strategy is the reference strategy)
relative_cases_averted_lollipop_plot <- cases_averted_df_cc_mod %>% 
  group_by(cold_chain) %>% 
  arrange(cases_averted, .by_group = T) %>% 
  ggplot(aes(group = cold_chain)) + 
  geom_linerange(aes(x = order(cold_chain, cases_averted), 
                     ymin = 0, 
                     ymax = ifelse(relative_cases_averted == 0, 
                                   0, 
                                   ifelse(relative_cases_averted < 0, 
                                          (relative_cases_averted/1000 + 0.3), 
                                          (relative_cases_averted/1000) - 0.1)
                     ),
                     color = cold_chain),
                 size = 1.5
  ) +
  geom_point(aes(x = order(cold_chain, relative_cases_averted),
                 y = relative_cases_averted/1000,
                 shape = mt_equip_type,
                 fill = vial_type
  ),
  size = 6,
  stroke = 1.5
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
  scale_x_discrete(breaks = cases_averted_df_cc_mod$cold_chain,
                   labels = ifelse(cases_averted_df_cc_mod$cold_chain == 'cc',
                                   'Full cold chain',
                                   ifelse(cases_averted_df_cc_mod$cold_chain == 'part_cc',
                                          'Part cold chain', 'Outside cold chain')
                   )
  ) +
  scale_y_continuous(breaks = seq(-9, 2, 1),
                     labels = seq(-9, 2, 1)
                     ) +
  guides(shape = guide_legend(override.aes = list(size = 6, stroke = 1.2)
                              ), 
         fill = guide_legend(override.aes = list(size = 6, 
                                          color = 'black', 
                                          fill = 'white', 
                                          shape = 22)
  )
  ) +
  facet_grid(~ cold_chain, 
             scales = 'free_x', 
             switch = 'x', 
             labeller = labeller(cold_chain = c(cc = 'Full cold chain', 
                                                part_cc = 'Partial cold chain', 
                                                no_cc = 'Outside cold chain')
             )
  ) +
  labs(x = 'Strategy', 
       y = 'Cases averted (thousands)',
       fill = 'Vial type'
       ) + 
  theme_minimal(base_size = 16) +
  NULL

plot(relative_cases_averted_lollipop_plot)

ggsave(plot = relative_cases_averted_lollipop_plot,
       filename = './figures/deterministic_framework_analysis_figures/main_analysis/fig5_relative_cases_averted_main_analysis_lollipop_plot.jpg',
       width = 23.76,
       height = 17.86,
       units = 'cm'
)

#' Cases averted (here, the reference is the no orv counterfactual)
cases_averted_lollipop_plot <- cases_averted_df_cc_mod %>% 
  arrange(cases_averted, .by_group = T) %>% 
  ggplot(aes(group = cold_chain)) + 
  geom_linerange(aes(x = order(cold_chain, cases_averted), 
                     ymin = 0, 
                     ymax = ifelse(cases_averted == 0, 
                                   0, 
                                   cases_averted/1000-1
                                   ),
                     color = cold_chain
                     ),
                 size = 1.5
                 ) +
  geom_point(aes(x = order(cold_chain, cases_averted),
                 y = cases_averted/1000,
                 shape = mt_equip_type,
                 fill = vial_type
  ),
  size = 6,
  stroke = 1.5
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
  scale_x_discrete(breaks = cases_averted_df_cc_mod$cold_chain,
                   labels = ifelse(cases_averted_df_cc_mod$cold_chain == 'cc',
                                   'Full cold chain',
                                   ifelse(cases_averted_df_cc_mod$cold_chain == 'part_cc',
                                          'Part cold chain', 'Outside cold chain')
                   )
  ) +
  scale_y_continuous(breaks = round(seq(0, 
                                        max(cases_averted_df_cc_mod$cases_averted)/1000, 
                                        15
                                        ), 1
                                    ),
  labels = round(seq(0, 
                     max(cases_averted_df_cc_mod$cases_averted)/1000, 
                     15), 1
                 )
  ) +
  guides(shape = guide_legend(override.aes = list(size = 6, 
                                                  stroke = 1.2
                                                  )
                              ), 
  fill = guide_legend(override.aes = list(size = 6, 
                                          color = 'black', 
                                          fill = 'white', 
                                          shape = 22)
                      )
  ) +
  facet_grid(~ cold_chain, 
             scales = 'free_x', 
             switch = 'x', 
             labeller = labeller(cold_chain = c(cc = 'Full cold chain', 
                                                part_cc = 'Partial cold chain', 
                                                no_cc = 'Outside cold chain')
             )
  ) +
  labs(x = 'Strategy', 
       y = 'Cases averted (thousands)',
       fill = 'Vial type') + 
  theme_minimal(base_size = 18) +
  NULL

plot(cases_averted_lollipop_plot)

#save plot
ggsave(plot = cases_averted_lollipop_plot,
       filename = './figures/deterministic_framework_analysis_figures/main_analysis/fig5_cases_averted_main_analysis_lollipop_plot.jpg',
       width = 23.76,
       height = 17.86,
       units = 'cm'
       )


#' lollipop plot from ggalt package

# cases_averted_lollipop_plot_with_package <- cases_averted_df_cc_mod %>% 
#   group_by(cold_chain) %>%
#   arrange(cases_averted, .by_group = T) %>%
#   ggplot(aes(group = cold_chain)) + 
#   geom_lollipop(aes(x = order(cold_chain, cases_averted),
#                     y = cases_averted/1000,
#                     shape = mt_equip_type, 
#                     color = cold_chain,
#                     fill = vial_type), 
#                 size = 6) +
#   scale_shape_manual(name = 'Mobile team equipment',
#                      values = c(21, 24),
#                      labels = c('rcw25' = 'RCW25',
#                                 'vaxCarr' = 'Vaccine carrier')
#   ) +
#   scale_color_manual(name = 'Cold chain option', 
#                      breaks = c('cc', 
#                                 'no_cc', 
#                                 'part_cc'),
#                      labels = c('Cold chain' , 
#                                 'Outside cold chain', 
#                                 'Partial cold chain' ),
#                      values = c('cc' = 'seagreen3', 
#                                 'no_cc' = 'dodgerblue3', 
#                                 'part_cc' = 'purple3')) + 
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
#   ) +
#   scale_x_discrete(breaks = cases_averted_df_cc_mod$cold_chain,
#                    labels = ifelse(cases_averted_df_cc_mod$cold_chain == 'cc',
#                                    'Full cold chain',
#                                    ifelse(cases_averted_df_cc_mod$cold_chain == 'part_cc',
#                                           'Part cold chain', 'Outside cold chain'
#                                    )
#                    )
#   ) +
#   scale_y_continuous(breaks = seq(-45, 30, 5),
#                      labels = seq(-45, 30, 5)
#   ) +
#   guides(shape = guide_legend(override.aes = list(size = 6, stroke = 1.2), order = 1), 
#          fill = guide_legend(override.aes = list(size = 6, 
#                                                  color = 'black', 
#                                                  fill = 'white', 
#                                                  shape = 22)
#          )
#   ) +
#   labs(#title = 'Cases averted by mobile team equipment, vial type, and cold chain decision',
#     x = 'Strategy', 
#     y = 'Cases averted (thousands)'
#   ) + 
#   ggpubr::font('xy.text', face = 'plain') +
#   ggpubr::font('xy.title', face = 'plain') +
#   ggpubr::theme_pubr(legend = 'right', base_size = 18) +
#   NULL
# 
# plot(cases_averted_lollipop_plot_with_package)
# 
# ggsave(filename = 'cases_averted_lollipop_plot_with_package_plain_shapes.eps', 
#        plot = cases_averted_lollipop_plot_with_package, 
#        device = "eps", 
#        path = plot_path,
#        width = 28.4,
#        height = 21,
#        units = 'cm'
# )


### Earlier options ----

#' option 1a: label the x-axis with mobile team equip, use vial type as color border
#' cases_averted_plot_option1a <- ggplot(data = cases_averted_df_mod_strategy,
#'                              aes(x = reorder(strategy, cases_averted),
#'                                  y = cases_averted/1000
#'                              )
#' ) + 
#'   geom_col(aes(fill = cold_chain,
#' #               linetype = mt_equip_type, 
#'                color = vial_type),
#'            size = 1,
#'            position = position_dodge2(preserve = 'single')
#'   ) + 
#'  # scale_linetype_manual(name = 'Mobile team equipment',
#'  #                       values = c('solid', 'twodash')) +
#'   scale_fill_brewer(palette = 'Set3', 
#'                     name = 'Cold chain option', 
#'                     breaks = c('cc', 
#'                                'no_cc', 
#'                                'part_cc'),
#'                     labels = c('Cold chain' , 
#'                                'Outside cold chain', 
#'                                'Partial cold chain' )) + 
#'   scale_color_brewer(name = 'Vial type', 
#'                      palette = 'Dark2') + 
#'   scale_x_discrete(breaks = cases_averted_df_mod_strategy$strategy, 
#'                    labels = cases_averted_df_mod_strategy$mt_equip_type) +
#'   guides(color = guide_legend(override.aes = list(fill = NA)),
#'          linetype = guide_legend(override.aes = list(fill = NA, col = 'black'))) +
#'   labs(#title = 'Cases averted by mobile team equipment, vial type, and cold chain decision',
#'        x = 'Mobile team equipment', 
#'        y = 'Cases averted (thousands)') +
#'   ggpubr::font('xy.text', face = 'plain') +
#'   ggpubr::font('xy.title', face = 'plain') +
#'   ggpubr::theme_pubr(legend = 'right', base_size = 18) +
#'   NULL
#' 
#' plot(cases_averted_plot_option1a)
#' 
#' 
#' 
#' #' option 1b: label the x-axis with mobile team equip, use vial type as color border
#' cases_averted_plot_option1b <- ggplot(data = cases_averted_df_mod_strategy,
#'                                      aes(x = reorder(strategy, cases_averted),
#'                                          y = cases_averted/1000
#'                                      )
#' ) + 
#'   geom_col(aes(fill = cold_chain,
#'                #               linetype = mt_equip_type, 
#'                color = vial_type),
#'            size = 1,
#'            position = position_dodge2(preserve = 'single')
#'   ) + 
#'   # scale_linetype_manual(name = 'Mobile team equipment',
#'   #                       values = c('solid', 'twodash')) +
#'   scale_fill_brewer(palette = 'Set3', 
#'                     name = 'Cold chain option', 
#'                     breaks = c('cc', 
#'                                'no_cc', 
#'                                'part_cc'),
#'                     labels = c('Cold chain' , 
#'                                'Outside cold chain', 
#'                                'Partial cold chain' )) + 
#'   scale_color_brewer(name = 'Vial type', 
#'                      palette = 'Dark2') + 
#'   scale_x_discrete(breaks = cases_averted_df_mod_strategy$strategy, 
#'                    labels = cases_averted_df_mod_strategy$mt_equip_type) +
#'   guides(color = guide_legend(override.aes = list(fill = NA)),
#'          linetype = guide_legend(override.aes = list(fill = NA, col = 'black'))) +
#'   labs(#title = 'Cases averted by mobile team equipment, vial type, and cold chain decision',
#'        x = 'Mobile team equipment', 
#'        y = 'Cases averted (thousands)') +
#'   coord_flip() +
#'   ggpubr::font('xy.text', face = 'plain') +
#'   ggpubr::font('xy.title', face = 'plain') +
#'   ggpubr::theme_pubr(legend = 'right', base_size = 18) +
#'   NULL
#' 
#' plot(cases_averted_plot_option1b)
#' 
#' 
#' #' option 2a: flipped, label x-axis with vial type, use linetypes as borders
#' #' 
#' cases_averted_plot_option2a <- ggplot(data = cases_averted_df_mod_strategy,
#'                                      aes(x = reorder(strategy, cases_averted),
#'                                          y = cases_averted/1000
#'                                      )
#' ) + 
#'   geom_col(aes(fill = cold_chain,
#'                linetype = mt_equip_type),
#'            color = 'black',
#'            size = 1,
#'            position = position_dodge2(preserve = 'single')
#'   ) + 
#'    scale_linetype_manual(name = 'Mobile team equipment',
#'                          values = c('solid', 'twodash')) +
#'   scale_fill_brewer(palette = 'Set3', 
#'                     name = 'Cold chain option', 
#'                     breaks = c('cc', 
#'                                'no_cc', 
#'                                'part_cc'),
#'                     labels = c('Cold chain' , 
#'                                'Outside cold chain', 
#'                                'Partial cold chain' )) + 
#'   scale_x_discrete(breaks = cases_averted_df_mod_strategy$strategy, 
#'                    labels = cases_averted_df_mod_strategy$vial_type) +
#'   guides(linetype = guide_legend(override.aes = list(fill = NA, col = 'black'))) +
#'   labs(#title = 'Cases averted by mobile team equipment, vial type, and cold chain decision',
#'        x = 'Vial type choice', 
#'        y = 'Cases averted (thousands)') +
#'   coord_flip() + 
#'   ggpubr::font('xy.text', face = 'plain') +
#'   ggpubr::font('xy.title', face = 'plain') +
#'   ggpubr::theme_pubr(legend = 'right', base_size = 18) +
#'   NULL
#' 
#' plot(cases_averted_plot_option2a)
#' 
#' 
#' 
#' 
#' #' option 2b: label x-axis with vial type, use linetypes as borders
#' #' 
#' cases_averted_plot_option2b <- ggplot(data = cases_averted_df_mod_strategy,
#'                                       aes(x = reorder(strategy, cases_averted),
#'                                           y = cases_averted/1000
#'                                       )
#' ) + 
#'   geom_col(aes(fill = cold_chain,
#'                linetype = mt_equip_type),
#'            color = 'black',
#'            size = 1,
#'            position = position_dodge2(preserve = 'single')
#'   ) + 
#'   scale_linetype_manual(name = 'Mobile team equipment',
#'                         values = c('solid', 'twodash')) +
#'   scale_fill_brewer(palette = 'Set3', 
#'                     name = 'Cold chain option', 
#'                     breaks = c('cc', 
#'                                'no_cc', 
#'                                'part_cc'),
#'                     labels = c('Cold chain' , 
#'                                'Outside cold chain', 
#'                                'Partial cold chain' )) + 
#'   scale_x_discrete(breaks = cases_averted_df_mod_strategy$strategy, 
#'                    labels = cases_averted_df_mod_strategy$vial_type) +
#'   guides(linetype = guide_legend(override.aes = list(fill = NA, col = 'black'))) +
#'   labs(#title = 'Cases averted by mobile team equipment, vial type, and cold chain decision',
#'        x = 'Vial type choice', 
#'        y = 'Cases averted (thousands)') +
#'   ggpubr::font('xy.text', face = 'plain') +
#'   ggpubr::font('xy.title', face = 'plain') +
#'   ggpubr::theme_pubr(legend = 'right', base_size = 18) +
#'   NULL
#' 
#' plot(cases_averted_plot_option2b)
#' 
#' 
#' #' option 3: Group by cold chain choice, sort the cases averted within each group from
#' #' lowest to highest. Identify each bar by cold chain, vial type, and mobile team
#' #' equipment type
#' 
#' 

#' cases_averted_plot_option3 <- cases_averted_df_cc_mod %>% 
#'   group_by(cold_chain) %>% 
#'   arrange(cases_averted, .by_group = T) %>% 
#'   ggplot(aes(group = cold_chain)) + 
#'   geom_col(aes(x = cold_chain, 
#'                y = cases_averted/1000, 
#'                fill = cold_chain,
#'                linetype = mt_equip_type, 
#'                color = vial_type),
#'            size = 1,
#'            position = position_dodge2(preserve = 'single')
#'   ) + 
#'   scale_linetype_manual(name = 'Mobile team equipment',
#'                         values = c('solid', 'twodash')) +
#'   scale_fill_manual(name = 'Cold chain option', 
#'                     breaks = c('cc', 
#'                                'no_cc', 
#'                                'part_cc'),
#'                     labels = c('Cold chain' , 
#'                                'Outside cold chain', 
#'                                'Partial cold chain' ),
#'                     values = c('cc' = 'seagreen1', 
#'                                'no_cc' = 'dodgerblue', 
#'                                'part_cc' = 'plum1')) + 
#'   scale_color_manual(name = 'Vial type', 
#'                      breaks = c('dose10', 
#'                                  'monodose', 
#'                                  'dose10 + monodose'),
#'                       values = c('dose10' = 'seagreen4', 
#'                                 'monodose' = 'dodgerblue4', 
#'                                 'dose10 + monodose' = 'plum4')) +
#'   scale_x_discrete(breaks = cases_averted_df_cc_mod$cold_chain, 
#'                    labels = ifelse(cases_averted_df_cc_mod$cold_chain == 'cc', 
#'                                    'Full cold chain',
#'                                    ifelse(cases_averted_df_cc_mod$cold_chain == 'part_cc',
#'                                           'Part cold chain', 'Outside cold chain'))) +
#'   guides(linetype = guide_legend(override.aes = list(fill = NA, col = 'black')),
#'          color = guide_legend(override.aes = list(fill = NA))) +
#'   labs(#title = 'Cases averted by mobile team equipment, vial type, and cold chain decision',
#'        x = 'Strategy', 
#'        y = 'Cases averted (thousands)') +
#'   ggpubr::font('xy.text', face = 'plain') +
#'   ggpubr::font('xy.title', face = 'plain') +
#'   ggpubr::theme_pubr(legend = 'right', base_size = 18) +
#'   NULL
#' 
#' plot(cases_averted_plot_option3)







