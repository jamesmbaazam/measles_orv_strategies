library(ggplot2)
library(ggthemes)
library(stringr)
library(forcats)
library(dplyr)

#load plotting data
cases_averted_df <- readRDS("./model_output/deterministic_framework_analysis_output/sc_epi_analysis_summary_10_teams.rds") %>%
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


#' I want to rearrange the bars so I have to reduce the dimensions
#' by combining some variables
#' 
cases_averted_df_mod <- cases_averted_df %>% 
  unite(col = 'strategy', c(strategy, mt_equip_type), remove = F)

################################################################################
#' visualisations
################################################################################

#' option 1a: label the x-axis with mobile team equip, use vial type as color border
cases_averted_plot_option1a <- ggplot(data = cases_averted_df_mod,
                             aes(x = reorder(strategy, cases_averted),
                                 y = cases_averted/1000
                             )
) + 
  geom_col(aes(fill = cold_chain,
#               linetype = mt_equip_type, 
               color = vial_type),
           size = 1,
           position = position_dodge2(preserve = 'single')
  ) + 
 # scale_linetype_manual(name = 'Mobile team equipment',
 #                       values = c('solid', 'twodash')) +
  scale_fill_brewer(palette = 'Set3', 
                    name = 'Cold chain use', 
                    breaks = c('cc', 
                               'no_cc', 
                               'part_cc'),
                    labels = c('Cold chain' , 
                               'Out of Cold Chain', 
                               'Part Cold Chain' )) + 
  scale_color_brewer(name = 'Vial type', 
                     palette = 'Dark2') + 
  scale_x_discrete(breaks = cases_averted_df_mod$strategy, 
                   labels = cases_averted_df_mod$mt_equip_type) +
  guides(color = guide_legend(override.aes = list(fill = NA)),
         linetype = guide_legend(override.aes = list(fill = NA, col = 'black'))) +
  labs(title = 'Cases averted by mobile team equipment, vial type, and cold chain decision',
       x = 'Mobile team equipment', 
       y = 'Cases averted (thousands)') +
  theme_minimal()

plot(cases_averted_plot_option1a)



#' option 1b: label the x-axis with mobile team equip, use vial type as color border
cases_averted_plot_option1b <- ggplot(data = cases_averted_df_mod,
                                     aes(x = reorder(strategy, cases_averted),
                                         y = cases_averted/1000
                                     )
) + 
  geom_col(aes(fill = cold_chain,
               #               linetype = mt_equip_type, 
               color = vial_type),
           size = 1,
           position = position_dodge2(preserve = 'single')
  ) + 
  # scale_linetype_manual(name = 'Mobile team equipment',
  #                       values = c('solid', 'twodash')) +
  scale_fill_brewer(palette = 'Set3', 
                    name = 'Cold chain use', 
                    breaks = c('cc', 
                               'no_cc', 
                               'part_cc'),
                    labels = c('Cold chain' , 
                               'Out of Cold Chain', 
                               'Part Cold Chain' )) + 
  scale_color_brewer(name = 'Vial type', 
                     palette = 'Dark2') + 
  scale_x_discrete(breaks = cases_averted_df_mod$strategy, 
                   labels = cases_averted_df_mod$mt_equip_type) +
  guides(color = guide_legend(override.aes = list(fill = NA)),
         linetype = guide_legend(override.aes = list(fill = NA, col = 'black'))) +
  labs(title = 'Cases averted by mobile team equipment, vial type, and cold chain decision',
       x = 'Mobile team equipment', 
       y = 'Cases averted (thousands)') +
  theme_minimal() + coord_flip()

plot(cases_averted_plot_option1b)


#' option 2a: flipped, label x-axis with vial type, use linetypes as borders
#' 
cases_averted_plot_option2a <- ggplot(data = cases_averted_df_mod,
                                     aes(x = reorder(strategy, cases_averted),
                                         y = cases_averted/1000
                                     )
) + 
  geom_col(aes(fill = cold_chain,
               linetype = mt_equip_type),
           color = 'black',
           size = 1,
           position = position_dodge2(preserve = 'single')
  ) + 
   scale_linetype_manual(name = 'Mobile team equipment',
                         values = c('solid', 'twodash')) +
  scale_fill_brewer(palette = 'Set3', 
                    name = 'Cold chain use', 
                    breaks = c('cc', 
                               'no_cc', 
                               'part_cc'),
                    labels = c('Cold chain' , 
                               'Out of Cold Chain', 
                               'Part Cold Chain' )) + 
  # scale_color_brewer(name = 'Vial type', 
  #                    palette = 'Dark2') + 
  scale_x_discrete(breaks = cases_averted_df_mod$strategy, 
                   labels = cases_averted_df_mod$vial_type) +
  # theme(axis.title.x=element_blank(),
  #       axis.text.x=element_blank(),
  #       axis.ticks.x=element_blank()) +
  guides(linetype = guide_legend(override.aes = list(fill = NA, col = 'black'))) +
  labs(title = 'Cases averted by mobile team equipment, vial type, and cold chain decision',
       x = 'Vial type choice', 
       y = 'Cases averted (thousands)') +
  theme_minimal() + coord_flip()

plot(cases_averted_plot_option2a)




#' option 2b: label x-axis with vial type, use linetypes as borders
#' 
cases_averted_plot_option2b <- ggplot(data = cases_averted_df_mod,
                                      aes(x = reorder(strategy, cases_averted),
                                          y = cases_averted/1000
                                      )
) + 
  geom_col(aes(fill = cold_chain,
               linetype = mt_equip_type),
           color = 'black',
           size = 1,
           position = position_dodge2(preserve = 'single')
  ) + 
  scale_linetype_manual(name = 'Mobile team equipment',
                        values = c('solid', 'twodash')) +
  scale_fill_brewer(palette = 'Set3', 
                    name = 'Cold chain use', 
                    breaks = c('cc', 
                               'no_cc', 
                               'part_cc'),
                    labels = c('Cold chain' , 
                               'Out of Cold Chain', 
                               'Part Cold Chain' )) + 
  # scale_color_brewer(name = 'Vial type', 
  #                    palette = 'Dark2') + 
  scale_x_discrete(breaks = cases_averted_df_mod$strategy, 
                   labels = cases_averted_df_mod$vial_type) +
  # theme(axis.title.x=element_blank(),
  #       axis.text.x=element_blank(),
  #       axis.ticks.x=element_blank()) +
  guides(linetype = guide_legend(override.aes = list(fill = NA, col = 'black'))) +
  labs(title = 'Cases averted by mobile team equipment, vial type, and cold chain decision',
       x = 'Vial type choice', 
       y = 'Cases averted (thousands)') +
  theme_minimal() 

plot(cases_averted_plot_option2b)





