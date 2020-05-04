library(ggplot2)
library(ggthemes)
library(stringr)
library(forcats)
library(dplyr)
library(tidyr)
library(conflicted)


#resolve conflicts
conflict_prefer('select', 'dplyr')

#load plotting data
cases_averted_results <- readRDS("./model_output/deterministic_framework_analysis_output/sc_epi_analysis_summary_10_teams.rds") 


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
  select(strategy, mt_equip_type, cold_chain, vial_type, cases_averted)  


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


#' option 3: Group by cold chain choice, sort the cases averted within each group from
#' lowest to highest. Identify each bar by cold chain, vial type, and mobile team
#' equipment type


#' modify the cold chain factor level order
cases_averted_df_mod_cc <- cases_averted_df_mod

cases_averted_df_mod_cc$cold_chain <- factor(cases_averted_df_mod$cold_chain, levels = c('cc', 'part_cc', 'no_cc'))
  
cases_averted_plot_option3 <- ggplot(data = cases_averted_df_mod_cc %>% 
                                       group_by(cold_chain) %>% 
                                       arrange(cases_averted, .by_group = T),
                                      aes(x = cold_chain,
                                          y = cases_averted/1000,
                                          group = cold_chain
                                      )) + 
  geom_col(aes(fill = cold_chain,
               linetype = mt_equip_type, 
               color = vial_type),
           size = 1,
           position = position_dodge2(preserve = 'single')
  ) + 
  scale_linetype_manual(name = 'Mobile team equipment',
                        values = c('solid', 'twodash')) +
  scale_fill_manual(name = 'Cold chain use', 
                    breaks = c('cc', 
                               'no_cc', 
                               'part_cc'),
                    labels = c('Cold chain' , 
                               'Out of Cold Chain', 
                               'Part Cold Chain' ),
                    values = c('cc' = 'seagreen1', 
                               'no_cc' = 'dodgerblue', 
                               'part_cc' = 'plum1')) + 
  scale_color_manual(name = 'Vial type', 
                     breaks = c('dose10', 
                                 'monodose', 
                                 'dose10 + monodose'),
                      values = c('dose10' = 'seagreen4', 
                                'monodose' = 'dodgerblue4', 
                                'dose10 + monodose' = 'plum4')) +
  scale_x_discrete(breaks = cases_averted_df_mod$cold_chain, 
                   labels = ifelse(cases_averted_df_mod$cold_chain == 'cc', 
                                   'Full cold chain',
                                   ifelse(cases_averted_df_mod$cold_chain == 'part_cc',
                                          'Part cold chain', 'Outside cold chain'))) +
  guides(linetype = guide_legend(override.aes = list(fill = NA, col = 'black')),
         color = guide_legend(override.aes = list(fill = NA))) +
  labs(title = 'Cases averted by mobile team equipment, vial type, and cold chain decision',
       x = 'Scenario', 
       y = 'Cases averted (thousands)') +
  theme_minimal() 

plot(cases_averted_plot_option3)



#' lollipop plot - hacked
cases_averted_lollipop_plot <- ggplot(data = cases_averted_df_mod_cc %>% 
                                       group_by(cold_chain) %>% 
                                       arrange(cases_averted, .by_group = T),
                                     aes(group = cold_chain
                                     )) + 
  geom_point(aes(x = reorder(strategy, cases_averted),
                 y = cases_averted/1000, 
                 shape = mt_equip_type), 
             size = 10) + 
  geom_segment(aes(x = strategy, 
                   xend = strategy, 
                   y = 0, 
                   yend = cases_averted/1000,
                   color = cold_chain),
               size = 3.5
               ) +
  scale_shape_manual(values = c(21, 24)) + 
  scale_color_manual(name = 'Cold chain use', 
                    breaks = c('cc', 
                               'no_cc', 
                               'part_cc'),
                    labels = c('Cold chain' , 
                               'Out of Cold Chain', 
                               'Part Cold Chain' ),
                    values = c('cc' = 'seagreen1', 
                               'no_cc' = 'dodgerblue', 
                               'part_cc' = 'plum1')) + 
  scale_x_discrete(breaks = cases_averted_df_mod$cold_chain, 
                   labels = ifelse(cases_averted_df_mod$cold_chain == 'cc', 
                                   'Full cold chain',
                                   ifelse(cases_averted_df_mod$cold_chain == 'part_cc',
                                          'Part cold chain', 'Outside cold chain'))) +
  guides(shape = guide_legend(override.aes = list(size = 5))) +
  labs(title = 'Cases averted by mobile team equipment, vial type, and cold chain decision',
       x = 'Scenario', 
       y = 'Cases averted (thousands)') +
  theme_minimal() 

plot(cases_averted_lollipop_plot)


#' lollipop plot from ggalt package
library(ggalt)

cases_averted_lollipop_plot_v2 <- ggplot(data = cases_averted_df_mod_cc %>% 
                                           group_by(cold_chain) %>% 
                                           arrange(cases_averted, .by_group = T),
                                         aes(group = cold_chain
                                         )) + 
  geom_lollipop(aes(x = reorder(strategy, cases_averted),
                 y = cases_averted/1000, 
                 shape = mt_equip_type),
             size = 5) +
  scale_shape_manual(values = c(21, 24)) + 
  scale_color_manual(name = 'Cold chain use', 
                     breaks = c('cc', 
                                'no_cc', 
                                'part_cc'),
                     labels = c('Cold chain' , 
                                'Out of Cold Chain', 
                                'Part Cold Chain' ),
                     values = c('cc' = 'seagreen1', 
                                'no_cc' = 'dodgerblue', 
                                'part_cc' = 'plum1')) + 
  scale_x_discrete(breaks = cases_averted_df_mod$cold_chain, 
                   labels = ifelse(cases_averted_df_mod$cold_chain == 'cc', 
                                   'Full cold chain',
                                   ifelse(cases_averted_df_mod$cold_chain == 'part_cc',
                                          'Part cold chain', 'Outside cold chain'))) +
  guides(shape = guide_legend(override.aes = list(size = 5))) +
  labs(title = 'Cases averted by mobile team equipment, vial type, and cold chain decision',
       x = 'Scenario', 
       y = 'Cases averted (thousands)') +
  theme_minimal() 

plot(cases_averted_lollipop_plot_v2)






