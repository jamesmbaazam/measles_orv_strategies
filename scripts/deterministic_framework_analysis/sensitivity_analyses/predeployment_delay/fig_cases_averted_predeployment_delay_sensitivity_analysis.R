library(ggplot2)
library(ggbeeswarm)
library(ggthemes)
library(ggpubr)
library(stringr)
library(forcats)
library(dplyr)
library(tidyr)
library(ggalt) #the geom_lollipop function comes from this package
library(extrafont)
library(conflicted)

#resolve conflicts
conflict_prefer('select', 'dplyr')
conflict_prefer('filter', 'dplyr')

#set up for publication-ready plots
#font_import() only do this one time - it takes a while
loadfonts(device = "win")
windowsFonts(Arial = windowsFont("Arial"))
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
cases_averted_predeployment_delay_sensitivity_analysis <- readRDS("./model_output/deterministic_framework_analysis_output/sensitivity_analysis/predeployment_delay/cases_averted_predeployment_delay_sensitivity_analysis.rds") 


#create new columns and remove the old ones we don't need
cases_averted_df <- cases_averted_predeployment_delay_sensitivity_analysis %>% mutate(
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

#' Cases averted (here, the reference is the no orv counterfactual)
cases_averted_predeployment_delay_sensitivity_analysis_plot <- cases_averted_df_cc_mod %>% 
    group_by(cold_chain) %>% 
    ggplot(aes(group = cold_chain)) +
    geom_beeswarm(aes(x = predeployment_delay, 
                   y = cases_averted/1000,
                   color = cold_chain,
                   shape = mt_equip_type,
                   fill = vial_type 
    ),
    size = 4
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
    scale_x_continuous(breaks = seq(21, 84, 7),
                       labels = seq(21, 84, 7)
                       ) +
    scale_y_continuous(breaks = round(seq(min(range(cases_averted_df_cc_mod$cases_averted)[1]/1000),
                                    max(range(cases_averted_df_cc_mod$cases_averted)[2]/1000 + 5),
                                    5
                                    ), 1),
                       labels = round(seq(min(range(cases_averted_df_cc_mod$cases_averted)[1]/1000),
                                          max(range(cases_averted_df_cc_mod$cases_averted)[2]/1000 + 5),
                                          5
                       ), 1)
        
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
    ) + guides(shape = guide_legend(override.aes = list(size = 6, stroke = 1.2), 
                                    order = 1
    ),
    fill = guide_legend(override.aes = list(size = 6, 
                                            color = 'black', 
                                            fill = 'white', 
                                            shape = 22)
    )
    ) +
    labs(x = 'Pre-deployment delay',
         y = 'Cases averted (thousands)'
         ) +
    theme_minimal(base_size = 16)


plot(cases_averted_predeployment_delay_sensitivity_analysis_plot)


ggsave(plot = cases_averted_predeployment_delay_sensitivity_analysis_plot,
       filename = './figures/deterministic_framework_analysis_figures/sensitivity_analysis/predeployment_delay/fig6_cases_averted_predeployment_delay_sensitivity_analysis_plot.eps',
       width = 23.76,
       height = 17.86,
       units = 'cm'
)

