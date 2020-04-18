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

#' visualisations
cases_averted_plot <- ggplot(data = cases_averted_df,
                             aes(x = cold_chain,
                                 y = cases_averted/1000
                                 )
                             ) + 
    geom_col(aes(fill = cold_chain,
                 linetype = mt_equip_type, 
                 color = vial_type),
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
  scale_color_brewer(name = 'Vial type', 
                     palette = 'Dark2') + 
  guides(color = guide_legend(override.aes = list(fill = NA)),
         linetype = guide_legend(override.aes = list(fill = NA, col = 'black'))) +
    labs(title = 'Cases averted by mobile team equipment, vial type, and cold chain decision',
         x = 'Strategy', 
         y = 'Cases averted (thousands)') +
    theme_minimal()

plot(cases_averted_plot)

# ggsave(filename = './figures/fig4a_cases_averted.pdf', 
#        plot = fig4a_cases_averted, 
#        device = 'pdf'
#        )





