#' packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(conflicted)

#' source helper scripts
source('./scripts/deterministic_framework_analysis/global_scripts/analyses_parameters.R')
source('./scripts/deterministic_framework_analysis/main_analysis/simulation_params_main_analysis.R')

#' Resolve package conflicts 
conflict_prefer('select', 'dplyr')
conflict_prefer('merge', 'base')
conflict_prefer('summarise', 'dplyr')


#'load data and rescale them to the actual numbers
orv_near_dynamics <- readRDS('./model_output/deterministic_framework_analysis_output/main_analysis/orv_near_dynamics_proportions.rds') %>% 
    mutate(S = S*site_pop_dist$near_pop[1],
           E = E*site_pop_dist$near_pop[1],
           I = I*site_pop_dist$near_pop[1],
           R = R*site_pop_dist$near_pop[1],
           K = K*site_pop_dist$near_pop[1]
           )

no_orv_near_dynamics <- readRDS('./model_output/deterministic_framework_analysis_output/main_analysis/no_orv_near_dynamics_proportions.rds') %>% 
    mutate(S = S*site_pop_dist$near_pop[1],
           E = E*site_pop_dist$near_pop[1],
           I = I*site_pop_dist$near_pop[1],
           R = R*site_pop_dist$near_pop[1],
           K = K*site_pop_dist$near_pop[1]
           )

orv_far_dynamics <- readRDS('./model_output/deterministic_framework_analysis_output/main_analysis/orv_far_dynamics_proportions.rds') %>% 
    mutate(S = S*site_pop_dist$far_pop[1],
           E = E*site_pop_dist$far_pop[1],
           I = I*site_pop_dist$far_pop[1],
           R = R*site_pop_dist$far_pop[1],
           K = K*site_pop_dist$far_pop[1]
           )

no_orv_far_dynamics <- readRDS('./model_output/deterministic_framework_analysis_output/main_analysis/no_orv_far_dynamics_proportions.rds') %>% 
    mutate(S = S*site_pop_dist$far_pop[1],
           E = E*site_pop_dist$far_pop[1],
           I = I*site_pop_dist$far_pop[1],
           R = R*site_pop_dist$far_pop[1],
           K = K*site_pop_dist$far_pop[1]
           )


#' quick QA assessment of the epi model run (no orv)
# no_orv_near_dynamics_qa <- no_orv_near_dynamics %>%
#     group_split(strategy, location_id, mt_equip_type, predeployment_delay) %>%
#     pluck(1) 

#line plot of one sample of dynamics
#' ggplot() +
#'     geom_line(data = no_orv_near_dynamics_qa,
#'     aes(x = time, y = S),
#'               colour = 'blue') +
#'     geom_line(data = no_orv_near_dynamics_qa,
#'               aes(x = time,
#'                   y = I
#'               ),
#'               colour = 'red'
#'     ) +
#'     geom_line(data = no_orv_near_dynamics_qa,
#'               aes(x = time,
#'                   y = R
#'               ),
#'               colour = 'green4')
#' 
#' #' quick QA assessment of the epi model run (orv after day 84)
#' orv_near_dynamics_day21_and_84_qa <- orv_near_dynamics %>%
#'     filter((strategy == 'dose10_fcc' & 
#'                 location_id == 1 & 
#'                 mt_equip_type == 'rcw25'
#'             ) & 
#'                (predeployment_delay == 84 | predeployment_delay == 21)
#'            ) %>% 
#'     group_split(strategy, 
#'                 location_id, 
#'                 mt_equip_type, 
#'                 predeployment_delay
#'                 ) %>%
#'     pluck(1) 
#' 
#' #line plot of one sample of dynamics
#' orv_near_dynamics_day84_qa_line_plot <- ggplot(data = orv_near_dynamics_day21_and_84_qa) +
#'     geom_line(aes(x = time, 
#'                   y = S,
#'                   linetype = factor(predeployment_delay)
#'                   ),
#'               color = 'blue',
#'               size = 1.5
#'               ) +
#'     geom_line(data = orv_near_dynamics_day21_and_84_qa,
#'               aes(x = time,
#'                   y = I,
#'                   linetype = factor(predeployment_delay)
#'               ),
#'               color = 'red',
#'               size = 1.5
#'               ) +
#'     geom_line(data = orv_near_dynamics_day21_and_84_qa,
#'               aes(x = time,
#'                   y = R,
#'                   linetype = factor(predeployment_delay)
#'               ),
#'               color = 'green4',
#'               size = 1.5
#'               ) +
#'     labs(title = 'ORV after 21 and 84 days of predeployment delay',
#'          linetype = 'Pre-deployment delay')
#' 
#' plot(orv_near_dynamics_day84_qa_line_plot)
#' 
#' ggsave(plot = orv_near_dynamics_day84_qa_line_plot,
#'        filename = './figures/deterministic_framework_analysis_figures/main_analysis/orv_near_dynamics_day84_qa_line_plot.jpg',
#'        width = 23.76,
#'        height = 17.86,
#'        units = 'cm'
#' )

#'Full Quality Assurance
#quick plot to inspect the outbreak sizes
# ggplot() + geom_bar(data = orv_near_dynamics %>%
#                         group_by(strategy, mt_equip_type, predeployment_delay) %>%
#                         filter(time == orv_model_params$model_time),
#                     aes(x = location_id, y = K),
#                     color = 'black',
#                     fill = 'tomato3',
#                     stat = 'identity'
#                     ) +
#     facet_wrap(strategy + mt_equip_type ~ predeployment_delay)

#' plot the outbreak dynamics and save to pdf for inspection
# orv_near_dynamics_list_by_strategy <- orv_near_dynamics %>%
#     group_split(strategy, location_id, mt_equip_type, predeployment_delay) %>% 
#     purrr::map(filter, time <= 100)
# 
# #line plots to be saved to pdf for inspection
# orv_near_dynamics_plot_list <- orv_near_dynamics_list_by_strategy %>% 
#     purrr::map(function(x){
#     ggplot(data = x) + 
#             geom_line(aes(x = time, 
#                           y = S
#                           ), 
#                       colour = 'blue'
#                       ) +
#             geom_line(data = x, 
#                       aes(x = time, 
#                           y = I
#                       ), 
#                       colour = 'red'
#                       ) + 
#             geom_line(data = x, 
#                       aes(x = time, 
#                           y = R
#                           ), 
#                       colour = 'purple'
#                       ) + 
#             scale_x_continuous(breaks = seq(0, 100, 10),
#                                labels = seq(0, 100, 10)
#                                ) +
#             labs(title = paste(x$strategy[1], 
#                                paste0('L', x$location_id[1]), #location ID to be printed as L1, L2, etc
#                                x$mt_equip_type[1], 
#                                paste(x$predeployment_delay[1], ' days'), #predeployment delay to be printed as 21 days, etc.
#                                sep = ', '
#                                )
#                  )
#         }
#     )
# 
# #arrange in a 4x4 grid
# orv_near_dynamics_line_plot <- gridExtra::marrangeGrob(orv_near_dynamics_plot_list, 
#                                             ncol = 1, 
#                                             nrow = 2
#                                             )
# 
# ggsave("./figures/deterministic_framework_analysis_figures/main_analysis/orv_near_dynamics_line_plot.pdf", 
#        orv_near_dynamics_line_plot
#        )


#' total outbreak size across all near locations (orv) ----
#' near pop (orv) ====
orv_near_dynamics_outbreak_size_df <- orv_near_dynamics %>% 
    group_by(strategy, mt_equip_type, predeployment_delay) %>% 
    filter(time == orv_model_params$model_time) %>% 
    summarise(orv_near_total_cases = sum(K),
              .groups = 'drop'
    ) 

#' far pop (orv) ====
orv_far_dynamics_outbreak_size_df <- orv_far_dynamics %>% 
    group_by(strategy, mt_equip_type, predeployment_delay) %>% 
    filter(time == orv_model_params$model_time) %>% 
    summarise(orv_far_total_cases = sum(K),
              .groups = 'drop'
    )
#' total outbreak size across all near locations (baseline; no orv) ----
#' near pop (no orv) ====
no_orv_near_dynamics_outbreak_size <- no_orv_near_dynamics %>% 
    filter(time == orv_model_params$model_time) %>% 
    summarise(total_cases = sum(K)) %>% 
    as.numeric()

no_orv_near_dynamics_outbreak_size

#' far pop (no orv) ====
no_orv_far_dynamics_outbreak_size <- no_orv_far_dynamics %>% 
    filter(time == orv_model_params$model_time) %>% 
    summarise(total_cases = sum(K)) %>% 
    as.numeric()

no_orv_far_dynamics_outbreak_size


# calculate the total outbreak size across all locations, near and far
orv_outbreak_sizes_aggregated <- left_join(orv_near_dynamics_outbreak_size_df, 
                                           orv_far_dynamics_outbreak_size_df,
                                           by = c("strategy", "mt_equip_type", 
                                                  "predeployment_delay")
                                           ) %>% 
    mutate(orv_total_cases = orv_near_total_cases + orv_far_total_cases)

#' cases averted ====

#the counterfactual is the no orv scenario
counterfactual_outbreak_size <- no_orv_near_dynamics_outbreak_size + no_orv_far_dynamics_outbreak_size


#' we use the 10-dose fcc with vaccine carrier as the baseline because it is what is 
#' currently the practice
baseline_outbreak_size <- orv_outbreak_sizes_aggregated %>% 
    filter(strategy == 'dose10_fcc', 
           mt_equip_type == 'vaxCarr') %>% 
    pull(orv_total_cases) 

#duplicate the baseline values, one per predeployment scenario
baseline_outbreak_size_mod <- rep(baseline_outbreak_size, 
                                  times = length(unique(orv_outbreak_sizes_aggregated$predeployment_delay
                                                        )
                                                 )
                                  )


cases_averted_df <- orv_outbreak_sizes_aggregated %>% 
    mutate(cases_averted = counterfactual_outbreak_size - orv_total_cases,
           relative_cases_averted = baseline_outbreak_size_mod - orv_total_cases
           )

cases_averted_df


#'save the cases averted results

#' Main analysis (assumes the predeployment delay takes 21 days)
saveRDS(cases_averted_df %>% filter(predeployment_delay == 21), 
        './model_output/deterministic_framework_analysis_output/main_analysis/cases_averted_main_analysis.rds'
        )

#Sensitivity analysis (assumes the predeployment delay takes 21-84 days)
saveRDS(cases_averted_df, 
        './model_output/deterministic_framework_analysis_output/sensitivity_analysis/predeployment_delay/cases_averted_predeployment_delay_sensitivity_analysis.rds')


#' Combine all simulations into a FINAL data.frame for post-processing

#'load supply chain summary
#'
sc_main_analysis_aggregated <- readRDS('./model_output/deterministic_framework_analysis_output/main_analysis/sc_main_analysis_results_summarized.rds') 

sc_main_analysis_aggregated_mod <- sc_main_analysis_aggregated %>% 
    mutate(strategy = as_factor(stringr::str_replace(strategy, 
                                                     pattern = '_parallel', 
                                                     replacement = '')
                                )
           ) #shorten the strategy names

#Combine the supply chain summary results with the epidemiological results (cases averted at different pre-deployment delays)
main_analysis_final_outcomes <- left_join(sc_main_analysis_aggregated_mod, 
                             cases_averted_df, 
                             by = c('strategy', 'mt_equip_type'
                                    )
                             )

saveRDS(main_analysis_final_outcomes, 
        file = './model_output/deterministic_framework_analysis_output/main_analysis/main_analysis_final_outcomes.rds')

