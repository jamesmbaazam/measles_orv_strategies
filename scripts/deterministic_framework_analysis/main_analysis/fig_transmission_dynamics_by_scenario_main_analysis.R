#' packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(conflicted)


#' helper scripts
source('./scripts/deterministic_framework_analysis/simulation_params.R')


## Resolve package conflicts ####
conflict_prefer('filter', 'dplyr') #anytime I call the function 'filter', I mean dplyr::filter
conflict_prefer('select', 'dplyr')
conflict_prefer('merge', 'base')
conflict_prefer('summarise', 'dplyr')


#'load data and rescale them to the actual numbers
orv_near_dynamics <- readRDS('./model_output/deterministic_framework_analysis_output/orv_near_dynamics_proportions.rds') %>% 
    mutate(S = S*50000,
           E = E*50000,
           I = I*50000,
           R = R*50000,
           K = K*50000
           )
    
no_orv_near_dynamics <- readRDS('./model_output/deterministic_framework_analysis_output/no_orv_near_dynamics_proportions.rds') %>% 
    mutate(S = S*50000,
           E = E*50000,
           I = I*50000,
           R = R*50000,
           K = K*50000
           )

orv_far_dynamics <- readRDS('./model_output/deterministic_framework_analysis_output/orv_far_dynamics_proportions.rds') %>% 
    mutate(S = S*50000,
           E = E*50000,
           I = I*50000,
           R = R*50000,
           K = K*50000
           )

no_orv_far_dynamics <- readRDS('./model_output/deterministic_framework_analysis_output/no_orv_far_dynamics_proportions.rds') %>% 
    mutate(S = S*50000,
           E = E*50000,
           I = I*50000,
           R = R*50000,
           K = K*50000
           )


################################################################################
#' plots ----
################################################################################
#' orv dynamics for near locations ====
orv_near_dynamics_plot <- ggplot(data = orv_near_dynamics %>% filter(time <= 200)) + 
    geom_line(aes(x = time, 
                  y = S
    ), 
    color = 'blue', 
    size = 1
    ) + 
    geom_line(aes(x = time, 
                  y = I), 
              color = 'red', 
              size = 1) + 
    labs(x = 'Time (Days)', 
         y = 'Proportion of individuals', 
         title = 'Transmission dynamics following vaccination in the near locations'
    ) + 
    facet_grid(strategy ~ mt_equip_type + location_id)

plot(orv_near_dynamics_plot)


#' orv dynamics for far locations ====
orv_far_dynamics_plot <- ggplot(data = orv_far_dynamics %>% filter(time <= 200)) + 
    geom_line(aes(x = time, 
                  y = S
    ), 
    color = 'blue', 
    size = 1
    ) + 
    geom_line(aes(x = time, 
                  y = I), 
              color = 'red', 
              size = 1) + 
    labs(x = 'Time (Days)', 
         y = 'Proportion of individuals', 
         title = 'Transmission dynamics following vaccination in the far locations'
    ) + 
    facet_grid(strategy ~ mt_equip_type + location_id)

plot(orv_far_dynamics_plot)


#' transmission dynamics without vaccination for near locations ====
no_orv_near_dynamics_plot <- ggplot(data = no_orv_near_dynamics %>% filter(time <= 200)) + 
    geom_line(aes(x = time, 
                  y = S
    ), 
    color = 'blue', 
    size = 2
    ) + 
    geom_line(aes(x = time, 
                  y = I), 
              color = 'red', 
              size = 2) + 
    labs(x = 'Time (Days)', 
         y = 'Proportion of individuals', 
         title = 'Transmission dynamics of near locations without vaccination'
    ) + 
    facet_grid( ~ location_id)

plot(no_orv_near_dynamics_plot)


#' transmission dynamics without vaccination for far locations ====
no_orv_far_dynamics_plot <- ggplot(data = no_orv_far_dynamics %>% filter(time <= 200)) + 
    geom_line(aes(x = time, 
                  y = S
    ), 
    color = 'blue', 
    size = 2
    ) + 
    geom_line(aes(x = time, 
                  y = I), 
              color = 'red', 
              size = 2) + 
    labs(x = 'Time (Days)', 
         y = 'Proportion of individuals', 
         title = 'Transmission dynamics of far locations without vaccination'
    ) + 
    facet_grid( ~ location_id)

plot(no_orv_far_dynamics_plot)


#' Plots (Monodose fcc for illustration) ----

#' #' 1a. Outbreak size: by near locations ====
#' monodose_fcc_near_outbreak_size_plot <- orv_near_dynamics %>% 
#'     group_by(strategy, mt_equip_type) %>% 
#'     #  filter(time == orv_model_params$model_time) %>% 
#'     filter(time == orv_model_params$model_time, strategy == 'monodose_fcc') %>% 
#'     ggplot(aes(x = time, y = K)) + 
#'     geom_bar(stat = 'identity') + 
#'     theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
#'     labs(x = 'Location', y = 'Outbreak size', title = 'Outbreak sizes by location and equipment used (Near Locations)') +
#'     facet_grid(strategy ~ mt_equip_type + location_id)
#' 
#' plot(monodose_fcc_near_outbreak_size_plot)
#' 
#' #' 1b. Incidence: by near locations
#' monodose_fcc_near_incidence_plot <- orv_near_dynamics %>% 
#'     group_by(strategy, mt_equip_type) %>% 
#'     #  filter(time == orv_model_params$model_time) %>% 
#'     filter(strategy == 'monodose_fcc') %>% 
#'     ggplot(aes(x = time, y = I)) + 
#'     geom_line(size = 2, color = 'red') + 
#'     labs(x = 'Time (Days)', y = 'Incidence', title = 'Monodose FCC incidence by location and equipment used (Near Locations)') +
#'     facet_grid(strategy ~ mt_equip_type + location_id)
#' 
#' plot(monodose_fcc_near_incidence_plot)
#' 
#' #' 2a. Outbreak size: by far locations
#' monodose_fcc_far_outbreak_size_plot <- orv_far_dynamics %>% 
#'     group_by(strategy, mt_equip_type) %>% 
#'     #  filter(time == orv_model_params$model_time) %>% 
#'     filter(time == orv_model_params$model_time, strategy == 'monodose_fcc') %>% 
#'     ggplot(aes(x = time, y = K)) + 
#'     geom_bar(stat = 'identity') + 
#'     theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
#'     labs(x = 'Location', y = 'Outbreak size', title = 'Monodose FCC outbreak sizes per location and equipment used (Far Locations)') +
#'     facet_grid(strategy ~ mt_equip_type + location_id)
#' 
#' plot(monodose_fcc_far_outbreak_size_plot)
#' 
#' 
#' #' 2b. Incidence: by far locations
#' monodose_fcc_far_incidence_plot <- orv_far_dynamics %>% 
#'     group_by(strategy, mt_equip_type) %>% 
#'     #  filter(time == orv_model_params$model_time) %>% 
#'     filter(strategy == 'monodose_fcc') %>% 
#'     ggplot(aes(x = time, y = I)) + 
#'     geom_line(size = 2, color = 'red') + 
#'     labs(x = 'Time (Days)', y = 'Incidence', title = 'Monodose FCC incidence by location and equipment used (Far Locations)') +
#'     facet_grid(strategy ~ mt_equip_type + location_id)
#' 
#' plot(monodose_fcc_far_incidence_plot)



#' print the monodose data to support the explanation of the plots:
# library(xtable)
# monodose_sc_results <- orv_model_inputs %>% 
#   filter(strategy == 'monodose_fcc') %>% 
#   select(-c(near_pop, far_pop, ft_dur_constrained, mt_dur_constrained)) 
# 
#   print(xtable(monodose_sc_results), 
#       floating=FALSE, 
#       latex.environments = NULL, 
#       booktabs = TRUE
#       )

