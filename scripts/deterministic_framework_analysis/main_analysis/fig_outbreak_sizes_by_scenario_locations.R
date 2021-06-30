#' packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(conflicted)
library(stringr)


#' helper scripts
source('./scripts/deterministic_framework_analysis/simulation_params.R')


## Resolve package conflicts ####
conflict_prefer('filter', 'dplyr') #anytime I call the function 'filter', I mean dplyr::filter
conflict_prefer('select', 'dplyr')
conflict_prefer('merge', 'base')
conflict_prefer('summarise', 'dplyr')


#'load data and rescale them to the actual numbers

orv_near_dynamics_final_size <- readRDS('./model_output/deterministic_framework_analysis_output/orv_near_dynamics_proportions.rds') %>% 
    mutate(S = S*50000,
           E = E*50000,
           I = I*50000,
           R = R*50000,
           K = K*50000
    ) %>% 
    as_tibble() %>% 
    select(-c(S, E, I, R)) %>% 
    group_by(strategy, location_id, mt_equip_type) %>% 
    filter(time == orv_model_params$model_time)

orv_far_dynamics_final_size <- readRDS('./model_output/deterministic_framework_analysis_output/orv_far_dynamics_proportions.rds') %>% 
    mutate(S = S*50000,
           E = E*50000,
           I = I*50000,
           R = R*50000,
           K = K*50000) %>% 
    as_tibble() %>% 
    select(-c(S, E, I, R)) %>% 
    group_by(strategy, location_id, mt_equip_type) %>% 
    filter(time == orv_model_params$model_time) 

#' plots

#' Near pops
orv_near_dynamics_final_size_plot <- ggplot(data = orv_near_dynamics_final_size, 
                                            aes(x = location_id,
                                                y = K/1000)) +
    geom_bar(aes(fill = mt_equip_type), 
             stat = 'identity',
             position = 'dodge') +
    facet_grid( ~ strategy) + 
    scale_fill_brewer(palette = 'Set2') +
    labs(title = 'Outbreak size of near locations by scenario', 
         x = 'Location', 
         y = 'Number of individuals (thousands)')

plot(orv_near_dynamics_final_size_plot)

#' far locations

orv_far_dynamics_final_size_plot <- ggplot(data = orv_far_dynamics_final_size, 
                                            aes(x = location_id,
                                                y = K/1000)) +
    geom_bar(aes(fill = mt_equip_type), 
             stat = 'identity',
             position = 'dodge') +
    facet_grid( ~ strategy) + 
    scale_fill_brewer(palette = 'Set2') +
    labs(title = 'Outbreak size of far locations by scenario', 
         x = 'Location', 
         y = 'Number of individuals (thousands)')

plot(orv_far_dynamics_final_size_plot)
