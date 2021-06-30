#packages
library(dplyr)
library(ggplot2)
library(ggthemes)
library(forcats)


#load helper scripts
source('./scripts/epidemics7_analysis/simulation_params.R')

#load the data
no_vax_outbreak_sizes <- readRDS('./model_output/no_vax_epi_total_full.rds')
orv_near_pop_outbreak_sizes <- readRDS('./model_output/orv_near_pop_dynamics_epi_total.rds')
orv_far_pop_outbreak_sizes <- readRDS('./model_output/orv_far_pop_dynamics_epi_total.rds')


#' outbreak sizes ----
#' 
orv_np <- orv_near_pop_outbreak_sizes %>% 
    mutate(strategy = sim_params_table$strategy)

#near pop
orv_near_pop_outbreak_sizes_plot <- ggplot(data = orv_np) + 
    geom_bar(aes(x = strategy, 
                 y = near_pop_epi_total, 
                 fill = mt_equip_type
                 ),
             stat = 'identity',
             position = 'dodge'
             ) + 
    coord_flip() + 
    facet_grid(~ location_id) 

plot(orv_near_pop_outbreak_sizes_plot)



#far pop
orv_fp <- orv_far_pop_outbreak_sizes %>% 
    mutate(strategy = sim_params_table$strategy)


orv_far_pop_outbreak_sizes_plot <- ggplot(data = orv_fp) + 
    geom_bar(aes(x = strategy, 
                 y = far_pop_epi_total, 
                 fill = mt_equip_type
    ),
    stat = 'identity',
    position = 'dodge'
    ) + 
    coord_flip() + 
    facet_grid(~ location_id) 

plot(orv_far_pop_outbreak_sizes_plot)
