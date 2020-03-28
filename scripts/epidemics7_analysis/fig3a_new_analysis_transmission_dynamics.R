#packages
library(dplyr)
library(ggplot2)
library(ggthemes)
library(forcats)

#load the data
no_vax_near_dynamics_detailed <- readRDS('./model_output/no_vax_near_dynamics_detailed.rds')
no_vax_far_dynamics_detailed <- readRDS('./model_output/no_vax_far_dynamics_detailed.rds')
orv_near_pop_dynamics_detailed <- readRDS('./model_output/orv_near_pop_dynamics_detailed.rds')
orv_far_pop_dynamics_detailed <- readRDS('./model_output/orv_far_pop_dynamics_detailed.rds')


#no vaccination near dynamics
no_vax_near_dynamics_detailed_plot_df <- no_vax_near_dynamics_detailed %>% 
    as_tibble() %>% 
    filter(time <=100) %>% 
    mutate(location_id = as_factor(location_id))


no_vax_near_sir_plot <- ggplot(data = no_vax_near_dynamics_detailed_plot_df) + 
    geom_line(aes(x = time, 
                  y = Inf1,
                  color = location_id),
              size = 2
    ) + 
    geom_line(data = no_vax_near_dynamics_detailed_plot_df, 
              aes(x = time, 
                  y = Sus1,
                  color = location_id), 
              color = 'blue', 
              size = 2) + 
    geom_line(data = no_vax_near_dynamics_detailed_plot_df, 
              aes(x = time, 
                  y = Rec,
                  color = location_id), 
              color = 'green', 
              size = 2) 

plot(no_vax_near_sir_plot)


#no vaccination far dynamics
no_vax_far_dynamics_detailed_plot_df <- no_vax_far_dynamics_detailed %>% 
    as_tibble() %>% 
    filter(time <=100) %>% 
    mutate(location_id = as_factor(location_id))


no_vax_far_pop_sir_plot <- ggplot(data = no_vax_far_dynamics_detailed_plot_df) + 
    geom_line(aes(x = time, 
                  y = Inf1,
                  color = location_id),
              size = 2
    ) + 
    labs('Number of individuals') + 
    geom_line(data = no_vax_far_dynamics_detailed_plot_df, 
              aes(x = time, 
                  y = Sus1,
                  color = location_id), 
              color = 'blue', 
              size = 2) + 
    geom_line(data = no_vax_far_dynamics_detailed_plot_df, 
              aes(x = time, 
                  y = Rec,
                  color = location_id), 
              color = 'green', 
              size = 2) 

plot(no_vax_far_pop_sir_plot)

#' orv near dynamics ----

#data for plot
orv_near_pop_dynamics_detailed_plot_df <- orv_near_pop_dynamics_detailed %>% 
    as_tibble() %>% 
    filter(time <=100) %>% 
    mutate(location_id = as_factor(location_id))

#split the results by unique id and find total cases across all locations per scenario

orv_near_pop_dynamics_detailed_by_loc <- orv_near_pop_dynamics_detailed %>% 
    as_tibble() %>% 
    filter(time <=100) %>% 
    mutate(location_id = as_factor(location_id)) %>% 
    group_split(strategy, mt_equip_type)

#near site susceptible dynamics
orv_near_pop_sus_plot <- ggplot(data = orv_near_pop_dynamics_detailed_plot_df) + 
    geom_line(aes(x = time, 
                  y = Sus1,
                  color = strategy),
              size = 1) +
    labs(y = 'Susceptibles') + 
    facet_grid(~ location_id)

plot(orv_near_pop_sus_plot)


#near site incidence dynamics
orv_near_inf_plot <- ggplot(data = orv_near_pop_dynamics_detailed_plot_df) + 
    geom_line(aes(x = time, 
                  y = Inf1, 
                  color = strategy
    ), size = 1) +
    labs(y = 'Incidence') +
    facet_grid(~ location_id)

plot(orv_near_inf_plot)


#' orv far dynamics ----

#data for plot
orv_far_pop_dynamics_detailed_plot_df <- orv_far_pop_dynamics_detailed %>% 
    as_tibble() %>% 
    filter(time <=100) %>% 
    mutate(location_id = as_factor(location_id))

#split the results by unique id and find total cases across all locations per scenario

orv_far_pop_dynamics_detailed_by_loc <- orv_far_pop_dynamics_detailed %>% 
    as_tibble() %>% 
    filter(time <=100) %>% 
    mutate(location_id = as_factor(location_id)) %>% 
    group_split(strategy, mt_equip_type)

#far site susceptible dynamics
orv_far_pop_sus_plot <- ggplot(data = orv_far_pop_dynamics_detailed_plot_df) + 
    geom_line(aes(x = time, 
                  y = Sus1,
                  color = strategy),
              size = 1) +
    labs(y = 'Susceptibles') +
    facet_grid(~ location_id)

plot(orv_far_pop_sus_plot)

#far site incidence dynamics
orv_far_inf_plot <- ggplot(data = orv_far_pop_dynamics_detailed_plot_df) + 
    geom_line(aes(x = time, 
                  y = Inf1, 
                  color = strategy
    ), size = 1) +
    labs(y = 'Incidence') +
    facet_grid(~ location_id)

plot(orv_far_inf_plot)
