#packages
library(dplyr)
library(ggplot2)
library(ggthemes)
library(forcats)
library(tidyr)
library(purrr)

#load the data
no_vax_near_dynamics_detailed <- readRDS('./model_output/no_vax_near_dynamics_detailed.rds')
no_vax_far_dynamics_detailed <- readRDS('./model_output/no_vax_far_dynamics_detailed.rds')
orv_near_pop_dynamics_detailed <- readRDS('./model_output/orv_near_pop_dynamics_detailed.rds')
orv_far_pop_dynamics_detailed <- readRDS('./model_output/orv_far_pop_dynamics_detailed.rds')
orv_near_pop_dynamics_collapsed <- readRDS('./model_output/orv_near_pop_dynamics_collapsed.rds')
orv_far_pop_dynamics_collapsed <- readRDS('./model_output/orv_far_pop_dynamics_collapsed.rds')


#no vaccination near dynamics
no_vax_near_dynamics_detailed_plot_df <- no_vax_near_dynamics_detailed %>% 
    as_tibble() %>% 
    filter(time <=100) 



no_vax_near_sir_plot <- ggplot(data = no_vax_near_dynamics_detailed_plot_df) + 
    geom_line(aes(x = time, 
                  y = Inf1),
              size = 2
    ) + 
    geom_line(data = no_vax_near_dynamics_detailed_plot_df, 
              aes(x = time, 
                  y = Sus1), 
              color = 'blue', 
              size = 2) + 
    geom_line(data = no_vax_near_dynamics_detailed_plot_df, 
              aes(x = time, 
                  y = Rec
              ), 
              color = 'green', 
              size = 2) +
    facet_grid(~location_id) +
    labs(title = 'Transmission dynamics (near locations)')

plot(no_vax_near_sir_plot)


#no vaccination far dynamics
no_vax_far_dynamics_detailed_plot_df <- no_vax_far_dynamics_detailed %>% 
    as_tibble() %>% 
    filter(time <=100) 


no_vax_far_pop_sir_plot <- ggplot(data = no_vax_far_dynamics_detailed_plot_df) + 
    geom_line(aes(x = time, 
                  y = Inf1
    ),
    size = 2
    ) + 
    labs('Number of individuals') + 
    geom_line(data = no_vax_far_dynamics_detailed_plot_df, 
              aes(x = time, 
                  y = Sus1
              ), 
              color = 'blue', 
              size = 2) + 
    geom_line(data = no_vax_far_dynamics_detailed_plot_df, 
              aes(x = time, 
                  y = Rec
              ), 
              color = 'green', 
              size = 2) + 
    labs(title = 'Transmission dynamics (far locations)') +
    facet_grid(~location_id) + 
    scale_color_manual()

plot(no_vax_far_pop_sir_plot)

#' orv near dynamics ----

#data for plot
orv_near_pop_dynamics_detailed_plot_df <- orv_near_pop_dynamics_detailed %>% 
    as_tibble() %>% 
    filter(time <=100) 

#split the results by unique id and find total cases across all locations per scenario

orv_near_pop_dynamics_detailed_by_loc <- orv_near_pop_dynamics_detailed %>% 
    as_tibble() %>% 
    filter(time <=100) %>% 
    group_split(strategy, mt_equip_type)

#near site susceptible dynamics
orv_near_pop_si_plot <- ggplot(data = orv_near_pop_dynamics_detailed_plot_df) + 
    geom_line(aes(x = time, 
                  y = Sus1,
                  color = strategy),
              size = 1) +
    geom_line(data = orv_near_pop_dynamics_detailed_plot_df, 
              aes(x = time, 
                  y = Inf1, 
                  color = strategy
              ), size = 1) +
    labs(title = 'Susceptibles and incidence per near location') +
    facet_grid(~ location_id)

plot(orv_near_pop_si_plot)


#near site incidence dynamics
orv_near_pop_incidence_plot <- ggplot(data = orv_near_pop_dynamics_detailed_plot_df) + 
    geom_line(aes(x = time, 
                  y = Inf1, 
                  color = strategy
    ), size = 1) +
    labs(y = 'Incidence') +
    labs(title = 'Incidence per far location') +
    facet_grid(~ location_id)

plot(orv_near_pop_incidence_plot)


#' orv far dynamics ----

#data for plot
orv_far_pop_dynamics_detailed_plot_df <- orv_far_pop_dynamics_detailed %>% 
    as_tibble() %>% 
    filter(time <=100)

#split the results by unique id and find total cases across all locations per scenario

orv_far_pop_dynamics_detailed_by_loc <- orv_far_pop_dynamics_detailed %>% 
    as_tibble() %>% 
    filter(time <=100) %>% 
    group_split(strategy, mt_equip_type)

#far site susceptible dynamics
orv_far_pop_sus_plot <- ggplot(data = orv_far_pop_dynamics_detailed_plot_df) + 
    geom_line(aes(x = time, 
                  y = Sus1,
                  color = strategy),
              size = 1) +
    geom_line(data = orv_far_pop_dynamics_detailed_plot_df,
              aes(x = time, 
                  y = Inf1, 
                  color = strategy
              ), size = 1) +
    labs(y = 'Susceptibles and incidence per far location') +
    facet_grid(~ location_id)

plot(orv_far_pop_sus_plot)

#far site incidence dynamics
orv_far_incidence_plot <- ggplot(data = orv_far_pop_dynamics_detailed_plot_df) + 
    geom_line(aes(x = time, 
                  y = Inf1, 
                  color = strategy
    ), size = 1) +
    labs(title = 'Incidence per far location (R0 = 12)', y = 'Incidence') +
    theme_minimal() +
    facet_grid(~ location_id)

plot(orv_far_incidence_plot)


#' reshape the data ####
#' near_pop
orv_near_pop_dynamics_detailed_wide <- orv_near_pop_dynamics_detailed %>% 
    select(-c(Sus2, Exp1:Exp10, Inf2:Inf6)) %>% #I only need the incidence and susceptibles
    pivot_wider(id_cols = c(time, strategy, location_id, mt_equip_type), #these are the unique identifiers
                names_from = c(location_id), #spread the classes over the locations in columns so I can sum across
                values_from = c(Sus1, Inf1, Rec) #spread these values over the locations
    ) %>% 
    group_by(strategy, mt_equip_type) %>% 
    mutate(total_sus = Sus1_1 + Sus1_2 + Sus1_3 + Sus1_4 + Sus1_5,
           total_inf = Inf1_1 + Inf1_2 + Inf1_3 + Inf1_4 + Inf1_5,
           total_Rec = Rec_1 + Rec_2 + Rec_3 + Rec_4 + Rec_5) %>% 
    ungroup()

orv_near_pop_dynamics_plot_df <- orv_near_pop_dynamics_detailed_wide %>% filter(time <= 120)

#' susceptible and incidence plots (near locations aggregated) ####
orv_near_sir_aggregated_plot <- ggplot(data = orv_near_pop_dynamics_plot_df, 
                                       aes(x = time, 
                                           y = total_sus
                                       ), 
                                       color = 'blue'
) +
    geom_line(size = 2) +
    geom_line(data = orv_near_pop_dynamics_plot_df, 
              aes(x = time, 
                  y = total_inf
              ),
              color = 'red',
              size = 2) + 
    labs(title = 'Number of susceptibles and new cases per time (5 near locations  aggregated)',
         y = 'Number of individuals') +
    facet_wrap(~ strategy + mt_equip_type)

plot(orv_near_sir_aggregated_plot)


#' incidence plots (near locations aggregated) ####
orv_near_incidence_aggregated_plot <- ggplot(data = orv_near_pop_dynamics_plot_df, 
                                             aes(x = time, 
                                                 y = total_inf
                                             )) +
    geom_line(size = 2, color = 'red') +
    labs(title = 'Incidence for R0 = (5 near locations aggregated)',
         y = 'Incidence') +
    facet_wrap(~ strategy + mt_equip_type)

plot(orv_near_incidence_aggregated_plot)



#' far_pop

orv_far_pop_dynamics_detailed_wide <- orv_far_pop_dynamics_detailed %>% 
    select(-c(Sus2, Exp1:Exp10, Inf2:Inf6)) %>% #I only need the incidence and susceptibles
    pivot_wider(id_cols = c(time, strategy, location_id, mt_equip_type), #these are the unique identifiers
                names_from = c(location_id), #spread the classes over the locations in columns so I can sum across
                values_from = c(Sus1, Inf1, Rec) #spread these values over the locations
    ) %>% 
    group_by(strategy, mt_equip_type) %>% 
    mutate(total_sus = Sus1_1 + Sus1_2 + Sus1_3 + Sus1_4 + Sus1_5,
           total_inf = Inf1_1 + Inf1_2 + Inf1_3 + Inf1_4 + Inf1_5,
           total_Rec = Rec_1 + Rec_2 + Rec_3 + Rec_4 + Rec_5
    ) %>% 
    ungroup()

orv_far_pop_dynamics_plot_df <- orv_far_pop_dynamics_detailed_wide %>% filter(time <= 120)



#' susceptible and incidence plots (far locations aggregated) ####
orv_far_sir_aggregated_plot <- ggplot(data = orv_far_pop_dynamics_plot_df, 
                                      aes(x = time, 
                                          y = total_sus
                                      ), 
                                      color = 'blue'
) +
    geom_line(size = 2) +
    geom_line(data = orv_far_pop_dynamics_plot_df, 
              aes(x = time, 
                  y = total_inf
              ),
              color = 'red',
              size = 2) + 
    labs(title = 'Number of susceptibles and new cases per time (5 far locations  aggregated)',
         y = 'Number of individuals') +
    facet_wrap(~ strategy + mt_equip_type)

plot(orv_far_sir_aggregated_plot)


#' incidence plots (far locations aggregated) ####
orv_far_incidence_aggregated_plot <- ggplot(data = orv_far_pop_dynamics_plot_df, 
                                            aes(x = time, 
                                                y = total_inf
                                            )) +
    geom_line(size = 2, color = 'red') +
    labs(title = 'Number of new cases per time (5 far locations aggregated)',
         y = 'Incidence') +
    facet_wrap(~ strategy + mt_equip_type)

plot(orv_far_incidence_aggregated_plot)


#' vaccination scenarios: near population dynamics by scenario and location ----

orv_near_pop_dynamics_detailed$strategy <- gsub('_parallel', '', orv_near_pop_dynamics_detailed$strategy) 

orv_near_pop_dynamics_by_strategy_plot <- orv_near_pop_dynamics_detailed %>% filter(time <=120) %>% 
    ggplot(aes(x = time, y = Inf1)) + 
    geom_line(color = 'red', size = 1) + 
    labs(y = 'Incidence', title = 'Incidence per scenario and location (near sites)') +
    facet_grid(strategy + mt_equip_type ~ location_id) 

plot(orv_near_pop_dynamics_by_strategy_plot)


# vaccination scenarios: far population dynamics by scenario and location

orv_far_pop_dynamics_detailed$strategy <- gsub('_parallel', '', orv_far_pop_dynamics_detailed$strategy) 

orv_far_pop_dynamics_by_strategy_plot <- orv_far_pop_dynamics_detailed %>% filter(time <=120) %>% 
    ggplot(aes(x = time, y = Inf1)) + 
    geom_line(color = 'red', size = 1) + 
    labs(y = 'Incidence', title = 'Incidence per scenario and location (far sites)') +
    facet_grid(strategy + mt_equip_type ~ location_id) 

plot(orv_far_pop_dynamics_by_strategy_plot)


# no vaccination scenarios: near population dynamics by scenario and location
no_vax_near_sir_dynamics_by_loc_plot <- no_vax_near_dynamics_detailed_plot_df %>% filter(time <=120) %>% 
    ggplot(aes(x = time, y = Inf1)) + 
    geom_line(color = 'red', size = 1) + 
    geom_line(data = no_vax_near_dynamics_detailed_plot_df %>% 
                  filter(time <=120), 
              aes(x = time, 
                  y = Sus1
              ), 
              color = 'blue', 
              size = 1) +
    geom_line(data = no_vax_near_dynamics_detailed_plot_df %>% 
                  filter(time <=120), 
              aes(x = time, 
                  y = Rec
              ), 
              color = 'green', 
              size = 1) +
    labs(y = 'Number of individuals', title = 'Transmission dynamics per near location without vaccination') +
    facet_grid(~ location_id) 

plot(no_vax_near_sir_dynamics_by_loc_plot)


# no vaccination scenarios: far population dynamics by scenario and location
no_vax_far_sir_dynamics_by_loc_plot <- no_vax_far_dynamics_detailed_plot_df %>% filter(time <=120) %>% 
    ggplot(aes(x = time, y = Inf1)) + 
    geom_line(color = 'red', size = 1) + 
    geom_line(data = no_vax_far_dynamics_detailed_plot_df %>% 
                  filter(time <=120), 
              aes(x = time, 
                  y = Sus1
              ), 
              color = 'blue', 
              size = 1) +
    geom_line(data = no_vax_far_dynamics_detailed_plot_df %>% 
                  filter(time <=120), 
              aes(x = time, 
                  y = Rec
              ), 
              color = 'green', 
              size = 1) +
    labs(y = 'Number of individuals', title = 'Transmission dynamics per far location without vaccination') +
    facet_grid(~ location_id) 

plot(no_vax_far_sir_dynamics_by_loc_plot)
