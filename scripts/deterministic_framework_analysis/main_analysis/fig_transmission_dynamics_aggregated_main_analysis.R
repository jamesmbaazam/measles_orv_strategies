library(dplyr)
library(ggplot2)
library(tidyr)

#' near location dynamics ----
#simulation output for orv in near locations
orv_near_pop_dynamics <- readRDS('./model_output/ottar_seir_model_output/orv_near_dynamics.rds')

#reshape the data fro aggregation
orv_near_pop_dynamics_wide <- orv_near_pop_dynamics %>% 
    pivot_wider(id_cols = c(time, strategy, location_id, mt_equip_type), #these are the unique identifiers
                names_from = c(location_id), #spread the classes over the locations in columns so I can sum across
                values_from = c(S, E, I, R, K) #spread these values over the locations
    ) %>% 
    group_by(strategy, mt_equip_type) %>% 
    mutate(total_S = S_1 + S_2 + S_3 + S_4 + S_5,
           total_E = E_1 + E_2 + E_3 + E_4 + E_5,
           total_I = I_1 + I_2 + I_3 + I_4 + I_5,
           total_R = R_1 + R_2 + R_3 + R_4 + R_5,
           total_K = K_1 + K_2 + K_3 + K_4 + K_5) %>% 
    ungroup()


#plot the aggregated data
orv_near_sir_aggregated_plot <- ggplot(data = orv_near_pop_dynamics_wide) + 
    geom_line(aes(x = time, 
                  y = total_S), 
              size = 1.5, 
              color = 'blue') +
    geom_line(aes(x = time, 
                  y = total_I), 
              size = 1.5, 
              color = 'red') +
    geom_line(aes(x = time, 
                  y = total_R), 
              size = 1.5, 
              color = 'green') +
    labs(y = 'Proportion of individuals', title = 'Transmission dynamics following orv in near locations (aggregagted)') +
    facet_grid(strategy ~ mt_equip_type)


plot(orv_near_sir_aggregated_plot)



#simulation output for orv in near locations
orv_near_pop_dynamics <- readRDS('./model_output/ottar_seir_model_output/orv_near_dynamics.rds')

#reshape the data fro aggregation
orv_near_pop_dynamics_wide <- orv_near_pop_dynamics %>% 
    pivot_wider(id_cols = c(time, strategy, location_id, mt_equip_type), #these are the unique identifiers
                names_from = c(location_id), #spread the classes over the locations in columns so I can sum across
                values_from = c(S, E, I, R, K) #spread these values over the locations
    ) %>% 
    group_by(strategy, mt_equip_type) %>% 
    mutate(total_S = S_1 + S_2 + S_3 + S_4 + S_5,
           total_E = E_1 + E_2 + E_3 + E_4 + E_5,
           total_I = I_1 + I_2 + I_3 + I_4 + I_5,
           total_R = R_1 + R_2 + R_3 + R_4 + R_5,
           total_K = K_1 + K_2 + K_3 + K_4 + K_5) %>% 
    ungroup()


#plot the aggregated data
orv_near_sir_aggregated_plot <- ggplot(data = orv_near_pop_dynamics_wide) + 
    geom_line(aes(x = time, 
                  y = total_S), 
              size = 1.5, 
              color = 'blue') +
    geom_line(aes(x = time, 
                  y = total_I), 
              size = 1.5, 
              color = 'red') +
    geom_line(aes(x = time, 
                  y = total_R), 
              size = 1.5, 
              color = 'green') +
    labs(y = 'Proportion of individuals', title = 'Transmission dynamics following orv in near locations (aggregagted)') +
    facet_grid(strategy ~ mt_equip_type)


plot(orv_near_sir_aggregated_plot)




#' near location dynamics ----
#simulation output for orv in near locations
orv_near_pop_dynamics <- readRDS('./model_output/ottar_seir_model_output/orv_near_dynamics.rds')

#reshape the data fro aggregation
orv_near_pop_dynamics_wide <- orv_near_pop_dynamics %>% 
    pivot_wider(id_cols = c(time, strategy, location_id, mt_equip_type), #these are the unique identifiers
                names_from = c(location_id), #spread the classes over the locations in columns so I can sum across
                values_from = c(S, E, I, R) #spread these values over the locations
    ) %>% 
    group_by(strategy, mt_equip_type) %>% 
    mutate(total_S = S_1 + S_2 + S_3 + S_4 + S_5,
           total_E = E_1 + E_2 + E_3 + E_4 + E_5,
           total_I = I_1 + I_2 + I_3 + I_4 + I_5,
           total_R = R_1 + R_2 + R_3 + R_4 + R_5) %>% 
    ungroup()


#plot the aggregated data
orv_near_sir_aggregated_plot <- ggplot(data = orv_near_pop_dynamics_wide) + 
    geom_line(aes(x = time, 
                  y = total_S), 
              size = 1.5, 
              color = 'blue') +
    geom_line(aes(x = time, 
                  y = total_I), 
              size = 1.5, 
              color = 'red') +
    geom_line(aes(x = time, 
                  y = total_R), 
              size = 1.5, 
              color = 'green') +
    labs(y = 'Proportion of individuals', title = 'Transmission dynamics following orv in near locations (aggregagted)') +
    facet_grid(strategy ~ mt_equip_type)


plot(orv_near_sir_aggregated_plot)



#simulation output for orv in far locations
orv_far_pop_dynamics <- readRDS('./model_output/ottar_seir_model_output/orv_far_dynamics.rds')

#reshape the data fro aggregation
orv_far_pop_dynamics_wide <- orv_far_pop_dynamics %>% 
    pivot_wider(id_cols = c(time, strategy, location_id, mt_equip_type), #these are the unique identifiers
                names_from = c(location_id), #spread the classes over the locations in columns so I can sum across
                values_from = c(S, E, I, R) #spread these values over the locations
    ) %>% 
    group_by(strategy, mt_equip_type) %>% 
    mutate(total_S = S_1 + S_2 + S_3 + S_4 + S_5,
           total_E = E_1 + E_2 + E_3 + E_4 + E_5,
           total_I = I_1 + I_2 + I_3 + I_4 + I_5,
           total_R = R_1 + R_2 + R_3 + R_4 + R_5) %>% 
    ungroup()


#plot the aggregated data
orv_far_sir_aggregated_plot <- ggplot(data = orv_far_pop_dynamics_wide) + 
    geom_line(aes(x = time, 
                  y = total_S), 
              size = 1.5, 
              color = 'blue') +
    geom_line(aes(x = time, 
                  y = total_I), 
              size = 1.5, 
              color = 'red') +
    geom_line(aes(x = time, 
                  y = total_R), 
              size = 1.5, 
              color = 'green') +
    labs(y = 'Proportion of individuals', title = 'Transmission dynamics following orv in far locations (aggregagted)') +
    facet_grid(strategy ~ mt_equip_type)


plot(orv_far_sir_aggregated_plot)