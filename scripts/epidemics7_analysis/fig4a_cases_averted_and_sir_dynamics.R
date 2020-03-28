library(ggplot2)
library(ggthemes)

#laod plotting data
cases_averted_df <- readRDS('./model_output/sc_epi_analysis_summary_10_teams.rds')

#' visualisations
fig4a_cases_averted <- ggplot(data = cases_averted_df) + 
    geom_jitter(aes(x = campaign_duration, 
                    y = average_coverage, 
                    color = mt_equip_type,
                    size = epi_results_summary_10_teams1$cases_averted), 
                width = 0.10, 
                height = 0.025
    ) +
    labs(title = 'Campaign duration and average vaccination coverage for 10 fixed post and 10 mobile teams',
         x = 'Campaign duration', 
         y = 'Vaccination coverage') +
    theme_minimal()

plot(fig4a_cases_averted)

# ggsave(filename = './figures/fig4a_cases_averted.pdf', 
#        plot = fig4a_cases_averted, 
#        device = 'pdf'
#        )



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
    geom_line(data = no_vax_near_dynamics_detailed_plot, 
              aes(x = time, 
                  y = Sus1,
                  color = location_id), 
              color = 'blue', 
              size = 2) + 
    geom_line(data = no_vax_near_dynamics_detailed_plot, 
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
    geom_line(data = no_vax_far_dynamics_detailed_plot, 
              aes(x = time, 
                  y = Sus1,
                  color = location_id), 
              color = 'blue', 
              size = 2) + 
    geom_line(data = no_vax_far_dynamics_detailed_plot, 
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
    facet_wrap(~ location_id)

plot(orv_near_pop_sus_plot)

#near site incidence dynamics
orv_near_inf_plot <- ggplot(data = orv_near_pop_dynamics_detailed_plot_df) + 
    geom_line(aes(x = time, 
                  y = Inf1, 
                  color = strategy
                  ), size = 1) +
    facet_wrap(~ location_id)

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
    facet_wrap(~ location_id)

plot(orv_far_pop_sus_plot)

#far site incidence dynamics
orv_far_inf_plot <- ggplot(data = orv_far_pop_dynamics_detailed_plot_df) + 
    geom_line(aes(x = time, 
                  y = Inf1, 
                  color = strategy
    ), size = 1) +
    facet_wrap(~ location_id)

plot(orv_far_inf_plot)


