library(ggplot2)
library(dplyr)

source('./scripts/orv_model_functions.r')

# no vaccination baseline
times <- 1:200
N <- 50000

pop_init <- c(S = 1-1/N, 
              E = 0, 
              I = 1/N, 
              R = 0, 
              K = 0
)

orv_params <- c(B = 0.5, 
                r = 1/7, 
                g = 1/7, 
                q = 1, 
                P = 0.2, 
                Dt = 10, 
                vax_day = 3
                )

no_orv_params <- c(B = 0.5, 
                   r = 1/7, 
                   g = 1/7, 
                   q = 1, 
                   P = 0, 
                   Dt = 0, 
                   vax_day = Inf
                   )

orv_results <- as.data.frame(lsoda(pop_init, times, simod, orv_params))
no_orv_results <- as.data.frame(lsoda(pop_init, times, simod, no_orv_params))


no_orv_dynamics_plot <- ggplot() +
    geom_line(data = no_orv_results, 
              aes(x = time, 
                  y = S
                  ),
              color = 'blue',
              size = 2,
              linetype = 2
              ) + 
    geom_line(data = no_orv_results, 
              aes(x = time, 
                  y = I
              ),
              color = 'red',
              linetype = 2,
              size = 2
              ) + 
    geom_line(data = no_orv_results, 
              aes(x = time, 
                  y = R
              ),
              color = 'green',
              linetype = 2,
              size = 2
    ) + 
    labs(x = 'Time (Days)', 
         y = 'Proportion of individuals',
         title = 'Transmission dynamics (No vaccination)') +
    theme_minimal()

plot(no_orv_dynamics_plot)

orv_dynamics_plot <- ggplot() +
    geom_line(data = orv_results, 
              aes(x = time, 
                  y = S
              ),
              color = 'blue',
              size = 2,
              linetype = 2
    ) + 
    geom_line(data = orv_results, 
              aes(x = time, 
                  y = I
              ),
              color = 'red',
              linetype = 2,
              size = 2
    ) + 
    geom_line(data = orv_results, 
              aes(x = time, 
                  y = R
              ),
              color = 'green',
              linetype = 2,
              size = 2
    ) + 
    labs(x = 'Time (Days)', 
         y = 'Proportion of individuals',
         title = 'Transmission dynamics following vaccination') +
    theme_minimal()

plot(orv_dynamics_plot)
