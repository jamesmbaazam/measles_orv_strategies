source('scripts/epi_functions.R')


orv_dat <- p_red(R = R0, 
                vaccine_efficacy = 0.95
                 , target_vaccination = 0.9
                 , intervention_length = 15
                 , N = far_pop
                 )
plot(orv_dat)
