source('scripts/epi_functions.R')
source('scripts/parameters.R')

R <- 20 
vaccine_efficacy <- 0.9
target_vaccination <- 0.7
intervention_length <- 10
mtime = 120
LP = 7 #LP = Latent period
IP = 7 #IP = Infectious period
N <- far_pop
step = 1
beta <- R / IP




xstrt <- c(S = 1 - 1 / N, E = 0, I = 1 / N, R = 0, K = 0)



orv_dat <- p_red(R = R0, 
                vaccine_efficacy = 0.95
                 , target_vaccination = 0.9
                 , intervention_length = 15
                 , N = far_pop
                 )


plot(orv_dat)
