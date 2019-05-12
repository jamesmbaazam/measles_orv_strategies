#packages
library('ggplot2')
library('dplyr')
library('reshape2')
library('purrr')
library('gridExtra')
library('tidyr')





#scripts
source('scripts/epi_functions.R')
source('scripts/parameters.R')
source('scripts/analyses_supply_chain.R')

# R0 <- 20 
# vaccine_efficacy <- 0.9
# target_vaccination <- 1
# intervention_length <- 10
# mtime = 120
# LP = 7 #LP = Latent period
# IP = 7 #IP = Infectious period
# N <- site_table$added_sites$far_pop
# step = 1
# beta <- R0 / IP
# 
# cam
campaign_duration <- spread(td_results, strategy, team_days)

xstrt <- c(S = 1 - 1 / N, E = 0, I = 1 / N, R = 0, K = 0)



orv_dat <- p_red(R = R0, 
                vaccine_efficacy = 0.95
                 , target_vaccination = 0.7
                 , intervention_length = 15
                 , N = site_table$added_sites$far_pop
                 )


plot(orv_dat)
