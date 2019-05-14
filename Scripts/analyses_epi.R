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

delay_to_start <- dplyr::rename(freezing_time_results, delay_to_start = time)
campaign_duration <- subset(td_results, team_type == 'Mobile team', select = -team_type)
sc_to_epi_inputs <- left_join(campaign_duration, delay_to_start, by = 'strategy')


xstrt <- c(S = 1 - 1 / N, E = 0, I = 1 / N, R = 0, K = 0)



orv_dose10 <- p_red(R = orv_model_params$R0
                    , vaccine_efficacy = orv_model_params$vaccine_efficacy
                    , intervention_length = as.numeric(subset(sc_to_epi_inputs, strategy == '10-dose FCC')['team_days'])
                    , N = site_data$far_pop
                    , vax_rate = as.numeric(sc_model_params$vax_rate['fixed_team'])
                    , mtime = orv_model_params$model_time
                    )


plot(orv_dat)
