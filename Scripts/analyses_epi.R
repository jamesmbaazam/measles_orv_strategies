rm(list = ls())

#packages
library('ggplot2')
library('dplyr')
library('reshape2')
library('purrr')
library('gridExtra')
library('tidyr')





#scripts
#source('scripts/epi_functions.R')
source('scripts/parameters.R')
source('scripts/measlesFunctions.R')
source('scripts/analyses_supply_chain.R')

# R0 <- 20 
# vaccine_efficacy <- 0.9
#mtime = 60

#parameters
# steps = 1:60
# beta <- orv_model_params$R0/orv_model_params$IP
# time_to_immunity <- 7 # seven days before people are immune




#SC outputs as inputs to Epi model
delay_to_start <- dplyr::rename(freezing_time_results, delay_to_start = time)
campaign_duration <- subset(td_results, team_type == 'Mobile team', select = -team_type)
sc_to_epi_inputs <- left_join(campaign_duration, delay_to_start, by = 'strategy')





out_dose10FCC <- runSimulations(
    R0 = orv_model_params$R0 # transmission coeficient
    , run_time = orv_model_params$model_time # 1 yr!
    , pop = initializePop(N = site_data$far_pop, initPropImmune = 0.5, I0 = 1)
    , vaxDay = as.numeric(subset(delay_to_start, strategy == '10-dose FCC')['delay_to_start'])
    , orv_duration = as.numeric(subset(sc_to_epi_inputs, strategy == '10-dose FCC')['team_days'])
    , vax_eff = orv_model_params$vaccine_efficacy
    , team_performance = as.numeric(sc_model_params$vax_rate['mobile_team'])
    , time_to_immunity = orv_model_params$immune_response_timing
    , browse = F)

#View(out_dose10FCC$Detailed)

out_monodoseFCC <- runSimulations(
    R0 = orv_model_params$R0 # transmission coeficient
    , run_time = orv_model_params$model_time # 1 yr!
    , pop = initializePop(N = site_data$far_pop, initPropImmune = 0.5, I0 = 1)
    , vaxDay = as.numeric(subset(delay_to_start, strategy == 'Monodose FCC')['delay_to_start'])
    , orv_duration = as.numeric(subset(sc_to_epi_inputs, strategy == 'Monodose FCC')['team_days'])
    , vax_eff = orv_model_params$vaccine_efficacy
    , team_performance = as.numeric(sc_model_params$vax_rate['mobile_team'])
    , time_to_immunity = orv_model_params$immune_response_timing
    , browse = F)
#View(out_monodoseFCC$Detailed)

out_mixedFCC <- runSimulations(
    R0 = orv_model_params$R0 # transmission coeficient
    , run_time = orv_model_params$model_time # 1 yr!
    , pop = initializePop(N = site_data$far_pop, initPropImmune = 0.5, I0 = 1)
    , vaxDay = as.numeric(subset(delay_to_start, strategy == 'Mixed FCC')['delay_to_start'])
    , orv_duration = as.numeric(subset(sc_to_epi_inputs, strategy == 'Mixed FCC')['team_days'])
    , vax_eff = orv_model_params$vaccine_efficacy
    , team_performance = as.numeric(sc_model_params$vax_rate['mobile_team'])
    , time_to_immunity = orv_model_params$immune_response_timing
    , browse = F)
#View(out_mixedFCC$Detailed)

out_partOCC <- runSimulations(
    R0 = orv_model_params$R0 # transmission coeficient
    , run_time = orv_model_params$model_time # 1 yr!
    , pop = initializePop(N = site_data$far_pop, initPropImmune = 0.5, I0 = 1)
    , vaxDay = as.numeric(subset(delay_to_start, strategy == 'Part OCC')['delay_to_start'])
    , orv_duration = as.numeric(subset(sc_to_epi_inputs, strategy == 'Part OCC')['team_days'])
    , vax_eff = orv_model_params$vaccine_efficacy
    , team_performance = as.numeric(sc_model_params$vax_rate['mobile_team'])
    , time_to_immunity = orv_model_params$immune_response_timing
    , browse = F)

#View(out_partOCC$Detailed)



incidence_plot <- ggplot(data = out_dose10FCC$Detailed) + geom_point(aes(x = time, y = Sus1)) + geom_line(aes(x = time, y = Sus1 )) +
    geom_point(data = out_monodoseFCC$Detailed, aes(x = time, y = Sus1), color = 'red') + geom_line(data = out_monodoseFCC$Detailed, aes(x = time, y = Sus1), color = 'red') +
    geom_point(data = out_mixedFCC$Detailed, aes(x = time, y = Sus1), color = 'blue') + geom_line(data = out_mixedFCC$Detailed, aes(x = time, y = Sus1), color = 'blue') +
    geom_point(data = out_partOCC$Detailed, aes(x = time, y = Sus1), color = 'green') + geom_line(data = out_partOCC$Detailed, aes(x = time, y = Sus1), color = 'green') +
    labs(x = 'time (days)', y = 'Incidence')

plot(incidence_plot)






# 
# #xstrt <- c(S = 1 - 1 / N, E = 0, I = 1 / N, R = 0, K = 0)
# xstrt <- c(S = site_data$far_pop
#            , E = 0
#            , I = 1
#            , R = 0
#            , K = 0
# )
# 
# 
# # orv_dose10 <- p_red(R = orv_model_params$R0
# #                     , vaccine_efficacy = orv_model_params$vaccine_efficacy
# #                     , delay = as.numeric(subset(delay_to_start, strategy == 'Monodose FCC')['delay_to_start'])
# #                     , intervention_length = as.numeric(subset(sc_to_epi_inputs, strategy == 'Monodose FCC')['team_days'])
# #                     , N = site_data$far_pop
# #                     , vax_rate = as.numeric(sc_model_params$vax_rate['fixed_team'])
# #                     , mtime = orv_model_params$model_time
# #                     )
# # 
# # 
# # plot(orv_dat)
# 
# #10-dose FCC parameters
# par_dose10FCC <- c(B = beta 
#                    , r = 1 / orv_model_params$LP
#                    , g = 1 / orv_model_params$IP
#                    , vax_eff = orv_model_params$vaccine_efficacy
#                    , immunity_delay = orv_model_params$immune_response_timing
#                    , orv_dur = as.numeric(subset(sc_to_epi_inputs, strategy == '10-dose FCC')['team_days'])
#                    , campaign_delay = as.numeric(subset(delay_to_start, strategy == '10-dose FCC')['delay_to_start'])
#                    , vax_rate = as.numeric(sc_model_params$vax_rate['fixed_team'])
# )
# 
# #monodose FCC parameters
# par_monodoseFCC <- c(B = beta 
#                      , r = 1 / orv_model_params$LP
#                      , g = 1 / orv_model_params$IP
#                      , vax_eff = orv_model_params$vaccine_efficacy
#                      , immunity_delay = orv_model_params$immune_response_timing
#                      , orv_dur = as.numeric(subset(sc_to_epi_inputs, strategy == 'Monodose FCC')['team_days'])
#                      , campaign_delay = as.numeric(subset(delay_to_start, strategy == 'Monodose FCC')['delay_to_start'])
#                      , vax_rate = as.numeric(sc_model_params$vax_rate['fixed_team'])
# )
# 
# 
# #Mixed FCC parameters
# par_mixedFCC <- c(B = beta 
#                   , r = 1 / orv_model_params$LP
#                   , g = 1 / orv_model_params$IP
#                   , vax_eff = orv_model_params$vaccine_efficacy
#                   , immunity_delay = orv_model_params$immune_response_timing
#                   , orv_dur = as.numeric(subset(sc_to_epi_inputs, strategy == 'Mixed FCC')['team_days'])
#                   , campaign_delay = as.numeric(subset(delay_to_start, strategy == 'Mixed FCC')['delay_to_start'])
#                   , vax_rate = as.numeric(sc_model_params$vax_rate['fixed_team'])
# )
# 
# #Partial OCC parameters
# par_partOCC <- c(B = beta 
#                  , r = 1 / orv_model_params$LP
#                  , g = 1 / orv_model_params$IP
#                  , vax_eff = orv_model_params$vaccine_efficacy
#                  , immunity_delay = orv_model_params$immune_response_timing
#                  , orv_dur = as.numeric(subset(sc_to_epi_inputs, strategy == 'Part OCC')['team_days'])
#                  , campaign_delay = as.numeric(subset(delay_to_start, strategy == 'Part OCC')['delay_to_start'])
#                  , vax_rate = as.numeric(sc_model_params$vax_rate['fixed_team'])
# )
# 
# 
# 
# 
# #model runs
# out_dose10FCC <- as.data.frame(lsoda(xstrt, steps, simod, par_dose10FCC))
# out_monodoseFCC <- as.data.frame(lsoda(xstrt, steps, simod, par_monodoseFCC))
# out_mixedFCC <- as.data.frame(lsoda(xstrt, steps, simod, par_mixedFCC))
# out_partOCC <- as.data.frame(lsoda(xstrt, steps, simod, par_partOCC))
# 
# 
#plots





