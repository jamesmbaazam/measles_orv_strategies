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
#1. Delays caused by preparing teams and logistics
delay_to_start <- dplyr::rename(freezing_time_results, delay_to_start = time)
#campaign_duration <- subset(td_results, team_type == 'Mobile team', select = -team_type)

#2. The site campaign duration is assumed to be the slowest of the two team types
campaign_duration <- spread(td_results, key = team_type, value = team_days) %>%
    rowwise() %>% 
    mutate(orv_length = max(fixed_post, mobile_team))

sc_to_epi_inputs <- left_join(campaign_duration, delay_to_start, by = 'strategy')



###############################################################################
#Running the simulations for each strategy
###############################################################################


# orv_strategy_results <- purrr:::map_df(strategy_list 
#                                , runSimulations
#                              #  , strategy 
#                                , vaxDay = as.numeric(subset(delay_to_start, strategy == strategy_list)['delay_to_start'])
#                                , orv_duration = as.numeric(subset(sc_to_epi_inputs, strategy == strategy_list)['orv_length'])
#                                , R0 = orv_model_params$R0 # transmission coeficient
#                                , run_time = orv_model_params$model_time # 1 yr!
#                                , pop = initializePop(N = site_data$far_pop, initPropImmune = 0.5, I0 = 1)
#                                , vax_eff = orv_model_params$vaccine_efficacy
#                                , team_performance = as.numeric(sc_model_params$vax_rate['mobile_team'])
#                                , time_to_immunity = orv_model_params$immune_response_timing
#                                , browse = F
#                                )

orv_strategy_results <- list()
for (i in 1:length(strategy_list)) {
    orv_strategy_results[[strategy_list[i]]] <- runSimulations(
        R0 = orv_model_params$R0 # transmission coeficient
        , run_time = orv_model_params$model_time # 1 yr!
        , pop = initializePop(N = site_data$far_pop, initPropImmune = 0.25, I0 = 1)
        , strategy_name = strategy_list[i]
        , vaxDay = as.numeric(subset(delay_to_start, strategy == strategy_name)['delay_to_start'])
        , orv_duration = as.numeric(subset(sc_to_epi_inputs, strategy == strategy_name)['mobile_team']) #for now we're only looking at the far campaigns 
        , vax_eff = orv_model_params$vaccine_efficacy
        , team_performance = as.numeric(sc_model_params$vax_rate['mobile_team'])
        , time_to_immunity = orv_model_params$immune_response_timing
        , browse = F) 
}


################################################################################
#Plotting the SC decision's consequence on the epidemic

################################################################################

#pre-processing the orv model output
#1. Combine this into a single df

orv_results_detailed <- orv_strategy_results %>% 
    purrr::map('Detailed')


orv_plot_dat <- do.call("rbind", args = c(orv_results_detailed, make.row.names = F))

orv_plot_dat <- orv_plot_dat %>% mutate(strategy = factor(strategy))


#plot of cases
fin_epi_size <- ggplot(data = orv_plot_dat %>% group_by(strategy)) + 
    geom_point(aes(x = time, y = Inf5, group = strategy)) + 
    geom_line(aes(x = time, y = Inf5, color = strategy)) +
    labs(x = 'Time (days)', y = 'Final epidemic size') + 
    scale_color_manual(name = "Strategy"
                      , values = c('green', 'blue', 'black', 'red')
                       , labels = c("10-dose FCC", "Monodose FCC", "Mixed FCC", 'Part OCC')
                       , breaks = c("dose10_fcc", "monodose_fcc", "mixed_fcc", 'part_occ')
                       )

if (display_epi_plots) {
    plot(fin_epi_size)  
}



#plot of susceptibles
# SusProgression_plot <- ggplot(data = orv_plot_dat) + 
#     geom_point(aes(x = time, y = totalSus   , color = strategy)) + 
#     geom_line(aes(x = time, y = totalSus    , color = strategy)) +
#     labs(x = 'time (days)', y = 'Susceptibles') + 
#     scale_color_manual(name = "Strategy"
#                        , values = c('green', 'blue', 'black', 'red')
#                        , labels = c("10-dose FCC", "Monodose FCC", "Mixed FCC", 'Part OCC')
#                        , breaks = c("dose10FCC", "monodoseFCC", "mixedFCC", 'partOCC')
#     )
# if (display_epi_plots) {
# plot(SusProgression_plot)
# }
# 

