#rm(list = ls())

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


######################################################
#Plot parameters
######################################################
#Control parameters
display_epi_plots <- TRUE
save_epi_plots <- TRUE



###############################################################################
#Running the simulations for each strategy
###############################################################################

strategy_names_subset <- names(strategy_analysis_list)[c(2, 4, 6, 7)]
orv_strategy_results <- list()
for (i in 1:length(strategy_names_subset)) {
    orv_strategy_results[[strategy_names_subset[i]]] <- runSimulations(
        R0 = orv_model_params$R0 # transmission coeficient
        , run_time = orv_model_params$model_time # 1 yr!
        , pop = initializePop(N = site_data$far_pop, initPropImmune = 0.25, I0 = 1)
        , strategy_name = strategy_names_subset[i]
        , vaxDay = as.numeric(subset(strategy_campaign_prep_delays, strategy == strategy_names_subset[i])['mt_freezing_time'])
        , orv_duration = as.numeric(subset(strategy_team_days_long, strategy_name == strategy_names_subset[i] & team_type == 'mobile_team')[ ,'team_days']) #for now we're only looking at the far campaigns 
        , vax_eff = orv_model_params$vaccine_efficacy
        , team_performance = as.numeric(sc_model_params$vax_rate['mobile_team'])
        , time_to_immunity = orv_model_params$immune_response_timing
        , browse = F
        ) 
}

################################################################################
#Plotting the SC decision's consequence on the epidemic

################################################################################

#pre-processing the orv model output
#1A. Extract the detailed dynamics and bind them into one df
orv_results_detailed <- orv_strategy_results %>% 
    purrr::map('Detailed')
#data for plotting
epi_dyn_detailed <- do.call("rbind", args = c(orv_results_detailed, make.row.names = F)) %>% 
    mutate(strategy = factor(strategy))

#1B. Extract the sub-summed results and bind them into one df
orv_results_collapsed <- orv_strategy_results %>% 
    purrr::map('Collapsed')
#data for plotting
epi_dyn_summed <- do.call("rbind", args = c(orv_results_collapsed, make.row.names = F)) %>% 
    mutate(strategy = factor(strategy))




################################################################################
#Plots
################################################################################

#1. Final epidemic size taken to be class I5 of I1 to I6: Just an assumption
fin_epi_size <- ggplot(data = epi_dyn_detailed %>% filter(time <= 200)) + 
    geom_point(aes(x = time, y = Inf5, color = strategy), size = 2) + 
    geom_line(aes(x = time, y = Inf5, color = strategy), size = 1) +
    labs(x = 'Time (days)', y = 'Final epidemic size') + 
    guides(color = guide_legend(ncol = 2, nrow = 2, byrow = TRUE)) + 
    theme(legend.position = 'bottom') +
    scale_color_manual(name = "Strategy"
                      , values = c('green', 'blue', 'black', 'red', 'orange')
                       , labels = x_axis_labels
                       , breaks = strategy_names_subset
                       )

if (display_epi_plots) {
    plot(fin_epi_size)  
}

if(save_plots){
    ggsave(file = 'figures/final_epi_size.pdf', plot = fin_epi_size)
}
#View(head(orv_plot_dat %>% filter(strategy == 'monodose_fcc'), n = 20))



#2. Incidence: This plot is just a backward translation of the final size plot because the model is deterministic

# incidence_plot <- ggplot(data = epi_dyn_detailed) + 
#     geom_point(aes(x = time, y = Exp1, color = strategy), size = 2) + 
#     geom_line(aes(x = time, y = Exp1, color = strategy), size = 1) +
#     labs(x = 'Time (days)', y = 'New cases (exposed)') + 
#     scale_color_manual(name = "Strategy"
#                        , values = c('green', 'blue', 'black', 'red')
#                        , labels = c("10-dose FCC", "Monodose FCC", "Mixed FCC", 'Part OCC')
#                        , breaks = c("dose10_fcc", "monodose_fcc", "mixed_fcc", 'part_occ')
#     )
# 
# if (display_epi_plots) {
#     plot(incidence_plot)  
# }

#################################################################################
#Sensitivity on number of freezers
#################################################################################

#mf314_quant <- 1:10 #we currently run the sc model on only one freezer. What if the base has more than 1?