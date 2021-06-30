library(dplyr)
library(tibble)
library(tidyr)




# inventory_doses() ----
#' a function to inventory/track daily vaccine usage. 
#' It will also track the number ice packs used and 
#' replaced throughout the course of the campaign

track_vax <- function(team_type = 'mobile'
                      , n_team_type = 1
                      , vial_type = 'dose10'
                      , is_fcc = TRUE
                      , campaign_duration
                      , tp_expected
                      , init_stock
                      , buffer_size = 0.1 
                      , run_type = 'det'
                      , browse = F){ 
    #initial values
    doses_at_base <- init_stock
    doses_new <- 0
    doses_surplus <- 0
    doses_shortage <- 0
    doses_for_day_campaign <- 0
    doses_per_team <- tp_expected * (1 + buffer_size)
    encounters_actual <- list(rep(0, times  = n_team_type))
    encounters_expected <- list(rep(0, times = n_team_type))
    
    
    
    #' the model: tracks how many doses are in the fridge at the base (doses_at_base), how 
    #' many fresh ones are taken out daily (doses_new) and how many are unused for
    #' that day and should be 
    #' prioritised the next day (doses_surplus). 
    #' 
    #' Assumptions: I am going to assume that we'll spread the 
    #' unused doses (or rather vaccines) over the teams to ensure they are used 
    #' up the next day, i.e, if the teams are more than one.
    #' 
    if(browse)browser()
    # initial_values <- data.frame(campaign_day = 0,
    #                              vax_at_base = doses_at_base[1], 
    #                              vax_new = doses_new[1], 
    #                              vax_old = doses_surplus[1],
    #                              vax_total = doses_for_day_campaign[1]
    #                              )
    # 
    
    t <- 1
    while (t <= campaign_duration & doses_at_base[t] >= doses_for_day_campaign[t]) {
        
        #compute the actual encounters at each time step.    
        if(run_type == 'stoc'){
            encounters_actual[[t + 1]] = rpois(n = n_team_type, lambda = tp_expected)
        }else if(run_type == 'det'){
            encounters_actual[[t + 1]] = rep(tp_expected, times = n_team_type)  
        }else{stop('unknown run type')}
        
        #' we compute the expected encounters at each time step as proxy to determine
        #' how many doses to transport, buffered for uncertainty. 
        #' Even though deterministic,
        #' and inefficient, I do this to allow for consistency
        #'       
        encounters_expected[[t + 1]] = rep(doses_per_team, n_team_type)
        
        #the vaccine model
        doses_new[t + 1] = sum(encounters_expected[[t + 1]]) - doses_surplus[t]
        
        doses_for_day_campaign[t + 1] = doses_new[t + 1] + doses_surplus[t]
        
        doses_usage = doses_new[t + 1] + doses_surplus[t] - sum(encounters_actual[[t + 1]]) #
        
        doses_surplus[t + 1] = ifelse(doses_usage < 0, 0, doses_usage)
        
        doses_shortage[t + 1] = ifelse(doses_usage < 0, abs(doses_usage), 0)
        
        doses_at_base[t + 1] = doses_at_base[t] - doses_new[t + 1]
        
        t <- t + 1
    }
    
    out <- tibble(campaign_day = 1:t-1,
                      doses_at_base = doses_at_base
                     # vax_at_base = ifelse(vial_type == 'monodose', doses_at_base, doses_at_base/10), 
                      , doses_new = doses_new
                      #vax_new = ifelse(vial_type == 'monodose', doses_new, doses_new/10),
                      , doses_surplus = doses_surplus
                     # vax_surplus = ifelse(vial_type == 'monodose', doses_surplus, doses_surplus/10),
                      , doses_shortage = doses_shortage
                      #vax_shortage = ifelse(vial_type == 'monodose', doses_shortage, doses_shortage/10),
                      , doses_for_day_campaign = doses_for_day_campaign
                     # vax_for_day_campaign = ifelse(vial_type == 'monodose', doses_for_day_campaign, doses_for_day_campaign/10)
                      )
    
    return(list(out = out, team_encounters_daily = encounters_actual, team_type = team_type, vial_type = vial_type, is_fcc = is_fcc))
}


example_results <- track_vax(team_type = 'mobile'
                             , n_team_type = 2
                             , vial_type = 'dose10'
                             , is_fcc = TRUE
                             , campaign_duration = 10
                             , tp_expected = 250
                             , init_stock = 2500
                             , buffer_size = 0 
                             , run_type = 'stoc'
                             , browse = F
)

example_results$out
