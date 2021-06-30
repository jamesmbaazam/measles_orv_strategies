# inventory_doses() ----
#' a function to inventory/track how the vaccines are taken in or outside of the
#' cold chain and utilised. It will also track the number ice packs used and 
#' replaced throughout the course of the campaign

# determine_vax_events <- function(){
#     
# }


inventory_doses <- function(team_type
                      , team_type_count
                      , vial_type
                      , is_fcc = TRUE
                      , campaign_t
                      , tp_expected
                      , init_stock
                      , buffer_size = 0.1 
                      , run_type = 'det'){ 

# parameters ----
#' team_type = short for "team type". Options: "fixed" or "mobile"
#' run_type = 'det' for deterministics and 'stoc' for stochastic
#' team_type_count = number of teams of a single type
#' is_fcc = by default set to TRUE, is a boolean for switching between full cold chain and out of cold chain strategies
#' campaign_t = campaign duration
#' init_stock = the initial stock size of a vaccine of one type for one team type
#' buffer_size = a parameter for adjusting how many vaccines will be taken out daily (ranges from 0 to 1); default is 0.1 or 10%
#' tp_expected = expected number of vaccinations per single team, aka, team performance
#' vial_type = options: 'dose10' 'monodose'.
 
# assumptions ---- 
#' we assume we know the average team performance per team type and that all teams
#' vaccinate at the same rate (for simplicity).
#' I am currently assuming that the number of actual vaccinations is Poisson 
#' distributed because:  (a) Poisson has the same mean and variance, which the experts 
#' know the value for, (b) the encounters are happen within a fixed period (daily)
#' (c) 
# actual vaccination events ---- 
#' tp_actual = actual number of vaccinations per single team, aka, team performance


  
if (run_type == 'det') 
  {
  
  # First day (Day = 1) ----
#  stock_at_base <- init_stock #' here, we are assuming that initially, all vaccines arrive in fcc even if they are meant for occ

  stock_per_team <- tp_expected * (1 + buffer_size) #Number of doses a team is supposed to take in total

  stock_fresh <- stock_per_team #fresh vax from the fridge
  
  encounters <- tp_expected ##number of children the team encountered for that day; this is the deterministic part
  
  stock_priority <- stock_per_team -  encounters#' The priority stock are those that were sent for the previous day's campaign but returned unused hence must be prioritised for the next day's trip
}

for (day in 2: campaign_t + 1) {
  
    stock_per_team[day] <- tp_expected * (1 + buffer_size)
  
    stock_fresh[day] <- stock_per_team[day] - stock_priority[day - 1]

    encounters[day] <- tp_expected
    
    stock_priority[day] <- stock_per_team[day] - encounters[day] 
    
}
else if (run_type == 'stoc') {
  
  stock_per_team <- tp_expected * (1 + buffer_size) #Number of doses a team is supposed to take in total
  
  stock_fresh <- stock_per_team #fresh vax from the fridge
  
  encounters <- rpois(n = 1, lambda = tp_expected) ##number of children the team encountered for that day; this is the stochastic part
  
  stock_priority <- stock_per_team -  encounters#' The priority stock are those that were sent for the previous day's campaign but returned unused hence must be prioritised for the next day's trip
}
  
  for (day in 2: campaign_t + 1) {
    
    stock_per_team[day] <- tp_expected * (1 + buffer_size)
    
    stock_fresh[day] <- stock_per_team[day] - stock_priority[day - 1]
    
    encounters[day] <- rpois(n = 1, lambda = tp_expected)
    
    stock_priority[day] <- stock_per_team[day] - encounters[day] 
    
  }
  
  
  
#   
#   for (day in 1: campaign_t){
#     
#     team_encounters <- rpois(n = 1, lambda = tp_expected) #this is the stochastic part
#     
#     if (any(team_encounters > stock_per_team)) { #if any of the teams encounter more kids than the vax they have for the trip, there will be a shortage, which they can learn from to adapt their decision (not yet implemented)
#       
#       tp_actual[day + 1] <- mean(c(team_encounters[team_encounters <= stock_per_team], rep(stock_per_team, times = length((team_encounters > stock_per_team) == TRUE))))
#       
#       stock_for_reuse[day + 1] <- sum(stock_per_team - team_encounters[team_encounters < stock_per_team])
#       
#       }else{
#         
#       tp_actual[day + 1] <- mean(team_encounters)
#       
#       stock_for_reuse[day + 1] <- sum(stock_per_team - team_encounters[team_encounters < stock_per_team])
#     
#       }
#     
#     
#  
#     stock_for_trip[day + 1] <- tp_expected * team_type_count * (1 + buffer_size) #at the end of the day, teams took out this number of vaccines
#     
#   
#     stock_fcc[day + 1] <-  stock_fcc[day] - ifelse(stock_for_reuse[day + 1] > 0, stock_for_trip[day + 1] - stock_for_reuse[day + 1], stock_for_trip[day + 1]) 
#     
#      stock_occ[day + 1] <- stock_occ[day] + ifelse(is_fcc, 0 , stock_for_reuse[day])
#   }
#   
# } 
 #  else{
 #    stop()
 # }

inventory <- data.frame(campaign_day = 0:campaign_t
                        , stock_per_team = stock_per_team
                        , stock_fresh = stock_fresh
                        , stock_priority_day_end = stock_priority
                       # , stock_used = stock_for_trip - stock_for_reuse
                        , children_encountered = encounters
                        )

return(inventory)

}

# trial runs ----

# deterministic run
det_run <- inventory_doses(team_type = 'fixed_team', team_type_count = 1, vial_type = 'monodose', campaign_t = 4, tp_expected = 450, init_stock = 4500)

det_run

# stochastic run
stock_run <- inventory_doses(team_type = 'fixed_team', team_type_count = 1, vial_type = 'monodose', campaign_t = 10, tp_expected = 450, init_stock = 4500, run_type = 'stoc')

stock_run

# many stochastic runs
stoc_runs_reslist <- lapply(1:200, inventory_doses, team_type_count = 1, campaign_t = 10, run_type = 'stoc', tp_expected = 250, init_stock = 2500)

# plot a histogram of the times that stock were unused on first trip
stock_for_reuse_res <- unlist(lapply(stoc_runs_reslist, '[', 'stock_for_reuse'), use.names = F)
hist(stock_for_reuse_res, xlab = 'Unused vax occ after first trip', main = '')
