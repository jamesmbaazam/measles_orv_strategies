# require(shiny)
require(deSolve)

######################################################
# SEIR model
######################################################
################
# Parameters
# B = transmission rate
# 1/r = latent period
# 1/g = infectious period
# q = vaccine efficacy
#
# P = target coverage
# campaign_duration = length of vaccination campaign
# vax_day = Day of campaign start
# USAGE:
# times<-1:100
# xstrt<-c(S=.999,E=0,I=.001,R=0,K=0)
# par<-c(B=.5, r=1/7, g = 1/7, q = .8, P = 0, campaign_duration = 10, vax_day = 80)
# out<-as.data.frame(lsoda(xstrt,times,simod,par))
# plot(out$time,out$I,type="l")
#
#
# par<-c(B=.5, r=1/7, g = 1/7, q = .8, P = .99, campaign_duration = 10, vax_day = 50)
# out<-as.data.frame(lsoda(xstrt,times,simod,par))
# lines(out$time,out$I,col="red")
simod <- function(t, x, parms, browse = F) {
  if(browse) browser()
  S <- x[1]
  E <- x[2]
  I <- x[3]
  R <- x[4]
  K <- x[5]
  #
  with(as.list(parms), {
    Q <- ifelse(t < vax_day | t > vax_day + campaign_duration, 0, (-log(1 - P) / campaign_duration))
    dS <- -B * S * I - q * Q * S
    dE <- B * S * I - r * E
    dI <- r * E - g * I
    dR <- g * I + q * Q * S
    dK <- r * E
    res <- c(dS, dE, dI, dR, dK)
    list(res)
  })
}




#' run_orv_model
#'
#' @param strategy 
#' @param location_id 
#' @param mt_equip_type 
#' @param target_pop_size 
#' @param infectious_period 
#' @param vax_efficacy 
#' @param scenario_coverage 
#' @param vax_day 
#' @param scenario_campaign_duration 
#' @param R0 
#' @param browse 
#' @param latent_period 
#' @param I0 
#' @param max_time 
#'
#' @return
#' @export
#'
#' @examples
run_orv_model <- function(strategy, 
                          location_id, 
                          mt_equip_type, 
                          target_pop_size,
                          latent_period,
                          infectious_period,
                          I0,
                          R0,
                          vax_efficacy,
                          scenario_coverage,
                          vax_day,
                          scenario_campaign_duration,
                          max_time,
                          browse = F){
    
  if(browse) browser()
  
  #initial population
  pop_init <- c(S = target_pop_size -I0, 
                E = 0, 
                I = I0, 
                R = 0, 
                K = 0
                )
  beta <- R0/infectious_period
  
  model_time <- 1:max_time
  
  
  orv_params <- c(B = beta, 
                  r = 1/latent_period, 
                  g = 1/infectious_period, 
                  q = vax_efficacy, 
                  P = scenario_coverage, 
                  campaign_duration = scenario_campaign_duration, 
                  vax_day = vax_day
                  )
  
  results_df <- as.data.frame(lsoda(pop_init, times = model_time, simod, orv_params))
  
  scenario_table <- data.frame(strategy = rep(strategy, times = nrow(results_df)), 
                               location_id = rep(location_id, times = nrow(results_df)),
                               mt_equip_type = rep(mt_equip_type, times = nrow(results_df))
                               )
  
  sim_results <- cbind(scenario_table, results_df)

  return(sim_results)
  }