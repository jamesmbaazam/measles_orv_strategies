# require(shiny)
library(deSolve)
# library(dplyr)
# library(tidyr)
# library(ggplot2)

######################################################
# SEIR model
######################################################
################
# Parameters
# B = transmission rate
# 1/r = latent period
# 1/g = infectious period
# vax_efficacy = vaccine efficacy
#
# coverage = target coverage
# campaign_duration = length of vaccination campaign
# vax_day = Day of campaign start
# USAGE:
# times<-1:100
# xstrt<-c(S=.999,E=0,I=.001,R=0,K=0)
# par<-c(B=.5, r=1/7, g = 1/7, vax_efficacy = .8, coverage = 0, campaign_duration = 10, vax_day = 80)
# out<-as.data.frame(lsoda(xstrt,times,simod,par))
# plot(out$time,out$I,type="l")
#
#
# par<-c(B=.5, r=1/7, g = 1/7, vax_efficacy = .8, coverage = .99, campaign_duration = 10, vax_day = 50)
# out<-as.data.frame(lsoda(xstrt,times,simod,par))
# lines(out$time,out$I,col="red")
#' Title
#'
#' @param t #model time
#' @param x #initial population vector
#' @param parms #list of parameters
#' @param coverage_correction # this is a parameter to correct the coverage from being a perfectly observed value. It's actual purpose is to check that the hazard rate in the model, the log transformation, does not go to infinity.
#' @param browse #for debugging
#'
#' @return
#' @export
#'
#' @examples

#A corrected version of the model above to track those who fail to sero-convert

simod <- function(t, x, parms, coverage_correction = 0.9999, browse = F) {
    if(browse) browser()
    S <- x[1]
    S_f <- x[2]
    E <- x[3]
    I <- x[4]
    R <- x[5]
    K <- x[6]
    #
    with(as.list(parms), {
        Q <- ifelse(t < vax_day | t > vax_day + campaign_duration, 0, (-log(1 - coverage*coverage_correction) / campaign_duration))
        dS <- - B * S * I - vax_efficacy * Q * S - (1 - vax_efficacy) * Q * S #corrected line
        dS_f <- (1 - vax_efficacy) * Q * S  -  B * S_f * I #Those who fail to sero-convert are subject to the force of infection
        dE <- B * S * I + B * S_f * I - r * E
        dI <- r * E - g * I
        dR <- g * I + vax_efficacy * Q * S
        dK <- r * E
        res <- c(dS, dS_f, dE, dI, dR, dK)
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
#' @param init_prop_immune #initial immune proportion, a number between 0 and 1
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
                          init_prop_immune,
                          R0,
                          vax_efficacy,
                          scenario_coverage,
                          vax_day,
                          predeployment_delay,
                          scenario_campaign_duration,
                          max_time,
                          browse = F){
    
  if(browse) browser()
  
  #initial population
  pop_init <- c(S = 1 - I0/target_pop_size - init_prop_immune, 
                E = 0, 
                I = I0/target_pop_size, 
                R = init_prop_immune, 
                K = 0
                )
  beta <- R0/infectious_period
  
  model_time <- 1:max_time
  
  
  orv_params <- c(B = beta, 
                  r = 1/latent_period, 
                  g = 1/infectious_period, 
                  vax_efficacy = vax_efficacy, 
                  coverage = scenario_coverage, 
                  campaign_duration = scenario_campaign_duration, 
                  predeployment_delay = predeployment_delay,
                  vax_day = vax_day
                  )
  
  results_df <- as.data.frame(lsoda(pop_init, times = model_time, simod, orv_params))
  
  scenario_table <- data.frame(strategy = rep(strategy, times = nrow(results_df)), 
                               location_id = rep(location_id, times = nrow(results_df)),
                               mt_equip_type = rep(mt_equip_type, times = nrow(results_df)),
                               predeployment_delay = rep(predeployment_delay, times = nrow(results_df))
                               )
  
  sim_results <- cbind(scenario_table, results_df)

  return(sim_results)
}



# no_orv_example_run <- run_orv_model(strategy = 'dummy', 
#                                     location_id = 1,
#                                     mt_equip_type = 'vaxCarr',
#                                     target_pop_size = 50000,
#                                     latent_period = 7,
#                                     infectious_period = 7,
#                                     I0 = 1,
#                                     init_prop_immune = 0.7,
#                                     R0 = 12,
#                                     vax_efficacy = 0,
#                                     scenario_coverage = 0,
#                                     vax_day = 0,
#                                     predeployment_delay = 0,
#                                     scenario_campaign_duration = 0,
#                                     max_time = 365,
#                                     browse = F
#                                     )
# 
# 
# plot(no_orv_example_run$time, no_orv_example_run$S, type = 'b', col = 'blue', ylim = c(0, 1))
# lines(no_orv_example_run$time, no_orv_example_run$E, type = 'b', col = 'purple')
# lines(no_orv_example_run$time, no_orv_example_run$I, type = 'b', col = 'red')
# lines(no_orv_example_run$time, no_orv_example_run$R, type = 'b', col = 'green')
# 
#                                     
# orv_example_run <- run_orv_model(strategy = 'dummy',
#                                  location_id = 1,
#                                  mt_equip_type = 'vaxCarr',
#                                  target_pop_size = 50000,
#                                  latent_period = 7,
#                                  infectious_period = 7,
#                                  I0 = 10,
#                                  init_prop_immune = 0.7,
#                                  R0 = 12,
#                                  vax_efficacy = 0.84,
#                                  scenario_coverage = 0.7,
#                                  vax_day = 2,
#                                  predeployment_delay = 21,
#                                  scenario_campaign_duration = 10,
#                                  max_time = 365,
#                                  browse = F 
#                                  )
# plot(orv_example_run$time, orv_example_run$R, type = 'b', col = 'green', ylim = c(0,1))
# lines(orv_example_run$time, orv_example_run$S, type = 'b', col = 'blue')
# lines(orv_example_run$time, orv_example_run$E, type = 'b', col = 'purple')
# lines(orv_example_run$time, orv_example_run$I, type = 'b', col = 'red')
                                    