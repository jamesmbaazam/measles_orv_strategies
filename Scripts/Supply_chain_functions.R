################################################################################
#'compute_rcw25_icepacks() & compute_varCarr_icepacks(): Calculate ice pack needs 
#' for each vax carrier and RCW25s, based on ambient temperature
################################################################################
compute_rcw25_icepacks <- function(amb_temp){
   switch (amb_temp,
           "below 40" = 12,
           "above 40" = 24
   ) #I use the upper and lower limits of ice required. Lowest to use is 12 and highest is 24. 
}


compute_vaxCarr_icepacks <- function(amb_temp){
   switch (amb_temp,
           "below 40" = 6,
           "above 40" = 8
   )
}




################################################################################
#'extract_near_pop() & extract_far_pop(): Extract population sizes for near and far
#' kids for further calculations
################################################################################
extract_near_pop <- function(df, site_rows_selected){
   df %>%
      dplyr::slice(site_rows_selected) %>% # for now, we are only going to concentrate on one site. User indicates which site to analyse
      .$near_pop
}


extract_far_pop <- function(df, site_rows_selected){
   df %>%
      dplyr::slice(site_rows_selected) %>% # for now, we are only going to concentrate on one site. User indicates which site to analyse
      .$far_pop
}


################################################################################
#' extract_site_team_size(): Extract size of allocated teams for a site from the 
#' sites table
################################################################################

extract_site_team_size <- function(df, site_rows_selected) { # for now, we are only going to concentrate on one site. User indicates which site to analyse
  teams <- df %>%
    dplyr::slice(site_rows_selected) %>%
    .$site_team_alloc # number of teams allocated to site
  return(teams)
}



##########
#calc_effective_doses(): adjusts for wastage and determines the actual number of doses we may be transporting
##########
calc_effective_doses <- function(dose_quantity, wastage){
   eff_doses <- dose_quantity*(1 - wastage/100)
   return(eff_doses)
}


################################################################################
#' print_site_team_dur(): Calculates and outputs to the UI how long the allocated 
#' teams will spend on a site
################################################################################

print_site_team_dur <- function(site_team_quant, td_fixed, td_mobile){ #td_fixed = team days for fixed site team, #td_mobile = team days for mobile team
 if (site_team_quant == 0) {
   renderText(
     print("<b> No teams were allocated. </b>")
  )
 } else if (site_team_quant == 1) {
   renderText({
     paste(
       "Teams will work sequentially.",
       '<br>',
       "First, the <b> FIXED post </b> team will spend:",
       td_fixed,
       "day(s).",
       "<br>",
       "Afterwards, the <b> MOBILE </b> team will spend:",
       td_mobile,
       'day(s).', 
       "<br>",
       "<b> Total: </b>", td_fixed + td_mobile,
       'day(s).'
     )
   })
 } else if (site_team_quant == 2) {
    renderText({
       paste(
          "The <b> Fixed post </b> team will spend",
          td_fixed,
          "day(s).",
          "<br>",
          "The <b> Mobile </b> team will spend",
          td_mobile, 
          "day(s).", 
          "<br>",
          "<b> Total: </b>", 
          td_fixed + td_mobile,
          'day(s).'
       )
    })
 }else {
   renderText({
     paste(
        'The',
        site_team_quant - 1,
       "<b> Fixed post </b> teams will each spend",
       round((td_fixed / (site_team_quant - 1)), digits = 1),
       "day(s).",
       "<br>",
       "The <b> Mobile </b> team will spend",
       td_mobile, "day(s).", 
       "<br>",
       "<b> Total: </b>", td_fixed + td_mobile,
       'day(s).'
     )
   })
 }
}

################################################################################
#' team days calculations
################################################################################

calc_monodose_team_days <- function(target_pop, 
                                    carrier_vol_capacity, 
                                    team_performance = tp_mobile #how many you are expected to vaccinate on average per day
                                    ) 
   {
   if ((carrier_vol_capacity >= target_pop) & (target_pop <= team_performance)) {
      return(1) #we can carry more doses than the target population size and are within the maximum kids we are expected to vaccinate, so it'll take us 1 day
   }
   else if ((carrier_vol_capacity > team_performance) & (team_performance >= target_pop)) {
      return(1) #we can carry more than we are expected to vaccinate but the target population is less than how many kids we are expected to vaccinate, so it'll take us 1 day
   }
   else if ((carrier_vol_capacity > team_performance) & (team_performance < target_pop)) {
      return(round(target_pop / team_performance, 3)) #we can carry more than we are expected to vaccinate but the target population is more than how many kids we are expected to vaccinate, so the team performance becomes the constraint
   }
   else if ((target_pop > carrier_vol_capacity) & (carrier_vol_capacity <= team_performance)) {
      return(round(target_pop / carrier_vol_capacity, 3)) # we can't carry enough doses to even vaccinate the expected number of kids per day, so the volume capacity becomes a constrainst 
   }
   # else if (carrier_vol_capacity > team_performance) { 
   #    return(round(target_pop / team_performance, 2)) #We can carry more than we are expected to, so the team performance becomes the constraint
   # }
}


calc_dose10_team_days <- function(target_pop, 
                                  dose10_wastage,
                                  vaxCarr_capacity,
                                  team_performance = tp_mobile)  #how many you are expected to vaccinate on average per day 
{
   effective_doses <- ceiling(vaxCarr_capacity * (1 - dose10_wastage/100)) #effectively, how many vaccinations is a mobile team actually undertaking?
   if ((effective_doses >= target_pop) & (target_pop <= team_performance)) {
      return(1) #we can carry more doses than the target population size and are within the maximum kids we are expected to vaccinate, so it'll take us 1 day
   }
   else if ((effective_doses > team_performance) & (team_performance >= target_pop)) {
      return(1) #we can carry more than we are expected to vaccinate but the target population is less than how many kids we are expected to vaccinate, so it'll take us 1 day
   }
   else if ((effective_doses > team_performance) & (team_performance < target_pop)) {
      return(round(target_pop / team_performance, 3)) #we can carry more than we are expected to vaccinate but the target population is more than how many kids we are expected to vaccinate, so the team performance becomes the constraint
   }
   else if ((target_pop > effective_doses) & (effective_doses <= team_performance)) {
      return(round(target_pop / effective_doses, 3)) # we can't carry enough doses to even vaccinate the expected number of kids per day, so the volume capacity becomes a constrainst 
   }
}
################################################################################
#' passive cold chain dose capacity
################################################################################

calc_dose_capacity <- function(vial_type, vax_vol, equip_type, with_ice = T) #vial_type = monodose/dose10 and vax_vol depends on monodose_vialVol/dose10_vialVol #equip_type = c('rcw25','vaxCarr')
   { 
   if(vial_type == 'dose10' & vax_vol == 2.1 & equip_type == 'rcw25' & with_ice == T)
   {5000}
   else if (vial_type == 'dose10' & vax_vol == 3 & equip_type == 'rcw25' & with_ice == T)
   {3300}
   else if (vial_type == 'dose10' & vax_vol == 2.1 & equip_type == 'vaxCarr' & with_ice == T) #I've not yet thrown in the numbers for 10 dose OCC because it's not operational and even being considered but might be good for a counterfactual.
   {750}
   else if (vial_type == 'dose10' & vax_vol == 3 & equip_type == 'vaxCarr' & with_ice == T)
   {500}
   else if(vial_type == 'monodose' & vax_vol == 21.09 & equip_type == 'rcw25' & with_ice == T)
   {616}
   else if (vial_type == 'monodose' & vax_vol == 21.09 & equip_type == 'rcw25' & with_ice == F)
   {1301}
   else if (vial_type == 'monodose' & vax_vol == 21.09 & equip_type == 'vaxCarr' & with_ice == T)
   {77}
   else if (vial_type == 'monodose' & vax_vol == 21.09 & equip_type == 'vaxCarr' & with_ice == F)
   {170}
   else{
      stop('Wrong input entered')
      }
}




calc_transport_equipment_needs <- function(equip_type, vial_type, vax_vol, with_ice = T, doses_to_transport)
   { 
   #NB: numbers here are doses and are based on the packed volume per dose, i.e, how much volume is required to pack one dose. 
      if(vial_type == 'dose10' & vax_vol == 2.1 & equip_type == 'rcw25' & with_ice == T)
      {ceiling(doses_to_transport / 5000)}
      else if (vial_type == 'dose10' & vax_vol == 3 & equip_type == 'rcw25' & with_ice == T)
      {ceiling(doses_to_transport / 3300)}
      else if (vial_type == 'dose10' & vax_vol == 2.1 & equip_type == 'vaxCarr' & with_ice == T) #I've not yet thrown in the numbers for 10 dose OCC because it's not operational and even being considered but might be good for a counterfactual.
      {ceiling(doses_to_transport / 750)}
      else if (vial_type == 'dose10' & vax_vol == 3 & equip_type == 'vaxCarr' & with_ice == T)
      {ceiling(doses_to_transport / 500)}
      else if(vial_type == 'monodose' & vax_vol == 21.09 & equip_type == 'rcw25' & with_ice == T)
      {ceiling(doses_to_transport / 616)}
      else if (vial_type == 'monodose' & vax_vol == 21.09 & equip_type == 'rcw25' & with_ice == F)
      {ceiling(doses_to_transport / 1301)}
      else if (vial_type == 'monodose' & vax_vol == 21.09 & equip_type == 'vaxCarr' & with_ice == T)
      {ceiling(doses_to_transport / 77)}
      else if (vial_type == 'monodose' & vax_vol == 21.09 & equip_type == 'vaxCarr' & with_ice == F)
      {ceiling(doses_to_transport / 170)}
      else{
         stop('Check inputs')
      } 
}

################################################################################
#' Full Cold Chain (FCC) calculations
################################################################################

#calc_doses_required(): calculates number of doses needed based on the target population size; indicate if you need doses for the near or far people; the type of vial you need calcalations for; and the row numbers of the sites you want to analyse
calc_doses_required <- function(df, site_rows_selected, is_dose10 = T, pop_type){ #df is the dataframe of added sites; site_row = row numbers to analyse; is.dose10, if false, means monodose; #pop_type = c(near, far)
   selected_sites <- df %>% 
            dplyr::slice(site_rows_selected)
   
   if(pop_type == 'near' & is_dose10 == T){
      doses_required <- selected_sites %>% 
            dplyr::summarise(sum(.$near_pop))
      return(ceiling(as.numeric(doses_required) / 10))
      }else if(pop_type == 'near' & is_dose10 == F){
         doses_required <- selected_sites %>% 
            dplyr::summarise(sum(.$near_pop))
         return(as.numeric(doses_required))
      } else if(pop_type == 'far' & is_dose10 == T){
         doses_required <- selected_sites %>% 
            dplyr::summarise(sum(.$far_pop))
         return(as.numeric(ceiling(doses_required / 10)))
      }else if(pop_type == 'far' & is_dose10 == F){
         doses_required <- selected_sites %>% 
            dplyr::summarise(sum(.$far_pop))
         return(as.numeric(doses_required))
      } else{
         stop('Error in dose calculation function; check your inputs')
      }
   }

# ###########################################################
# #buffer_doses(): account for factors that'll require us to transport
# #more doses than needed
# ###########################################################
# 
# apply_buffer <- function(buffer_size, doses){
#    req_doses <- doses * (1 + buffer_size / 100)
#    return(req_doses)
# }


##########
#calc_freezing_time()
##########

calc_freezing_time <- function(mf314_available, large_icepacks_quantity, small_icepacks_quantity){
   ceiling(
      (1/mf314_available)*((large_icepacks_quantity / mf314_largepack_fr) + (small_icepacks_quantity / mf314_smallpack_fr)) 
   )
   
}

################
#plotting functions
###################

#every_nth() is a useful function I got from SO for adding blank labels inbetween
#vectors for labelling graphs, if you want to have unlabelled ticks
every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) 
{
   if (!inverse) {
      if(empty) {
         x[1:nth == 1] <- ""
         x
      } else {
         x[1:nth != 1]
      }
   } else {
      if(empty) {
         x[1:nth != 1] <- ""
         x
      } else {
         x[1:nth == 1]
      }
   }
}