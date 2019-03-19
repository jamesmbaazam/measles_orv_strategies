
# Extract size of allocated team from the sites table

extract_site_team_size <- function(df, site = 1) { # for now, we are only going to concentrate on one site. User indicates which site to analyse
  teams <- df %>%
    dplyr::slice(site) %>%
    .$site_team_alloc # number of teams allocated to site
  return(teams)
}



################################################################################
#print_site_team_dur() calculates how long the allocated teams will spend on the site
################################################################################

print_site_team_dur <- function(site_team_quant, td_fixed, td_mobile){ #td_fixed = team days for fixed site team, #td_mobile = team days for mobile team
 if (site_team_quant == 0) {
   renderText(
     print("<b> no teams were allocated </b>")
  )
 } else if (site_team_quant == 1) {
   renderText({
     paste(
       "in sequence,",
       "<b> Fixed post </b> team will spend",
       td_fixed,
       "days",
       "<br>",
       "<b> Mobile </b> team will spend",
       td_fixed,
       "<br>",
       "<b> total: </b>", td_fixed + td_mobile
     )
   })
 } else {
   renderText({
     paste(
       site_team_quant - 1,
       "<b> Fixed post </b> teams will each spend",
       round((td_fixed / (site_team_quant - 1)), digits = 1),
       "days.",
       "<br>",
       "1",
       "<b> Mobile </b> team will spend",
       td_mobile, "days."
     )
   })
 }
}



#######################################
#Calculate ice pack needs for vax carrier and RCW25s
#######################################
compute_rcw25_icepacks <- function(amb_temp){
   switch (amb_temp,
        "below 40" = 12,
        "above 40" = 18
   )
}


compute_vaxCarr_icepacks <- function(amb_temp){
   switch (amb_temp,
        "below 40" = 6,
        "above 40" = 8
   )
}



#################################################
#Extract population sizes
################################################
extract_near_pop <- function(df){
   df %>%
      dplyr::slice(1) %>% # for now, we are only going to concentrate on one site. User indicates which site to analyse
      .$near_pop
}


extract_far_pop <- function(df){
   df %>%
      dplyr::slice(1) %>% # for now, we are only going to concentrate on one site. User indicates which site to analyse
      .$far_pop
}