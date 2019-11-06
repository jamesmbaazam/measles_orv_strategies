library(tidyverse)
set.seed(20191023)

#scripts
source('scripts/plotting_functions.R')


teamDay <- function(mkpd = 250 # mean kids encountered per team per day
                    , kk = 1 # shape parameter
                    , vialLimit = 170 # max number of vials that can be carried
                    , doses = 10 # doses per vial
                    , hours = 8 # daily vaccination window
                    , vialLife = 0.5 # time vial can be used post-reconstitution
                    , kid_buffer = ifelse(kk < 1, 2.5, 1.5)
){
  
  encounterRate <- mkpd / hours * kk
  
  tmp <- rgamma(mkpd*kid_buffer, shape = kk, rate = encounterRate)
  tmp <- diffinv(tmp)
  if(max(tmp) < hours) stop(paste0('Kid buffer insufficient (kk = ',kk,').'))
  vaxTimes <- tmp[tmp > 0 & tmp <= hours]
  
  vials <- 0
  vaxed <- 0
  reconstTime <- vaxTimes[1]
  
  while(reconstTime <= hours & vaxed < length(vaxTimes) & vials < vialLimit){
    vials <- vials + 1
    vialTimes <- vaxTimes[vaxTimes >= reconstTime & vaxTimes < reconstTime + vialLife]
    if(length(vialTimes) > doses){
      vialTimes <- vialTimes[1:doses]
    }
    vaxed <- vaxed + length(vialTimes)
    reconstTime <- vaxTimes[vaxed + 1]
  }
  
  if(vials < vialLimit & vaxed != length(vaxTimes)) warning('Inconsistency in vaccination numbers.')
  
  used <- sum(doses * vials)
  return(c(kids_vaxed = vaxed
           , vials_used = vials
           , doses_used = used
           , doses_wasted = used - vaxed
  ))
}

x <- t(replicate(100, teamDay(kk = 0.25, mkpd = 300, doses = 1), simplify = T))
apply(x, 2, mean)


pargrid <- expand.grid(meankpd = seq(50, 250, 50), kk = c(0.25, 0.5, 1, 2, 4), doses = c(10, 1))

REPS <- 500
out <- NULL
for(ii in 1:nrow(pargrid)){
  tmp <- unname(t(replicate(REPS,teamDay(mkpd = pargrid$meankpd[ii], kk = pargrid$kk[ii], doses = pargrid$doses[ii]), simplify = T)))
  tmp <- data.frame(mkpd = as.character(pargrid$meankpd[ii]), kk = as.character(pargrid$kk[ii]), doses = as.character(pargrid$doses[ii]), kids_vaxed = tmp[,1], vials_used = tmp[,2], doses_used = tmp[,3], doses_wasted = tmp[,4])
  out <- rbind(out, tmp)
}

wastage_plot <- ggplot(out, aes(x = doses, y = doses_wasted, color = doses)) +
  geom_boxplot() +
  labs(x = 'Vial type', y = 'Wasted doses') +
  facet_grid(kk ~ mkpd) +
  presentation_plot_theme

plot(wastage_plot)

vaccinations_plot <- ggplot(out, aes(x = doses, y = kids_vaxed, color = doses)) +
  geom_boxplot() +
  labs(x = 'Vial type', y = 'Children vaccinated') +
  facet_grid(kk ~ mkpd) +
  presentation_plot_theme

plot(vaccinations_plot)

dose_usage_plot <- ggplot(out, aes(x = doses, y = doses_used, color = doses)) +
  geom_boxplot() +
  labs(x = 'Vial presentation', y = 'Doses used') +
  facet_grid(kk ~ mkpd) +
  presentation_plot_theme

plot(dose_usage_plot)

vial_usage <- ggplot(out, aes(x = doses, y = vials_used, color = doses)) +
  geom_boxplot() +
  labs(x = 'Vial type', y = 'Vials used') +
  facet_grid(kk ~ mkpd) +
  presentation_plot_theme

plot(vial_usage)


#' Interpretation: (1) We find that there is virtually no wasted associated with
#' the monodose but the 10-dose vials tend to experience more wastage as the
#' population size decreases and population clustering decreases. This seems to
#' suggest the monodose will be useful in targetting small pockets of populations
#' in sparce locations. However, even in such settings, the 10-dose still achieves
#' comparable vaccinations as the 10-dose. Other questions about cost and speed of
#' response may tend to favour (or not) the monodose. 
