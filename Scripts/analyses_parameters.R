#####################################
#packages
######################################
library(dplyr)





#####################################
#inputs
######################################
sc_model_params <- list(ambient_temp = c("below 40", "above 40")
                , mf314_quant = 1
                , buffer_stock = 0 #value between 0 and 100
                , monodose_vial_vol = 21.09
                , dose10_vial_vol = c(2.1, 3)
                , dose10_wr_ft = 15 #value between 0 and 100;
                , dose10_wr_mt = 30 #value between 0 and 100; I'm assuming far campaigns have twice the near campaign wastage
                , vax_rate = c(fixed_team = 450, mobile_team = 250)
                )


orv_model_params <- list(R0 = 12
                         , vaccine_efficacy = 0.95
                         , model_time = 365 #run model for a year
                         , LP = 7 #LP = Latent period
                         , IP = 7 #IP = Infectious period
                         , immune_response_timing = 7
                         )


site_data <- tibble(near_pop = 7500 
                           , far_pop = 500
                           , site_team_alloc = 1 #number of teams allocated to site
)
