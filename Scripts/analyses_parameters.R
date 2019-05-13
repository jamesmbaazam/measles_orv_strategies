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
                )


site_data <- tibble(near_pop = 1500 
                           , far_pop = 500
                           , site_team_alloc = 1 #number of teams allocated to site
)
