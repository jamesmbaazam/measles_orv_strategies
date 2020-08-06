#####################################
#packages
######################################
library(dplyr)


#######################################
# pre-requisite scripts
#######################################

source('./scripts/supply_chain_functions.R')



#####################################
#inputs
######################################
sc_model_params <- list(ambient_temp = c("below 40", "above 40")
                , mf314_quant = 10
                , buffer_stock = 0 #value between 0 and 100
                , monodose_vial_vol = 21.09
                , dose10_vial_vol = c(2.1, 3)
                , dose10_ovw_fixed_team = 15 #ovw = Open Vial Wastage: value between 0 and 100;
                , dose10_ovw_mobile_team = 15 #ovw = Open Vial Wastage: value between 0 and 100; I'm assuming far campaigns have twice the near campaign wastage
                , monodose_ovw_fixed_team = 3 #ovw = Open Vial Wastage: value between 0 and 100;
                , monodose_ovw_mobile_team = 3 #ovw = Open Vial Wastage: value between 0 and 100; I'm assuming far campaigns have twice the near campaign wastage
                , vax_rate = c(fixed_team = 450, mobile_team = 250)
                , rcw25_ice_replacement_days = c(2, 3)
                , site_campaign_dur_constraint = 10
                )


orv_model_params <- list(near_pop_R0 = seq(2, 18, 0.5)
                         , far_pop_R0 = seq(2, 18, 0.5)
                         , vaccine_efficacy = 1
                         , model_time = 365*5 #run model for 5 years
                         , LP = 7 #LP = Latent period
                         , IP = 7 #IP = Infectious period
                         , immune_response_timing = 7
                         )




