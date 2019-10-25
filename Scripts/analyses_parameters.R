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
                , dose10_ovw_fixed_team = 15 #ovw = Open Vial Wastage: value between 0 and 100;
                , dose10_ovw_mobile_team = 30 #ovw = Open Vial Wastage: value between 0 and 100; I'm assuming far campaigns have twice the near campaign wastage
                , monodose_ovw_fixed_team = 0 #ovw = Open Vial Wastage: value between 0 and 100;
                , monodose_ovw_mobile_team = 0 #ovw = Open Vial Wastage: value between 0 and 100; I'm assuming far campaigns have twice the near campaign wastage
                , vax_rate = c(fixed_team = 450, mobile_team = 250)
                )


orv_model_params <- list(near_pop_R0 = 12
                         , far_pop_R0 = 12
                         , vaccine_efficacy = 0.95
                         , model_time = 365 #run model for a year
                         , LP = 7 #LP = Latent period
                         , IP = 7 #IP = Infectious period
                         , immune_response_timing = 7
                         )


# site_data <- tibble(near_pop = 1000
#                     , far_pop = 169)
#                            

###########################################################################
#Plot control parameters
###########################################################################
#Supply chain plot control parameters
display_sc_plots <- TRUE
save_sc_plots <- F
#epidemiological
display_epi_plots <- TRUE
save_epi_plots <- F