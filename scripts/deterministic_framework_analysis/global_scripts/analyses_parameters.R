#####################################
#packages
######################################
library(dplyr)


#######################################
# pre-requisite scripts
#######################################

source('./scripts/deterministic_framework_analysis/global_scripts/supply_chain_functions.R')



#####################################
#inputs
######################################
sc_model_params <- list(ambient_temp = c("below 40", "above 40")
                , mf314_quant = 10
                , buffer_stock = 15 #value between 0 and 100
                , monodose_vial_vol = 21.09
                , dose10_vial_vol = c(2.1, 3)
                , n_teams_fixed = 10
                , n_teams_mobile = 10
                , dose10_ovw_fixed_team = 15 #ovw = Open Vial Wastage: value between 0 and 100;
                , dose10_ovw_mobile_team = 15 #ovw = Open Vial Wastage: value between 0 and 100; I'm assuming far campaigns have twice the near campaign wastage
                , monodose_ovw_fixed_team = 3 #ovw = Open Vial Wastage: value between 0 and 100;
                , monodose_ovw_mobile_team = 3 #ovw = Open Vial Wastage: value between 0 and 100; I'm assuming far campaigns have twice the near campaign wastage
                , vax_rate = c(fixed_team = 450, mobile_team = 250)
                , rcw25_ice_replacement_days = c(2, 3)
                , site_campaign_dur_constraint = 10
                , predeployment_delay = 21
                )


orv_model_params <- list(index_cases = 10 
                         , near_pop_R0 = 12.8 #Ref = {Guerra, 2017; The basic reproduction number (R0) of measles: a systematic review (Lancet)]
                         , far_pop_R0 = 12.8
                         , vaccine_efficacy = 0.84 #source = WHO measles vaccine position paper
                         , init_prop_immune = 0.75 #Source(s): 10.1186/s12889-019-7500-z; 10.1093/aje/kwy114/5033615 (estimate is 83.2 (CI: 74.7 - 87.7))
                         , model_time = 365 #run model for 1 year
                         , LP = 10 #LP = Latent period #Ref = {Heymann, D. (ed.) 2004 Control of communicable diseases manual, p. 417, 18th edn}
                         , IP = 8 #IP = Infectious period #Ref = {Heymann, D. (ed.) 2004 Control of communicable diseases manual, p. 417, 18th edn}
                         )




