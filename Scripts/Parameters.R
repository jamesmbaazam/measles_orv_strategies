#packages
library(ggplot2) # to prevent errors from me specifying theme settings here

########################################################
#Supply chain parameters
########################################################

#Data table row selection control
num_of_sites_selectable <- 'single' #'single' means only one row can be selected at a time. To prevent row selection, set to 'none'. Selected rows are available through input$id_of_table_rows_selected


# FIXED PARAMETERS

strategy_list <- c("Monodose FCC", "10-dose FCC", "Mixed FCC", "Part OCC") #OCC = out of cold chain
team_type_list <- c('Fixed post', 'Mobile team')

monodose_vial_vol <- 21.09
dose10_vial_vol <- c(2.1, 3)


# active cold chain
mf314_largepack_capacity <- 323 # large pack refers to 0.6L icepacks
mf314_smallpack_capacity <- 450 # small pack refers to 0.4L icepacks

mf314_largepack_fr <- 54 #fr = freezing rate per day for large icepacks
mf314_smallpack_fr <- 81 #fr = freezing rate per day for large icepacks


# Passive cold chain gross volume
rcw25_grossVol <- 43735.296 / 1E3 # convert to litres
vax_carrier_grossVol <- 9504 / 1E3

# Passive cold chain net volume (after ice packs)
rcw25_netVol <- 20700 / 1E3
vax_carrier_netVol <- 2600 / 1E3

# passive cold chain ice pack capacity
rcw25_ice_capacity <- rcw25_grossVol - rcw25_netVol
vax_carrier_ice_capacity <- vax_carrier_grossVol - vax_carrier_netVol

# Vaccine vial packed volume per dose (pvd) i.e vaccine + diluent
# Vaccines are assumed to have a volume of 2.5cm^3 per dose
monodose_pvd <- 33.618 / 1E3
dose_10_pvd <- 2.42324 / 1E3 # multi-dose refers to 10 doses



#expected vaccination rate per team type
tp_fixed <- 450 #tp= team performance, in other words, how many people a fixed team is expected to vax per day (unit = people per day)
tp_mobile <- 250



#wastage factors
#Wastage here refers to open vial wastage. Other forms of wastage haven't been accounted for yet.
#we assume the monodose has no wastage.
dose10_wr_ft <- 15 #value between 0 and 100; #wastage rate for fixed teams, ft, 15% wastage of every 100 doses; known/commonly used by MSF for all campaigns across all strategies
dose10_wr_mt <- dose10_wr_ft*2 #value between 0 and 100; #wastage rate for mobile teams, mt, 30% (twice, compared to fixed teams); I'm assuming this 





# DATA
# icepack_quants <- read_excel('/data/passiveCC_icepack_quantity_data.xlsx')
# vax_storage_capacity <- read_excel('/data/passiveCC_vaccine_storage_capacity_data.xlsx')

######################################################
#Plot parameters
######################################################
#Control parameters
display_epi_plots <- TRUE
display_sc_plots <- FALSE
save_plots <- FALSE


# formatting the labels
presentation_plot_theme <- theme(
    axis.text.x = element_text(size = 15)
    , axis.text = element_text(size = 15)
    , axis.title.x = element_text(size = 20)
    , legend.text = element_text(size = 15)
    , axis.title.y = element_text(size = 20)
    , legend.title = element_text(size = 20)
)


#plot theme
shiny_plot_theme <-  theme(title = element_text(size = 12,
                                          face = 'bold'
)
)
