library(dplyr)
library(readr)
library(readxl)
library(gridExtra)
library(ggplot2)
library(shinythemes)
library(DT)


# FIXED PARAMETERS

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


# DATA
# icepack_quants <- read_excel('/data/passiveCC_icepack_quantity_data.xlsx')
# vax_storage_capacity <- read_excel('/data/passiveCC_vaccine_storage_capacity_data.xlsx')
