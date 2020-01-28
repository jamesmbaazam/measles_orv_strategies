# Project overview
This project is about assessing various measles outbreak response vaccination strategies. The assessments will be done based on the logistical (cold chain) needs in terms of ice packs, freezing times for ice packs, and passive cold chain requirements for vaccination teams.

# Requirements
## Directories
1. app - contains the R scripts to run the shiny app
2. data - stores input data in csv format
3. figures - figures from analysis
4. model_output - stores the simulation output for further wrangling and visualizations 
5. scripts - all analysis scripts

## Key scripts
*NOTE: To run the analysis scripts, first run _measles\_orv\_strategies.rproj_ to open an R project session.*
1. _strategy\_list\_complete.R_ - the complete list of all possible vaccination strategies.
2. _analyses\_parameters.R_ - all shared parameters across scenarios go in this script
    a. add supply chain params to the sc_model_params list and epidemiological params to the orc_model_params list.
3. _measlesFunctions.R_ - the discrete-time daily time step SEIR transmission dynamics model and associated functions, i.e., vaccination, and infection.
4. _supply\_chain\_functions.R_ - functions for calculating individual units in the vaccine supply chain components, e.g., number of ice packs needed based on ambient temperature, number of doses per equipment type and vaccine type, etc.
5. _wrappers\_supply\_chain.R_ - wrapper functions that combine individual functions in supply_chain_functions.R to achieve a more complex output, e.g., time needed to freeze ice.
6. _full\_analysis.R_ - run this script to execute a full analysis of both the supply chain and epidemiological model to produce plots and save simulation output to file.
7. *Epidemics7 poster analysis*: Go into the scripts directory and run _full\_analysis\_epidemics7\_poster.R_
8. _plotting\_functions.R_: custom themes and functions for formatting plots for publications and presentations
8. _visualizations.R_: script for accessing and visualizing model output
9. 


## This repo _SHOULD_ contain:
1. Scripts for running the analysis
2. Simulation figures and outputs mostly as .csv, .rds, and the generic image formats

## This repo _SHOULDN'T_ contain:
1. Large files, for e.g., videos, audios, large data files (> 100 mb)
2. Microsoft office files, unless completely necessary because these filetypes will not be tracked
