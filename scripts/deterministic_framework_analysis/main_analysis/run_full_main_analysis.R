#'This script sources the scripts and runs the individual components of the modelling framework,
#'combines the results, performs aggregations, saves the output to file, and makes plots

#'Step 1. Run the supply chain(sc) analyses and save the results to file
source('./scripts/deterministic_framework_analysis/main_analysis/constrained_campaign_sc_main_analysis.R')

#'Step 2. Run the epidemiological(epi) model using some of the supply chain outputs and save the results to file
source('./scripts/deterministic_framework_analysis/main_analysis/constrained_campaign_orv_main_analysis.R')

#'Step 3. Combine the supply chain and epi results and calculate the cases averted and save to file
source('./scripts/deterministic_framework_analysis/main_analysis/cases_averted_calculation_main_analysis.R')

#'Step 4. Plot the cases averted from the main analysis (pre-deployment delay of 21 days)
source('./scripts/deterministic_framework_analysis/main_analysis/fig_cases_averted_main_analysis.R')

#'Step 5. Plot the vaccination coverage vrs campaign duration from the main analysis (pre-deployment delay of 21 days)
source('./scripts/deterministic_framework_analysis/main_analysis/fig_coverage_vs_duration_scenario_ranking_main_analysis.R')



