#packages
library(conflicted)
library(dplyr)


#load pre-requisite scripts
source('./scripts/strategy_list_complete.R')
source('./scripts/epidemics7_analysis/scenarios.R')
source('./scripts/analyses_parameters.R')

#resolve conflicts
conflict_prefer('mutate', 'dplyr')


##we're only interested in a subset of scenarios and strategies for now
##subset the scenarios 
scenario_subset <- scenarios %>% filter((equip_type == 'rcw25'| equip_type == 'vaxCarr') & dispatch == 'asap')

#subset the strategies 
strategy_subset <- c("dose10_fcc_asap", "monodose_fcc_asap", "monodose_occ_asap")
strategy_subset_config <- filter(strategy_list, strategy %in% strategy_subset)

#key table
key_table <- expand.grid(location_id = site_pops_df$location_id,
                         strategy = strategy_subset_config$strategy, 
                         equip_type = scenario_subset$equip_type
                         )



## simulation data

sim_data <- merge(key_table, strategy_subset_config, by = 'strategy')
sim_data
