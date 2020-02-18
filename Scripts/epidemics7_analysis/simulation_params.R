#packages
library(conflicted)
library(dplyr)


#load pre-requisite scripts
source('./scripts/strategy_list_complete.R')
source('./scripts/epidemics7_analysis/scenarios.R')
source('./scripts/analyses_parameters.R')

#resolve conflicts
conflict_prefer('mutate', 'dplyr')
conflict_prefer('filter', 'dplyr')


##we're only interested in a subset of scenarios and strategies for now
##subset the scenarios 
scenario_subset <- scenarios %>% filter((equip_type == 'rcw25'| equip_type == 'vaxCarr') & dispatch == 'asap')

#subset the strategies 
strategy_subset <- c("dose10_fcc_asap", "dose10_occ_asap", "monodose_fcc_asap", "monodose_occ_asap")

strategy_subset_config <- filter(strategy_list, strategy %in% strategy_subset)

#key table
key_table <- expand.grid(location_id = site_pops_df$location_id,
                         equip_type = scenario_subset$equip_type,
                         strategy = strategy_subset_config$strategy
                         )



## simulation data

sim_params_tmp <- left_join(key_table, strategy_subset_config, by = 'strategy')

sim_params_table <- left_join(sim_params_tmp, site_pops_df, by = "location_id")

sim_params_table
