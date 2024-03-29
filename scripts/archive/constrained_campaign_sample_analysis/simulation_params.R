#packages
library(conflicted)
library(dplyr)
library(forcats)


#load pre-requisite scripts
source('./scripts/epidemics7_analysis/scenarios.R')
source('./scripts/analyses_parameters.R')
source('./scripts/strategy_list_complete.R')

#resolve conflicts
conflict_prefer('filter', 'dplyr')


## we're only interested in a subset of scenarios and strategies for now
## subset the scenarios 
scenario_subset <- scenarios %>% 
    filter((equip_type == 'rcw25'| equip_type == 'vaxCarr') & 
               dispatch == 'parallel'
           )

#subset the strategies 
strategy_subset <- c("dose10_fcc_parallel", 
                     "dose10_occ_parallel", 
                     "monodose_fcc_parallel", 
                     "monodose_occ_parallel",
                     "mixed_pcc_parallel"
                     )

strategy_subset_config <- filter(strategies, strategy %in% strategy_subset)


#Location characteristics
#far_pop_sizes <- c(100000, 100000)
#near_pop_sizes <- c(50000, 50000)
#site_pop_dist <- expand.grid(near_pop = near_pop_sizes, far_pop = far_pop_sizes)

teams <- expand.grid(n_ft = seq(10, 30, 10), n_mt = seq(10, 30, 10))

site_pop_dist <- data.frame(near_pop = rep(50000, 5), far_pop = rep(50000, 5))

site_pops_df <- make_site_data(site_pop_dist$near_pop, site_pop_dist$far_pop)


#key table
key_table <- expand.grid(location_id = site_pops_df$location_id,
                         equip_type = scenario_subset$equip_type,
                         strategy = strategy_subset_config$strategy
                         ) %>% as_tibble()


key_table


## simulation data

sim_params_tmp <- left_join(key_table, strategy_subset_config, by = 'strategy')

sim_params_table <- left_join(sim_params_tmp, site_pops_df, by = "location_id") %>% 
    mutate(location_id = as_factor(location_id)) %>% 
    as_tibble()

sim_params_table
