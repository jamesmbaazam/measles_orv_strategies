#packages
library(conflicted)
library(dplyr)
library(forcats)


#load pre-requisite scripts
source('./scripts/deterministic_framework_analysis/scenarios.R')
source('./scripts/analyses_parameters.R')
source('./scripts/strategy_list_complete.R')

#resolve conflicts
conflict_prefer('filter', 'dplyr')


## we're only interested in a subset of scenarios and strategies for now
## subset the scenarios 
scenario_subset <- scenarios %>% 
    filter((mt_equip_type == 'rcw25'| mt_equip_type == 'vaxCarr') & 
               dispatch == 'parallel'
    )

#subset the strategies 
strategy_subset <- c("dose10_fcc_parallel", 
                     "dose10_occ_parallel", 
                     "monodose_fcc_parallel", 
                     "monodose_occ_parallel",
                     "mixed_pcc_parallel"
)


strategy_subset_config <- dplyr::filter(strategies, strategy %in% strategy_subset) %>% 
    mutate(strategy = forcats::fct_drop(strategy))


#Location characteristics
near_pop_sizes <- rep(c(25, 50, 75), each = 50)*1000

far_pop_sizes <- rev(rep(c(25, 50, 75), each = 50))*1000

site_pops_df <- tibble(near_pop = near_pop_sizes, 
                           far_pop = far_pop_sizes,
                           location_id = as_factor(rep(1:5, times = 30))
                           )
                           


#key table
key_table <- expand.grid(location_id = unique(site_pops_df$location_id),
                         mt_equip_type = scenario_subset$mt_equip_type,
                         strategy = strategy_subset_config$strategy
                         ) %>% 
    as_tibble()


key_table


## simulation data

sim_params_tmp <- left_join(key_table, 
                            strategy_subset_config, 
                            by = 'strategy'
                            ) 
    

sim_params_full <- do.call('rbind', replicate(3, sim_params_tmp, simplify = F))

sim_params_pop_size_sensitivity <- cbind(sim_params_full, 
                          select(site_pops_df, -location_id)
                          ) %>% 
    as_tibble()

sim_params_pop_size_sensitivity


