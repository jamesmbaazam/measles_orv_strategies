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
conflict_prefer('select', 'dplyr')


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

n_teams <- 20

near_pop_sizes <- rep(seq(10, 50, 10), each = 50)*1000

far_pop_sizes <- rev(rep(seq(10, 50, 10), each = 50))*1000

n_ft <- ceiling(near_pop_sizes/(near_pop_sizes + far_pop_sizes)*n_teams)

n_mt <- ceiling(far_pop_sizes/(near_pop_sizes + far_pop_sizes)*n_teams)

site_pops_df <- tibble(near_pop = near_pop_sizes, 
                       far_pop = far_pop_sizes, 
                       location_id = as_factor(rep(1:5, times = 50)),
                       n_teams_fixed = n_ft,
                       n_teams_mobile = n_mt
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
    

sim_params_full <- do.call('rbind', replicate(5, sim_params_tmp, simplify = F))

sim_params_pop_size_prop_teams_sensitivity <- cbind(sim_params_full, 
                                         select(site_pops_df, -location_id)
                                         ) %>% as_tibble() 




