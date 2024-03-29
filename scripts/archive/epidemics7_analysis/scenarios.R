#packages
library(conflicted)
library(dplyr)


#resolve conflicts
conflict_prefer('mutate', 'dplyr')


#Data frame of scenarios
scenarios <- expand.grid(equip_type = c('rcw25', 'vaxCarr'), 
                         dispatch = c('asap', 'parallel')
                         )



#auto-generate the scenario names

scenarios <- scenarios %>% 
    mutate(scenario_name = sprintf('%s_%s', equip_type, dispatch))
