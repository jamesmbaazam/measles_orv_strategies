#all possible combinations
# strategy_scenarios <- expand.grid(fixed_team_with_dose10 = c(T, F)
#                         , fixed_team_with_ice = c(T, F)
#                         , mobile_team_with_dose10 = c(T, F)
#                         , mobile_team_with_ice = c(T, F)
#                         , team_dispatch = c('parallel', 'asap')
#                         )

#Full list of strategies
strategy_analysis_list <- list(
    # 10-dose fcc
    dose10_fcc_asap = data.frame(strategy_name = 'dose10_fcc_asap',
                                 fixed_team_with_dose10 = T
                                 , fixed_team_with_ice = T
                                 , mobile_team_with_dose10 = T
                                 , mobile_team_with_ice = T
                                 , team_dispatch = 'asap'
    ),
    dose10_fcc_parallel = data.frame(strategy_name = 'dose10_fcc_parallel'
                                     , fixed_team_with_dose10 = T
                                     , fixed_team_with_ice = T
                                     , mobile_team_with_dose10 = T
                                     , mobile_team_with_ice = T
                                     , team_dispatch = 'parallel'
    ),
    # monodose fcc
    monodose_fcc_asap = data.frame(strategy_name = 'monodose_fcc_asap'
                                   , fixed_team_with_dose10 = F
                                   , fixed_team_with_ice = T
                                   , mobile_team_with_dose10 = F
                                   , mobile_team_with_ice = T
                                   , team_dispatch = 'asap'
    ),
    monodose_fcc_parallel = data.frame(strategy_name = 'monodose_fcc_parallel'
                                       , fixed_team_with_dose10 = F
                                       , fixed_team_with_ice = T
                                       , mobile_team_with_dose10 = F
                                       , mobile_team_with_ice = T
                                       , team_dispatch = 'parallel'
    ),
    # monodose occ
    monodose_occ_asap = data.frame(strategy_name = 'monodose_occ_asap'
                                   , fixed_team_with_dose10 = F
                                   , fixed_team_with_ice = F
                                   , mobile_team_with_dose10 = F
                                   , mobile_team_with_ice = F
                                   , team_dispatch = 'asap'
    ),
    
    monodose_occ_parallel = data.frame(strategy_name = 'monodose_occ_parallel'
                                       , fixed_team_with_dose10 = F
                                       , fixed_team_with_ice = F
                                       , mobile_team_with_dose10 = F
                                       , mobile_team_with_ice = F
                                       , team_dispatch = 'parallel'
    ),
    #mixed fcc
    mixed_fcc_asap = data.frame(strategy_name = 'part_occ_asap'
                                , fixed_team_with_dose10 = T
                                , fixed_team_with_ice = T
                                , mobile_team_with_dose10 = F
                                , mobile_team_with_ice = T
                                , team_dispatch = 'asap'
    ),
    
    mixed_fcc_parallel = data.frame(strategy_name = 'part_occ_parallel'
                                    , fixed_team_with_dose10 = T
                                    , fixed_team_with_ice = T
                                    , mobile_team_with_dose10 = F
                                    , mobile_team_with_ice = T
                                    , team_dispatch = 'parallel'
    ),
    
    #part occ
    part_occ_asap = data.frame(strategy_name = 'part_occ_asap'
                               , fixed_team_with_dose10 = T
                               , fixed_team_with_ice = T
                               , mobile_team_with_dose10 = F
                               , mobile_team_with_ice = F
                               , team_dispatch = 'asap'
    ),
    
    part_occ_parallel = data.frame(strategy_name = 'part_occ_parallel'
                                   , fixed_team_with_dose10 = T
                                   , fixed_team_with_ice = T
                                   , mobile_team_with_dose10 = F
                                   , mobile_team_with_ice = F
                                   , team_dispatch = 'parallel'
    )
)
