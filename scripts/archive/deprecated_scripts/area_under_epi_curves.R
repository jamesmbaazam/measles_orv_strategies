library(zoo)

dose10_inf4 <- filter(epi_dyn_detailed, strategy == 'dose10_fcc_parallel')[, 'Inf4']
dose10_inf4_ordered_id <- order(dose10_inf4)

dose10_fcc_auc <- sum(diff(dose10_inf4[dose10_inf4_ordered_id])*rollmean(unique(epi_dyn_detailed$time), 2))


monodose_fcc_inf4 <- filter(epi_dyn_detailed, strategy == 'monodose_fcc_parallel')[, 'Inf4']
monodose_fcc_inf4_ordered_id <- order(monodose_fcc_inf4)

monodose_fcc_auc <- sum(diff(monodose_fcc_inf4[monodose_fcc_inf4_ordered_id])*rollmean(unique(epi_dyn_detailed$time), 2))


mixed_fcc_inf4 <- filter(epi_dyn_detailed, strategy == 'mixed_fcc_parallel')[, 'Inf4']
mixed_fcc_inf4_ordered_id <- order(mixed_fcc_inf4)

mixed_fcc_auc <- sum(diff(mixed_fcc_inf4[mixed_fcc_inf4_ordered_id])*rollmean(unique(epi_dyn_detailed$time), 2))


part_occ_inf4 <- filter(epi_dyn_detailed, strategy == 'part_occ_asap')[, 'Inf4']
part_occ_inf4_ordered_id <- order(part_occ_inf4)

part_occ_auc <- sum(diff(part_occ_inf4[part_occ_inf4_ordered_id])*rollmean(unique(epi_dyn_detailed$time), 2))


strategy_outbreak_auc <- data.frame(strategy = strategy_names_subset, 
           area_Inf4 = c(dose10_fcc_auc, monodose_fcc_auc, mixed_fcc_auc, part_occ_auc))

strategy_outbreak_auc
