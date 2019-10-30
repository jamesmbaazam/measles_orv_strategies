#packages
library(dplyr)
library(gridExtra)
library(ggthemes)
library(ggplot2)

#scripts
source('scripts/Parameters.R')
source('scripts/Supply_chain_functions.R')


team_vax_carrier_needs <- tibble(
    small_packs_frozen = 0.5*mf314_smallpack_fr*(1:floor(mf314_smallpack_capacity/mf314_smallpack_fr)),
    vax_carriers_below40 = floor(small_packs_frozen/compute_vaxCarr_icepacks('below 40')),
    teams_for_below40 = vax_carriers_below40,
    vax_carriers_above40 = floor(small_packs_frozen/compute_vaxCarr_icepacks('above 40')),
    teams_for_above40 = vax_carriers_above40,
    days_to_freeze = seq_along(small_packs_frozen)
    )


team_vaxCarr_needs_plot <- ggplot(data = team_vax_carrier_needs, aes(x = paste0(days_to_freeze, ' (', small_packs_frozen, ')'), y = teams_for_below40)) + 
    geom_bar(stat = 'identity', aes(fill = 'forestgreen')) +
    geom_bar(data = team_vax_carrier_needs, aes(x = days_to_freeze, y = teams_for_above40, fill = 'tomato3'), stat = 'identity') +
    scale_y_continuous(breaks = seq(0, max(team_vax_carrier_needs$teams_for_below40) + 10, 5),
                       labels = seq(0, max(team_vax_carrier_needs$teams_for_below40) + 10, 5)
                       ) +
    scale_fill_manual(name = 'Ambient Temperature', values = c('forestgreen', 'tomato3'), labels = c('< 40', '> 40')) +
    labs(title = 'Quantity of ready vax carriers as a function of freezing rate (MF314 freezer)', 
         x = 'Time (Days) and 0.4L ice packs frozen', 
         y = 'Vax carriers'
         ) +
    theme(legend.position = 'top')

plot(team_vaxCarr_needs_plot)


team_rcw25_needs <- tibble(
    large_packs_frozen = 0.5*mf314_largepack_fr*(1:floor(mf314_largepack_capacity/mf314_largepack_fr)), #the 0.5 means that the other half space will be occupied by the other ice pack type
    rcw25_below40 = floor(large_packs_frozen/compute_rcw25_icepacks('below 40')),
    teams_for_below40 = rcw25_below40,
    rcw25_above40 = floor(large_packs_frozen/compute_rcw25_icepacks('above 40')),
    teams_for_above40 = rcw25_above40,
    days_to_freeze = seq_along(large_packs_frozen)
)

team_rcw25_needs_plot <- ggplot(data = team_rcw25_needs, aes(x = paste0(days_to_freeze, ' (', large_packs_frozen, ')'), y = teams_for_below40)) + 
    geom_bar(stat = 'identity', aes(fill = 'forestgreen')) +
    geom_bar(data = team_rcw25_needs, aes(x = days_to_freeze, y = teams_for_above40, fill = 'tomato3'), stat = 'identity') +
    scale_y_continuous(breaks = seq(0, max(team_rcw25_needs$teams_for_below40) + 15, 2),
                       labels = seq(0, max(team_rcw25_needs$teams_for_below40) + 15, 2)
    ) +
    scale_fill_manual(name = 'Ambient Temperature', values = c('forestgreen', 'tomato3'), labels = c('< 40', '> 40')) +
    labs(title = 'Quantity of ready RCW25 cold boxes as a function of freezing rate (MF314 freezer)', 
         x = 'Time (Days) and 0.6L ice packs frozen', 
         y = 'RCW25 cold boxes'
         ) + 
    theme(legend.position = 'none')

plot(team_rcw25_needs_plot)


all_equipment <- grid.arrange(team_vaxCarr_needs_plot, team_rcw25_needs_plot, nrow = 2)

ggsave(filename = 'figures/all_equipment.png', plot = all_equipment)
