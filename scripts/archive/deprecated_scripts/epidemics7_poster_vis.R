library('ggplot2')


#scripts
source('scripts/plotting_functions.R')


#hypothetical outbreak sizes
outbreak_sizes <- data.frame(strategy = c('dose10_fcc', 'monodose_fcc', 'monodose_occ'),
                 outbreak_size = c(100, 160, 80)
                 )



ggplot(data = outbreak_sizes, aes(x = strategy, y = outbreak_size, fill = strategy)) + 
    geom_bar(stat = 'identity') + 
    scale_fill_manual(values = c('red', 'green4', 'black')) +
     scale_x_discrete(labels = c('10-dose (FCC)', '1-dose (FCC)', '1-dose (OCC)')) + 
    labs(x = 'Strategy', y = 'Outbreak size') + 
    theme(legend.position = 'none') + 
    presentation_plot_theme



set.seed(123)
compounded_benefits <- data.frame(site = paste0('site', 1:4),
                                  percent_cases_averted = round(runif(4, 1, 10))
                                  )



ggplot(data = compounded_benefits, aes(x = site, y = sort(percent_cases_averted, decreasing = T))) + 
    geom_bar(stat = 'identity') +
    scale_x_discrete(labels = 1:4) + 
    labs(x = 'Location', y = 'Cases averted (percentage)') +
    presentation_plot_theme
