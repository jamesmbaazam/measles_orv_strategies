# plotting functions ----

###########################################################################
#Plot control parameters
###########################################################################
#Supply chain plot control parameters
display_sc_plots <- TRUE
save_sc_plots <- F
#epidemiological
display_epi_plots <- TRUE
save_epi_plots <- F

# every_nth() ====
# is a useful function I got from Stack Overflow for adding blank labels inbetween
# vectors for labelling graphs, if you want to have unlabelled ticks

every_nth <- function(x,
                      nth,
                      empty = TRUE,
                      inverse = FALSE) {
    if (!inverse) {
        if (empty) {
            x[1:nth == 1] <- ""
            x
        } else {
            x[1:nth != 1]
        }
    } else {
        if (empty) {
            x[1:nth != 1] <- ""
            x
        } else {
            x[1:nth == 1]
        }
    }
}


######################################################
#Plot parameters
######################################################

# formatting the labels
presentation_plot_theme <- theme(
    axis.text.x = element_text(size = 15)
    , axis.text = element_text(size = 15)
    , axis.title.x = element_text(size = 20)
    , legend.text = element_text(size = 15)
    , axis.title.y = element_text(size = 20)
    , legend.title = element_text(size = 20)
)


#plot theme
shiny_plot_theme <-  theme(title = element_text(size = 12,
                                                face = 'bold'
)
)
