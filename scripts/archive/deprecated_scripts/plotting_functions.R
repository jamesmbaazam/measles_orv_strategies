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

#Color-blind palettes

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#plot theme
shiny_plot_theme <-  theme(title = element_text(size = 12,
                                                face = 'bold'
)
)


#function for creating gap axes (solution from SO)
library(scales)
squish_trans <- function(from, to, factor) {
    
    trans <- function(x) {
        
        # get indices for the relevant regions
        isq <- x > from & x < to
        ito <- x >= to
        
        # apply transformation
        x[isq] <- from + (x[isq] - from)/factor
        x[ito] <- from + (to - from)/factor + (x[ito] - to)
        
        return(x)
    }
    
    inv <- function(x) {
        
        # get indices for the relevant regions
        isq <- x > from & x < from + (to - from)/factor
        ito <- x >= from + (to - from)/factor
        
        # apply transformation
        x[isq] <- from + (x[isq] - from) * factor
        x[ito] <- to + (x[ito] - (from + (to - from)/factor))
        
        return(x)
    }
    
    # return the transformation
    return(trans_new("squished", trans, inv))
}

