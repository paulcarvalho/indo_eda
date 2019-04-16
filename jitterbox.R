### AUTHOR: Elle Wibisono 
### Modified by: Paul Carvalho
### Date created: 03/15/2018
# Function to create half boxplot half scatter plot ala nature article.
# Inputs: data_in .......... dataframe
#         factor_col ....... bare column name (not a string)
#         numeric_col ...... y-axis continuous variable
#         offset............ numeric indicating the width of the boxplots and jitter cloud

#__________________________________________________________________________
# Load libraries
#__________________________________________________________________________
library(rlang)
library(dplyr)

#__________________________________________________________________________
# Function
#__________________________________________________________________________
gg_jitterbox <- function(data_in, factor_col, numeric_col, offset, names_x, label_x, label_y) {
  
  # turn bare args into quosures
  quo_factor <- enquo(factor_col)
  quo_numeric <- enquo(numeric_col)
  
  # do the base R stuff that doesn't play nice with quosures
  # the extra factor() call deals with the factor_col parameter potentially
  # being character type - otherwise finding levels() etc will fail
  # quo_text(quo_factor) just gives back the string of the column name that we
  # put as a bare parameter
  numeric_factor <- as.numeric(factor(data_in[[quo_text(quo_factor)]]))
  ftr_breaks <- seq(length(levels(factor(data_in[[quo_text(quo_factor)]]))))
  ftr_labels <- levels(factor(data_in[[quo_text(quo_factor)]]))
  
  # easiest to do this first bit with dplyr instead of pulling out
  # of the ggplot object, because we then only have to call ggplot() once
  data_in %>% 
    
    # another check to make sure we have factors when expected
    dplyr::mutate_if(is.character, as.factor) %>%
    
    # !! unquotes quosures, but only works well with tidyverse
    dplyr::group_by(!!quo_factor) %>%
    dplyr::mutate(d_ymin = min(!!quo_numeric),
                  d_ymax = max(!!quo_numeric),
                  d_lower = quantile(!!quo_numeric, 0.25),
                  d_middle = median(!!quo_numeric),
                  d_upper = quantile(!!quo_numeric, 0.75)) %>%
    
    ggplot() +
    
    # aes_() requires quoted formula types, but allows mixing
    # of enquo-ed bare names (e.g. quo_factor) and just
    # normal dplyr-style column names
    geom_boxplot(aes_(x = ~numeric_factor - offset,
                      ymin = ~d_lower,
                      ymax = ~d_upper,
                      lower = ~d_lower,
                      middle = ~d_middle,
                      upper = ~d_upper,
                      # width = 2 * offset,
                      fill = quo_factor),
                      stat = "identity") +
    
    geom_jitter(aes_(x = ~numeric_factor + offset,
                     y = quo_numeric),
                #color = quo_factor),
                # width = offset - 0.25 * offset,
                height = 0) +
    
    # bottom vertical segment
    geom_segment(aes(x = numeric_factor,
                     y = d_ymin,
                     xend = numeric_factor,
                     yend = d_lower)) +
    
    # top vertical segment
    geom_segment(aes(x = numeric_factor,
                     y = d_ymax,
                     xend = numeric_factor,
                     yend = d_upper)) +
    
    # top horizontal segment
    geom_segment(aes(x = numeric_factor - offset,
                     y = d_ymax,
                     xend = numeric_factor,
                     yend = d_ymax)) +
    
    # top vertical segment
    geom_segment(aes(x = numeric_factor - offset,
                     y = d_ymin,
                     xend = numeric_factor,
                     yend = d_ymin)) +
    
    # have to manually add in the x scale because we made everything numeric
    # to do the shifting
    scale_x_continuous(breaks = ftr_breaks,
                       labels = names_x) +
    
    # this also needs to be added manually because of the change to numeric
    # labs(x = quo_text(quo_factor)) #original code 
    labs(x=label_x, y=label_y) + 
    #bruteforcing the labels 
    
    # Code modified by PC
    theme_classic() +
    theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))+
    theme(legend.position="none")+
    theme(text = element_text(size=11)) +
    theme(axis.text.x = element_text(size=11), axis.text.y = element_text(size=11))
}

### End of the gg_jitterbox function.
