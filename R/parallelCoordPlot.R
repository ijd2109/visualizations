########################################################
# Function for producing a prallel coordinate plot from
# tabular data with a group or unique row identifier
# ------------------------
# Ian Douglas - March 2020
# 0.0.1
# 

parallelCoordPlot <- function(data, 
                              unique.id.col, 
                              group.col, 
                              selection = everything(), 
                              key = "key", 
                              value = "value", 
                              plot.it = TRUE, 
                              plot_group_means = FALSE, 
                              ...)
{
  require(ggplot2)
  require(rlang)
  require(dplyr)
  require(tidyr)
  dat <- dplyr::select(data, !!enquo(unique.id.col), !!enquo(group.col), all_of(selection))
  # Gather the columns whose values will be plotted into long format
  reshaped <- tidyr::gather(data = dat, key = key, value = value, 
                            -!!enquo(unique.id.col), -!!enquo(group.col))
  # If plotting the group means as well:
  if (plot_group_means) {
    mean.dat <- dplyr::select(data, all_of(selection)) %>% select(-!!enquo(unique.id.col)) %>%
      group_by(!!enquo(group.col)) %>% summarize_all(~mean(.))
    reshaped.means <- tidyr::gather(mean.dat, key = key, value = value, -!!enquo(group.col))
  }
  # The `group` aesthetic is mapped to the unit whose values
  # we want to connect along a single line (unique.id)
  # A larger grouping variable may be used to color points/lines for
  # units who belong to the same group (group.col)
  plt <- ggplot(data = reshaped,
                aes(x = key, y = value,
                    group = !!enquo(unique.id.col),
                    color = !!enquo(group.col))) +
    geom_point(alpha = ifelse(plot_group_means,.4,.6), size = 3) +
    geom_line(alpha = .3, size = 2) +
    theme_linedraw() +
    theme(plot.background = element_rect(fill="beige"),
          panel.background = element_rect(fill="black"),
          panel.grid = element_line(color = "white"),
          axis.text.x = element_text(angle = 80, hjust=.9, vjust = .9)) +
    theme(...)
  if (plot_group_means) {
    plt <- plt + 
      geom_point(inherit.aes = FALSE,
                 data = reshaped.means,
                 aes(x = key, y = value,
                     group = !!rlang::enquo(group.col),
                     fill = !!rlang::enquo(group.col)), 
                 color = "red", alpha = 1, shape = 21, size = 3.5) +
      geom_line(inherit.aes = FALSE,
                data = reshaped.means,
                aes(x = key, y = value,
                    group = !!rlang::enquo(group.col),
                    color = !!rlang::enquo(group.col)), 
                alpha = 1, size = 2.4)
  }
  
  if (plot.it) {print(plt)} else return(plt)
}

