########################################################
# Function for producing a prallel coordinate plot from
# tabular data with a group or unique row identifier
# ------------------------
# Ian Douglas - March 2020
# 0.0.1
# 

parallelCoordPlot <- function(data, unique.id.col, group.col, selection = everything(), key = "key", value = "value", plot.it = TRUE, ...) {
  require(ggplot2)
  dat <- select(data, !!enquo(unique.id.col), !!enquo(group.col), all_of(selection))
  # Gather the columns whose values will be plotted into long format
  reshaped <- gather(data = dat, key = key, value = value, 
                     -!!enquo(unique.id.col), -!!enquo(group.col))
  
  # The `group` aesthetic is mapped to the unit whose values
  # we want to connect along a single line (unique.id)
  # A larger grouping variable may be used to color points/lines for
  # units who belong to the same group (group.col)
  plt <- ggplot(reshaped,
                aes(x = key, y = value, 
                    group = !!enquo(unique.id.col),
                    color = !!enquo(group.col))) +
    geom_point(alpha = .75, size = 1.5) +
    geom_line(alpha = .33) +
    theme_linedraw() +
    theme(plot.background = element_rect(fill="beige"),
          panel.background = element_rect(fill="black"),
          panel.grid = element_line(color = "white")) +
    theme(...)
  
  if (plot.it) {print(plt)} else return(plt)
}