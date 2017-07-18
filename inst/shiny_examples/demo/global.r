# Histograms
histograms <- function(data, variable, binwidth) {
  ggplot(data, aes_string(x = variable)) + 
    theme_bw(base_size = 16) + 
    geom_histogram(binwidth = binwidth) 
}

power_point <- function(data, x, y) {
  ggplot(data, aes_string(x = x, y = y)) + 
    theme_bw(base_size = 16) + 
    geom_point(size = 4) + 
    #geom_line(aes_string(linetype = group, color = group)) + 
    ylab("Power")
}

power_point_group <- function(data, x, y, group_var = NULL, 
                              facet_var = NULL) {
  p <- ggplot(data, aes_string(x = x, y = y, group = group_var)) + 
    theme_bw(base_size = 16) + 
    geom_point(size = 4, aes_string(color = group_var, shape = group_var)) + 
    geom_line(aes_string(linetype = group_var, color = group_var)) + 
    ylab("Power") 
  if(is.null(facet_var)) {
    p
  } else {
    p + facet_wrap(facet_var)
  }
}

# param_table <- htmltools::withTags(
#   table(
#     class = 'display',
#     thead(
#       tr(
#         
#       )
#     )
#   )
# )
