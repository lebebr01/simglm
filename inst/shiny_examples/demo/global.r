# Histograms
histograms <- function(data, variable, binwidth) {
  ggplot(data, aes_string(x = variable)) + 
    theme_bw(base_size = 16) + 
    geom_histogram(binwidth = binwidth) 
}

power_point <- function(data, x_variable, group = NULL) {
  ggplot(data, aes_string(x = x_variable, y = power)) + 
    theme_bw(base_size = 16) + 
    geom_point(size = 3, aes_string(color = group, shape = group)) + 
    geom_line(aes_string(linetype = group, color = group)) + 
    ylab("Power")
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
