# Histograms
histograms <- function(data, variable, binwidth) {
  ggplot(data, aes_string(x = variable)) + 
    theme_bw(base_size = 16) + 
    geom_histogram(binwidth = binwidth) 
}

power_point <- function(data) {
  ggplot(data, aes(x = var, y = power)) + 
    theme_bw(base_size = 16) + 
    geom_point(size = 4) + 
    #geom_line(aes_string(linetype = group, color = group)) + 
    ylab("Power")
}

power_point_group <- function(data, group_var = NULL) {
  ggplot(data, aes(x = var, y = power, group = group_var)) + 
    theme_bw(base_size = 16) + 
    geom_point(size = 4, aes_(color = group_var, shape = group_var)) + 
    geom_line(aes_(linetype = group_var, color = group_var)) + 
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
