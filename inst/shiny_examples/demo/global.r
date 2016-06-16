# Histograms
histograms <- function(data, variable, binwidth) {
  ggplot(data, aes_string(x = variable)) + 
    theme_bw(base_size = 16) + 
    geom_histogram(binwidth = binwidth) 
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
