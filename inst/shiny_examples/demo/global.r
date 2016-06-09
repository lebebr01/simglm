# Histograms
histograms <- function(data, variable) {
  ggplot(data, aes_string(x = variable)) + 
    theme_bw(base_size = 16) + 
    geom_histogram() 
}