"%ni%" <- Negate("%in%")

prop_limits <- function(prop) {
  if(prop > .5) {
    u_diff <- 1 - prop
    u_max <- 1
    u_min <- prop - u_diff
  } else {
    u_diff <- 0 + prop
    u_max <- prop + u_diff
    u_min <- 0
  }
  return(c(u_min, u_max))
}

standardize <- function(x, mean, sd) {
  new <- (x - mean) / sd
  return(new)
}

search_factors <- function(x) {
  x <- strsplit(x, ':')
  loc <- lapply(seq_along(x), function(xx) 
    grep("\\.f$|\\.c$|_f$|_c$|\\.k$|_k$", x[[xx]], ignore.case = TRUE))
  len <- lapply(x, length)
  for(i in seq_along(x)) {
    x[[i]][loc[[i]]] <- paste0('factor(', x[[i]][loc[[i]]], ')')
    if(len[[i]] > 1) {
      x[[i]] <- paste(x[[i]], collapse = ':')
    }
  }
  x <- as.formula(paste('~', paste(x, collapse = '+')))
  x
}

sample_sizes <- function(sample_size) {
  
  min_max_loc <- grep('min', lapply(sample_size, names))
  if(length(min_max_loc) == 1) {
    gen_sample_size <- purrr::invoke_map('runif', 
                                         sample_size[min_max_loc],
                                         n = sample_size[[min_max_loc + 1]]
    ) %>%
      unlist() %>%
      round(0) %>%
      list()
    sample_size[min_max_loc] <- gen_sample_size
  } else {
    level2 <- runif(n = sample_size[[3]], 
                    min = sample_size[[2]]$min,
                    max = sample_size[[2]]$max) %>%
      round(0) %>%
      list()
    level1 <- runif(n = sum(unlist(level2)),
                    min = sample_size[[1]]$min,
                    max = sample_size[[1]]$max) %>%
      round(0) %>%
      list()
    
    sample_size[2] <- level2
    sample_size[1] <- level1
  }
  sample_size
}

# Horrible hack to keep CRAN happy and suppress NOTES about
# parts of the code that use non-standard evaluation.
# See:
# http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
# https://github.com/smbache/magrittr/issues/29
utils::globalVariables(c('test_stat', 'reject', 'estimate', 'term', 'std.error'))
