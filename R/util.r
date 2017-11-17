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
  
  if(length(sample_size) == 1) {
    sample_size <- list(level1 = sample_size)
  } else {
    if(2 %in% min_max_loc) {
      level2 <- runif(n = sample_size[['level3']], 
                      min = sample_size[['level2']]$min,
                      max = sample_size[['level2']]$max) %>%
        round(0)
    } else {
      if(length(sample_size) == 3) {
        level2 <- rep(sample_size[['level2']], sample_size[['level3']])
      } else {
        level2 <- sample_size[['level2']]
      }
    }
    total_level2_samplesize <- sum(level2)
    sample_size['level2'] <- list(level2)
    
    if(1 %in% min_max_loc) {
      level1 <- runif(n = total_level2_samplesize,
                      min = sample_size[['level1']]$min,
                      max = sample_size[['level1']]$max) %>%
        round(0)
    } else {
      level1 <- rep(sample_size[['level1']], total_level2_samplesize)
    }
    sample_size['level1'] <- list(level1)
    
    if(length(sample_size) == 3) {
      sample_size['level3_total'] <- list(sample_size_level3(sample_size)) 
    }
  }
  sample_size
}

sample_size_level3 <- function(sample_size) {
  
  end <- cumsum(sample_size[['level2']])
  beg <- c(1, cumsum(sample_size[['level2']]) + 1)
  beg <- beg[-length(beg)]
  
  lvl3ss <- unlist(lapply(lapply(1:length(beg), function(xx) 
    sample_size[['level1']][beg[xx]:end[xx]]), sum))
  
  lvl3ss
}

create_ids <- function(sample_size_list, id_names) {
  
  if(length(id_names) == 3) {
    id_vars <- data.frame(unlist(lapply(1:length(sample_size_list[['level1']]), 
                                        function(xx) 1:sample_size_list[['level1']][xx])),
                          rep(1:sum(sample_size_list[['level2']]), times = sample_size_list[['level1']]),
                          rep(1:sample_size_list[['level3']], times = sample_size_list[['level3_total']])
    )
  } else {
    if(length(id_names) == 2) {
      id_vars <- data.frame(unlist(lapply(1:length(sample_size_list[['level1']]), 
                                          function(xx) 1:sample_size_list[['level1']])),
                            rep(1:sum(sample_size_list[['level2']]), times = sample_size_list[['level1']])
      )
    } else {
      id_vars <- data.frame(1:sample_size_list[['level1']])
    }
  }
  
  names(id_vars) <- id_names
  id_vars
}


create_function_calls <- function(sim_args) {
  
  sim_args_names <- names(sim_args)
  
  simulate_functions <- paste('simulate', 
      sim_args_names[sim_args_names %in% c('fixed', 'randomeffect', 'error')],
      sep = '_')
  
  
  
}


# Horrible hack to keep CRAN happy and suppress NOTES about
# parts of the code that use non-standard evaluation.
# See:
# http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
# https://github.com/smbache/magrittr/issues/29
utils::globalVariables(c('test_stat', 'reject', 'estimate', 'term', 'std.error'))
