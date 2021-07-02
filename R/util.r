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
      if(length(sample_size[['level1']]) == sample_size[['level2']]) {
        level1 <- sample_size[['level1']]
      } else {
        level1 <- rep(sample_size[['level1']], total_level2_samplesize)
      }
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
    id_vars <- data.frame(unlist(lapply(seq_along(sample_size_list[['level1']]), 
                                        function(xx) 1:sample_size_list[['level1']][xx])),
                          rep(1:sum(sample_size_list[['level2']]), times = sample_size_list[['level1']]),
                          rep(1:sample_size_list[['level3']], times = sample_size_list[['level3_total']])
    )
  } else {
    if(length(id_names) == 2) {
      id_vars <- data.frame(unlist(lapply(seq_along(sample_size_list[['level1']]), 
                                          function(xx) 1:sample_size_list[['level1']][xx])),
                            rep(1:sum(sample_size_list[['level2']]), times = sample_size_list[['level1']])
      )
    } else {
      id_vars <- data.frame(1:sample_size_list[['level1']])
    }
  }
  
  names(id_vars) <- id_names
  id_vars
}

compute_samplesize <- function(data, sim_args) {
  
  id_vars <- parse_randomeffect(parse_formula(sim_args)[['randomeffect']])[['cluster_id_vars']]
  
  samp_size <- lapply(seq_along(id_vars), function(xx) as.numeric(table(data[id_vars[xx]])))
  
  if(length(id_vars) == 2) {
    level2_ss <- aggregate(as.formula(paste0(id_vars[1], "~", id_vars[2])),
                           data = data, 
                           FUN = length_unique)[[2]]
    
    list(level1 = samp_size[[1]],
         level2 = level2_ss,
         level3 = sim_args[['sample_size']][['level3']],
         level3_total = samp_size[[2]])
  } else {
    if(length(id_vars) == 1) {
      list(level1 = samp_size[[1]],
           level2 = sim_args[['sample_size']][['level2']])
    } else {
      list(level1 = sim_args[['sample_size']])
    }
  }
}

length_unique <- function(x) length(unique(x))

is_factor_var <- function(sim_args) {
  unlist(lapply(seq_along(sim_args[['fixed']]), function(yy) 
    isTRUE(sim_args[['fixed']][[yy]][['var_type']] == 'factor')))
}

factor_names <- function(sim_args, fixed_vars) {
  
  which_factor <- is_factor_var(sim_args)
  
  fixed_vars_continuous <- fixed_vars[!which_factor]
  fixed_vars_cat <- fixed_vars[which_factor]
  
  if(any(grepl(":|^I", fixed_vars_cat))) {
    int_loc <- grep(":|^I", fixed_vars_cat)
    fixed_vars_cat <- fixed_vars_cat[-int_loc]
  } 
  
  num_levels <- lapply(fixed_vars_cat, function(xx) 
    sim_args[['fixed']][[xx]][['levels']])
  num_levels <- purrr::modify_if(num_levels, is.character, length)
    
  loc <- num_levels > 1
  fixed_levels_gt2 <- fixed_vars_cat[loc]
  num_levels_gt2 <- num_levels[loc]
  
  fixed_levels_gt2_names <- lapply(seq_along(fixed_levels_gt2), function(xx) 
    paste0(fixed_levels_gt2[xx], '_', 1:(num_levels_gt2[[xx]] - 1)))
  
  if(any(grepl(":|^I", fixed_vars))) {
    int_loc <- grep(":|^I", fixed_vars)
    var_loc <- lapply(seq_along(fixed_levels_gt2), function(xx) 
      grep(fixed_levels_gt2[xx], fixed_vars[-int_loc]))
  } else {
    var_loc <- lapply(seq_along(fixed_levels_gt2), function(xx) 
      grep(fixed_levels_gt2[xx], fixed_vars))
  }
  
  updated_names <- lapply(seq_along(fixed_levels_gt2), function(ii) 
    lapply(seq_along(fixed_levels_gt2_names[[ii]]), function(xx) 
      gsub(fixed_levels_gt2[ii], 
           fixed_levels_gt2_names[[ii]][xx], 
           fixed_vars[var_loc[[ii]]])
    )
  )
  
  reordered_names <- lapply(seq_along(fixed_levels_gt2), function(ii)
       reorder_names(updated_names[[ii]])
    )
  
  new_interaction_names <- interaction_names(fixed_vars, 
                                             fixed_levels_gt2_names)
  
  # need to generalize these so that they match better, 
  # this would particularly be in the [ii] for reordered_names and new_interaction_names
  for(ii in seq_along(var_loc)) {
    fixed_vars[var_loc[[ii]]] <- reordered_names[ii]
  }
  if(any(grepl(":|^I", fixed_vars))) {
    int_loc <- grep(":|^I", fixed_vars)
    for(ii in seq_along(int_loc)) {
      fixed_vars[[int_loc[ii]]] <- new_interaction_names[ii]
    }
  }
  
  unlist(fixed_vars)
}

interaction_names <- function(fixed_vars, renamed_vars) {
  
  int_loc <- grep(":|^I", fixed_vars)
  
  int_names <- lapply(int_loc, function(ii) 
    unlist(strsplit(fixed_vars[ii], split = ":"))
    )
  
  int_vars_location <- lapply(seq_along(int_names), function(ii) 
    grep(paste(int_names[[ii]], collapse = "|"), fixed_vars[-int_loc[ii]]))
  
  renamed_int_vars <- lapply(seq_along(int_vars_location), function(ii)
    do.call(paste, c(expand.grid(renamed_vars, stringsAsFactors = FALSE), sep = ":"))
  )
  
  renamed_int_vars
  
}

reorder_names <- function(names) {
  unlisted_names <- do.call('c', names)
  if(any(grepl(":|^I", unlisted_names))) { 
    int_loc <- grep(":|^I", unlisted_names)
    c(unlisted_names[-int_loc], unlisted_names[int_loc])
  } else {
    unlisted_names
  }
  
}


poly_ns_names <- function(sim_args) {
  fixed_formula <- parse_formula(sim_args)[['fixed']]
  
  fixed_vars <- attr(terms(fixed_formula), "term.labels") 
  
  ns_loc <- grep("^ns|^bs", fixed_vars)
  if(any(ns_loc)) {
    ns_new_names <- ns_df_names(fixed_vars[ns_loc])
    if(grepl("df", fixed_vars[ns_loc])) {
      ns_new_names <- lapply(seq_along(ns_loc), function(xx) {
        ns_df_names(fixed_vars[ns_loc[xx]])
      })
      for(i in seq_along(ns_new_names)) {
        fixed_vars[ns_loc[i]] <- ns_new_names[i]
      }
      fixed_vars <- unlist(fixed_vars)
    } else {
      # placeholder for bs names
    }
  }
  poly_loc <- grep("^poly", fixed_vars)
  if(any(poly_loc)) {
    poly_new_names <- lapply(seq_along(poly_loc), function(xx) {
      poly_names(fixed_vars[poly_loc[xx]])
    })
    for(i in seq_along(poly_new_names)) {
      fixed_vars[poly_loc[i]] <- poly_new_names[i]
    }
    fixed_vars <- unlist(fixed_vars)
  }
  
  fixed_vars
  
} 

ns_df_names <- function(x) {
  name <- gsub("ns\\(|bs\\(|\\,.+\\)$", "", x)

  func_arg <- unlist(regmatches(x, regexec("df\\s+=\\s+[0-9]+", x)))
  num <- unlist(regmatches(func_arg, regexec("[0-9]+", func_arg)))
  
  paste(name, 1:as.numeric(num), sep = "_")
  
}

poly_names <- function(x) {
  degree_arg <- unlist(regmatches(x, regexec("degree\\s+=\\s+[0-9]+", x)))
  num <- unlist(regmatches(degree_arg, regexec("[0-9]+", degree_arg)))
  name <- gsub("poly\\(|\\,.+\\)", "", x)
  
  paste(name, 1:as.numeric(num), sep = "_")
}


unique_columns <- function(x, y) {
  unlist(
    lapply(seq_along(names(y)), function(xx) 
      any(y[[xx]] != x[[grep(names(y)[[xx]], names(x))[1]]])
    )
  )
}

dataframe2matrix <- function(data, corr_variable, var_names) {
  
  if(is.null(data)) {
    NULL
  } else {
    n <- ((sqrt(1 + 8 * nrow(data)) + 1) / 2)
    
    corr_mat <- matrix(NA, nrow = n, ncol = n)
    lower <- lower.tri(corr_mat, diag = FALSE)
    upper <- upper.tri(corr_mat, diag = FALSE)
    
    corr_mat[lower] <- data[[corr_variable]]
    corr_mat[upper] <- data[[corr_variable]]
    diag(corr_mat) <- 1
    
    mat_names <- unique(unlist(data[var_names]))
    
    colnames(corr_mat) <-mat_names
    rownames(corr_mat) <- mat_names
    
    corr_mat
  }
}


# Horrible hack to keep CRAN happy and suppress NOTES about
# parts of the code that use non-standard evaluation.
# See:
# http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
# https://github.com/smbache/magrittr/issues/29
utils::globalVariables(c('test_stat', 'reject', 'estimate', 'term', 
                         'std.error', '.', 'sd_estimate', 'avg_se',
                         't1e', 'power_args', 'adjusted_teststat',
                         't1e_args', 'param_estimate_sd', 'avg_standard_error',
                         'statistic', 'n', 'miss_prob', 'miss_prop',
                         'test_statistic'))
