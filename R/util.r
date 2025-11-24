"%ni%" <- Negate("%in%")

is_odd <- function(x) x %% 2 != 0

whole_number <- function(x) x %% 1 == 0

comp_list <- function(x) length(unique.default(x)) == 1L

prop_limits <- function(prop) {
  if (prop > 0.5) {
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
  loc <- lapply(seq_along(x), function(xx) {
    grep("\\.f$|\\.c$|_f$|_c$|\\.k$|_k$", x[[xx]], ignore.case = TRUE)
  })
  len <- lapply(x, length)
  for (i in seq_along(x)) {
    x[[i]][loc[[i]]] <- paste0('factor(', x[[i]][loc[[i]]], ')')
    if (len[[i]] > 1) {
      x[[i]] <- paste(x[[i]], collapse = ':')
    }
  }
  x <- as.formula(paste('~', paste(x, collapse = '+')))
  x
}

sample_sizes <- function(sample_size) {
  if (length(sample_size) == 1) {
    sample_size <- list(level1 = sample_size)
  } else {
    if (length(sample_size) == 3) {
      if (length(sample_size[['level2']]) == sample_size[['level3']]) {
        level2 <- sample_size[['level2']]
      } else {
        level2 <- rep(sample_size[['level2']], sample_size[['level3']])
      }
      sample_size['level2'] <- list(level2)
    }
    total_level2_samplesize <- sum(sample_size[['level2']])

    if (length(sample_size[['level1']]) == total_level2_samplesize) {
      level1 <- sample_size[['level1']]
    } else {
      level1 <- rep(sample_size[['level1']], total_level2_samplesize)
    }
    sample_size['level1'] <- list(level1)
    if (length(sample_size) == 3) {
      sample_size['level3_total'] <- list(sample_size_level3(sample_size))
    }
  }
  sample_size
}

sample_size_level3 <- function(sample_size) {
  end <- cumsum(sample_size[['level2']])
  beg <- c(1, cumsum(sample_size[['level2']]) + 1)
  beg <- beg[-length(beg)]

  lvl3ss <- unlist(lapply(
    lapply(1:length(beg), function(xx) {
      sample_size[['level1']][beg[xx]:end[xx]]
    }),
    sum
  ))

  lvl3ss
}

create_ids <- function(sample_size_list, id_names) {
  if (length(id_names) == 3) {
    id_vars <- data.frame(
      unlist(lapply(seq_along(sample_size_list[['level1']]), function(xx) {
        1:sample_size_list[['level1']][xx]
      })),
      rep(
        1:sum(sample_size_list[['level2']]),
        times = sample_size_list[['level1']]
      ),
      rep(
        1:sample_size_list[['level3']],
        times = sample_size_list[['level3_total']]
      )
    )
  } else {
    if (length(id_names) == 2) {
      id_vars <- data.frame(
        unlist(lapply(seq_along(sample_size_list[['level1']]), function(xx) {
          1:sample_size_list[['level1']][xx]
        })),
        rep(
          1:sum(sample_size_list[['level2']]),
          times = sample_size_list[['level1']]
        )
      )
    } else {
      id_vars <- data.frame(1:sample_size_list[['level1']])
    }
  }

  names(id_vars) <- id_names
  id_vars
}

compute_samplesize <- function(data, sim_args) {
  id_vars <- parse_randomeffect(parse_formula(sim_args)[['randomeffect']])[[
    'cluster_id_vars'
  ]]
  multiple_member <- parse_multiplemember(
    sim_args,
    parse_randomeffect(parse_formula(sim_args)[['randomeffect']])
  )

  id_vars <- id_vars[id_vars %ni% multiple_member[['multiple_member_idvars']]]

  samp_size <- lapply(seq_along(id_vars), function(xx) {
    as.numeric(table(data[id_vars[xx]]))
  })

  if (length(id_vars) == 2) {
    level2_ss <- aggregate(
      as.formula(paste0(id_vars[1], "~", id_vars[2])),
      data = data,
      FUN = length_unique
    )[[2]]

    list(
      level1 = samp_size[[1]],
      level2 = level2_ss,
      level3 = sim_args[['sample_size']][['level3']],
      level3_total = samp_size[[2]]
    )
  } else {
    if (length(id_vars) == 1) {
      list(
        level1 = samp_size[[1]],
        level2 = sim_args[['sample_size']][['level2']]
      )
    } else {
      list(level1 = sim_args[['sample_size']])
    }
  }
}

length_unique <- function(x) length(unique(x))

is_factor_var <- function(sim_args) {
  unlist(lapply(seq_along(sim_args[['fixed']]), function(yy) {
    isTRUE(sim_args[['fixed']][[yy]][['var_type']] == 'factor')
  }))
}

factor_names <- function(sim_args, fixed_vars) {
  which_factor <- is_factor_var(sim_args)
  factor_names <- names(sim_args[['fixed']][which_factor])

  fixed_vars_continuous <- fixed_vars[
    !grepl(paste(factor_names, collapse = "|"), fixed_vars)
  ]
  fixed_vars_cat <- fixed_vars[grepl(
    paste0('^', paste(factor_names, collapse = "|"), "$"),
    fixed_vars
  )]

  if (any(grepl(":|^I", fixed_vars_cat))) {
    int_loc <- grep(":|^I", fixed_vars_cat)
    fixed_vars_cat <- fixed_vars_cat[-int_loc]
  }

  num_levels <- lapply(fixed_vars_cat, function(xx) {
    sim_args[['fixed']][[xx]][['levels']]
  })
  num_levels <- purrr::modify_if(num_levels, is.character, length)

  loc <- num_levels > 1
  fixed_levels_gt2 <- fixed_vars_cat[loc]
  num_levels_gt2 <- num_levels[loc]

  fixed_levels_gt2_names <- lapply(seq_along(fixed_levels_gt2), function(xx) {
    paste0(fixed_levels_gt2[xx], '_', 1:(num_levels_gt2[[xx]] - 1))
  })
  names(fixed_levels_gt2_names) <- fixed_vars_cat

  if (any(grepl(":|^I", fixed_vars))) {
    int_loc <- grep(":|^I", fixed_vars)
    var_loc <- lapply(seq_along(fixed_levels_gt2), function(xx) {
      grep(paste0('^', fixed_levels_gt2[xx], "$"), fixed_vars[-int_loc])
    })
  } else {
    var_loc <- lapply(seq_along(fixed_levels_gt2), function(xx) {
      grep(paste0('^', fixed_levels_gt2[xx], "$"), fixed_vars)
    })
  }

  updated_names <- lapply(seq_along(fixed_levels_gt2), function(ii) {
    lapply(seq_along(fixed_levels_gt2_names[[ii]]), function(xx) {
      gsub(
        fixed_levels_gt2[ii],
        fixed_levels_gt2_names[[ii]][xx],
        fixed_vars[var_loc[[ii]]]
      )
    })
  })

  reordered_names <- lapply(seq_along(fixed_levels_gt2), function(ii) {
    reorder_names(updated_names[[ii]])
  })

  if (any(grepl(":|^I", fixed_vars))) {
    fixed_vars_cat_rename <- fixed_vars[grepl(
      paste(factor_names, collapse = "|"),
      fixed_vars
    )]

    if (any(grepl("_post$", fixed_vars_cat_rename[int_loc]))) {
      fixed_vars_cat_rename <- fixed_vars_cat_rename[
        !grepl("_post$", fixed_vars_cat_rename)
      ]
    }
    int_loc <- grep(":|^I", fixed_vars_cat_rename)

    new_interaction_names <- interaction_names(
      fixed_vars_cat_rename,
      fixed_levels_gt2_names,
      sim_args
    )
    for (ii in seq_along(int_loc)) {
      # int_loc <- grep(":|^I", fixed_vars)
      int_loc_rename <- grep(
        paste0('^', fixed_vars_cat_rename[int_loc[ii]], '$', collapse = ""),
        fixed_vars
      )
      fixed_vars[int_loc_rename] <- new_interaction_names[ii]
    }
  }

  # gsub(fixed_vars_cat[1], reordered_names[[1]], fixed_vars)

  # fixed_vars <- c(fixed_vars_continuous, fixed_vars_cat_rename)

  for (ii in seq_along(var_loc)) {
    fixed_vars[[var_loc[[ii]]]] <- reordered_names[ii]
  }

  unlist(fixed_vars)
}

interaction_names <- function(fixed_vars, renamed_vars, sim_args) {
  int_loc <- grep(":|^I", fixed_vars)

  int_names <- lapply(int_loc, function(ii) {
    unlist(strsplit(fixed_vars[ii], split = ":"))
  })

  factor_vars <- unlist(lapply(seq_along(sim_args[['fixed']]), function(xx) {
    sim_args[['fixed']][[xx]][['var_type']] == 'factor'
  }))
  factor_names <- names(sim_args[['fixed']])[factor_vars]

  # int_vars_location_f <- lapply(seq_along(int_names), function(ii)
  #   grep(paste(int_names[[ii]][factor_vars], collapse = "|"), fixed_vars[-int_loc[ii]]))
  #
  renamed_int_vars <- lapply(seq_along(int_names), function(ii) {
    if (length(int_names[[ii]][int_names[[ii]] %in% factor_names]) == 1) {
      cont_vars <- int_names[[ii]][!int_names[[ii]] %in% factor_names]
      if (length(cont_vars) > 1) {
        cont_vars <- paste0(cont_vars, collapse = ":")
      }
      do.call(
        paste,
        c(
          expand.grid(
            cont_vars,
            renamed_vars[[int_names[[ii]][int_names[[ii]] %in% factor_names]]],
            stringsAsFactors = FALSE
          ),
          sep = ":"
        )
      )
    } else {
      if (all(int_names[[ii]] %in% factor_names)) {
        do.call(
          paste,
          c(
            expand.grid(
              renamed_vars[int_names[[ii]][int_names[[ii]] %in% factor_names]],
              stringsAsFactors = FALSE
            ),
            sep = ":"
          )
        )
      } else {
        cont_vars <- int_names[[ii]][!int_names[[ii]] %in% factor_names]
        factor_expand <- expand.grid(
          renamed_vars[int_names[[ii]][int_names[[ii]] %in% factor_names]],
          stringsAsFactors = FALSE
        )
        do.call(paste, c(cbind(cont_vars, factor_expand), sep = ":"))
      }
    }
  })

  renamed_int_vars
}

reorder_names <- function(names) {
  unlisted_names <- do.call('c', names)
  if (any(grepl(":|^I", unlisted_names))) {
    int_loc <- grep(":|^I", unlisted_names)
    c(unlisted_names[-int_loc], unlisted_names[int_loc])
  } else {
    unlisted_names
  }
}


poly_ns_names <- function(sim_args, fixed_vars) {
  if (is.null(parse_formula(sim_args)[['fixed']])) {
    list_formula <- parse_formula(sim_args)
    fixed_list <- lapply(seq_along(list_formula), function(xx) {
      as.character(list_formula[[xx]][['fixed']])
    })
    if (comp_list(fixed_list)) {
      fixed_formula <- list_formula[[1]][['fixed']]
    } else {
      NULL
    }
  } else {
    fixed_formula <- parse_formula(sim_args)[['fixed']]
  }

  # fixed_vars <- attr(terms(fixed_formula), "term.labels")

  ns_loc <- grep("^ns|^bs", fixed_vars)
  if (any(ns_loc)) {
    ns_new_names <- ns_df_names(fixed_vars[ns_loc])
    if (grepl("df", fixed_vars[ns_loc])) {
      ns_new_names <- lapply(seq_along(ns_loc), function(xx) {
        ns_df_names(fixed_vars[ns_loc[xx]])
      })
      for (i in seq_along(ns_new_names)) {
        fixed_vars[ns_loc[i]] <- ns_new_names[i]
      }
      fixed_vars <- unlist(fixed_vars)
    } else {
      # placeholder for bs names
    }
  }
  poly_loc <- grep("^poly", fixed_vars)
  if (any(poly_loc)) {
    poly_new_names <- lapply(seq_along(poly_loc), function(xx) {
      poly_names(fixed_vars[poly_loc[xx]])
    })
    for (i in seq_along(poly_new_names)) {
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
    lapply(seq_along(names(y)), function(xx) {
      any(y[[xx]] != x[[grep(names(y)[[xx]], names(x))[1]]])
    })
  )
}

dataframe2matrix <- function(data, corr_variable, var_names) {
  if (is.null(data)) {
    NULL
  } else {
    n <- (sqrt(1 + 8 * nrow(data)) + 1) / 2

    corr_mat <- matrix(NA, nrow = n, ncol = n)
    lower <- lower.tri(corr_mat, diag = FALSE)
    upper <- upper.tri(corr_mat, diag = FALSE)

    corr_mat[lower] <- data[[corr_variable]]
    corr_mat[upper] <- data[[corr_variable]]
    diag(corr_mat) <- 1

    mat_names <- unique(unlist(data[var_names]))

    colnames(corr_mat) <- mat_names
    rownames(corr_mat) <- mat_names

    corr_mat
  }
}

density_quantile <- function(data, quantiles) {
  data_density <- data.frame(density(data, na.rm = TRUE, n = 10000)[c(
    'x',
    'y'
  )])

  matches <- lapply(quantiles, closest_match, data = data_density$x)

  data_density[unlist(matches), ]
}

closest_match <- function(data, value) {
  which.min(abs(data - value))
}

list_select <- function(list, names, exclude = TRUE, simplify = FALSE) {
  if (exclude) {
    index <- which(!(names(list) %in% names))
  } else {
    index <- which(names(list) %in% names)
  }
  if (simplify) {
    list[[index]]
  } else {
    list[index]
  }
}

# Horrible hack to keep CRAN happy and suppress NOTES about
# parts of the code that use non-standard evaluation.
# See:
# http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
# https://github.com/smbache/magrittr/issues/29
utils::globalVariables(c(
  'test_stat',
  'reject',
  'estimate',
  'term',
  'std.error',
  '.',
  'sd_estimate',
  'avg_se',
  't1e',
  'power_args',
  'adjusted_teststat',
  't1e_args',
  'param_estimate_sd',
  'avg_standard_error',
  'statistic',
  'n',
  'miss_prob',
  'miss_prop',
  'test_statistic',
  'group',
  'ID',
  'p.value'
))
