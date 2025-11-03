#' Simulate response variable
#'
#' @param data Data simulated from other functions to pass to this function.
#' @param sim_args A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'   }
#' @param keep_intermediate TRUE/FALSE flag indicating whether intermediate steps
#'   should be kept. This would include fixed effects times regression weights,
#'   random effect summations, etc. Default is TRUE.
#' @param ... Other arguments to pass to error simulation functions.
#'
#' @export
generate_response <- function(data, sim_args, keep_intermediate = TRUE, ...) {
  if (is.list(sim_args[['formula']])) {
    gen_data <- lapply(seq_along(sim_args[['formula']]), function(xx) {
      generate_response_list(
        data = data,
        sim_args = sim_args,
        formula = sim_args[['formula']][[xx]],
        reg_weights = sim_args[['reg_weights']][[xx]],
        keep_intermediate = keep_intermediate,
        ...
      )
    })

    outcome_names <- unlist(lapply(
      seq_along(parse_formula(sim_args)),
      function(xx) {
        parse_formula(sim_args)[[xx]][['outcome']]
      }
    ))
    outcome_data <- data.frame(do.call(
      'cbind',
      lapply(seq_along(outcome_names)[2:length(outcome_names)], function(xx) {
        gen_data[[xx]][[outcome_names[xx]]]
      })
    ))
    names(outcome_data) <- outcome_names[2:length(outcome_names)]
    cbind.data.frame(gen_data[[1]], outcome_data)
  } else {
    generate_response_one(
      data = data,
      sim_args = sim_args,
      keep_intermediate = keep_intermediate,
      ...
    )
  }
}

generate_response_list <- function(
  data,
  sim_args,
  formula,
  reg_weights,
  keep_intermediate = TRUE,
  ...
) {
  outcome_name <- as.character(formula)[2]
  outcome_type <- sim_args[['outcome_type']]
  fixed_formula <- as.formula(paste0(
    "~",
    gsub(
      "^\\s+|\\s+$",
      "",
      gsub("\\+\\s*(\\s+|\\++)\\(.*?\\)", "", as.character(formula)[3])
    )
  ))

  fixed_vars <- attr(terms(fixed_formula), "term.labels")

  if (any(grepl('^factor\\(', fixed_vars))) {
    fixed_vars <- gsub("factor\\(|\\)$", "", fixed_vars)
  }
  if (any(grepl('^ns\\(', fixed_vars))) {
    fixed_vars <- gsub("ns\\(|\\,.+\\)$", "", fixed_vars)
  }
  if (any(grepl("^ns|^poly", attr(terms(fixed_formula), "term.labels")))) {
    fixed_vars <- poly_ns_names(sim_args, fixed_vars)
  }
  if (any(grepl("^poly\\(", fixed_vars))) {
    fixed_vars <- gsub("poly\\(|\\,.+\\)", "", fixed_vars)
  }

  if (
    any(
      unlist(lapply(seq_along(sim_args[['fixed']]), function(xx) {
        sim_args[['fixed']][[xx]]$var_type
      })) ==
        'factor'
    )
  ) {
    num_levels <- lapply(seq_along(sim_args[['fixed']]), function(xx) {
      sim_args[['fixed']][[xx]][['levels']]
    })
    num_levels <- purrr::modify_if(num_levels, is.character, length)

    if (
      any(unlist(lapply(seq_along(sim_args[['fixed']]), function(xx) {
        num_levels[[xx]] > 1 &
          sim_args[['fixed']][[xx]][['var_type']] == 'factor'
      })))
    ) {
      fixed_vars <- factor_names(sim_args, fixed_vars)
    }
  }

  if (any(grepl(':', fixed_vars))) {
    fixed_vars <- gsub(":", "\\.", fixed_vars)
  }

  # Xmat <- model.matrix(fixed_formula, data.frame(data), contrasts.arg = contrasts)
  Xmat <- dplyr::select(data, dplyr::all_of(fixed_vars))
  if (any(grepl('Intercept', names(data)))) {
    Xmat <- cbind(data['X.Intercept.'], Xmat)
  }

  fixed_outcome <- as.matrix(Xmat) %*% reg_weights

  if (length(parse_formula(sim_args)[['randomeffect']]) != 0) {
    random_formula <- parse_formula(sim_args)[['randomeffect']]
    random_formula_parsed <- parse_randomeffect(random_formula)
    random_effects_names <- names(sim_args[['randomeffect']])

    random_formula <- lapply(
      seq_along(random_formula_parsed[['random_effects']]),
      function(xx) {
        as.formula(random_formula_parsed[['random_effects']][xx])
      }
    )

    Zmat <- lapply(
      lapply(random_formula, model.matrix, data = data),
      data.frame
    )

    multiple_member <- parse_multiplemember(
      sim_args,
      parse_randomeffect(parse_formula(sim_args)[['randomeffect']])
    )
    if (any(multiple_member[['multiple_member_re']])) {
      Zmat <- do.call('cbind', Zmat)
    } else {
      Zmat <- dplyr::bind_cols(Zmat)
    }

    rand_effects <- subset(data, select = random_effects_names)

    random_effects <- rowSums(rand_effects * Zmat)
  } else {
    random_effects <- NULL
    random_effects <- 0
  }

  if (keep_intermediate) {
    if (is.list(sim_args[['reg_weights']])) {
      response_outcomes <- data.frame(
        fixed_outcome,
        random_effects = random_effects
      )
    } else {
      response_outcomes <- data.frame(
        fixed_outcome = fixed_outcome,
        random_effects = random_effects
      )
    }

    data <- cbind(data, response_outcomes, row.names = NULL)
  }

  if (is.null(data[['error']])) {
    data['error'] <- 0
  }

  outcome <- fixed_outcome + random_effects + data[['error']]

  if (
    !is.null(sim_args[['outcome_level']]) &&
      !is.null(sim_args[['outcome_type']]) &&
      sim_args[['outcome_type']] == 'binary' &&
      sim_args[['outcome_level']] > 1
  ) {
    # aggregated at cluster-level first
    outcome_unaggregated <- outcome

    outcome <- aggregate_outcome_by_level(
      outcome = outcome,
      data = data,
      sim_args = sim_args
    )

    if (exists("outcome_unaggregated")) {
      data[['propensity_unit_logit']] <- as.numeric(outcome_unaggregated)
    }

    return(simulate_cluster_binary(
      outcome = outcome,
      data = data,
      sim_args = sim_args,
      outcome_name = outcome_name
    ))
  }

  if (!is.null(sim_args[['outcome_type']])) {
    if (is.null(sim_args[['multinomial_categories']])) {
      multinomial_categories <- NULL
    } else {
      multinomial_categories <- sim_args[['multinomial_categories']]
    }
    trans_outcome <- transform_outcome(
      outcome,
      type = sim_args[['outcome_type']],
      categories = multinomial_categories
    )
    if (ncol(outcome) > 1) {
      names(outcome) <- paste0('untransformed_outcome', 1:ncol(outcome))
      data <- cbind(data, outcome)
    } else {
      data <- cbind(data, untransformed_outcome = outcome)
    }
    if (sim_args[['outcome_type']] == 'multinomial') {
      data <- cbind(data, trans_outcome)
      if (is.null(multinomial_categories)) {
        names(data)[names(data) == 'outcome_num'] <- outcome_name
      } else {
        names(data)[names(data) == 'outcome_category'] <- outcome_name
      }
    } else {
      data[outcome_name] <- trans_outcome
    }
  } else {
    data[outcome_name] <- outcome
  }

  data
}


generate_response_one <- function(
  data,
  sim_args,
  keep_intermediate = TRUE,
  ...
) {
  outcome_name <- parse_formula(sim_args)[['outcome']]
  outcome_type <- sim_args[['outcome_type']]
  fixed_formula <- parse_formula(sim_args)[['fixed']]

  fixed_vars <- attr(terms(fixed_formula), "term.labels")

  if (any(grepl('^factor\\(', fixed_vars))) {
    fixed_vars <- gsub("factor\\(|\\)$", "", fixed_vars)
  }
  if (any(grepl('^ns\\(', fixed_vars))) {
    fixed_vars <- gsub("ns\\(|\\,.+\\)$", "", fixed_vars)
  }
  if (any(grepl("^ns|^poly", attr(terms(fixed_formula), "term.labels")))) {
    fixed_vars <- poly_ns_names(sim_args, fixed_vars)
  }
  if (any(grepl("^poly\\(", fixed_vars))) {
    fixed_vars <- gsub("poly\\(|\\,.+\\)", "", fixed_vars)
  }

  if (
    any(
      unlist(lapply(seq_along(sim_args[['fixed']]), function(xx) {
        sim_args[['fixed']][[xx]]$var_type
      })) ==
        'factor'
    )
  ) {
    num_levels <- lapply(seq_along(sim_args[['fixed']]), function(xx) {
      sim_args[['fixed']][[xx]][['levels']]
    })
    num_levels <- purrr::modify_if(num_levels, is.character, length)

    if (
      any(unlist(lapply(seq_along(sim_args[['fixed']]), function(xx) {
        num_levels[[xx]] > 1 &
          sim_args[['fixed']][[xx]][['var_type']] == 'factor'
      })))
    ) {
      fixed_vars <- factor_names(sim_args, fixed_vars)
    }
  }

  if (any(grepl(':', fixed_vars))) {
    fixed_vars <- gsub(":", "\\.", fixed_vars)
  }

  # Xmat <- model.matrix(fixed_formula, data.frame(data), contrasts.arg = contrasts)
  Xmat <- dplyr::select(data, dplyr::all_of(fixed_vars))
  if (any(grepl('Intercept', names(data)))) {
    Xmat <- cbind(data['X.Intercept.'], Xmat)
  }

  if (is.list(sim_args[['reg_weights']])) {
    fixed_outcome <- data.frame(do.call(
      "cbind",
      lapply(seq_along(sim_args[['reg_weights']]), function(xx) {
        as.matrix(Xmat) %*% sim_args[['reg_weights']][[xx]]
      })
    ))
    names(fixed_outcome) <- paste0('logit', 1:ncol(fixed_outcome))
  } else {
    fixed_outcome <- as.matrix(Xmat) %*% sim_args[['reg_weights']]
  }

  if (length(parse_formula(sim_args)[['randomeffect']]) != 0) {
    random_formula <- parse_formula(sim_args)[['randomeffect']]
    random_formula_parsed <- parse_randomeffect(random_formula)
    random_effects_names <- names(sim_args[['randomeffect']])

    random_formula <- lapply(
      seq_along(random_formula_parsed[['random_effects']]),
      function(xx) {
        as.formula(random_formula_parsed[['random_effects']][xx])
      }
    )

    Zmat <- lapply(
      lapply(random_formula, model.matrix, data = data),
      data.frame
    )

    multiple_member <- parse_multiplemember(
      sim_args,
      parse_randomeffect(parse_formula(sim_args)[['randomeffect']])
    )
    if (any(multiple_member[['multiple_member_re']])) {
      Zmat <- do.call('cbind', Zmat)
    } else {
      Zmat <- dplyr::bind_cols(Zmat)
    }

    rand_effects <- subset(data, select = random_effects_names)

    random_effects <- rowSums(rand_effects * Zmat)
  } else {
    random_effects <- NULL
    random_effects <- 0
  }

  if (keep_intermediate) {
    if (is.list(sim_args[['reg_weights']])) {
      response_outcomes <- data.frame(
        fixed_outcome,
        random_effects = random_effects
      )
    } else {
      response_outcomes <- data.frame(
        fixed_outcome = fixed_outcome,
        random_effects = random_effects
      )
    }

    data <- cbind(data, response_outcomes, row.names = NULL)
  }

  if (is.null(data[['error']])) {
    data['error'] <- 0
  }

  outcome <- fixed_outcome + random_effects + data[['error']]

  if (!is.null(sim_args[['outcome_type']])) {
    if (is.null(sim_args[['multinomial_categories']])) {
      multinomial_categories <- NULL
    } else {
      multinomial_categories <- sim_args[['multinomial_categories']]
    }

    # Do we need cluster-level assignment?
    is_cluster_outcome <- !is.null(sim_args[['outcome_level']]) &&
      sim_args[['outcome_level']] > 1

    if (is_cluster_outcome) {
      # Save individual-level latent outcome BEFORE aggregation
      outcome_unaggregated <- outcome

      # aggregate_outcome_by_level returns FINAL treatment / continuous / multinomial values
      trans_outcome <- aggregate_outcome_by_level(
        outcome = outcome,
        data = data,
        sim_args = sim_args,
        multinomal_categories = multinomial_categories
      )
    } else {
      # normal individual-level assignment
      trans_outcome <- transform_outcome(
        outcome,
        type = sim_args[['outcome_type']],
        categories = multinomial_categories
      )
    }

    if (!is.null(ncol(outcome)) && ncol(outcome) > 1) {
      names(outcome) <- paste0('untransformed_outcome', 1:ncol(outcome))
      data <- cbind(data, outcome)
    } else {
      data <- cbind(data, untransformed_outcome = outcome)
    }

    # If cluster-level, store the pre-aggregated LP for diagnostics
    if (is_cluster_outcome) {
      data <- cbind(
        data,
        untransformed_outcome_unaggregated = outcome_unaggregated
      )
    }
    # Store unaggregated outcome if it exists
    if (
      !is.null(sim_args[['outcome_level']]) && sim_args[['outcome_level']] > 1
    ) {
      if (exists('outcome_unaggregated')) {
        data <- cbind(
          data,
          untransformed_outcome_unaggregated = outcome_unaggregated
        )
      }
    }

    if (is.data.frame(trans_outcome)) {
      data <- cbind(data, trans_outcome)

      if (sim_args[['outcome_type']] == 'multinomial') {
        if (is.null(multinomial_categories)) {
          names(data)[names(data) == 'outcome_num'] <- outcome_name
        } else {
          names(data)[names(data) == 'outcome_category'] <- outcome_name
        }
      }
    } else {
      data[outcome_name] <- trans_outcome
    }
  } else {
    data[outcome_name] <- outcome
  }

  data
}
#' Aggregate outcome to specified cluster level
#'
#' Takes individual-level linear predictors and aggregates them to the
#' specified cluster level. This is used for cluster-level treatment
#' assignment in propensity score models.
#'
#' @param outcome Numeric vector or matrix of linear predictors (untransformed outcomes)
#' @param data Data frame containing cluster ID variables
#' @param sim_args Simulation arguments list containing outcome_level specification
#' @param multinomal_categories Categories for multinomial outcome transformation
#'
#' @return Aggregated outcome vector with cluster-level values repeated
#'   for all individuals within each cluster
#' @export
aggregate_outcome_by_level <- function(
  outcome,
  data,
  sim_args,
  multinomal_categories = NULL
) {
  outcome_level <- sim_args[['outcome_level']]
  outcome_name <- parse_formula(sim_args)[['outcome']]

  # If no outcome_level specified or level 1, return outcome as-is
  if (is.null(outcome_level) || outcome_level == 1) {
    return(outcome)
  }

  # Get cluster variable for the specified level
  random_formula <- parse_formula(sim_args)[['randomeffect']]

  if (length(random_formula) == 0) {
    stop("outcome_level specified but no random effects in formula")
  }

  cluster_vars <- parse_randomeffect(random_formula)[['cluster_id_vars']]

  if (outcome_level == 2) {
    # Use first (lowest level) cluster variable
    group_var <- cluster_vars[1]
  } else if (outcome_level == 3) {
    # Use second cluster variable for level 3
    if (length(cluster_vars) < 2) {
      stop("outcome_level = 3 requires at least 2 random effects in formula")
    }
    group_var <- cluster_vars[2]
  } else {
    stop("outcome_level must be 1, 2, or 3")
  }

  # Create temporary data frame for aggregation
  outcome_data <- cbind.data.frame(outcome = outcome, data[group_var])

  # Aggregate to cluster level (mean of linear predictors)
  agg_formula <- as.formula(paste0('outcome ~ ', group_var))
  outcome_agg <- aggregate(agg_formula, data = outcome_data, FUN = mean)

  outcome_agg[outcome_name] <- transform_outcome(
    outcome_agg[['outcome']],
    type = sim_args[['outcome_type']],
    categories = multinomial_categories
  )

  outcome_merged <- merge(
    data.frame(cluster_id = data[[group_var]], idx = seq_len(nrow(data))),
    outcome_agg,
    by.x = "cluster_id",
    by.y = group_var,
    all.x = TRUE,
    sort = FALSE
  )

  # Reorder back to original order using idx
  outcome_merged <- outcome_merged[order(outcome_merged$idx), ]

  # Drop helper columns
  outcome_merged$idx <- NULL
  outcome_merged$cluster_id <- NULL

  return(outcome_merged)
}
