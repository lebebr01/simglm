#' Tidy error simulation
#' 
#' @param data Data simulated from other functions to pass to this function.
#' @param sim_args A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'   }
#' @param ... Other arguments to pass to error simulation functions.
#' 
#' @export 
simulate_error <- function(data, sim_args, ...) {
  
  if(is.null(data)) {
    n <- sample_sizes(sim_args[['sample_size']])
    ids <- create_ids(n, 
                      c('level1_id', parse_randomeffect(parse_formula(sim_args)[['randomeffect']])[['cluster_id_vars']]))
    error <- purrr::invoke(sim_error, 
                           sim_args[['error']],
                           n = n
    ) %>% 
      unlist()
  } else {
    n <- compute_samplesize(data, sim_args)
    error <- purrr::invoke(sim_error, 
                           sim_args[['error']],
                           n = n
    ) %>% 
      unlist()
  }
  
  if(is.null(data)) {
    data.frame(error = error, ids)
  } else {
    data.frame(data, error = error)
  }
}

heterogeneity <- function(variance, fixef, variable, err) {
  
  fixef_h <- data.frame(r_num = as.numeric(rownames(fixef)), err = err)
  
  if(length(unique(fixef[, variable])) == length(variance)) {
    fixef_h <- cbind(fixef_h, h_var = fixef[, variable])
  } else {
    fixef_h <- cbind(fixef_h, h_var = cut(fixef[, variable], 
                                          length(variance), labels = FALSE))
  }
  
  list_dat <- split(fixef_h, fixef_h$h_var)
  
  l_dat <- lapply(seq_along(variance), function(xx) 
    list_dat[[xx]]['err'] * sqrt(variance[xx]))
  
  dat <- cbind(do.call('rbind', list_dat), do.call('rbind', l_dat))
  colnames(dat) <- c('r_num', 'err_old', 'h_var', 'err')
  dat <- dat[order(dat$r_num),]
  
  dat[, 'err']
}

sim_error <- function(...) {
  
  sim_continuous2(...)
  
}
