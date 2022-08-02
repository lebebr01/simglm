#' Missing Data Functions
#' 
#' Function that inputs simulated data and returns data frame with 
#' new response variable that includes missing data. Missing data types
#' incorporated include dropout missing data, missing at random, and 
#' random missing data.
#' 
#' @param sim_data Simulated data frame
#' @param resp_var Character string of response variable with complete data.
#' @param new_outcome Character string of new outcome variable name that includes
#'   the missing data.
#' @param clust_var Cluster variable used for the grouping, set to 
#'           NULL by default which means no clustering.
#' @param within_id ID variable within each cluster.
#' @param miss_prop Proportion of missing data overall 
#' @param dropout_location A vector the same length as the number of clusters 
#'   representing the number of data observations for each individual.
#' @param type The type of missing data to generate, currently supports
#'           dropout, random, or missing at random (mar) missing data.
#' @param miss_cov Covariate that the missing values are based on.
#' @param mar_prop Proportion of missing data for each unique value 
#'   specified in the miss_cov argument.
#' @export 
#' @rdname missing
missing_data <- function(sim_data, resp_var = 'sim_data',
                         new_outcome = 'sim_data2',
                         clust_var = NULL, within_id = NULL, miss_prop = NULL,
                         dropout_location = NULL,
                         type = c('dropout', 'random', 'mar'),
                         miss_cov, mar_prop) {
  switch(type,
         dropout = dropout_missing(sim_data, resp_var, new_outcome, clust_var, 
                                   within_id, miss_prop, dropout_location),
         random = random_missing(sim_data, resp_var, new_outcome, miss_prop, 
                                 clust_var, within_id),
         mar = mar_missing(sim_data, resp_var, new_outcome, miss_cov, mar_prop)
         )
}

#' Tidy Missing Data Function
#' 
#' @param data Data simulated from other functions to pass to this function.
#' @param sim_args A named list with special model formula syntax. See details and examples
#'   for more information. The named list may contain the following:
#'   \itemize{
#'     \item fixed: This is the fixed portion of the model (i.e. covariates)
#'     \item random: This is the random portion of the model (i.e. random effects)
#'     \item error: This is the error (i.e. residual term).
#'   }
#' @export 
generate_missing <- function(data, sim_args) {
  
  resp_var <- parse_formula(sim_args)[['outcome']]
  
  purrr::invoke("missing_data",
                sim_args[['missing_data']],
                sim_data = data,
                resp_var = resp_var,
                within_id = 'level1_id')
}


#' @param sim_data Simulated data frame
#' @param resp_var Character string of response variable with complete data.
#' @param new_outcome Character string of new outcome variable name that includes
#'   the missing data.
#' @param clust_var Cluster variable used for the grouping, set to 
#'           NULL by default which means no clustering.
#' @param within_id ID variable within each cluster.
#' @param miss_prop Proportion of missing data overall 
#' @param dropout_location A vector the same length as the number of clusters 
#'   representing the number of data observations for each individual.
#' @export
#' @rdname missing 
dropout_missing <- function(sim_data, resp_var = 'sim_data', 
                            new_outcome = 'sim_data2', 
                        clust_var = 'clustID', within_id = "withinID", 
                        miss_prop = NULL, dropout_location = NULL) {
  
  if(resp_var %ni% names(sim_data)) {
    stop(paste(resp_var, 'not found in variables of data supplied'))
  }
  if(clust_var %ni% names(sim_data)) {
    stop(paste(clust_var, 'not found in variables of data supplied'))
  }
  
  sim_data <- data.frame(sim_data)
  
  len_groups <- tapply(sim_data[, resp_var], 
                       sim_data[, clust_var], 
                       length)
  
  num_obs <- length(unique(sim_data[, clust_var]))

  if(miss_prop > 1) {
    miss_prop <- miss_prop / 100
  }
  total_missing <- num_obs * miss_prop
  missing_range <- round((total_missing *.98):(total_missing * 1.02))
  n_groups <- length(unique(sim_data[, clust_var]))
  
  if(is.null(dropout_location)) {
    drop_missing <- data.frame(clust_var = seq_len(n_groups))
    colnames(drop_missing) <- clust_var
    drop_missing['missing_clust'] <- ifelse(runif(n_groups) < miss_prop, 1, 0)
    
    len_groups_people_missing <- len_groups[drop_missing[drop_missing[['missing_clust']] == 1,'id']]
    
    dropout_location <- unlist(lapply(seq_along(drop_missing[['missing_clust']]), function(xx) 
      dropout_helper(drop_missing[['missing_clust']][xx], len_groups[xx]))
    )
  }
    
  num_missing <- round(len_groups - dropout_location, 0)

    
  missing_obs <- lapply(1:length(num_missing), function(xx) 
    (len_groups[xx] - num_missing[xx] + 1):len_groups[xx])
  
  data_split <- split(sim_data, sim_data[, clust_var])
  
  sim_data['missing'] <- do.call("c", lapply(1:length(missing_obs), function(xx)
    ifelse(data_split[[xx]][, within_id] %in% missing_obs[[xx]], 1, 0)))
  
  sim_data[new_outcome] <- sim_data[resp_var]
  sim_data[sim_data['missing'] == 1, new_outcome] <- NA
  
  sim_data
}

dropout_helper <- function(data, num_obs) {
  if(data == 0) {
    num_obs
  } else {
    round(runif(1, min = 1, max = num_obs), 0)
  }
  
}


#' @param sim_data Simulated data frame
#' @param resp_var Character string of response variable with complete data.
#' @param new_outcome Character string of new outcome variable name that includes
#'   the missing data.
#' @param miss_prop Proportion of missing data overall 
#' @param clust_var Cluster variable used for the grouping, set to 
#'           NULL by default which means no clustering.
#' @param within_id ID variable within each cluster.
#' @export 
#' @rdname missing
random_missing <- function(sim_data, resp_var = 'sim_data', 
                           new_outcome = 'sim_data2', miss_prop,
                           clust_var = NULL, within_id = "withinID") {
  if(resp_var %ni% names(sim_data)) {
    stop(paste(resp_var, 'not found in variables of data supplied'))
  }
  
  sim_data <- data.frame(sim_data)

  if(is.null(clust_var)) {
    sim_data['miss_prob'] <- round(runif(nrow(sim_data)), 3)
    sim_data['missing'] <- ifelse(sim_data['miss_prob'] < miss_prop, 1, 0)
    sim_data[new_outcome] <- sim_data[resp_var]
    sim_data[sim_data['missing'] == 1, new_outcome] <- NA
  } else {
    if(clust_var %ni% names(sim_data)) {
      stop(paste(clust_var, 'not found in variables of data supplied'))
    }
    
    len_groups <- tapply(sim_data[, resp_var], 
                         sim_data[, clust_var], 
                         length)
    
    num_obs <- nrow(sim_data)
    
    if(length(miss_prop) == 1) {
      if(miss_prop > 1) {
        miss_prop <- miss_prop / 100
      }
      total_missing <- num_obs * miss_prop
      missing_range <- round((total_missing *.98):(total_missing * 1.02))
      n_groups <- with(sim_data, length(unique(eval(parse(text = clust_var)))))
      
      lim <- prop_limits(miss_prop)
      
      num_missing <- 0
      while(sum(num_missing) %ni% missing_range) {
        num_missing <- round(len_groups * round(runif(n_groups, lim[1], 
                                                      lim[2]), 2))
      }
      
    } else {
      num_missing <- round(len_groups * miss_prop, 0)
    }
    
    missing_obs <- lapply(1:length(num_missing), function(xx) 
      sample(1:len_groups[xx], num_missing[xx]))
    
    data_split <- split(sim_data, sim_data[, clust_var])
    
    sim_data['missing'] <- do.call("c", lapply(1:length(missing_obs), function(xx)
      ifelse(data_split[[xx]][, within_id] %in% missing_obs[[xx]], 1, 0)))
    
    sim_data[new_outcome] <- sim_data[resp_var]
    sim_data[sim_data['missing'] == 1, new_outcome] <- NA
  }

  sim_data
}

#' @param sim_data Simulated data frame
#' @param resp_var Character string of response variable with complete data.
#' @param new_outcome Character string of new outcome variable name that includes
#'   the missing data.
#' @param miss_cov Covariate that the missing values are based on.
#' @param mar_prop Proportion of missing data for each unique value 
#'   specified in the miss_cov argument.
#' @importFrom dplyr count select slice left_join mutate
#' @export 
#' @rdname missing
mar_missing <- function(sim_data, resp_var = 'sim_data', 
                        new_outcome = 'sim_data2', miss_cov, mar_prop) {
  
  if(as.character(resp_var) %ni% names(sim_data)) {
    stop(paste(resp_var, 'not found in variables of data supplied'))
  }
  if(as.character(miss_cov) %ni% names(sim_data)) {
    stop(paste(miss_cov, 'not found in variables of data supplied'))
  }
  
  sim_data <- data.frame(sim_data)
  
  num_obs <- nrow(sim_data)

  var_enq <- rlang::sym(miss_cov)
  uniq_vals <- dplyr::count(sim_data, !!var_enq)
  
  if(nrow(uniq_vals) != length(mar_prop)) {
    uniq_vals[['group']] <- cut(uniq_vals[[miss_cov]], breaks = length(mar_prop), labels = FALSE)
    missing_prop <- data.frame(group = 1:length(mar_prop),
                               miss_prop = mar_prop)
    miss_per <- left_join(dplyr::select(uniq_vals, !!var_enq, group),
                      missing_prop, 
                      by = 'group')
  } else {
    miss_per <- cbind(dplyr::select(uniq_vals, !!var_enq), 
                      miss_prop = mar_prop)
  }
  sim_data <- dplyr::left_join(sim_data, miss_per, by = miss_cov)
  sim_data <- dplyr::mutate(sim_data, miss_prob = runif(nrow(sim_data)),
                            missing = ifelse(miss_prob < miss_prop, 1, 0))
  
  sim_data[new_outcome] <- sim_data[resp_var]
  sim_data[sim_data['missing'] == 1, new_outcome] <- NA
  
  sim_data
}
