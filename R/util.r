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

sample_sizes <- function(sim_args) {
  
  if(is.null(sim_args$unbalanced)) {
    if(length(sim_args$sample_size) == 1) {
      
    }
  } else {
    
  }
  
  if(unbal$level3 == FALSE) {
    lvl2ss <- rep(n, k)
    n <- sum(lvl2ss)
  } else {
    if(is.null(unbal_design$level3)) {
      stop("Must specify unbal_design$level3 when unbal$level3 = TRUE")
    }
    if(is.null(names(unbal_design$level3))) {
      if(length(unbal_design$level3) != k) {
        stop('unbal_design$level3 must be same length as k')
      }
      lvl2ss <- unbal_design$level3
    } else {
      lvl2ss <- round(runif(n = k, min = unbal_design$level3$min, 
                            max = unbal_design$level3$max), 0)
    }
    n <- sum(lvl2ss)
  }
  
  if(unbal$level2 == FALSE) {
    lvl1ss <- rep(p, n)
    if(is.null(lvl1ss)) stop("lvl1ss is NULL")
  } else {
    if(is.null(unbal_design$level2)) {
      stop("Must specify unbal_design$level2 when unbal$level2 = TRUE")
    }
    if(is.null(names(unbal_design$level2))) {
      if(length(unbal_design$level2) != n) {
        stop('unbal_design$level2 must be same length as n')
      }
      lvl1ss <- unbal_design$level2
    } else {
      lvl1ss <- round(runif(n = n, min = unbal_design$level2$min, 
                            max = unbal_design$level2$max), 0)
    }
  }
  
  end <- cumsum(lvl2ss)
  beg <- c(1, cumsum(lvl2ss) + 1)
  beg <- beg[-length(beg)]
  
  lvl3ss <- sapply(lapply(1:length(beg), function(xx) 
    lvl1ss[beg[xx]:end[xx]]), sum)
}

# Horrible hack to keep CRAN happy and suppress NOTES about
# parts of the code that use non-standard evaluation.
# See:
# http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
# https://github.com/smbache/magrittr/issues/29
utils::globalVariables(c('test_stat', 'reject', 'estimate', 'term', 'std.error'))
