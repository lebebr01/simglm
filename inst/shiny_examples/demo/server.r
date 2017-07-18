library(shiny)
library(shinydashboard)
library(simglm)
library(ggplot2)
library(lme4)
library(highcharter)

source('global.r')
options(useFancyQuotes = FALSE)

extract_needed_args <- function(func, remove_n = TRUE) {
  if(remove_n){
    formalArgs(func)[sapply(formals(func), 'is.symbol') == TRUE & 
                       formalArgs(func) != 'n']
  } else {
    formalArgs(func)[sapply(formals(func), 'is.symbol') == TRUE]
  }
}

server <- function(input, output, session) {
  
  output$change_cov <- renderUI({
    num_covs <- input$number_cov
    lapply(1:num_covs, function(i)
    div(style = 'display:inline-block',
        textInput(paste0('cov', i), label = paste0('Cov', i), 
                  value = 'cov', width = '75px'))
    )
  })
  output$cov_dist_misc <- renderUI({
    args <- extract_needed_args(input$cov_dist)
    if(length(args) == 0) {
      NULL
    } else {
      lapply(1:length(args), function(i)
        div(style = 'display:inline-block', 
            textInput(args[i], label = as.character(args[i]), 
                      value = '', width = '75px'))
      )
    }
  })
  output$lvl1_err_misc <- renderUI({
    args <- extract_needed_args(input$lvl1_err_dist)
    if(length(args) == 0) {
      NULL
    } else {
      lapply(1:length(args), function(i)
        div(style = 'display:inline-block', 
            textInput(args[i], label = as.character(args[i]), 
                      value = '', width = '75px'))
      )
    }
  })
  output$lvl2_err_misc <- renderUI({
    args <- extract_needed_args(input$lvl2_err_dist)
    if(length(args) == 0) {
      NULL
    } else {
      lapply(1:length(args), function(i)
        div(style = 'display:inline-block', 
            textInput(args[i], label = as.character(args[i]), 
                      value = '', width = '75px'))
      )
    }
  })
  output$lvl3_err_misc <- renderUI({
    args <- extract_needed_args(input$lvl3_err_dist)
    if(length(args) == 0) {
      NULL
    } else {
      lapply(1:length(args), function(i)
        div(style = 'display:inline-block', 
            textInput(args[i], label = as.character(args[i]), 
                      value = '', width = '75px'))
      )
    }
  })
  
  cov_names <- reactive({
    num_covs <- input$number_cov
    if(input$change_name == FALSE) {
      paste('cov', 1:num_covs, sep = '_')
    } else {
      sapply(1:num_covs, function(i) input[[paste0('cov', i)]])
    }
  })
  
  output$select_missing_cov <- renderUI({
    selectInput('miss_cov', 'Select Missing Covariate',
                choices = cov_names())
  })
  
  output$beta <- renderUI({
    num_covs <- input$number_cov
    if(input$type_nested == 2) {
      num_covs <- num_covs + 1
    }
    if(input$incl_int) {
      num_covs <- num_covs + 1
    }
    beta_names <- paste0('Beta ', cov_names())
    
    if(input$type_nested == 2) {
      beta_names <- c('Beta Time', beta_names)
    }
    if(input$incl_int) {
      beta_names <- c('Beta Int', beta_names)
    } 
    lapply(1:num_covs, function(i)
      div(style = 'display:inline-block',
          numericInput(paste0('beta', i), label = beta_names[i], 
                       value = 1, width = '75px'))
    )
  })
  
  output$mean_cov  <- renderUI({
    num_covs <- input$number_cov - input$num_discrete
    cov_names <- paste0('Mean ', cov_names())
    lapply(1:num_covs, function(i) 
      div(style = 'display:inline-block', 
          numericInput(paste0('mean', i), label = cov_names[i], 
                       value = 0, width = '75px'))
    )
  })
  
  output$sd_cov <- renderUI({
    num_covs <- input$number_cov - input$num_discrete
    cov_names <- paste0('SD ', cov_names())
    lapply(1:num_covs, function(i) 
      div(style = 'display:inline-block', 
          numericInput(paste0('sd', i), label = cov_names[i], 
                       value = 1, width = '75px'))
    )
  })
  
  output$type_cov <- renderUI({
    num_covs <- input$number_cov - input$num_discrete
    cov_names <- paste0('Type ', cov_names())
    if(input$type_model == 1) {
      lapply(1:num_covs, function(i) 
        div(style = 'display:inline-block', 
            textInput(paste0('type', i), label = cov_names[i], 
                      value = 'single', width = '75px'))
      )
    } else {
      lapply(1:num_covs, function(i) 
        div(style = 'display:inline-block', 
            textInput(paste0('type', i), label = cov_names[i], 
                      value = 'level1', width = '75px'))
      )
    }
  })
  
  output$cov_dist <- renderUI({
    num_covs <- input$number_cov - input$num_discrete
    cov_names <- paste0('Dist ', cov_names())
    lapply(1:num_covs, function(i) 
      div(style = 'display:inline-block', 
          textInput(paste0('c_dist', i), label = cov_names[i], 
                       value = 'rnorm', width = '75px'))
    )
  })
  
  output$lvl2_err <- renderUI({
    if(input$type_model %in% c(2, 3) & input$type_nested == 2) {
      lapply(c('int', 'time'), function(i)
        div(style = 'display:inline-block',
            textInput(paste0('var_', i), label = paste0('Var ', i), 
                      value = '1', width = '75px')
        )
      )
    } else {
      if(input$type_model %in% c(2, 3) & input$type_nested == 1) {
        textInput('var_int', label = 'Var int', value = '1')
      }
    }
  })
  output$num_levels <- renderUI({
    num_covs <- input$num_discrete
    cov_names <- paste0('Levels ', cov_names()[
      grep('\\.f|\\.c|\\.o|\\_f|\\_c|\\_o', cov_names())])
    lapply(1:num_covs, function(i)
      div(style = 'display:inline-block',
          numericInput(paste0('levels', i), label = cov_names[i],
                       value = 2, width = '75px'))
    )
  })
  output$var_type <- renderUI({
    num_covs <- input$num_discrete
    cov_names <- paste0('Type ', cov_names()[grep('\\.f|\\.c|\\.o', cov_names())])
    if(input$type_model == 1) {
      lapply(1:num_covs, function(i) 
        div(style = 'display:inline-block', 
            textInput(paste0('type_dis', i), label = cov_names[i], 
                      value = 'single', width = '75px'))
      )
    } else {
      lapply(1:num_covs, function(i) 
        div(style = 'display:inline-block', 
            textInput(paste0('type_dis', i), label = cov_names[i], 
                      value = 'level1', width = '75px'))
      )
    }
  })
  
  n <- reactive({
    if(input$type_model == 2 | input$type_model == 3) {
      input$samp_size_lvl2
    } else {
      input$samp_size_lvl1
    }
  })
  p <- reactive({
    if(input$type_model == 2 | input$type_model == 3) {
      input$samp_size_lvl1
    } else {
      input$samp_size_lvl2
    }
  })
  k <- reactive({
    input$samp_size_lvl3
  })
  error_var <- reactive({
    if(input$type_outcome == 1) {
      input$lvl1_err
    } else {
      NULL
    }
  })
  with_err_gen <- reactive({
    'rnorm'
  })
  fixed <- reactive({
    if(input$type_model == 1) {
      if(input$incl_int) {
        as.formula(paste0('~ 1 + ', 
                          paste(cov_names(), 
                                collapse = ' + ')))
      } else {
        as.formula(paste0('~ 0 + ', 
                          paste(cov_names(), 
                                collapse = ' + ')))
      }
    } else {
      if(input$type_model == 2 | input$type_model == 3) {
        if(input$type_nested == 1) {
          if(input$incl_int) {
            as.formula(paste0('~ 1 + ',
                              paste(cov_names(),
                                    collapse = ' + ')))
          } else {
            as.formula(paste0('~ 0 + ',
                              paste(cov_names(),
                                    collapse = ' + ')))
          }
        } else {
          if(input$incl_int) {
            as.formula(paste0('~ 1 + time + ',
                              paste(cov_names(),
                                    collapse = ' + ')))
          } else {
            as.formula(paste0('~ 0 + time + ',
                              paste(cov_names(),
                                    collapse = ' + ')))
          }
        }
      }
    }
    
  })
  fixed_param <- reactive({
    num_betas <- input$number_cov
    if(input$incl_int) {
      num_betas <- num_betas + 1
    }
    if(input$type_nested == 2) {
      num_betas <- num_betas + 1
    }
    sapply(1:num_betas, function(i) input[[paste0('beta', i)]])
  })
  cov_param <- reactive({
    num_cov <- input$number_cov - input$num_discrete
    if(input$change_cov_dist) {
      dist_fun_cov <- sapply(1:num_cov, function(i) input[[paste0('c_dist', i)]])
    } else {
      dist_fun_cov <- sapply(1:num_cov, function(i) 'rnorm')
    }
    mean_cov <- sapply(1:num_cov, function(i) input[[paste0('mean', i)]])
    sd_cov <- sapply(1:num_cov, function(i) input[[paste0('sd', i)]])
    type_cov <- sapply(1:num_cov, function(i) input[[paste0('type', i)]])
    opts <- lapply(1:num_cov, function(i) list(
      input[[paste0('mean', i)]], input[[paste0('sd', i)]]
    ))
    
    list(dist_fun = dist_fun_cov, 
         var_type = type_cov, 
         opts = opts
    )
  })
  data_str <- reactive({
    if(input$type_model == 1) {
      'single'
    } else {
      if(input$type_model == 2 | input$type_model == 3){
        if(input$type_nested == 1) {
          'cross'
        } else {
          'long'
        }
      }
    }
  })
  random <- reactive({
    if(input$type_nested == 1) {
      ~ 1
    } else {
      ~ 1 + time
    }
  })
  random_param <- reactive({
    if(input$type_nested == 1) {
      ran_var <- input[['var_int']]
      list(random_var = as.numeric(ran_var),
           rand_gen = 'rnorm')
    } else {
      ran_var <- sapply(c('int', 'time'), function(i) input[[paste0('var_', i)]])
      list(random_var = as.numeric(ran_var),
           rand_gen = 'rnorm')
    }
  })
  random3 <- reactive({
    ~ 1
  })
  random_param3 <- reactive({
    list(random_var = input$lvl3_err,
         rand_gen = 'rnorm')
  })
  unbal <- reactive({
    if(input$unbal_lvl2 & input$unbal_lvl3) {
      list(level2 = TRUE, level3 = TRUE)
    } else {
      if(input$unbal_lvl2 & !input$unbal_lvl3) {
        list(level2 = TRUE, level3 = FALSE)
      } else {
        if(!input$unbal_lvl2 & input$unbal_lvl3) {
          list(level2 = FALSE, level3 = TRUE)
        } else {
          list(level2 = FALSE, level3 = FALSE)
        }
      }
    }
  })
  unbal_design <- reactive({
    if(input$unbal_lvl2) {
      c(input$min_cl2, input$max_cl2)
    } else {
      list(level2 = NULL, level3 = NULL)
    }
  })
  # arima <- reactive({
  #   if(input$sc) {
  #     TRUE
  #   } else {
  #     FALSE
  #   }
  # })
  # arima_model <- reactive({
  #   if(input$sc == FALSE) {
  #     NULL
  #   } else {
  #     
  #   }
  # })

  err_misc_1 <- reactive({
    if(input$change_error_dist == FALSE) {
      NULL
    } else {
      args <- extract_needed_args(input$lvl1_err_dist)
      input[args]
    }
  })
  err_misc_2 <- reactive({
    if(input$change_error_dist == FALSE) {
      NULL
    } else {
      args <- extract_needed_args(input$lvl2_err_dist)
      input[args]
    }
  })
  err_misc_3 <- reactive({
    if(input$change_error_dist == FALSE) {
      NULL
    } else {
      args <- extract_needed_args(input$lvl3_err_dist)
      input[args]
    }
  })
  fact_vars <- reactive({
    if(input$dis_cov == FALSE) {
      NULL
    } else {
      levels <- sapply(1:input$num_discrete, function(i) input[[paste0('levels', i)]])
      var_type <- sapply(1:input$num_discrete, function(i) input[[paste0('type_dis', i)]])
      list(numlevels = levels, var_type = var_type)
    }
  })
  
  gen_code <- eventReactive(input$update | input$update_2, {
    if(input$type_outcome == 1) {
      if(input$type_model == 1) {
        sim_reg(fixed = fixed(), fixed_param = fixed_param(), cov_param = cov_param(),
                n = n(), error_var = error_var(), with_err_gen = with_err_gen(),
                data_str = data_str(), lvl1_err_params = err_misc_1(),
                fact_vars = fact_vars()
        )
      } else {
        if(input$type_model == 2) {
          sim_reg(fixed = fixed(), random = random(),
                  fixed_param = fixed_param(),
                  random_param = random_param(), cov_param = cov_param(),
                  k = NULL, n = n(), p = p(),
                  error_var = error_var(), with_err_gen = with_err_gen(),
                  data_str = data_str(), unbal = unbal(), 
                  unbal_design = unbal_design(),
                  fact_vars = fact_vars()
          )
        } else {
          sim_reg(fixed(), random(), random3(), fixed_param(), random_param(), 
                  random_param3(), cov_param(), k(), n(), p(), 
                  error_var(), with_err_gen(),
                  data_str = data_str(), unbal = unbal(), 
                  unbal_design = unbalCont(),
                  fact_vars = fact_vars()
          )
        }
      }
    } else {
      if(input$type_model == 1) {
        sim_glm(fixed = fixed(), fixed_param = fixed_param(), cov_param = cov_param(),
                n = n(), data_str = data_str(), fact_vars = fact_vars()
        )
      } else {
        if(input$type_model == 2) {
          sim_glm(fixed = fixed(), random = random(),
                  fixed_param = fixed_param(),
                  random_param = random_param(), cov_param = cov_param(),
                  k = NULL, n = n(), p = p(),
                  data_str = data_str(), unbal = unbal(), unbalCont = unbalCont(),
                  fact_vars = fact_vars()
          )
        } else {
          sim_glm(fixed(), random(), random3(), fixed_param(), random_param(), 
                  random_param3(), cov_param(), k(), n(), p(),
                  data_str = data_str(), unbal = unbal(), unbalCont = unbalCont(),
                  unbal3 = unbal3(), unbalCont3 = unbalCont3(),
                  fact_vars = fact_vars()
          )
        }
      }
    }
  })
  
  miss_clustvar <- reactive({
    if(input$type_missing %in% c(1, 2)) {
      NULL
    } else {
      'clustID'
    }
  })
  miss_withinid <- reactive({
    if(input$type_missing %in% c(1, 2)) {
      NULL
    } else {
      'withinID'
    }
  })
  missing_cov <- reactive({
    if(input$type_missing %in% c(1, 3)) {
      NULL
    } else {
      input$miss_cov
    }
  })
  missing_type <- reactive({
    if(input$type_missing == 1) {
      'random'
    } else {
      if(input$type_missing == 2) {
        'mar'
      } else {
        'dropout'
      }
    }
  })
  
  miss_data <- eventReactive(input$update | input$update_2, {
    missing_data(gen_code(), miss_prop = input$miss_prop, type = missing_type(),
                 clust_var = miss_clustvar(), within_id = miss_withinid(), 
                 miss_cov = missing_cov())
  })
  
  output$miss_data <- renderTable({
    if(input$verify_missing) {
      data.frame(Type = c('Not Missing', 'Missing'), 
                 Count = data.frame(table(miss_data()$missing))[, 2],
                 Proportion = data.frame(prop.table(table(miss_data()$missing)))[, 2])
    }
  })
  
  output$gen_examp <- output$gen_examp_2 <- renderDataTable(
    if(input$update == 0 & input$update_2 == 0) {
      NULL
    } else {
      if(input$missing == FALSE) {
        sapply(gen_code(), round, 3)
      } else {
        sapply(miss_data(), round, 3)
      }
    }
  )
  
  mod <- reactive({
    if(input$type_model == 1) {
      mod_formula <- as.formula(paste('sim_data ~ ', fixed()[2]))
      if(input$type_outcome == 1) {
        lm(mod_formula, data = gen_code())
      } else {
        glm(mod_formula, data = gen_code(), family = binomial)
      }
    } else {
      if(input$type_model == 2) {
        mod_formula <- as.formula(paste('sim_data ~ ', fixed()[2],
                                        ' + (', random()[2],
                                        '|clustID)'))
      }
      if(input$type_model == 3) {
        mod_formula <- as.formula(paste('sim_data ~ ', fixed()[2],
                                        ' + (', random()[2],
                                        '|clustID) + (',
                                        random3()[2],
                                        '|clust3ID)'))
      }
      if(input$type_outcome == 1) {
        lmer(mod_formula, data = gen_code())
      } else {
        glmer(mod_formula, data = gen_code(), family = binomial)
      }
    }
  })
  
  output$model_results <- renderPrint({
    if(input$update == 0 & input$update_2 == 0) {
      NULL
    } else {
      summary(mod())
    }
  })
  
  output$param_diff <- renderDataTable({
    if(input$type_model == 1) {
      est <- coef(mod())
      if(input$type_outcome == 1) {
        error <- summary(mod())$sigma^2
        est <- c(est, error)
      }
      terms <- unlist(strsplit(as.character(fixed())[2], '\\s*\\+\\s*'))
      terms <- gsub("^1", "Intercept", terms)
      
      params <- data.frame(Terms = as.character(terms),
                           Type = 'Fixed',
                        Parameter = fixed_param(),
                        stringsAsFactors = FALSE)
      if(input$type_outcome == 1) {
        params <- rbind(params, c('Residual Error', 'Random', error_var()))
      }
      params <- cbind(params, est)
      params$diff <- as.numeric(params$Parameter) - as.numeric(params$est)
    } else {
      est <- fixef(mod())
      if(input$type_model == 2) {
        rand_var <- attr(VarCorr(mod())[[1]], 'stddev')^2
      } else {
        rand_var <- c(attr(VarCorr(mod())[[1]], 'stddev')^2, 
                      attr(VarCorr(mod())[[2]], 'stddev')^2)
      }
      est <- c(est, rand_var)
      if(input$type_outcome == 1) {
        error <- attr(VarCorr(mod()), 'sc')^2
        est <- c(est, error)
      }
      terms <- unlist(strsplit(as.character(fixed())[2], '\\s*\\+\\s*'))
      terms <- gsub("^1", "Intercept", terms)
      
      params <- data.frame(Terms = as.character(terms),
                           Type = 'Fixed',
                           Parameter = fixed_param(),
                           stringsAsFactors = FALSE)
      if(input$type_model == 3) {
        terms_2 <- unlist(strsplit(as.character(random())[2], '\\s*\\+\\s*'))
        terms_2 <- gsub('^1', 'Intercept', terms_2)
        terms_3 <- unlist(strsplit(as.character(random3())[2], '\\s*\\+\\s*'))
        terms_3 <- gsub('^1', 'Intercept', terms_3)
        rand_terms <- c(paste('rand_lvl2', terms_2, sep = '_'),
                        paste('rand_lvl3', terms_3, sep = '_'))
        rand_params <- data.frame(Terms = rand_terms,
                                  Type = 'Random',
                                  Parameter = c(random_param()[[1]], random_param3()[[1]]),
                                  stringsAsFactors = FALSE)
      } else {
        terms_2 <- unlist(strsplit(as.character(random())[2], '\\s*\\+\\s*'))
        terms_2 <- gsub('^1', 'Intercept', terms_2)
        rand_terms <- paste('rand_lvl2', 
                            terms_2,
                            sep = '_')
        rand_params <- data.frame(Terms = rand_terms,
                                  Type = 'Random',
                                  Parameter = random_param()[[1]],
                                  stringsAsFactors = FALSE)
      }
      params <- rbind(params, rand_params)
      
      if(input$type_outcome == 1) {
        params <- rbind(params, c('Residual Error', 'Random', error_var()))
      }
      params <- cbind(params, est)
      params$diff <- as.numeric(params$Parameter) - as.numeric(params$est)
    }
    datatable(params, rownames = FALSE)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste('gen_data.csv', sep='') 
    },
    content = function(file) {
      if(input$missing == FALSE) {
        write.csv(gen_code(), file, row.names = FALSE)
      } else {
        write.csv(miss_data(), file, row.names = FALSE)
      }
      
    }
  )
  
  n_code <- reactive({
    if(input$type_model == 2 | input$type_model == 3) {
      paste0('n <- ', input$samp_size_lvl2)
    } else {
      paste0('n <- ', input$samp_size_lvl1)
    }
  })
  p_code <- reactive({
    if(input$type_model == 2 | input$type_model == 3) {
      paste0('p <- ', input$samp_size_lvl1)
    } else {
      paste0('p <- ', input$samp_size_lvl2)
    }
  })
  k_code <- reactive({
    paste0('k <- ', input$samp_size_lvl3)
  })
  error_var_code <- reactive({
    if(input$type_outcome == 1) {
      paste0('error_var <- ', input$lvl1_err)
    } else {
      NULL
    }
  })
  with_err_gen_code <- reactive({
    'with_err_gen <- rnorm'
  })
  fixed_code <- reactive({
    if(input$type_model == 1) {
      if(input$incl_int) {
        paste0('fixed <- ~ 1 + ', 
               paste(cov_names(), 
                     collapse = ' + '))
      } else {
        paste0('fixed <- ~ 0 + ', 
               paste(cov_names(), 
                     collapse = ' + '))
      }
    } else {
      if(input$type_model == 2 | input$type_model == 3) {
        if(input$type_nested == 1) {
          if(input$incl_int) {
            paste0('fixed <- ~ 1 + ',
                   paste(cov_names(),
                         collapse = ' + '))
          } else {
            paste0('fixed <- ~ 0 + ',
                   paste(cov_names(),
                         collapse = ' + '))
          }
        } else {
          if(input$incl_int) {
            paste0('fixed <- ~ 1 + time + ',
                   paste(cov_names(),
                         collapse = ' + '))
          } else {
            paste0('fixed <- ~ 0 + time + ',
                   paste(cov_names(),
                         collapse = ' + '))
          }
        }
      }
    }
    
  })
  fixed_param_code <- reactive({
    num_betas <- input$number_cov
    if(input$incl_int) {
      num_betas <- num_betas + 1
    }
    if(input$type_nested == 2) {
      num_betas <- num_betas + 1
    }
    paste0('fixed_param <- c(', 
           paste(sapply(1:num_betas, function(i) input[[paste0('beta', i)]]), collapse = ', '),
           ')')
  })
  cov_param_code <- reactive({
    
    num_cov <- input$number_cov - input$num_discrete
    if(input$change_cov_dist) {
      dist_fun_cov <- paste(sQuote(sapply(1:num_cov, function(i) input[[paste0('c_dist', i)]])),
                            collapse = ', ')
    } else {
      dist_fun_cov <- paste(sQuote(sapply(1:num_cov, function(i) 'rnorm')),
                            collapse = ', ')
    }
    type_cov <- paste(sQuote(sapply(1:num_cov, function(i) input[[paste0('type', i)]])),
                      collapse = ', ')
    opts <- lapply(1:num_cov, function(i) list(
      mean = input[[paste0('mean', i)]], 
      sd = input[[paste0('sd', i)]]
    ))
    opts_text <- lapply(1:num_cov, function(i) 
      paste(paste(attr(unlist(opts[[i]]), 'names'), 
                  unlist(opts[[i]]), sep = ' = '), collapse = ', ')
    )
    
    paste0('cov_param <- list(dist_fun = c(', dist_fun_cov, 
           '), var_type = c(', type_cov, 
           '), opts = list(list(', paste(opts_text, collapse = '), list('),
           ')))')
  })
  data_str_code <- reactive({
    if(input$type_model == 1) {
      'data_str <- "single"'
    } else {
      if(input$type_model == 2 | input$type_model == 3){
        if(input$type_nested == 1) {
          'data_str <- "cross"'
        } else {
          'data_str <- "long"'
        }
      }
    }
  })
  random_code <- reactive({
    if(input$type_nested == 1) {
      'random <- ~ 1'
    } else {
      'random <- ~ 1 + time'
    }
  })
  random_param_code <- reactive({
    if(input$type_nested == 1) {
      ran_var <- input[['var_int']]
      paste0('random_param <- list(random_var = ', ran_var,
                           ", rand_gen = 'rnorm')")
    } else {
      ran_var <- paste(sapply(c('int', 'time'), function(i) input[[paste0('var_', i)]]), 
                       collapse = ', ')
      paste0('random_param <- list(random_var = c(', ran_var, '), ',
                           "rand_gen = 'rnorm')")
    }
  })
  random3_code <- reactive({
    'random3 <- ~ 1'
  })
  random_param3_code <- reactive({
    paste0('random_param3 <- list(random_var = ', input$lvl3_err,
         ", rand_gen = 'rnorm')")
  })
  unbal_code <- reactive({
    if(input$unbal_lvl2 & input$unbal_lvl3) {
      'unbal <- list(level2 = TRUE, level3 = TRUE)'
    } else {
      if(input$unbal_lvl2 & !input$unbal_lvl3) {
        'unbal <- list(level2 = TRUE, level3 = FALSE)'
      } else {
        if(!input$unbal_lvl2 & input$unbal_lvl3) {
          'unbal <- list(level2 = FALSE, level3 = TRUE)'
        } else {
          'unbal <- list(level2 = FALSE, level3 = FALSE)'
        }
      }
    }
  })
  unbal_design_code <- reactive({
    if(input$unbal_lvl2) {
      'unbal_design <- list(level2 = c(input$min_cl2, input$max_cl2), 
                              level3 = NULL);'
    } else {
      'unbal_design <- list(level2 = NULL, level3 = NULL)'
    }
  })
  fact_vars_code <- reactive({
    if(input$dis_cov == FALSE) {
      'fact_vars <- list(NULL)'
    } else {
      levels <- paste(sapply(1:input$num_discrete, function(i) input[[paste0('levels', i)]]),
                      collapse = ', ')
      var_type <- paste(sapply(1:input$num_discrete, function(i) input[[paste0('type_dis', i)]]),
                        collapse = ', ')
      paste0('fact_vars <- list(numlevels = ', levels, ', var_type = ', var_type, ')')
    }
  })
  
  miss_clustvar_code <- reactive({
    if(input$type_missing %in% c(1, 2)) {
      'miss_clustvar <- NULL'
    } else {
      'miss_clustvar <- clustID'
    }
  })
  miss_withinid_code <- reactive({
    if(input$type_missing %in% c(1, 2)) {
      'miss_withinid <- NULL'
    } else {
      'miss_withinid <- withinID'
    }
  })
  missing_cov_code <- reactive({
    if(input$type_missing %in% c(1, 3)) {
      'missing_cov <- NULL'
    } else {
      paste0('missing_cov <- ', input$miss_cov)
    }
  })
  missing_type_code <- reactive({
    if(input$type_missing == 1) {
      'missing_type <- "random"'
    } else {
      if(input$type_missing == 2) {
        'missing_type <- "mar"'
      } else {
        'missing_type <- "dropout"'
      }
    }
  })
  missing_prop_code <- reactive({
    paste0('miss_prop <- ', input$miss_prop)
  })
  
  output$gen_examp_code <- output$gen_examp_code_2 <- renderUI({
    if(input$type_outcome == 1) {
      if(input$type_model == 1) {
        str7 <- 'temp_single <- sim_reg(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param, <br/>
        n = n, error_var = error_var, with_err_gen = with_err_gen, <br/>
        data_str = data_str, fact_vars = fact_vars)'
        code_out <- paste(fixed_code(), fixed_param_code(), cov_param_code(), n_code(),
                          error_var_code(), with_err_gen_code(), data_str_code(), 
                          fact_vars_code(), str7, 
                          sep = '<br/>')
      } else {
        if(input$type_model == 2) {
          str7 <- 'temp_nested <- sim_reg(fixed = fixed, random = random, fixed_param = fixed_param, <br/>
          random_param = random_param, cov_param = cov_param, k = NULL, n = n, p = p, <br/>
          error_var = error_var, with_err_gen = with_err_gen, data_str = data_str, fact_vars = fact_vars, <br/>
          unbal = unbal, unbalCont = unbalCont)'
          code_out <- paste(fixed_code(), random_code(), fixed_param_code(), random_param_code(),
                            cov_param_code(), n_code(), p_code(),
                            error_var_code(), with_err_gen_code(), data_str_code(), fact_vars_code(),
                            unbal_code(), unbal_design_code(), str7, sep = '<br/>')
        } 
        else {
          str7 <- 'temp_nested <- sim_reg(fixed, random, random3, fixed_param, random_param, <br/>
          random_param3, cov_param, k, n, p, error_var, with_err_gen, <br/>
          data_str = data_str, fact_vars = fact_vars, unbal = unbal, unbal3 = unbal3,  <br/> 
          unbalCont = unbalCont, unbalCont3 = unbalCont3)'
          code_out <- paste(fixed_code(), random_code(), random3_code(), 
                            fixed_param_code(), random_param_code(), random_param3_code(),
                            cov_param_code(), k_code(), n_code(), p_code(),
                            error_var_code(), with_err_gen_code(), data_str_code(), fact_vars_code(),
                            unbal_code(), unbal_design_code(), 
                            str7, sep = '<br/>')
        }
      }
    } else {
      if(input$type_model == 1) {
        str7 <- 'temp_single <- sim_glm(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param, <br/>
        n = n, data_str = data_str, fact_vars = fact_vars)'
        code_out <- paste(fixed_code(), fixed_param_code(), cov_param_code(), n_code(),
                          data_str_code(), fact_vars_code(), str7, 
                          sep = '<br/>')
      } else {
        if(input$type_model == 2) {
          str7 <- 'temp_nested <- sim_glm(fixed = fixed, random = random, fixed_param = fixed_param, <br/>
          random_param = random_param, cov_param = cov_param, k = NULL, n = n, p = p, <br/>
          data_str = data_str, fact_vars = fact_vars, unbal = unbal, unbalCont = unbalCont)'
          code_out <- paste(fixed_code(), random_code(), fixed_param_code(), random_param_code(),
                            cov_param_code(), n_code(), p_code(),
                            data_str_code(), fact_vars_code(), 
                            unbal_code(), unbalCont_code(), str7, sep = '<br/>')
        } 
        else {
          str7 <- 'temp_nested <- sim_glm(fixed, random, random3, fixed_param, random_param, <br/>
          random_param3, cov_param, k, n, p, data_str = data_str, fact_vars = fact_vars, unbal = unbal, <br/> 
          unbal3 = unbal3, unbalCont = unbalCont, unbalCont3 = unbalCont3)'
          code_out <- paste(fixed_code(), random_code(), random3_code(), 
                            fixed_param_code(), random_param_code(), random_param3_code(),
                            cov_param_code(), k_code(), n_code(), p_code(),
                            data_str_code(), fact_vars_code(), unbal_code(), unbal3_code(), 
                            unbalCont_code(), unbalCont3_code(), str7, 
                            sep = '<br/>')
        }
      }
    }
    if(input$missing) {
      if(input$type_model == 1) {
        miss_code <- 'temp_miss <- missing_data(temp_single, miss_prop = miss_prop, type = missing_type, 
                               clust_var = miss_clustvar, within_id = miss_withinid,
        miss_cov = missing_cov)'
      } else {
        miss_code <- 'temp_miss <- missing_data(temp_nest, miss_prop = miss_prop, type = missing_type, 
                               clust_var = miss_clustvar, within_id = miss_withinid,
        miss_cov = missing_cov)'
      }
      code_out <- paste(code_out, miss_clustvar_code(), miss_withinid_code(),
                        missing_cov_code(), missing_type_code(), missing_prop_code(),
                        miss_code, sep = '<br/>')
    }
    
    code(HTML(code_out))
  })
  
  output$vars <- renderUI({
    req(gen_code)
    variables <- names(gen_code())
    selectizeInput('hist_vars', 'Plot Variable', choices = variables)
  })
  
  output$hists <- renderPlot({
    histograms(gen_code(), input$hist_vars, input$binwidth)
  })
  
  output$vary_arg <- renderUI({
    if(input$type_model == 1) {
      arg_choices <- c('n', 'error_var')
      if(input$type_outcome == 2) {
        arg_choices <- 'n'
      }
    } else {
      if(input$type_model == 2) {
        arg_choices <- c('n', 'p', 'error_var')
        if(input$type_outcome == 2) {
          arg_choices <- c('n', 'p')
        }
      } else {
        arg_choices <- c('k', 'n', 'p', 'error_var')
        if(input$type_outcome == 2) {
          arg_choices <- c('k', 'n', 'p')
        }
      }
    }
    
    selectInput('vary_arg_sel', 'Select Arguments to Vary',
                choices = arg_choices, multiple = TRUE)
  })
  
  output$vary_arg_vals <- renderUI({
    vary_name <- input$vary_arg_sel
    lapply(vary_name, function(i)
      div(style = 'display:inline-block',
          textInput(paste0('vary_', i), label = paste0('Vary Arg: ', i), 
                    value = '')
      )
    )
  })
  
  power_sim <- eventReactive(input$update_power, {
    if(input$incl_int) {
      pow_param <- c('(Intercept)', attr(terms(fixed()),"term.labels"))
    } else {
      pow_param <- attr(terms(fixed()),"term.labels")
    }
    alpha <- input$alpha
    pow_dist = input$type_dist
    pow_tail = as.numeric(as.character(input$tails))
    replicates = input$repl
    missing = FALSE
    missing_args = list(NULL)
    
    if(is.null(input$vary_arg_sel)) {
      vary_vals <- NULL
    } else {
      vary_terms <- input$vary_arg_sel
      vary_vals <- lapply(vary_terms, function(i) 
        as.numeric(unlist(strsplit(input[[paste0('vary_', i)]], split = ','))))
      
      names(vary_vals) <- vary_terms
    }
    
    if(input$missing) {
      missing = TRUE
      missing_args = list(miss_prop = input$miss_prop, type = missing_type(),
                          clust_var = miss_clustvar(), within_id = miss_withinid(), 
                          miss_cov = missing_cov())
    }
    
    if(input$type_outcome == 1) {
      if(input$type_model == 1) {
        sim_pow(fixed = fixed(), fixed_param = fixed_param(), cov_param = cov_param(),
                n = n(), error_var = error_var(), with_err_gen = with_err_gen(), 
                data_str = data_str(), missing = missing, missing_args = missing_args, 
                pow_param = pow_param, alpha = alpha,
                pow_dist = pow_dist, pow_tail = pow_tail, replicates = replicates,
                terms_vary = vary_vals)
      } else {
        if(input$type_model == 2) {
          sim_pow(fixed = fixed(), random = random(), fixed_param = fixed_param(),
                  random_param = random_param(), cov_param = cov_param(), k = NULL,
                  n = n(), p = p(), error_var = error_var(), with_err_gen = with_err_gen(),
                  data_str = data_str(), unbal = unbal(), unbalCont = unbalCont(),
                  missing = missing, missing_args = missing_args, 
                  pow_param = pow_param, alpha = alpha, pow_dist = pow_dist, 
                  pow_tail = pow_tail, replicates = replicates,
                  terms_vary = vary_vals
          )
        } else {
          sim_pow(fixed = fixed(), random = random(), random3 = random3(), 
                  fixed_param = fixed_param(),
                  random_param = random_param(), random_param3 = random_param3(), 
                  cov_param = cov_param(), k = k(),
                  n = n(), p = p(), error_var = error_var(), with_err_gen = with_err_gen(),
                  data_str = data_str(), unbal = unbal(), unbal3 = unbal3(),
                  unbalCont = unbalCont(), unbalCont3 = unbalCont3(),
                  missing = missing, missing_args = missing_args, 
                  pow_param = pow_param, alpha = alpha, pow_dist = pow_dist, 
                  pow_tail = pow_tail, replicates = replicates,
                  terms_vary = vary_vals
          )
        }
      }
    } else {
      if(input$type_model == 1) {
        sim_pow_glm(fixed = fixed(), fixed_param = fixed_param(), cov_param = cov_param(),
                n = n(), data_str = data_str(), missing = missing, missing_args = missing_args, 
                pow_param = pow_param, alpha = alpha,
                pow_dist = pow_dist, pow_tail = pow_tail, replicates = replicates,
                terms_vary = vary_vals)
      } else {
        if(input$type_model == 2) {
          sim_pow_glm(fixed = fixed(), random = random(), fixed_param = fixed_param(),
                  random_param = random_param(), cov_param = cov_param(), k = NULL,
                  n = n(), p = p(), 
                  data_str = data_str(), unbal = unbal(), unbalCont = unbalCont(),
                  missing = missing, missing_args = missing_args, 
                  pow_param = pow_param, alpha = alpha, pow_dist = pow_dist, 
                  pow_tail = pow_tail, replicates = replicates,
                  terms_vary = vary_vals
          )
        } else {
          sim_pow_glm(fixed = fixed(), random = random(), random3 = random3(), 
                  fixed_param = fixed_param(),
                  random_param = random_param(), random_param3 = random_param3(), 
                  cov_param = cov_param(), k = k(),
                  n = n(), p = p(), 
                  data_str = data_str(), unbal = unbal(), unbal3 = unbal3(),
                  unbalCont = unbalCont(), unbalCont3 = unbalCont3(),
                  missing = missing, missing_args = missing_args, 
                  pow_param = pow_param, alpha = alpha, pow_dist = pow_dist, 
                  pow_tail = pow_tail, replicates = replicates,
                  terms_vary = vary_vals
          )
        }
      }
    }
    
  })
  
  output$power_x <- renderUI({
    selectInput('power_x_axis', 'Variable for x-axis',
                choices = c(Choose = '', names(power_sim())), width = '200px', 
                selected = 'var')
  })
  
  output$power_group <- renderUI({
    selectInput('power_group_var', 'Power plots by group',
                choices = c(Choose = '', names(power_sim())), width = '200px')
  })
  
  output$power_facet <- renderUI({
    selectInput('power_facet_var', 'Facet plots?',
                choices = c(Choose = '', names(power_sim())), width = '200px')
  })
  
  output$power_plot_out <- renderPlot({
    data <- power_sim()
    if(input$power_group_var == '') {
      power_point(data, x = input$power_x_axis, y = 'power')
    } else {
      if(input$power_facet_var == '') {
        data[[input$power_group_var]] <- as.character(data[[input$power_group_var]])
        power_point_group(data, x = input$power_x_axis, y = 'power', 
                          group = input$power_group_var)
      } else {
        data[[input$power_group_var]] <- as.character(data[[input$power_group_var]])
        data[[input$power_facet_var]] <- as.character(data[[input$power_facet_var]])
        power_point_group(data, x = input$power_x_axis, y = 'power', 
                          group = input$power_group_var, 
                          facet_var = input$power_facet_var) 
      }
    }
  })
  
  # output$hcontainter <- renderHighchart({
  #     hchart(power_sim(), 'spline', x = n, y = power, group = var)
  # })
  
  output$power_table <- renderDataTable({
    datatable(power_sim())
  })
  
  output$power_code <- renderUI({
    
    if(input$incl_int) {
      pow_param <- paste0("pow_param <- c('(Intercept)', ", 
                            paste(sQuote(attr(terms(fixed()),"term.labels")), 
                                  collapse = ', '), ')')
    } else {
      power_param <- paste0('pow_param <- ', 
                            paste(sQuote(attr(terms(fixed()),"term.labels")), 
                                  collapse = ', '))
    }
    alpha <- paste0('alpha <- ', input$alpha)
    pow_dist <- paste0('pow_dist = ', sQuote(ifelse(input$type_dist == 1, 't', 'z')))
    pow_tail <- paste0('pow_tail = ', as.numeric(as.character(input$tails)))
    replicates <- paste0('replicates = ', input$repl)
    missing <- 'missing <- FALSE'
    missing_args <- 'missing_args = list(NULL)'
    
    if(is.null(input$vary_arg_sel)) {
      vary_char <- 'vary_vals <- NULL'
    } else {
      vary_terms <- input$vary_arg_sel
      vary_vals <- lapply(vary_terms, function(i) 
        as.numeric(unlist(strsplit(input[[paste0('vary_', i)]], split = ','))))
      
      vary_vals <- paste0(vary_vals)
      vary_vals <- paste(sapply(seq_along(vary_terms), function(i) 
        paste0(vary_terms[i], ' = ', vary_vals[i])), collapse = ', ')
      vary_char <- paste0('vary_vals <- list(', vary_vals, ')')
    }
    
    if(input$missing) {
      missing <- 'missing = TRUE'
      missing_args <- paste0('missing_args <- list(miss_prop = ', input$miss_prop, ', ',
                             'type = ', missing_type(), ', ',
                          'clust_var = ', miss_clustvar(), ', ', 
                          'within_id = ', miss_withinid(), ', ',
                          'miss_cov = ', missing_cov(), ')')
    }
    
    if(input$type_outcome == 1) {
      if(input$type_model == 1) {
        str7 <- 'power_out <- sim_pow(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param, <br/>
                     n = n, error_var = error_var, with_err_gen = with_err_gen, <br/>
                     data_str = data_str, fact_vars = fact_vars, missing = missing, missing_args = missing_args,  <br/>
                     pow_param = pow_param, alpha = alpha, pow_dist = pow_dist, pow_tail = pow_tail, <br/>
                     replicates = replicates, terms_vary = vary_vals)'
        power_code <- paste(fixed_code(), fixed_param_code(), cov_param_code(), n_code(), 
                            error_var_code(), with_err_gen_code(), data_str_code(), fact_vars_code(),
                            missing, missing_args,
                            pow_param, alpha, pow_dist, pow_tail, replicates, vary_char, str7,
                            sep = '<br/>')
      } else {
        if(input$type_model == 2) {
          str7 <- 'power_out <- sim_pow(fixed = fixed, random = random, fixed_param = fixed_param, <br/>
                     random_param = random_param, cov_param = cov_param, <br/>
          k = NULL, n = n, p = p, error_var = error_var, with_err_gen = with_err_gen, <br/>
          data_str = data_str, fact_vars = fact_vars, missing = missing, missing_args = missing_args, <br/> 
          unbal = unbal, unbalCont = unbalCont, pow_param = pow_param, alpha = alpha, <br/>
          pow_dist = pow_dist, pow_tail = pow_tail, <br/>
          replicates = replicates, terms_vary = vary_vals)'
          power_code <- paste(fixed_code(), random_code(), fixed_param_code(), random_param_code(), 
                              cov_param_code(), n_code(), p_code(),
                              error_var_code(), with_err_gen_code(), data_str_code(), unbal_code(), 
                              unbalCont_code(), fact_vars_code(), missing, missing_args,
                              pow_param, alpha, pow_dist, pow_tail, replicates, vary_char, str7,
                              sep = '<br/>')
        } else {
          str7 <- 'power_out <- sim_pow(fixed = fixed, random = random, random3 = random3, fixed_param = fixed_param, <br/>
                     random_param = random_param, random_param3 = random_param3, cov_param = cov_param, <br/>
          k = k, n = n, p = p, error_var = error_var, with_err_gen = with_err_gen, <br/>
          data_str = data_str, fact_vars = fact_vars, missing = missing, missing_args = missing_args, <br/> 
          unbal = unbal, unbal3 = unbal3, unbalCont = unbalCont, unbalCont3 = unbalCont3,  <br/>
          pow_param = pow_param, alpha = alpha, pow_dist = pow_dist, pow_tail = pow_tail, <br/>
          replicates = replicates, terms_vary = vary_vals)'
          power_code <- paste(fixed_code(), random_code(), random3_code(), fixed_param_code(), random_param_code(), 
                              random_param3_code(), cov_param_code(), k_code(), n_code(), p_code(),
                              error_var_code(), with_err_gen_code(), data_str_code(), unbal_code(), unbal3_code(),
                              unbalCont_code(), unbalCont3_code(), fact_vars_code(), missing, missing_args,
                              pow_param, alpha, pow_dist, pow_tail, replicates, vary_char, str7,
                              sep = '<br/>')
        }
      }
    } else {
      if(input$type_model == 1) {
        str7 <- 'power_out <- sim_pow_glm(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param, <br/>
                     n = n, data_str = data_str, fact_vars = fact_vars, missing = missing, missing_args = missing_args, <br/>
        pow_param = pow_param, alpha = alpha, pow_dist = pow_dist, pow_tail = pow_tail, <br/>
        replicates = replicates, terms_vary = vary_vals)'
        power_code <- paste(fixed_code(), fixed_param_code(), cov_param_code(), n_code(), 
                            data_str_code(), fact_vars_code(), missing, missing_args,
                            pow_param, alpha, pow_dist, pow_tail, replicates, vary_char, str7,
                            sep = '<br/>')
      } else {
        if(input$type_model == 2) {
          str7 <- 'power_out <- sim_pow_glm(fixed = fixed, random = random, fixed_param = fixed_param, <br/>
                     random_param = random_param, cov_param = cov_param, k = NULL, n = n, p = p, <br/>
          data_str = data_str, fact_vars = fact_vars, missing = missing, missing_args = missing_args, <br/> 
          unbal = unbal, unbalCont = unbalCont, pow_param = pow_param, alpha = alpha, <br/>
          pow_dist = pow_dist, pow_tail = pow_tail, <br/>
          replicates = replicates, terms_vary = vary_vals)'
          power_code <- paste(fixed_code(), random_code(), fixed_param_code(), random_param_code(), 
                              cov_param_code(), n_code(), p_code(), data_str_code(), unbal_code(), 
                              unbalCont_code(), fact_vars_code(), missing, missing_args,
                              pow_param, alpha, pow_dist, pow_tail, replicates, vary_char, str7,
                              sep = '<br/>')
        } else {
          str7 <- 'power_out <- sim_pow_glm(fixed = fixed, random = random, random3 = random3, fixed_param = fixed_param, <br/>
                     random_param = random_param, random_param3 = random_param3, cov_param = cov_param, <br/>
          k = k, n = n, p = p, <br/>
          data_str = data_str, fact_vars = fact_vars, missing = missing, missing_args = missing_args, <br/> 
          unbal = unbal, unbal3 = unbal3, unbalCont = unbalCont, unbalCont3 = unbalCont3,  <br/>
          pow_param = pow_param, alpha = alpha, pow_dist = pow_dist, pow_tail = pow_tail, <br/>
          replicates = replicates, terms_vary = vary_vals)'
          power_code <- paste(fixed_code(), random_code(), random3_code(), fixed_param_code(), random_param_code(), 
                              random_param3_code(), cov_param_code(), k_code(), n_code(), p_code(),
                              data_str_code(), unbal_code(), unbal3_code(),
                              unbalCont_code(), unbalCont3_code(), fact_vars_code(), missing, missing_args,
                              pow_param, alpha, pow_dist, pow_tail, replicates, vary_char, str7,
                              sep = '<br/>')
        }
      }
    }
    code(HTML(power_code))
  })
  
}
