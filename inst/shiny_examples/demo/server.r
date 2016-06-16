library(shiny)
library(shinydashboard)
library(simglm)
library(ggplot2)
library(lme4)

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
                      value = 'lvl1', width = '75px'))
      )
    }
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
    cov_names <- paste0('Levels ', cov_names()[grep('\\.f|\\.c|\\.o', cov_names())])
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
                      value = 'lvl1', width = '75px'))
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
            fixed <- as.formula(paste0('~ 1 + ',
                                       paste(cov_names(),
                                             collapse = ' + ')))
          } else {
            fixed <- as.formula(paste0('~ 0 + ',
                                       paste(cov_names(),
                                             collapse = ' + ')))
          }
        } else {
          if(input$incl_int) {
            fixed <- as.formula(paste0('~ 1 + time + ',
                                       paste(cov_names(),
                                             collapse = ' + ')))
          } else {
            fixed <- as.formula(paste0('~ 0 + time + ',
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
    num_cov <- input$number_cov
    mean_cov <- sapply(1:num_cov, function(i) input[[paste0('mean', i)]])
    sd_cov <- sapply(1:num_cov, function(i) input[[paste0('sd', i)]])
    type_cov <- sapply(1:num_cov, function(i) input[[paste0('type', i)]])
    
    list(mean = mean_cov,
         sd = sd_cov,
         var_type = type_cov)
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
      random <- ~ 1
    } else {
      random <- ~ 1 + time
    }
  })
  random_param <- reactive({
    if(input$type_nested == 1) {
      ran_var <- input[['var_int']]
      random_param <- list(random_var = ran_var,
                           rand_gen = 'rnorm')
    } else {
      ran_var <- sapply(c('int', 'time'), function(i) input[[paste0('var_', i)]])
      random_param <- list(random_var = ran_var,
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
    if(input$unbal_lvl2) {
      TRUE
    } else {
      FALSE
    }
  })
  unbalCont <- reactive({
    if(input$unbal_lvl2) {
      c(input$min_cl2, input$max_cl2)
    } else {
      NULL
    }
  })
  unbal3 <- reactive({
    if(input$unbal_lvl3) {
      TRUE
    } else {
      FALSE
    }
  })
  unbalCont3 <- reactive({
    if(input$unbal_lvl3) {
      c(input$min_cl3, input$max_cl3)
    } else {
      NULL
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
      input[[args]]
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
                  data_str = data_str(), unbal = unbal(), unbalCont = unbalCont(),
                  fact_vars = fact_vars()
          )
        } else {
          sim_reg(fixed(), random(), random3(), fixed_param(), random_param(), 
                  random_param3(), cov_param(), k(), n(), p(), 
                  error_var(), with_err_gen(),
                  data_str = data_str(), unbal = unbal(), unbalCont = unbalCont(),
                  unbal3 = unbal3(), unbalCont3 = unbalCont3(),
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
  
  output$gen_examp <- output$gen_examp_2 <- renderDataTable(
    if(input$update == 0 & input$update_2 == 0) {
      NULL
    } else {
      gen_code()
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
      
      params <- data.frame(Terms = as.character(terms),
                        Parameter = fixed_param(),
                        stringsAsFactors = FALSE)
      if(input$type_outcome == 1) {
        params <- rbind(params, c('Error', error_var()))
      }
      params <- cbind(params, est)
      params$diff <- as.numeric(params$Parameter) - as.numeric(params$est)
      params
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
      
      params <- data.frame(Terms = as.character(terms),
                           Parameter = fixed_param(),
                           stringsAsFactors = FALSE)
      if(input$type_model == 3) {
        rand_terms <- c(paste('rand_lvl2', 
                              unlist(strsplit(as.character(random())[2], '\\s*\\+\\s*')),
                              sep = '_'),
                        paste('rand_lvl3', 
                              unlist(strsplit(as.character(random3())[2], '\\s*\\+\\s*')),
                              sep = '_'))
        rand_params <- data.frame(Terms = rand_terms,
                                  Parameter = c(random_param()[[1]], random_param3()[[1]]),
                                  stringsAsFactors = FALSE)
      } else {
        rand_terms <- paste('rand_lvl2', 
                            unlist(strsplit(as.character(random())[2], '\\s*\\+\\s*')),
                            sep = '_')
        rand_params <- data.frame(Terms = rand_terms,
                                  Parameter = random_param()[[1]],
                                  stringsAsFactors = FALSE)
      }
      params <- rbind(params, rand_params)
      
      if(input$type_outcome == 1) {
        params <- rbind(params, c('Error', error_var()))
      }
      params <- cbind(params, est)
      params$diff <- as.numeric(params$Parameter) - as.numeric(params$est)
      params
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste('gen_data.csv', sep='') 
    },
    content = function(file) {
      write.csv(gen_code(), file, row.names = FALSE)
    }
  )
  
  output$gen_examp_code <- renderUI({
    if(input$type_model == 1) {
      str1 <- paste0('n <- ', input$samp_size_lvl1)
      str2 <- paste0('error_var <- ', input$lvl1_err)
      str3 <- paste0('with_err_gen <- ', 'rnorm')
      if(input$incl_int) {
        str4 <- paste0('fixed <- ~ 1 + ', paste(paste('cov', 1:input$number_cov, sep = '_'),
                                                collapse = ' + '))
      } else {
        str4 <- paste0('fixed <- ~ 0 + ', paste(paste('cov', 1:input$number_cov, sep = '_'),
                                                collapse = ' + '))
      }
      num_betas <- input$number_cov
      if(input$incl_int) {
        num_betas <- num_betas + 1
      }
      beta <- sapply(1:num_betas, function(i) input[[paste0('beta', i)]])
      
      num_cov <- input$number_cov
      mean_cov <- sapply(1:num_cov, function(i) input[[paste0('mean', i)]])
      sd_cov <- sapply(1:num_cov, function(i) input[[paste0('sd', i)]])
      type_cov <- sapply(1:num_cov, function(i) input[[paste0('type', i)]])

      str5 <- paste0('fixed_param <- c(', paste(beta,
                                                collapse = ', '), ')')
      str6 <- paste0('cov_param <- list(mean = c(', 
                     paste(mean_cov,
                           collapse = ','),
                     '), sd = c(',
                     paste(sd_cov,
                           collapse = ','),
                     '), var_type = c(',
                     paste(sQuote(type_cov),
                           collapse = ','),
                     '))')
      str7 <- 'temp_single <- sim_reg(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param, <br/>
      n = n, error_var = error_var, with_err_gen = with_err_gen, <br/>
      data_str = "single")'
      code(HTML(paste(str1, str2, str3, str4, str5, str6, str7, sep = '<br/>')))
    } else {
      if(input$type_model == 2) {
        str1 <- paste0('n <- ', input$samp_size_lvl2)
        str_p <- paste0('p <- ', input$samp_size_lvl1)
        str2 <- paste0('error_var <- ', input$lvl1_err)
        str3 <- paste0('with_err_gen <- ', '"rnorm"')
        if(input$type_nested == 1) {
          str_random <- 'random <- ~ 1'
          str_data <- "data_str <- 'cross'"
          ran_var <- input[['var_int']]
          str_randparam <- paste0('random_param <- list(random_var = c(',
                                  as.numeric(ran_var),
                                  '), rand_gen = ',
                                  '"rnorm"', 
                                  ')')
          if(input$incl_int) {
            str4 <- paste0('fixed <- ~ 1 + ', paste(paste('cov', 1:input$number_cov, sep = '_'),
                                                    collapse = ' + '))
          } else {
            str4 <- paste0('fixed <- ~ 0 + ', paste(paste('cov', 1:input$number_cov, sep = '_'),
                                                    collapse = ' + '))
          }
        } else {
          str_data <- "data_str <- 'long'"
          str_random <- 'random <- ~ 1 + time'
          ran_var <- sapply(c('int', 'time'), function(i) input[[paste0('var_', i)]])
          str_randparam <- paste0('random_param <- list(random_var = c(',
                                  paste(ran_var,
                                        collapse = ', '),
                                  '), rand_gen = ',
                                  '"rnorm"', 
                                  ')')
          if(input$incl_int) {
            str4 <- paste0('fixed <- ~ 1 + time + ', 
                           paste(paste('cov', 1:input$number_cov, sep = '_'),
                                 collapse = ' + '))
          } else {
            str4 <- paste0('fixed <- ~ 0 + time + ', 
                           paste(paste('cov', 1:input$number_cov, sep = '_'),
                                 collapse = ' + '))
          }
        }
        num_betas <- input$number_cov
        if(input$type_nested == 2) {
          num_betas <- num_betas + 1
        }
        if(input$incl_int) {
          num_betas <- num_betas + 1
        }
        beta <- sapply(1:num_betas, function(i) input[[paste0('beta', i)]])
        
        num_cov <- input$number_cov
        mean_cov <- sapply(1:num_cov, function(i) input[[paste0('mean', i)]])
        sd_cov <- sapply(1:num_cov, function(i) input[[paste0('sd', i)]])
        type_cov <- sapply(1:num_cov, function(i) input[[paste0('type', i)]])
        
        str5 <- paste0('fixed_param <- c(', paste(beta,
                                                  collapse = ', '), ')')
        str6 <- paste0('cov_param <- list(mean = c(', 
                       paste(mean_cov,
                             collapse = ','),
                       '), sd = c(',
                       paste(sd_cov,
                             collapse = ','),
                       '), var_type = c(',
                       paste(sQuote(type_cov),
                             collapse = ','),
                       '))')
        str7 <- 'temp_nested <- sim_reg(fixed = fixed, random = random, fixed_param = fixed_param, <br/>
      random_param = random_param, cov_param = cov_param, k = NULL, n = n, p = p, <br/>
      error_var = error_var, with_err_gen = with_err_gen, data_str = data_str, unbal = FALSE)'
        code(HTML(paste(str1, str_p, str2, str3, str4, str_random, 
                   str5, str_randparam, str6, str_data, str7, sep = '<br/>')))
      } 
      else {
        str1 <- paste0('n <- ', input$samp_size_lvl2)
        str_p <- paste0('p <- ', input$samp_size_lvl1)
        str_k <- paste0('k <- ', input$samp_size_lvl3)
        str2 <- paste0('error_var <- ', input$lvl1_err)
        str3 <- paste0('with_err_gen <- ', '"rnorm"')
        if(input$type_nested == 1) {
          str_random <- 'random <- ~ 1'
          str_data <- "data_str <- 'cross'"
          ran_var <- input[['var_int']]
          str_randparam <- paste0('random_param <- list(random_var = c(',
                                  as.numeric(ran_var),
                                  '), rand_gen = ',
                                  '"rnorm"', 
                                  ')')
          if(input$incl_int) {
            str4 <- paste0('fixed <- ~ 1 + ', paste(paste('cov', 1:input$number_cov, sep = '_'),
                                                    collapse = ' + '))
          } else {
            str4 <- paste0('fixed <- ~ 0 + ', paste(paste('cov', 1:input$number_cov, sep = '_'),
                                                    collapse = ' + '))
          }
        } else {
          str_data <- "data_str <- 'long'"
          str_random <- 'random <- ~ 1 + time'
          ran_var <- sapply(c('int', 'time'), function(i) input[[paste0('var_', i)]])
          str_randparam <- paste0('random_param <- list(random_var = c(',
                                  paste(ran_var,
                                        collapse = ', '),
                                  '), rand_gen = ',
                                  '"rnorm"', 
                                  ')')
          if(input$incl_int) {
            str4 <- paste0('fixed <- ~ 1 + time + ', 
                           paste(paste('cov', 1:input$number_cov, sep = '_'),
                                 collapse = ' + '))
          } else {
            str4 <- paste0('fixed <- ~ 0 + time + ', 
                           paste(paste('cov', 1:input$number_cov, sep = '_'),
                                 collapse = ' + '))
          }
        }
        num_betas <- input$number_cov
        if(input$type_nested == 2) {
          num_betas <- num_betas + 1
        }
        if(input$incl_int) {
          num_betas <- num_betas + 1
        }
        beta <- sapply(1:num_betas, function(i) input[[paste0('beta', i)]])
        
        num_cov <- input$number_cov
        mean_cov <- sapply(1:num_cov, function(i) input[[paste0('mean', i)]])
        sd_cov <- sapply(1:num_cov, function(i) input[[paste0('sd', i)]])
        type_cov <- sapply(1:num_cov, function(i) input[[paste0('type', i)]])
        
        str5 <- paste0('fixed_param <- c(', paste(beta,
                                                  collapse = ', '), ')')
        str6 <- paste0('cov_param <- list(mean = c(', 
                       paste(mean_cov,
                             collapse = ','),
                       '), sd = c(',
                       paste(sd_cov,
                             collapse = ','),
                       '), var_type = c(',
                       paste(sQuote(type_cov),
                             collapse = ','),
                       '))')
        str_random3 <- 'random3 <- ~ 1'
        str_randparam3 <- paste0('random_param3 <- list(random_var = c(',
                                 input$lvl3_err, '), rand_gen = "rnorm")')
        str7 <- 'temp_nested <- sim_reg(fixed, random, random3, fixed_param, random_param, <br/>
                random_param3, cov_param, k,n, p, error_var, with_err_gen, <br/>
                data_str = data_str)'
        code(HTML(paste(str1, str_p, str_k, str2, str3, str4, str_random, str_random3,
                   str5, str_randparam, str_randparam3, str6, str_data, str7, 
                   sep = '<br/>')))
      }
      
    }
    
  })
  
  output$vars <- renderUI({
    req(gen_code)
    variables <- names(gen_code())
    selectizeInput('hist_vars', 'Plot Variable', choices = variables)
  })
  
  output$hists <- renderPlot({
    histograms(gen_code(), input$hist_vars, input$binwidth)
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
    
    if(input$type_model == 1) {
      sim_pow(fixed = fixed(), fixed_param = fixed_param(), cov_param = cov_param(),
              n = n(), error_var = error_var(), with_err_gen = with_err_gen(), 
              data_str = data_str(), pow_param = pow_param, alpha = alpha,
              pow_dist = pow_dist, pow_tail = pow_tail, replicates = replicates)
    } else {
      if(input$type_model == 2) {
        sim_pow(fixed = fixed(), random = random(), fixed_param = fixed_param(),
                random_param = random_param(), cov_param = cov_param(), k = NULL,
                n = n(), p = p(), error_var = error_var(), with_err_gen = with_err_gen(),
                data_str = data_str(), unbal = unbal(), unbalCont = unbalCont(),
                pow_param = pow_param, alpha = alpha, pow_dist = pow_dist, 
                pow_tail = pow_tail, replicates = replicates
                )
      } else {
        
      }
    }
  })
  
  output$power_table <- renderDataTable({
    datatable(power_sim())
  })
  
}
