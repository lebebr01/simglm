library(shiny)
library(shinydashboard)
library(simglm)

options(useFancyQuotes = FALSE)

server <- function(input, output, session) {
  
  gen_code <- reactive({
    if(input$type_model == 1) {
      n <- input$samp_size_lvl1
      error_var <- input$lvl1_err
      with_err_gen <- input$lvl1_err_dist
      if(input$incl_int) {
        fixed <- as.formula(paste0('~ 1 + ', 
                                   paste(paste('cov', 1:input$number_cov, sep = '_'), 
                                         collapse = ' + ')))
      } else {
        fixed <- as.formula(paste0('~ 0 + ', 
                                   paste(paste('cov', 1:input$number_cov, sep = '_'), 
                                         collapse = ' + ')))
      }
      fixed_param <- c(as.numeric(unlist(strsplit(input$beta, split = ','))))
      cov_param <- list(mean = c(as.numeric(unlist(strsplit(input$mean_cov, split = ',')))),
                        sd = c(as.numeric(unlist(strsplit(input$sd_cov, split = ',')))),
                        var_type = c(gsub('^\\s+|\\s+$', '', 
                                          unlist(strsplit(input$type_cov, split = ',')))))
      if(input$lvl1_err_misc != '') {
        opt_args <- list(eval(parse(text = input$lvl1_err_misc)))
        sim_reg(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param,
                n = n, error_var = error_var, with_err_gen = with_err_gen,
                data_str = "single", opt_args)
      } else {
        sim_reg(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param,
                n = n, error_var = error_var, with_err_gen = with_err_gen,
                data_str = "single")
      }
    } else {
      p <- input$samp_size_lvl1
      n <- input$samp_size_lvl2
      error_var <- input$lvl1_err
      with_err_gen <- input$lvl1_err_dist
      if(input$type_nested == 1) {
        random <- ~ 1
        data_str <- 'cross'
        random_param <- list(random_var = c(as.numeric(input$lvl2_err)),
                             rand_gen = input$lvl2_err_dist)
        if(input$incl_int) {
          fixed <- as.formula(paste0('~ 1 + ', 
                                     paste(paste('cov', 1:input$number_cov, sep = '_'), 
                                           collapse = ' + ')))
        } else {
          fixed <- as.formula(paste0('~ 0 + ', 
                                     paste(paste('cov', 1:input$number_cov, sep = '_'), 
                                           collapse = ' + ')))
        }
      } else {
        data_str <- 'long'
        random <- ~ 1 + time
        random_param <- list(random_var = c(as.numeric(unlist(strsplit(input$lvl2_err, split = ',')))),
                             rand_gen = input$lvl2_err_dist)
        if(input$incl_int) {
          fixed <- as.formula(paste0('~ 1 + time + ', 
                                     paste(paste('cov', 1:input$number_cov, sep = '_'), 
                                           collapse = ' + ')))
        } else {
          fixed <- as.formula(paste0('~ 0 + time + ', 
                                     paste(paste('cov', 1:input$number_cov, sep = '_'), 
                                           collapse = ' + ')))
        }
      }
      
      fixed_param <- c(as.numeric(unlist(strsplit(input$beta, split = ','))))
      cov_param <- list(mean = c(as.numeric(unlist(strsplit(input$mean_cov, split = ',')))),
                        sd = c(as.numeric(unlist(strsplit(input$sd_cov, split = ',')))),
                        var_type = c(gsub('^\\s+|\\s+$', '', 
                                          unlist(strsplit(input$type_cov, split = ',')))))
      if(input$lvl1_err_misc != '') {
        # opt_args <- list(eval(parse(text = input$lvl1_err_misc)))
        sim_reg(fixed = fixed, random = random, 
                fixed_param = fixed_param, 
                random_param = random_param, cov_param = cov_param, 
                k = NULL, n = n, p = p,
                error_var = error_var, with_err_gen = with_err_gen,
                data_str = data_str, unbal = FALSE)
      } else {
        sim_reg(fixed = fixed, random = random, 
                fixed_param = fixed_param, 
                random_param = random_param, cov_param = cov_param, 
                k = NULL, n = n, p = p,
                error_var = error_var, with_err_gen = with_err_gen,
                data_str = data_str, unbal = FALSE)
      }
    }
    
  })
  
  output$gen_examp <- renderDataTable({
    datatable(gen_code())
  })
  
  output$gen_examp_code <- renderUI({
    if(input$type_model == 1) {
      str1 <- paste0('n <- ', input$samp_size_lvl1)
      str2 <- paste0('error_var <- ', input$lvl1_err)
      str3 <- paste0('with_err_gen <- ', input$lvl1_err_dist)
      if(input$incl_int) {
        str4 <- paste0('fixed <- ~ 1 + ', paste(paste('cov', 1:input$number_cov, sep = '_'),
                                                collapse = ' + '))
      } else {
        str4 <- paste0('fixed <- ~ 0 + ', paste(paste('cov', 1:input$number_cov, sep = '_'),
                                                collapse = ' + '))
      }
      str5 <- paste0('fixed_param <- c(', paste(as.numeric(unlist(strsplit(input$beta, split = ','))),
                                                collapse = ', '), ')')
      str6 <- paste0('cov_param <- list(mean = c(', 
                     paste(as.numeric(unlist(strsplit(input$mean_cov, split = ','))),
                           collapse = ','),
                     '), sd = c(',
                     paste(as.numeric(unlist(strsplit(input$sd_cov, split = ','))),
                           collapse = ','),
                     '), var_type = c(',
                     paste(sQuote(gsub('^\\s+|\\s+$', '', 
                                       unlist(strsplit(input$type_cov, split = ',')))),
                           collapse = ','),
                     '))')
      str7 <- 'temp_single <- sim_reg(fixed = fixed, fixed_param = fixed_param, cov_param = cov_param, <br/>
      n = n, error_var = error_var, with_err_gen = with_err_gen, <br/>
      data_str = "single")'
      HTML(paste(str1, str2, str3, str4, str5, str6, str7, sep = '<br/>'))
    } else {
      str1 <- paste0('n <- ', input$samp_size_lvl2)
      str_p <- paste0('p <- ', input$samp_size_lvl1)
      str2 <- paste0('error_var <- ', input$lvl1_err)
      str3 <- paste0('with_err_gen <- ', input$lvl1_err_dist)
      if(input$type_nested == 1) {
        str_random <- 'random <- ~ 1'
        str_data <- "data_str <- 'cross'"
        str_randparam <- paste0('random_param <- list(random_var = c(',
                                as.numeric(input$lvl2_err),
                                '), rand_gen = ',
                             sQuote(input$lvl2_err_dist), 
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
        str_randparam <- paste0('random_param <- list(random_var = c(',
                                paste(as.numeric(unlist(strsplit(input$lvl2_err, split = ','))),
                                      collapse = ', '),
                             '), rand_gen = ',
                             sQuote(input$lvl2_err_dist), 
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
      str5 <- paste0('fixed_param <- c(', paste(as.numeric(unlist(strsplit(input$beta, split = ','))),
                                                collapse = ', '), ')')
      str6 <- paste0('cov_param <- list(mean = c(', 
                     paste(as.numeric(unlist(strsplit(input$mean_cov, split = ','))),
                           collapse = ','),
                     '), sd = c(',
                     paste(as.numeric(unlist(strsplit(input$sd_cov, split = ','))),
                           collapse = ','),
                     '), var_type = c(',
                     paste(sQuote(gsub('^\\s+|\\s+$', '', 
                                       unlist(strsplit(input$type_cov, split = ',')))),
                           collapse = ','),
                     '))')
      str7 <- 'temp_nested <- sim_reg(fixed = fixed, random = random, fixed_param = fixed_param, <br/>
      random_param = random_param, cov_param = cov_param, k = NULL, n = n, p = p, <br/>
      error_var = error_var, with_err_gen = with_err_gen, data_str = data_str, unbal = FALSE)'
      HTML(paste(str1, str_p, str2, str3, str4, str_random, 
                 str5, str_randparam, str6, str_data, str7, sep = '<br/>'))
    }
    
  })
  
}
