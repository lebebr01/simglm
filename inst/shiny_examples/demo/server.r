library(shiny)
library(shinydashboard)
library(simglm)

options(useFancyQuotes = FALSE)

server <- function(input, output, session) {
  
  gen_code <- reactive({
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
  })
  
  output$gen_examp <- renderDataTable({
    datatable(gen_code())
  })
  
  output$gen_examp_code <- renderUI({
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
  })
  
}
