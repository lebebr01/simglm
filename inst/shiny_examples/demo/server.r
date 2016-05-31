library(shiny)
library(shinydashboard)
library(simglm)

server <- function(input, output, session) {
  
  gen_code <- reactive({
    n <- input$samp_size_lvl1
    error_var <- input$lvl1_err
    with_err_gen <- input$lvl1_err_dist
    fixed <- paste('cov', 1:input$number_cov, sep = '_')
    fixed_param <- as.numeric(unlist(strsplit(input$beta, split = ',')))
  })
  
  output$gen_examp_code <- renderUI({
    str1 <- paste0('n <- ', input$samp_size_lvl1)
    str2 <- paste0('error_var <- ', input$lvl1_err)
    str3 <- paste0('with_err_gen <- ', input$lvl1_err_dist)
    str4 <- paste0('fixed <- ', paste('cov', 1:input$number_cov, sep = '_'))
    str5 <- paste0('fixed_param <- ', as.numeric(unlist(strsplit(input$beta, split = ','))))
    str6 <- paste0('cov_param <- list(mean = ', 
                   as.numeric(unlist(strsplit(input$mean_cov, split = ','))),
                   'sd = ',
                   as.numeric(unlist(strsplit(input$sd_cov, split = ','))),
                   'var_type = ',
                   as.numeric(unlist(strsplit(input$type_cov, split = ','))),
                   ')')
    HTML(paste(str1, str2, str3, str4, str5, str6, sep = '<br/>'))
  })
  
}
