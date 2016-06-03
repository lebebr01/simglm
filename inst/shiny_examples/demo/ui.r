library(shinydashboard)
library(DT)

ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "simglm Demo"),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Introduction', tabName = 'intro', icon = icon('info-circle')),
      menuItem('Optional Arguments', tabName = 'generate', icon = icon('plus-circle')),
      menuItem('View Data', tabName = 'view_data', icon = icon('table')),
      menuItem('Verify Data Sim', tabName = 'verify', icon = icon('check-circle')),
      menuItem('Power', tabName = 'power', icon = icon('bolt'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'intro',
              h2('Introduction to simglm'),
              fluidRow(
                box(title = 'Select Model', status = 'primary',
                    collapsible = TRUE, width = 2, collapsed = FALSE,
                    radioButtons('type_model', 'Type of Model:',
                                 choices = c('Single Level' = 1, 
                                             'Two-Level' = 2,
                                             'Three-Level' = 3),
                                 selected = 1),
                    conditionalPanel(
                      condition = 'input.type_model == 2 || input.type_model == 3',
                      radioButtons('type_nested', 'Type of Nesting:',
                                   choices = c('Cross-Sectional' = 1,
                                               'Longitudinal' = 2),
                                   selected = 1)
                    )
                ),
                box(title = 'Sample Sizes', status = 'primary',
                    collapsible = TRUE, width = 2, collapsed = FALSE,
                    numericInput('samp_size_lvl1', 'Sample Size Level 1', 
                                 value = 4),
                    conditionalPanel(
                      condition = 'input.type_model == 2 || input.type_model == 3',
                      numericInput('samp_size_lvl2', 'Sample Size Level 2',
                                   value = 2)
                    ),
                    conditionalPanel(
                      condition = 'input.type_model == 3',
                      numericInput('samp_size_lvl3', 'Sample Size Level 3',
                                   value = 2)
                    )
                    ),
                box(title = 'Random Errors', width = 2, collapsed = FALSE,
                    collapsible = TRUE, status = 'primary',
                    numericInput('lvl1_err', 'Level 1 Error Variance',
                              value = 5),
                    # textInput('lvl1_err_dist', 'Level 1 Error Dist',
                    #           value = 'rnorm'),
                    # uiOutput('lvl1_err_misc'),
                    # textInput('lvl1_err_misc', 'Level 1 Dist Params (separate by ";")',
                    #           value = ''),
                    conditionalPanel(
                      condition = 'input.type_model == 2 || input.type_model == 3',
                      uiOutput('lvl2_err')
                      # textInput('lvl2_err_dist', 'Level 2 Error Dist',
                      #           value = 'rnorm')
                      # textInput('lvl2_err_misc', 'Level 2 Dist Params (separate by ";")',
                      #           value = '')
                    ),
                    conditionalPanel(
                      condition = 'input.type_model == 3',
                      numericInput('lvl3_err', 'Level 3 Error Variance',
                                value = 1)
                    )
                    ),
                box(title = 'Covariate Details', width = 6, collapsed = FALSE,
                    collapsible = TRUE, status = 'primary',
                    checkboxInput('incl_int', 'Include Intercept?', TRUE),
                    numericInput('number_cov', 'Number of Covariates',
                                 value = 3, width = '300px'),
                    uiOutput('beta'),
                    uiOutput('mean_cov'),
                    uiOutput('sd_cov'),
                    uiOutput('type_cov')
                    # textInput('beta', 'Regression Coefficients',
                    #           value = '1, 1, 1, 1'),
                    # textInput('mean_cov', 'Mean of Covariate(s)',
                    #              value = '0, 5, 2'),
                    # textInput('sd_cov', 'SD of Covariate(s)',
                    #           value = '2, 4, 3'),
                    # textInput('type_cov', 'Level of Covariate(s)',
                    #           value = 'single, single, single')
                    )
              ),
              fluidRow(
                actionButton('update', "Update Simulation", icon("refresh"))
              ),
              fluidRow(
                box(width = 12, title = 'Generated Example Data',
                    collapsible = TRUE, collapsed = FALSE,
                    status = 'success',
                    dataTableOutput('gen_examp')
                )
              ),
              fluidRow(
                box(
                  width = 12, title = 'Generated Example Code',
                  collapsible = TRUE, collapsed = FALSE,
                  status = 'info',
                  htmlOutput('gen_examp_code')
                )
              )
              ),
      tabItem(tabName = 'generate',
              h2('Generate Data with simglm'),
              fluidRow(
                box(width = 3, collapsible = TRUE, collapsed = FALSE,
                    title = 'Select Model', status = 'primary',
                    radioButtons('type_model_gen', 'Model:',
                                 choices = c('Single Level' = 1, 
                                             'Two-Level' = 2,
                                             'Three-Level' = 3),
                                 selected = 1),
                    conditionalPanel(
                      condition = 'input.type_model_gen == 2 || input.type_model_gen == 3',
                      radioButtons('type_nested', 'Type of Nesting:',
                                   choices = c('Cross-Sectional' = 1,
                                               'Longitudinal' = 2),
                                   selected = 1)
                    )
                ),
                box(width = 3, collapsible = TRUE, collapsed = FALSE,
                    title = 'Outcome', status = 'primary',
                    radioButtons('type_outcome', 'Type of Outcome:',
                                 choices = c('Continuous' = 1,
                                             'Binary' = 2),
                                 selected = 1)
                )
              ),
              fluidRow(
                box(width = 12, collapsible = TRUE, collapsed = FALSE,
                  title = 'Required Arguments', status = 'primary'
                )
              ),
              fluidRow(
                box(width = 12, collapsible = TRUE, collapsed = FALSE,
                    title = 'Optional Arguments', status = 'warning',
                    checkboxInput('show_optional', 'Show Optional Arguments?',
                                  value = FALSE)
                    # conditionalPanel(
                    #   condition = 'input.show_optional = TRUE',
                    #   textInput()
                    # )
                )
              ),
              fluidRow(
                actionButton('update_2', "Simulate Data", icon("refresh"))
              ),
              fluidRow(
                box(
                  width = 12, title = 'Generated Example Code',
                  collapsible = TRUE, collapsed = FALSE,
                  status = 'info',
                  htmlOutput('gen_examp_code_2')
                )
              )
              ),
      tabItem(tabName = 'view_data',
              h2('View Data from Generate Tab')
              ),
      tabItem(tabName = 'verify',
              h2('Verify Data Simulation')
              
      ),
      tabItem(tabName = 'power',
              h2('Power Analysis')
              )
    )
  )
)
