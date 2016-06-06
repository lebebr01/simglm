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
              h5('Specify basic model simulation parameters here. Optional simulation arguments are found in the second tab.'),
              fluidRow(
                box(title = 'Model', status = 'primary',
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
              h2('Optional Arguments'),
              h5('Specify required simulation arguments from first tab prior to entering optional arguments.'),
              fluidRow(
                box(width = 2, collapsible = TRUE, collapsed = FALSE,
                    title = 'Outcome', status = 'warning',
                    radioButtons('type_outcome', 'Type of Outcome:',
                                 choices = c('Continuous' = 1,
                                             'Binary' = 2),
                                 selected = 1)
                ),
                box(width = 3, collapsible = TRUE, collapsed = FALSE,
                    title = 'Unbalanced', status = 'warning',
                    checkboxInput('unbal_lvl2', 'Unbalanced Lvl 2 Clusters?',
                                  value = FALSE),
                    conditionalPanel(
                      condition = 'input.unbal_lvl2 == true',
                      div(style = 'display:inline-block', 
                          numericInput('min_cl2', 'Min Clust. Lvl 2', value = 2, width = '100px')),
                      div(style = 'display:inline-block', 
                          numericInput('max_cl2', 'Max Clust. Lvl 2', value = 5, width = '100px'))
                    ),
                    checkboxInput('unbal_lvl3', 'Unbalanced Lvl 3 Clusters?',
                                  value = FALSE),
                    conditionalPanel(
                      condition = 'input.unbal_lvl3 == true',
                      div(style = 'display:inline-block', 
                          numericInput('min_cl3', 'Min Clust. Lvl 3', value = 2, width = '100px')),
                      div(style = 'display:inline-block', 
                          numericInput('max_cl3', 'Max Clust. Lvl 3', value = 5, width = '100px'))
                    )
                ),
                box(width = 3, collapsible = TRUE, collapsed = FALSE,
                    title = 'Missing Data', status = 'warning',
                    checkboxInput('missing', 'Simulate Missing Data?',
                                  value = FALSE),
                    conditionalPanel(
                      condition = 'input.missing == true',
                      radioButtons('type_missing', "Type of Missing:",
                                   choices = c('MCAR' = 1,
                                               'MAR' = 2,
                                               'Dropout' = 3),
                                   selected = 1)
                    )
                ),
                box(width = 3, collapsible = TRUE, collapsed = FALSE,
                    title = 'Serial Correlation', status = 'warning',
                    checkboxInput('sc', 'Simulated Serial Correlation?',
                                  value = FALSE),
                    conditionalPanel(
                      condition = 'input.sc == true',
                      radioButtons('type_sc', 'Type of Serial Correlation:',
                                   choices = c(''))
                    )
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
              h2('View Data from Generate Tab'),
              h5('Specify simulation arguments from first two tabs prior to viewing data here.'),
              box(width = 12, title = 'Generated Example Data',
                  collapsible = TRUE, collapsed = FALSE,
                  status = 'success',
                  dataTableOutput('gen_examp_2')
              )
              ),
      tabItem(tabName = 'verify',
              h2('Verify Data Simulation'),
              h5('Specify simulation arguments from first two tabs prior to verifying data here.')
              
      ),
      tabItem(tabName = 'power',
              h2('Power Analysis'),
              h5('Specify model simulation arguments in the preceding tabs prior to power simulation.'),
              fluidRow(
                box(width = 3, title = 'Distribution', 
                    collapsible = TRUE, collapsed = FALSE,
                    status = 'primary',
                    radioButtons('type_dist', 'Type of Distribution:',
                                 choices = c('t' = 1,
                                             'Normal' = 2),
                                 selected = 1)
                ),
                box(width = 3, title = 'One or Two Tailed',
                    collapsible = TRUE, collapsed = FALSE,
                    status = 'primary',
                    radioButtons('tails', 'Number of Tails:',
                                 choices = c('One' = 1,
                                             'Two' = 2),
                                 select = 2)
                ),
                box(width = 3, title = 'Alpha',
                    collapsible = TRUE, collapsed = FALSE,
                    status = 'primary',
                    numericInput('alpha', 'Set Alpha Value:',
                                 value = .05, 
                                 min = 0, max = 1)
                  
                ),
                box(
                  width = 3, title = 'Replications',
                  collapsible = TRUE, collapsed = FALSE,
                  status = 'primary',
                  numericInput('repl', 'Number of Replications:',
                               value = 10)
                )
              ),
              fluidRow(
                actionButton('update_power', "Simulate Power", icon("refresh"))
              ),
              fluidRow(
                box(
                  width = 12, title = 'Power Table',
                  collapsible = TRUE, collapsed = FALSE,
                  status = 'success',
                  dataTableOutput('power_table')
                )
              ),
              fluidRow(
                box(
                  width = 12, title = 'Power Code',
                  collapsible = TRUE, collapsed = FALSE,
                  status = 'info',
                  htmlOutput('power_code')
                )
              )
      )
    )
  )
)
