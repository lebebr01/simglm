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
                    conditionalPanel(
                      condition = 'input.type_outcome == 1',
                    numericInput('lvl1_err', 'Level 1 Error Variance',
                                 value = 5)
                    ),
                    conditionalPanel(
                      condition = 'input.type_model == 2 || input.type_model == 3',
                      uiOutput('lvl2_err')
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
                    p('Note: Number of covariates includes both continuous and discrete.'),
                    uiOutput('beta'),
                    uiOutput('mean_cov'),
                    uiOutput('sd_cov'),
                    uiOutput('type_cov'),
                    p('Note: If discrete variables, the number of covariates will
                      not match the mean, sds, and type above. See optional tab.')
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
                box(width = 2, collapsible = TRUE, collapsed = FALSE,
                    title = 'Unbalanced', status = 'warning',
                    checkboxInput('unbal_lvl2', 'Unbalanced Lvl 2 Clusters?',
                                  value = FALSE),
                    conditionalPanel(
                      condition = 'input.unbal_lvl2 == true',
                      div(style = 'display:inline-block', 
                          numericInput('min_cl2', 'Min Units Lvl 2', value = 2, width = '75px')),
                      div(style = 'display:inline-block', 
                          numericInput('max_cl2', 'Max Units Lvl 2', value = 5, width = '75px'))
                    ),
                    checkboxInput('unbal_lvl3', 'Unbalanced Lvl 3 Clusters?',
                                  value = FALSE),
                    conditionalPanel(
                      condition = 'input.unbal_lvl3 == true',
                      div(style = 'display:inline-block', 
                          numericInput('min_cl3', 'Min Units Lvl 3', value = 2, width = '75px')),
                      div(style = 'display:inline-block', 
                          numericInput('max_cl3', 'Max Units Lvl 3', value = 5, width = '75px'))
                    )
                ),
                box(width = 2, collapsible = TRUE, collapsed = FALSE,
                    title = 'Missing Data', status = 'warning',
                    checkboxInput('missing', 'Simulate Missing Data?',
                                  value = FALSE),
                    conditionalPanel(
                      condition = 'input.missing == true',
                      radioButtons('type_missing', "Type of Missing:",
                                   choices = c('MCAR' = 1,
                                               'MAR' = 2,
                                               'Dropout' = 3),
                                   selected = 1),
                      p('Note: To use dropout missing data must have a longitudinal design.'),
                      conditionalPanel(
                        condition = 'input.type_missing == 2',
                        uiOutput('select_missing_cov')
                      ),
                      numericInput('miss_prop', 'Missing proportion',
                                   value = 0.2, min = 0, max = 1),
                      p('Note: Missing proportion must be between 0 and 1.')
                    )
                ),
                box(width = 2, collapsible = TRUE, collapsed = FALSE,
                    title = 'Discrete Covariates', status = 'warning',
                    checkboxInput('dis_cov', 'Discrete Covariates?',
                                  value = FALSE),
                    conditionalPanel(
                      condition = 'input.dis_cov == true',
                      numericInput('num_discrete', 'Number of Discrete Covariates',
                                   value = 0)
                    ),
                    conditionalPanel(
                      condition = 'input.num_discrete > 0',
                      uiOutput('num_levels'),
                      uiOutput('var_type'),
                      p('Note: Need to add .f, .c, or .o in covariate names for discrete 
                        covariate simulation.')
                    )
                ),
                # box(width = 2, collapsible = TRUE, collapsed = FALSE,
                #     title = 'Serial Correlation', status = 'warning',
                #     checkboxInput('sc', 'Serial Correlation?',
                #                   value = FALSE),
                #     conditionalPanel(
                #       condition = 'input.sc == true',
                #       radioButtons('type_sc', 'Type of Serial Correlation:',
                #                    choices = c(''))
                #     )
                # ),
                box(width = 2, collapsible = TRUE, collapsed = FALSE,
                    title = 'Covariate Misc', status = 'warning',
                    checkboxInput('change_name', 'Specify Covariate Name?',
                                  value = FALSE),
                    conditionalPanel(
                      condition = 'input.change_name == true',
                      uiOutput('change_cov')
                    )
                    ),
                box(
                  width = 2, collapsible = TRUE, collapsed = FALSE,
                  title = 'Random Error Dist', status = 'warning',
                  checkboxInput('change_error_dist', 'Change Random Dist?',
                                value = FALSE),
                  conditionalPanel(
                    condition = 'input.change_error_dist == true',
                    textInput('lvl1_err_dist', 'Level 1 Error Dist',
                              value = 'rnorm', width = '75px'),
                    uiOutput('lvl1_err_misc')
                  ),
                  conditionalPanel(
                    condition = 'input.type_model == 2 || input.type_model == 3',
                    textInput('lvl2_err_dist', 'Level 2 Error Dist',
                              value = 'rnorm', width = '75px'),
                    uiOutput('lvl2_err_misc')
                  ),
                  conditionalPanel(
                    condition = 'input.type_model == 3',
                    textInput('lvl3_err_dist', 'Level 3 Error Dist',
                              value = 'rnorm', width = '75px'),
                    uiOutput('lvl3_err_misc')
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
              ),
              box(
                width = 12, collapsible = TRUE, collapsed = FALSE,
                status = 'warning',
                downloadButton('downloadData', 'Download Generated Data'),
                h5('Clicking the download button with download the current 
                   data generated in the table above.')
                )
      ),
      tabItem(tabName = 'verify',
              h2('Verify Data Simulation'),
              h5('Specify simulation arguments from first two tabs prior to verifying data here.'),
              fluidRow(
                box(
                  width = 3, title = 'Variables for Histograms',
                  collapsible = TRUE, collapsed = FALSE,
                  status = 'primary',
                  uiOutput('vars'),
                  numericInput('binwidth', 'Select Binwidth', 
                               value = 2)
                ),
                box(
                  width = 9, title = 'Histograms',
                  collapsible = TRUE, collapsed = FALSE,
                  status = 'info',
                  plotOutput('hists')
                )
              ),
              fluidRow(
                box(width = 6, title = 'Model Verification',
                    collapsible = TRUE, collapsed = FALSE,
                    status = 'info',
                    checkboxInput('verify_model', 'Model Results?',
                                  value = FALSE),
                    conditionalPanel(
                      condition = 'input.verify_model == true',
                      verbatimTextOutput('model_results')
                    )
                    ),
                conditionalPanel(
                  condition = 'input.verify_model == true',
                  box(
                    width = 6, title = 'Difference in Parameter and Estimates',
                    collapsible = TRUE, collapsed = FALSE,
                    status = 'info',
                    checkboxInput('est_diff', 'Parameter - Estimate',
                                  value = FALSE),
                    conditionalPanel(
                      condition = 'input.est_diff == true',
                      dataTableOutput('param_diff')
                    )
                  )
                )
              ),
              fluidRow(
                box(width = 12, title = 'Missing Data Verification',
                    collapsible = TRUE, collapsed = FALSE,
                    status = 'info', 
                    checkboxInput('verify_missing', 'Verify Missing Data?',
                                  value = FALSE),
                    conditionalPanel(
                      condition = 'input.verify_missing == TRUE',
                      tableOutput('miss_data')
                    )
                )
              )
      ),
      tabItem(tabName = 'power',
              h2('Power Analysis'),
              h5('Specify model simulation arguments in the preceding tabs prior to power simulation.'),
              fluidRow(
                box(width = 2, title = 'Distribution', 
                    collapsible = TRUE, collapsed = FALSE,
                    status = 'primary',
                    radioButtons('type_dist', 'Type of Distribution:',
                                 choices = c('t' = 1,
                                             'Normal' = 2),
                                 selected = 1)
                ),
                box(width = 2, title = 'One or Two Tailed',
                    collapsible = TRUE, collapsed = FALSE,
                    status = 'primary',
                    radioButtons('tails', 'Number of Tails:',
                                 choices = c('One' = 1,
                                             'Two' = 2),
                                 select = 2)
                ),
                box(width = 2, title = 'Alpha',
                    collapsible = TRUE, collapsed = FALSE,
                    status = 'primary',
                    numericInput('alpha', 'Set Alpha Value:',
                                 value = .05, 
                                 min = 0, max = 1)
                  
                ),
                box(
                  width = 2, title = 'Replications',
                  collapsible = TRUE, collapsed = FALSE,
                  status = 'primary',
                  numericInput('repl', 'Number of Replications:',
                               value = 10)
                ),
                box(
                  width = 2, title = 'Vary Arguments',
                  collapsible = TRUE, collapsed = FALSE,
                  status = 'primary',
                  uiOutput('vary_arg')
                ),
                box(width = 2, title = 'Vary Arguments Options',
                    collapsible = TRUE, collapsed = FALSE,
                    status = 'primary',
                    uiOutput('vary_arg_vals'),
                    h5('Note: Separate specific values with commas.')
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
              # fluidRow(
              #   box(
              #     width = 12, title = 'Power Graphics',
              #     collapsible = TRUE, collapsed = FALSE,
              #     status = 'warning',
              #     fluidRow(
              #       box(width = 3,
              #           uiOutput('power_x')
              #       ),
              #       box(width = 3,
              #           uiOutput('power_group')
              #       ),
              #       box(width = 3,
              #           uiOutput('power_facet')
              #       )
              #     ),
              #     plotOutput('power_plot_out')
              #   )
              # ),
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
