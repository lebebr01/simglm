library(shinydashboard)
library(DT)

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "simglm Demo"),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Introduction', tabName = 'intro', icon = icon('info-circle')),
      menuItem('Generate', tabName = 'generate', icon = icon('table')),
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
                box(title = 'Select Model', 
                    collapsible = TRUE, width = 1, collapsed = FALSE,
                    radioButtons('type_model', 'Type of Model:',
                                 choices = c('Single Level' = 1, 
                                             'Two-Level' = 2),
                                 selected = 1),
                    conditionalPanel(
                      condition = 'input.type_model == 2',
                      radioButtons('type_nested', 'Type of Nesting:',
                                   choices = c('Cross-Sectional' = 1,
                                               'Longitudinal' = 2),
                                   selected = 1)
                    )
                ),
                box(title = 'Sample Sizes', 
                    collapsible = TRUE, width = 1, collapsed = FALSE,
                    numericInput('samp_size_lvl1', 'Sample Size Level 1', 
                                 value = 10),
                    conditionalPanel(
                      condition = 'input.type_model == 2',
                      numericInput('samp_size_lvl2', 'Sample Size Level 2',
                                   value = 2)
                    )
                    ),
                box(title = 'Random Errors', width = 2, collapsed = FALSE,
                    collapsible = TRUE,
                    numericInput('lvl1_err', 'Level 1 Error Variance',
                              value = 5),
                    textInput('lvl1_err_dist', 'Level 1 Error Dist',
                              value = 'rnorm'),
                    textInput('lvl1_err_misc', 'Level 1 Dist Params (separate by ";")',
                              value = ''),
                    conditionalPanel(
                      condition = 'input.type_model == 2',
                      textInput('lvl2_err', 'Level 2 Error Variance',
                                   value = '3'),
                      textInput('lvl2_err_dist', 'Level 2 Error Dist',
                                value = 'rnorm'),
                      textInput('lvl2_err_misc', 'Level 2 Dist Params (separate by ";")',
                                value = '')
                    )
                    ),
                box(title = 'Covariate Details', width = 8, collapsed = FALSE,
                    collapsible = TRUE,
                    checkboxInput('incl_int', 'Include Intercept?', TRUE),
                    numericInput('number_cov', 'Number of Covariates',
                                 value = 3),
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
                box(width = 12,
                    h3('Generated Example Data'),
                    dataTableOutput('gen_examp')
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  h3('Generated Example Code'),
                  htmlOutput('gen_examp_code')
                )
              )
              ),
      tabItem(tabName = 'generate',
              h2('Generate Data with simglm')
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
