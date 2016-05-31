library(shinydashboard)
library(DT)

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "simglm Demo"),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Introduction', tabName = 'intro', icon = icon('info-circle')),
      menuItem('Generate', tabName = 'generate', icon = icon('table')),
      menuItem('View Data', tabName = 'view_data', icon = icon('table')),
      menuItem('Power', tabName = 'power', icon = icon('bolt'))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'intro',
              h2('Introduction to simglm'),
              fluidRow(
                box(title = 'Select Model', 
                    collapsible = TRUE, width = 3, collapsed = FALSE,
                    radioButtons('type_model', 'Type of Model:',
                                 choices = c('Single Level' = 1, 
                                             'Two-Level' = 2, 
                                             'Three-Level' = 3),
                                 selected = 1)
                ),
                box(title = 'Sample Sizes', 
                    collapsible = TRUE, width = 3, collapsed = FALSE,
                    numericInput('samp_size_lvl1', 'Sample Size Level 1', 
                                 value = 10),
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
                box(title = 'Random Errors', width = 3, collapsed = FALSE,
                    collapsible = TRUE,
                    numericInput('lvl1_err', 'Level 1 Error Variance',
                              value = 5),
                    textInput('lvl1_err_dist', 'Level 1 Error Dist',
                              value = 'rnorm'),
                    conditionalPanel(
                      condition = 'input.type_model ==2 || input.type_model == 3',
                      numericInput('lvl2_err', 'Level 2 Error Variance',
                                   value = 3),
                      textInput('lvl2_err_dist', 'Level 2 Error Dist',
                                value = 'rnorm')
                    ),
                    conditionalPanel(
                      condition = 'input.type_model == 3',
                      numericInput('lvl3_err', 'Level 3 Error Variance',
                                   value = 3),
                      textInput('lvl3_err_dist', 'Level 3 Error Dist',
                                value = 'rnorm')
                    )
                    ),
                box(title = 'Covariate Details', width = 3, collapsed = FALSE,
                    collapsible = TRUE,
                    checkboxInput('incl_int', 'Include Intercept?', TRUE),
                    numericInput('number_cov', 'Number of Covariates',
                                 value = 3),
                    textInput('beta', 'Regression Coefficients',
                              value = '1, 1, 1'),
                    textInput('mean_cov', 'Mean of Covariate(s)',
                                 value = '0, 5, 2'),
                    textInput('sd_cov', 'SD of Covariate(s)',
                              value = '2, 4, 3'),
                    textInput('type_cov', 'Level of Covariate(s)',
                              value = 'single, single, single')
                    )
              ),
              fluidRow(
                submitButton("Update Data", icon("refresh"))
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
      tabItem(tabName = 'power',
              h2('Power Analysis')
              )
    )
  )
)
