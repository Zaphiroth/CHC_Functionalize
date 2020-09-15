# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  CHC Functionalize
# Purpose:      UI
# programmer:   Zhe Liu
# Date:         2020-07-21
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


ui <- dashboardPagePlus(
  dashboardHeaderPlus(
    title = strong('CHC Projection')
  ), 
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = strong('1.Format Raw Data'), 
        menuSubItem(
          text = 'Raw Data', 
          tabName = 'raw'
        ), 
        menuSubItem(
          text = 'Mapping Table', 
          tabName = 'mapping'
        ), 
        tabName = 'format', 
        startExpanded = TRUE
      ), 
      menuItem(
        text = strong('2.Project on Sample City'), 
        menuSubItem(
          text = 'Sample Except Shanghai', 
          tabName = 'except'
        ), 
        menuSubItem(
          text = 'Sample of Shanghai', 
          tabName = 'sh'
        ), 
        tabName = 'sample', 
        startExpanded = FALSE
      ), 
      menuItem(
        text = '3.Projection on Nation', 
        tabName = 'nation'
      ), 
      menuItem(
        text = '4.Format Result', 
        tabName = 'result'
      )
    ), 
    
    dashboardBody(
      tabItems(
        tabItem(
          tabName = 'raw', 
          boxPlus(
            shinyFilesButton('raw', 
                             label = 'Choose raw data file', 
                             title = 'Choose raw data file', 
                             multiple = FALSE), 
            verbatimTextOutput('folder_raw'), 
            title = strong('Raw Data'), 
            width = 12, 
            closable = FALSE
          )
        ), 
        tabItem(
          tabName = 'mapping', 
          boxPlus(
            shinyFilesButton('universe', 
                             label = 'Choose universe CHC file', 
                             title = 'Choose universe CHC file', 
                             multiple = FALSE), 
            verbatimTextOutput('file_universe'), 
            title = strong('Universe CHC'), 
            width = 12, 
            closable = FALSE
          ), 
          br(), 
          boxPlus(
            shinyFilesButton('ims', 
                             label = 'Choose IMS pack file', 
                             title = 'Choose IMS pack file', 
                             multiple = FALSE), 
            verbatimTextOutput('file_ims'), 
            title = strong('IMS Pack Info'), 
            width = 12, 
            closable = FALSE
          ), 
          br(), 
          boxPlus(
            shinyFilesButton('market', 
                             label = 'Choose market definition file', 
                             title = 'Choose market definition file', 
                             multiple = FALSE), 
            verbatimTextOutput('file_market'), 
            title = strong('Market Definition'), 
            width = 12, 
            closable = FALSE
          ), 
          br(), 
          boxPlus(
            shinyFilesButton('tier', 
                             label = 'Choose city tier file', 
                             title = 'Choose city tier file', 
                             multiple = FALSE), 
            verbatimTextOutput('file_tier'), 
            title = strong('City tier'), 
            width = 12, 
            closable = FALSE
          )
        ), 
        tabItem(
          tabName = 'except', 
          boxPlus(
            actionButton('proj_except', label = 'Run'), 
            downloadButton('result_except'), 
            downloadButton('check_except'), 
            title = 'Project on Sample City Except Shanghai', 
            width = 12, 
            closable = FALSE
          )
        ), 
        tabItem(
          tabName = 'sh', 
          boxPlus(
            actionButton('proj_sh', label = 'Run'), 
            downloadButton('result_sh'), 
            downloadButton('check_sh'), 
            title = 'Project on Shanghai', 
            width = 12, 
            closable = FALSE
          )
        ), 
        tabItem(
          tabName = 'nation', 
          boxPlus(
            shinyFilesButton()
          )
        )
      )
    )
  )
)