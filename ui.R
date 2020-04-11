##### ui

library(shinydashboard)

header=dashboardHeader(title = 'Flow Cytometry Data Analysis Tool')

sidebar=dashboardSidebar(
  sidebarMenu(
    
    menuItem("Z Score Transformation", tabName = 'tab_zscore', icon = icon("pencil")),
    menuItem("Statistical Test", tabName = 'tab_stat', icon = icon("bar-chart"))
    
  )
)


tab_zscore <- fluidRow(
  
  #style='margin-left:0em',
  
  box(
    title = NULL, status = "primary", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    verbatimTextOutput("introduction"),
    
    column(6,
           fileInput("file.upload", h4(strong('Choose Excel File')),
                     multiple = FALSE,
                     accept = c(#"text/csv",
                       #"text/comma-separated-values,text/plain",
                       #".csv"
                       ".xlsx",
                       ".xls"))#,
    )
    
  ),
  
  
  box(
    title = 'Raw Data', status = "primary", solidHeader = TRUE, collapsible = TRUE,
    width = 12,
    DT::dataTableOutput("table.raw.data")
  ),
  
  
  box(
    title = 'Z Score', status = "primary", solidHeader = TRUE, collapsible = TRUE,
    width = 12,
    DT::dataTableOutput("table.zscore"),
    
    column(5, offset=5,
           downloadButton("download.zscore", strong("Download Z Score"))
    )
  ),
  
  
  box(
    title = 'Heatmap', status = "primary", solidHeader = TRUE, collapsible = TRUE,
    width = 12, height = 800,
    
    column(3, #offset = 1,
           checkboxGroupInput(inputId = "cluster", label =  h5(strong('Cluster')),
                              choices = c('Row', 'Column'), selected = 'Row',
                              inline = TRUE),
           checkboxGroupInput(inputId = "names", label = h5(strong('Name')),
                              choices = c('Row', 'Column'), selected = c('Row','Column'),
                              inline = TRUE),
           
           sliderInput(inputId = "font.row", label = h5(strong('Font size (Row)')), 
                       min = 1, max = 20,  value = 10, width = 200),
           
           sliderInput(inputId = "font.column", label = h5(strong('Font size (Column)')), 
                       min = 1, max = 20,  value = 14, width = 200),
           
           radioButtons(inputId = "angle.column", label = "Angle (Column)",
                        c(0, 45, 90), selected = 45,
                        inline = TRUE)
    ),
    
    
    column(9,
           plotOutput("heatmap")
    )
  )
  
)



tab_stat <- fluidRow(
  
  box(
    title = NULL, status = "primary", solidHeader = TRUE, collapsible = FALSE,
    width = 12, height = 900,
    
    column(12, uiOutput('columns')),
    column(12, uiOutput('control')),
    
    #column(8, DT::dataTableOutput("table.test")),
    br(),
    column(12, DT::dataTableOutput("table.wilcox")),
    br(),
    column(6, plotOutput("boxplot")),
    column(6, plotOutput("barplot"))
    
  )
  
)


body=dashboardBody(
  
  tabItems(
    tabItem(tabName="tab_zscore", tab_zscore),
    tabItem(tabName="tab_stat",tab_stat)
  )
  
)


ui <- dashboardPage(title='Flow Cytometry Data Analysis Tool', skin = 'blue', header, sidebar, body)
