
library(shiny)
library(shinydashboard)
library(readxl)
library(ggplot2)
library(Matrix)
library(stringr)
library(dplyr)
library(pheatmap)
library(writexl)

google.red <- '#EA4335'
google.yellow <- '#FBBC05'
google.green <- '#34A853'
google.blue <- '#4285F4'


##### Environment
options(shiny.maxRequestSize = 20*1024^2)

col_fun = colorRampPalette(rev(c(google.red,'white',google.blue)), space = "Lab")(100)


##### ui

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
           fileInput("file.upload", h5(strong('Choose Excel File')),
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
    width = 12, #height = 800,
    
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
                        c(0, 45, 90),
                        inline = TRUE)
    ),
    
    

    column(9,
           plotOutput("heatmap")
    )
  )
  
)



tab_stat <- fluidRow(
  
  #style='margin-left:0em',
  
  box(
    title = NULL, status = "primary", solidHeader = TRUE, collapsible = FALSE,
    width = 12,
    
    #selectizeInput(inputId = "gene", label= h4(strong("Gene")), 
    #               #style = "font-family: 'Lobster', 
    #               #cursive; font-weight: 500; 
    #               #line-height: 1.1; color: #4d3a7d;"),
    #               choices = genes, selected = gene.default, multiple=FALSE,
    #               width = 300,
    #               options = list(placeholder = 'Select a gene', maxOptions = 5, selectOnTab=TRUE)),
    
    radioButtons(inputId = "survival_radio", label = h4(strong('Survival')), #width = 6,
                 c('Overall Survival', 'Relapse-free Survival'),
                 inline = TRUE),
    
    sliderInput(inputId = "quantile", label = h4(strong('Quantile')), 
                min = 0, max = 100,  value = 50, width = 300)
  ),
  
  
  box(
    title = 'TCGA-PRAD', status = "primary", solidHeader = TRUE, collapsible = TRUE,
    width = 3,
    plotOutput("tcgasurvival", height = 250)
  )
  
)


body=dashboardBody(
  
  #selectizeInput(inputId = "gene", label='Gene', choices = genes, selected = gene.default, multiple=FALSE, 
  #               options = list(
  #                 placeholder = 'Select a gene', maxOptions = 10,
  #                 selectOnTab=TRUE)),
  
  tabItems(
    tabItem(tabName="tab_zscore", tab_zscore),
    tabItem(tabName="tab_stat",tab_stat)#,
    #tabItem(tabName="tab_dataset",tab_dataset)
  )
  
)

ui <- dashboardPage(title='Flow Cytometry Data Analysis Tool', skin = 'blue', header, sidebar, body)



#shinyApp(
#  ui = ui,
#  server = server
#)


######## Server

server <- function(input, output, session) { 
  
  output$introduction <- renderText({ 
    introduction <- 'This web tool is designed for Z score transformation, heatmap generation, and statistical test for Flow Cytometry data.\nPlease upload an Excel file with the Flow Cytometry data.\nThe group information should be in the 1st column and Flow Cytometry data starts from the 2nd column.'
    introduction
    
  })
  
  
  output$table.raw.data <- DT::renderDataTable({
    req(input$file.upload)
    dataForHeatmap <- read_xlsx(input$file.upload$datapath)
    dataForHeatmap
  
  },
  options = list(pageLength = 5, scrollX = "500px"), # lengthMenu = c(5, 30, 50)
  selection = list(mode='single', selected=1)
  )
  
  
  output$table.zscore <- DT::renderDataTable({
    req(input$file.upload)
    dataForHeatmap <- read_xlsx(input$file.upload$datapath)
    
    dataForHeatmap[dataForHeatmap=='n/a'] <- NA
    dataForHeatmap[dataForHeatmap==''] <- NA
    
    colnames(dataForHeatmap) <- gsub('\r\n', '', colnames(dataForHeatmap))
    
    column.names <- colnames(dataForHeatmap)
    
    colnames(dataForHeatmap)[1] <- 'Group'
    
    dataForHeatmap[,-1] <- apply(dataForHeatmap[,-1], 2, as.numeric)
    dataForHeatmap$Group <- factor(dataForHeatmap$Group, levels=unique(dataForHeatmap$Group))
    
    #dataForHeatmap <- dataForHeatmap[-which(dataForHeatmap$Group=='Disease'),]
    dataForHeatmap <- dataForHeatmap %>% group_by(Group) %>% summarise_all(funs(mean), na.rm = TRUE)

    dataForHeatmap[,-1] <- round(scale(dataForHeatmap[,-1], center = TRUE, scale = TRUE),3)

    colnames(dataForHeatmap) <- column.names
    
    dataForHeatmap
    
  },
  options = list(pageLength = 5, scrollX = "500px"),
  selection = list(mode='single', selected=1)
  )
  
  output$download.zscore <- downloadHandler(
    
    #fl <- strsplit(input$file.upload)

    filename = function() {gsub('.xlsx', '_Z_Score.xlsx', input$file.upload)},
    
    content = function(file) {
      
      dataForHeatmap <- read_xlsx(input$file.upload$datapath)
      
      dataForHeatmap[dataForHeatmap=='n/a'] <- NA
      dataForHeatmap[dataForHeatmap==''] <- NA
      
      colnames(dataForHeatmap) <- gsub('\r\n', '', colnames(dataForHeatmap))
      
      column.names <- colnames(dataForHeatmap)
      
      colnames(dataForHeatmap)[1] <- 'Group'
      
      dataForHeatmap[,-1] <- apply(dataForHeatmap[,-1], 2, as.numeric)
      dataForHeatmap$Group <- factor(dataForHeatmap$Group, levels=unique(dataForHeatmap$Group))
      
      #dataForHeatmap <- dataForHeatmap[-which(dataForHeatmap$Group=='Disease'),]
      dataForHeatmap <- dataForHeatmap %>% group_by(Group) %>% summarise_all(funs(mean), na.rm = TRUE)
      
      dataForHeatmap[,-1] <- round(scale(dataForHeatmap[,-1], center = TRUE, scale = TRUE),3)
      
      colnames(dataForHeatmap) <- column.names

      write_xlsx(dataForHeatmap, path = file)
    }
  )
  
  
  output$heatmap <- renderPlot({
    req(input$file.upload)
    dataForHeatmap <- read_xlsx(input$file.upload$datapath)
    
    dataForHeatmap[dataForHeatmap=='n/a'] <- NA
    dataForHeatmap[dataForHeatmap==''] <- NA
    
    colnames(dataForHeatmap) <- gsub('\r\n', '', colnames(dataForHeatmap))
    
    column.names <- colnames(dataForHeatmap)
    
    colnames(dataForHeatmap)[1] <- 'Group'
    
    dataForHeatmap[,-1] <- apply(dataForHeatmap[,-1], 2, as.numeric)
    dataForHeatmap$Group <- factor(dataForHeatmap$Group, levels=unique(dataForHeatmap$Group))
    
    #dataForHeatmap <- dataForHeatmap[-which(dataForHeatmap$Group=='Disease'),]
    dataForHeatmap <- dataForHeatmap %>% group_by(Group) %>% summarise_all(funs(mean), na.rm = TRUE)
    dataForHeatmap[,-1] <- scale(dataForHeatmap[,-1], center = TRUE, scale = TRUE)
    
    group.names <- dataForHeatmap$Group
    
    dataForHeatmap <- data.frame(dataForHeatmap[,-1], stringsAsFactors = F)

    rownames(dataForHeatmap) <- group.names
    colnames(dataForHeatmap) <- column.names[-1]
    
    #idx <- grep('Count', colnames(dataForHeatmap))
    #dataForHeatmap <- dataForHeatmap[,idx]
    
    dataForHeatmap <- t(dataForHeatmap)
    
    mx <- max(dataForHeatmap, na.rm = T)
    
    cluster.row <- cluster.col <- name.row <- name.col <- F
    
    if ('Row' %in% input$cluster) {
      cluster.row = T
    }
    
    if ('Column' %in% input$cluster) {
      cluster.col = T
    }
    
    if ('Row' %in% input$names) {
      name.row = T
    }
    
    if ('Column' %in% input$names) {
      name.col = T
    }
    
    p <- pheatmap(dataForHeatmap,
             scale = 'none',
             cluster_cols = cluster.col,
             border_color = NA,
             cluster_rows = cluster.row,
             #treeheight_row = 0,
             show_rownames = name.row,
             show_colnames = name.col,
             fontsize_row = input$font.row, 
             fontsize_col = input$font.column,
             angle_col = input$angle.column,
             #annotation_legend = F,
             breaks = c(seq(-1*mx,mx, 2*mx/100)),
             color=col_fun
    )
    
    p
    
  })
  
}


shinyApp(
  ui = ui,
  server = server
)

