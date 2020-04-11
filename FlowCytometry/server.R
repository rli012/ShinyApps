
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


######## Server

server <- function(input, output, session) { 
  
  output$introduction <- renderText({ 
    introduction <- 'This web tool is designed for Z score transformation, heatmap generation, and statistical test for Flow Cytometry data.\nPlease upload an Excel file with the Flow Cytometry data.\nGroup information should be in the 1st column and Flow Cytometry data starts from the 2nd column.'
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
    
  }, height = 700, width = 700)
  
  
  output$columns <- renderUI({
    
    req(input$file.upload)
    dataForHeatmap <- read_xlsx(input$file.upload$datapath)
    
    colnames(dataForHeatmap) <- gsub('\r\n', '', colnames(dataForHeatmap))
    
    selectInput('columns2', h4(strong('Columns')), colnames(dataForHeatmap)[-1])  ### input$columns2
  })
  
  
  output$control <- renderUI({
    
    req(input$file.upload)
    dataForHeatmap <- read_xlsx(input$file.upload$datapath)
    colnames(dataForHeatmap)[1] <- 'Group'
    
    radioButtons('control2', h4(strong('Control Group')), 
                 choices = unique(dataForHeatmap$Group), selected = unique(dataForHeatmap$Group)[1], inline = TRUE)  ### input$columns2
  })
  
  
  output$table.test <- DT::renderDataTable({
    req(input$columns2)
    dataForHeatmap <- read_xlsx(input$file.upload$datapath)
    
    dataForHeatmap[dataForHeatmap=='n/a'] <- NA
    dataForHeatmap[dataForHeatmap==''] <- NA
    
    colnames(dataForHeatmap) <- gsub('\r\n', '', colnames(dataForHeatmap))
    
    column.names <- colnames(dataForHeatmap)
    
    colnames(dataForHeatmap)[1] <- 'Group'
    
    dataForHeatmap[,-1] <- apply(dataForHeatmap[,-1], 2, as.numeric)
    dataForHeatmap$Group <- factor(dataForHeatmap$Group, levels=unique(dataForHeatmap$Group))
    
    group <- dataForHeatmap$Group
    idx <- which(column.names==input$columns2)
    expr <- as.numeric(unlist(dataForHeatmap[,idx]))
    
    dataForBoxPlot <- data.frame(expr=expr, group=group, dataset=input$columns2, stringsAsFactors = F)
    
    
    dataForBarPlot <- dataForBoxPlot %>% group_by(group) %>% 
      summarise(sd=sd(expr, na.rm=T), expr=mean(expr, na.rm=T))
    
    dataForBarPlot <- data.frame(Group=dataForBarPlot$group, 
                                 Mean=round(dataForBarPlot$expr,2), 
                                 SD=round(dataForBarPlot$sd,2),
                                 stringsAsFactors = F)
    
    dataForBarPlot
    
    
  })
  
  
  
  output$table.wilcox <- DT::renderDataTable({
    req(input$columns2)
    dataForHeatmap <- read_xlsx(input$file.upload$datapath)
    
    dataForHeatmap[dataForHeatmap=='n/a'] <- NA
    dataForHeatmap[dataForHeatmap==''] <- NA
    
    colnames(dataForHeatmap) <- gsub('\r\n', '', colnames(dataForHeatmap))
    
    column.names <- colnames(dataForHeatmap)
    
    colnames(dataForHeatmap)[1] <- 'Group'
    
    dataForHeatmap[,-1] <- apply(dataForHeatmap[,-1], 2, as.numeric)
    dataForHeatmap$Group <- factor(dataForHeatmap$Group, levels=unique(dataForHeatmap$Group))
    
    group <- dataForHeatmap$Group
    idx <- which(column.names==input$columns2)
    expr <- as.numeric(unlist(dataForHeatmap[,idx]))
    
    dataForBoxPlot <- data.frame(expr=expr, group=group, dataset=input$columns2, stringsAsFactors = F)
    
    groups <- levels(group)
    
    idx <- which(! groups %in% input$control2)
    
    dataForTestTable <- c()
    
    for (i in idx) {
      
      expr1 <- dataForBoxPlot$expr[dataForBoxPlot$group==groups[i]]
      expr2 <- dataForBoxPlot$expr[dataForBoxPlot$group==input$control2]
      
      fold.change <- mean(expr1, na.rm=T)/mean(expr2, na.rm=T)
      
      p <- wilcox.test(expr1, expr2)$p.value
      sig <- symnum(p, cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c("***",'**','*','ns'))
      
      p <- ifelse(p<0.01, formatC(p, format = "e", digits = 2), round(p, 2)) #formatC(p, format='g', digits=2)
      
      dataForTestTable <- rbind(dataForTestTable, 
                                c(groups[i], input$control2, 
                                  round(mean(expr1, na.rm=T),2), 
                                  round(mean(expr2, na.rm=T),2), 
                                  round(fold.change, 2),
                                  p, sig))
      
    }
    
    colnames(dataForTestTable) <- c('Treatment', 'Control', 'Mean (Treatment)', 'Mean (Control)', 'Fold Change (Tretment/Control)', 'P Value', 'Significance')
    
    dataForTestTable
    
  }, options = list(
    columnDefs = list(list(className = 'dt-center', targets = 0:6)))#,
  #list(targets = 6, visible = FALSE)))
  )
  
  
  output$boxplot <- renderPlot({
    req(input$columns2)
    dataForHeatmap <- read_xlsx(input$file.upload$datapath)
    
    dataForHeatmap[dataForHeatmap=='n/a'] <- NA
    dataForHeatmap[dataForHeatmap==''] <- NA
    
    colnames(dataForHeatmap) <- gsub('\r\n', '', colnames(dataForHeatmap))
    
    column.names <- colnames(dataForHeatmap)
    
    colnames(dataForHeatmap)[1] <- 'Group'
    
    dataForHeatmap[,-1] <- apply(dataForHeatmap[,-1], 2, as.numeric)
    dataForHeatmap$Group <- factor(dataForHeatmap$Group, levels=unique(dataForHeatmap$Group))
    
    group <- dataForHeatmap$Group
    idx <- which(column.names==input$columns2)
    expr <- as.numeric(unlist(dataForHeatmap[,idx]))
    
    dataForBoxPlot <- data.frame(expr=expr, group=group, dataset=input$columns2, stringsAsFactors = F)
    
    p <- ggplot(data=dataForBoxPlot, aes(x=group, y=expr)) +
      geom_boxplot(aes(fill=group),
                   outlier.shape = NA, outlier.size = NA, #outlier.colour = 'black',
                   outlier.fill = NA) +
      facet_wrap(~dataset, nrow=1) +
      geom_jitter(size=2, width=0.05, color='black') + #darkblue
      #scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
      labs(x='', y='') +
      #geom_segment(data=df,aes(x = x1, y = y1, xend = x2, yend = y2)) +
      #geom_text(data =anno, aes(x, y, label=label, group=NULL),
      #          size=4) +
      theme_bw()+
      theme(legend.title = element_blank(),
            legend.text = element_text(size=14),
            legend.position = 'none') +
      theme(axis.title=element_text(size=16),
            axis.text = element_text(color='black', size=14),
            axis.text.x = element_text(angle = 45, hjust=1),
            strip.text = element_text(angle=0, size=14, face='bold')) +
      theme(axis.line = element_line(colour = "black"),
            panel.border = element_rect(color = 'black'), #element_blank(),
            panel.background = element_blank()#,
            #panel.grid = element_blank(),
            #panel.grid.major = element_blank()
      ) +
      theme(plot.margin =  margin(t = 0.25, r = 0.25, b = 0.25, l = 0.25, unit = "cm"))
    
    p
    
  }, height = 450, width = 450)
  
  
  
  output$barplot <- renderPlot({
    req(input$columns2)
    dataForHeatmap <- read_xlsx(input$file.upload$datapath)
    
    dataForHeatmap[dataForHeatmap=='n/a'] <- NA
    dataForHeatmap[dataForHeatmap==''] <- NA
    
    colnames(dataForHeatmap) <- gsub('\r\n', '', colnames(dataForHeatmap))
    
    column.names <- colnames(dataForHeatmap)
    
    colnames(dataForHeatmap)[1] <- 'Group'
    
    dataForHeatmap[,-1] <- apply(dataForHeatmap[,-1], 2, as.numeric)
    dataForHeatmap$Group <- factor(dataForHeatmap$Group, levels=unique(dataForHeatmap$Group))
    
    group <- dataForHeatmap$Group
    idx <- which(column.names==input$columns2)
    expr <- as.numeric(unlist(dataForHeatmap[,idx]))
    
    dataForBoxPlot <- data.frame(expr=expr, group=group, dataset=input$columns2, stringsAsFactors = F)
    
    
    dataForBarPlot <- dataForBoxPlot %>% group_by(group) %>% 
      summarise(sd=sd(expr, na.rm=T), expr=mean(expr, na.rm=T))
    
    dataForBarPlot$dataset <- input$columns2
    
    p <- ggplot(data=dataForBarPlot, aes(x=group, y=expr, fill=group, color=group)) +
      geom_bar(stat='identity', width=.6) + #coord_flip()
      geom_errorbar(aes(ymin=expr, ymax=expr+sd), width=.2, size=0.5, #expr-sd
                    position=position_dodge(.9)) +
      labs(x='', y='') +
      facet_wrap(~dataset, nrow=1) +
      #scale_y_continuous(trans = 'sqrt',
      #                   breaks = c(0,2.5,50,250,750),
      #                   labels = c(0,2.5,50,250,750)) +
      #scale_y_sqrt() +
      #scale_y_continuous(trans='log2') +
      #scale_fill_manual(values = rep('black',nrow(dataForBarPlot))) +
      #scale_color_manual(values = rep('black',nrow(dataForBarPlot))) +
      theme_bw()+
      theme(legend.title = element_blank(),
            legend.text = element_text(size=14),
            legend.position = 'none') +
      theme(axis.title=element_text(size=16),
            axis.text = element_text(color='black', size=14),
            axis.text.x = element_text(angle = 45, hjust=1),
            strip.text = element_text(angle=0, size=14, face='bold')) +
      theme(axis.line = element_line(colour = "black"),
            panel.border = element_rect(color = 'black'), #element_blank(),
            panel.background = element_blank()#,
            #panel.grid = element_blank(),
            #panel.grid.major = element_blank()
      ) +
      theme(plot.margin =  margin(t = 0.25, r = 0.25, b = 0.25, l = 0.25, unit = "cm"))
    
    p
    
  }, height = 450, width = 450)
  
  
}


#shinyApp(
#  ui = ui,
#  server = server
#)


