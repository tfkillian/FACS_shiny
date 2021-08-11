############################ count matrix shiny app ############################
## This app takes as input a count matrix in .csv format with the first column a
## list of genes/markers/metabolites and the columns of the data correspond to
## the samples and the values are continuous or discrete counts. A PCA, tSNE,
## heatmap and violin plot are generated in addition to a dynamic HTML table.

## load libraries
library("dplyr")
library("tidyr")
library("tibble")
library("Rtsne")
library("ggplot2")
library("heatmaply")
library("shiny")
library("shinydashboard")

############################## User Interface ##################################
ui <- dashboardPage(
        dashboardHeader(title = "Count Matrix App"),
        dashboardSidebar(
          br(),
          h5("Upload a count dataset", align = "center"),
          fileInput(inputId = "file1",
                    label = "Choose CSV File",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          tags$hr(),
          h5("CSV file parameters", align = "center"),
          radioButtons(inputId = "sep",
                       label = "Separator",
                       choices = c(Comma = ",",
                                   Semicolon = ";",
                                   Tab = "\t"),
                       selected = ","),
          radioButtons(inputId = "quote",
                       label = "Quote",
                       choices = c(None = "",
                                   "Double Quote" = '"',
                                   "Single Quote" = "'"),
                       selected = '"'),
          checkboxInput(inputId = "header",
                        label = "Header",
                        value = TRUE),
          tags$hr(),
          checkboxInput(inputId = "log",
                        label = "Log transform data",
                        value = FALSE)
          ), ## end dashboardSidebar
        dashboardBody(
          tabsetPanel(type = "tabs",
            tabPanel(title = "HTML Table",
              fluidRow(column(width = 12, #h4("HTML table"),
                h5("Note: specific terms can be queried in the search bar."),
                DT::dataTableOutput("table"), align = "left"))),
            tabPanel(title = "PCA",
                fluidRow(box(title = "PCA plot",
                             plotlyOutput("pca", height = 350)),
                         box(title = "PCA parameters",
                             tags$hr(),
                             checkboxInput(inputId = "center",
                                           label = "Center",
                                           value = TRUE),
                             checkboxInput(inputId = "scale",
                                           label = "Scale",
                                           value = TRUE)
                         # ,tags$hr(),
                         # h5("Download this figure as a .pdf or .png file"),
                         # downloadButton('downloadData1', 'Download pdf'),
                         # downloadButton('downloadData2', 'Download png')
                         ))),
            tabPanel(title = "tSNE",         
              fluidRow(box(title = "tSNE plot",
                           plotlyOutput("tsne", height = 350)),
                       box(title = "tSNE parameters",
                           tags$hr(),
                           sliderInput(inputId = "perplexity",
                                       label = "tSNE Perplexity",
                                       min = 1,
                                       max = 100,
                                       value = 3),
                           sliderInput(inputId = "iterations",
                                       label = "tSNE Iterations",
                                       min = 1,
                                       max = 5000,
                                       value = 1000),
                           sliderInput(inputId = "theta",
                                       label = "tSNE Theta",
                                       min = 0,
                                       max = 1,
                                       value = 0.5)
                           # ,tags$hr(),
                           # h5("Download this figure as a .pdf or .png file"),
                           # downloadButton('downloadData3', 'Download pdf'),
                           # downloadButton('downloadData4', 'Download png')
                           ))),
            tabPanel(title = "Heatmap",
              fluidRow(box(title = "Heatmap plot",
                           plotlyOutput("heatmap1", height = 350)),
                       box(title = "Heatmap parameters",
                           tags$hr(),
                           checkboxInput(inputId = "label1",
                                         label = "Y-labels",
                                         value = FALSE),
                           radioButtons(inputId = "distance1",
                                        label = "Heatmap Distance Metric",
                                        choices = c("Euclidean" = "euclidean",
                                                    "Maximum" = "maximum",
                                                    "Manhattan" = "manhattan",
                                                    "Minkowski" = "minkowski",
                                                    "Canberra" = "canberra"),
                                        selected = "euclidean")
                          # ,tags$hr(),
                          # h5("Download this figure as a .pdf or .png file"),
                          # downloadButton('downloadData5', 'Download pdf'),
                          # downloadButton('downloadData6', 'Download png')
                          ))),
            tabPanel(title = "Corrplot",
              fluidRow(box(title = "Correlation plot",
                           plotlyOutput("heatmap2", height = 350)),
                       box(title = "Corrplot parameters",
                           tags$hr(),
                           radioButtons(inputId = "distance2",
                                        label = "Corrplot Distance Metric",
                                        choices = c("Euclidean" = "euclidean",
                                                    "Maximum" = "maximum",
                                                    "Manhattan" = "manhattan",
                                                    "Minkowski" = "minkowski",
                                                    "Canberra" = "canberra"),
                                        selected = "euclidean"),
                          radioButtons(inputId = "cluster",
                                        label = "Correlation Metric",
                                        choices = c("Distance" = "dist",
                                                    "Pearson" = "pearson",
                                                    "Spearman" = "spearman",
                                                    "Kendall" = "kendall"),
                                        selected = "dist")
                          # ,tags$hr(),
                          # h5("Download this figure as a .pdf or .png file"),
                          # downloadButton('downloadData7', 'Download pdf'),
                          # downloadButton('downloadData8', 'Download png')
                          ))),
            tabPanel(title = "Violin Plot",
            fluidRow(box(title = "Violin plot",
                         plotlyOutput("violin", height = 350))
                     # ,box(title = "Violin plot parameters",
                     #     tags$hr(),
                     #     h5("Download this figure as a .pdf or .png file"),
                     #     downloadButton('downloadData9', 'Download pdf'),
                     #     downloadButton('downloadData10', 'Download png'))
                     )
          )) ## tabsetPanel
      )) ## end dashboardBody + dashboardPage
## end UI
################################## Server ######################################
server <- function(input, output) {

   ################################ PCA ########################################
   output$pca <- renderPlotly({
      req(input$file1)
      tryCatch({
          ## log transform data (or not)
           if (input$log == TRUE) {
           read.csv(input$file1$datapath,
                 header = input$header,
                 sep = input$sep,
                 quote = input$quote) %>%
          dplyr::rename(X = names(.)[1]) %>%
          tibble::column_to_rownames(var = "X") %>%
          log2() %>% as.matrix() -> data_df
          data_df[is.nan(data_df)] <- 0
          data_df <- as.data.frame(data_df)
          } else {
          read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote) %>%
            dplyr::rename(X = names(.)[1]) %>%
            tibble::column_to_rownames(var = "X") -> data_df
          }
        keep <- rowSums(data_df) > 0 ## remove rows of all 0 counts
        data_df[keep, ] %>%
          t() %>% prcomp(center = input$center, scale. = input$scale) -> PC
        data.frame(PC$x, Sample = names(data_df)) %>%
           ggplot(aes(x = PC1, y = PC2, colour = Sample)) + geom_point() -> dfp
               },
      error = function(e) {## return a safeError if a parsing error occurs
        stop(safeError(e))
      })
      return(dfp)
   })
   
   ############################## tSNE #########################################
   output$tsne <- renderPlotly({
      req(input$file1)
      tryCatch({
          ## log transform data (or not)
           if (input$log == TRUE) {
           read.csv(input$file1$datapath,
                 header = input$header,
                 sep = input$sep,
                 quote = input$quote) %>%
          dplyr::rename(X = names(.)[1]) %>%
          tibble::column_to_rownames(var = "X") %>%
          log2() %>% as.matrix() -> data_df
          data_df[is.nan(data_df)] <- 0
          data_df <- as.data.frame(data_df)
          } else {
          read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote) %>%
            dplyr::rename(X = names(.)[1]) %>%
            tibble::column_to_rownames(var = "X") -> data_df
          }
        keep <- rowSums(data_df) > 0 ## get rid rows of all 0 counts
        data_df[keep, ] %>%
            t() %>% Rtsne(perplexity = input$perplexity,
                          max_iter = input$iterations,
                          theta = input$theta) -> tsne1
        data.frame(x = tsne1$Y[,1],
                   y = tsne1$Y[,2],
                   Sample = names(data_df)) -> df1
        df1 %>%
          ggplot(aes(x = x, y = y, colour = Sample)) + geom_point() +
             xlab("Dimension 1") + ylab("Dimension 2") -> dft
             },
      error = function(e) {## return a safeError if a parsing error occurs
        stop(safeError(e))
      })
      return(dft)
   })
   
   ############################### Heatmap 1 ###################################
   output$heatmap1 <- renderPlotly({
      req(input$file1)
      tryCatch({
          ## log transform data (or not)
           if (input$log == TRUE) {
           read.csv(input$file1$datapath,
                 header = input$header,
                 sep = input$sep,
                 quote = input$quote) %>%
          dplyr::rename(X = names(.)[1]) %>%
          tibble::column_to_rownames(var = "X") %>%
          log2() %>% as.matrix() -> data_df
          data_df[is.nan(data_df)] <- 0
          data_df <- as.data.frame(data_df)
          } else {
          read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote) %>%
            dplyr::rename(X = names(.)[1]) %>%
            tibble::column_to_rownames(var = "X") -> data_df
          }
        keep <- rowSums(data_df) > 0 ## get rid rows of all 0 counts
        data_df[keep, ] %>%
          heatmaply(showticklabels = c(TRUE, input$label1),
                    dist_method = input$distance1) -> dfh
              },
      error = function(e) {## return a safeError if a parsing error occurs
        stop(safeError(e))
      })
      return(dfh)
   })
   
   ############################### Heatmap 2 ###################################
   output$heatmap2 <- renderPlotly({
      req(input$file1)
      tryCatch({
          ## log transform data (or not)
           if (input$log == TRUE) {
           read.csv(input$file1$datapath,
                 header = input$header,
                 sep = input$sep,
                 quote = input$quote) %>%
          dplyr::rename(X = names(.)[1]) %>%
          tibble::column_to_rownames(var = "X") %>%
          log2() %>% as.matrix() -> data_df
          data_df[is.nan(data_df)] <- 0
          data_df <- as.data.frame(data_df)
          } else {
          read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote) %>%
            dplyr::rename(X = names(.)[1]) %>%
            tibble::column_to_rownames(var = "X") -> data_df
          }
        keep <- rowSums(data_df) > 0 ## get rid rows of all 0 counts
        data_df[keep, ] %>%
          cor() %>%
          heatmaply_cor(showticklabels = c(TRUE, TRUE),
                        dist_method = input$distance2,
                        distfun_col = input$cluster,
                        distfun_row = input$cluster) -> dfh
              },
      error = function(e) {## return a safeError if a parsing error occurs
        stop(safeError(e))
      })
      return(dfh)
   })
   
   ############################### Violin Plot #################################
   output$violin <- renderPlotly({
      req(input$file1)
      tryCatch({
          ## log transform data (or not)
           if (input$log == TRUE) {
           read.csv(input$file1$datapath,
                 header = input$header,
                 sep = input$sep,
                 quote = input$quote) %>%
          dplyr::rename(X = names(.)[1]) %>%
          tibble::column_to_rownames(var = "X") %>%
          log2() %>% as.matrix() -> data_df
          data_df[is.nan(data_df)] <- 0
          data_df <- as.data.frame(data_df)
          } else {
          read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote) %>%
            dplyr::rename(X = names(.)[1]) %>%
            tibble::column_to_rownames(var = "X") -> data_df
          }
        keep <- rowSums(data_df) > 0 ## get rid rows of all 0 counts
        data_df[keep, ] %>%
          tibble::rownames_to_column() %>%
          dplyr::rename(Marker = rowname) %>%
          tidyr::gather(key = "Sample", value = "Counts", -Marker) %>%
          ggplot(aes(x = Sample, y = Counts, fill = Sample)) +
             geom_violin(trim = FALSE) +
             theme_classic() +
             theme(legend.position = "none",
                   axis.text.x = element_text(angle = 45)) -> dfv
               },
      error = function(e) {## return a safeError if a parsing error occurs
        stop(safeError(e))
      })
      return(dfv)
   })
   
   ############################### HTML table ##################################
   output$table <- DT::renderDataTable({
       req(input$file1)
       tryCatch({
                   ## log transform data (or not)
           if (input$log == TRUE) {
           read.csv(input$file1$datapath,
                 header = input$header,
                 sep = input$sep,
                 quote = input$quote) %>%
          dplyr::rename(X = names(.)[1]) %>%
          tibble::column_to_rownames(var = "X") %>%
          log2() %>% as.matrix() -> data_df
          data_df[is.nan(data_df)] <- 0
          as.data.frame(data_df) %>%
            dplyr::mutate_if(is.numeric, round, 3) -> df
          } else {
          read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote) %>%
            dplyr::rename(X = names(.)[1]) %>%
            tibble::column_to_rownames(var = "X") %>% 
            dplyr::mutate_if(is.numeric, round, 3) -> df
          }
                },
      error = function(e) {## return a safeError if a parsing error occurs
        stop(safeError(e))
      })
      return(df)
  })
  
  ############################# download buttons ###############################
    
  #  output$downloadData1 <- downloadHandler(
  #   filename = "plot.pdf",
  #   content = function(file) {
  #     orca(input$pca, file = "plot.pdf")
  #     # pdf(file)
  #     # print(output$pca)
  #     # dev.off()
  #     # dev.copy2pdf(file="file")
  #   }
  # )
   
## end server   
}
## Run the application 
shinyApp(ui = ui, server = server)
