

library(shiny)
library(shinydashboard)
library(dplyr)
library(shinycssloaders)
library(umap)
library(Rtsne)
library(DT)


rdata <- c('iris','biopsy','Melanoma','Pima.te','Boston','diamonds','muscle','mtcars')

fun <- c("None","abs(x)","ceiling(x)","cos(x)","exp(x)","floor(x)","log(x)","log10(x)","sin(x)","sqrt(x)","trunc(x)","tan(x)")

dataview <- c("Pre-processed Dataset","Processed Dataset","Summary")

impute <- c("None","Remove all NA rows","Impute with Mean","Impute with Median","Impute with 0")

scale <- c("None","center = TRUE & scale = TRUE","center = FALSE & scale = TRUE","center = TRUE & scale = FALSE")

umapmetric <- c("euclidean","manhattan")

kmeansalg <- c("Hartigan-Wong","Lloyd","Forgy","MacQueen")


dbHeader <- dashboardHeader(title = "Dimension Reduction with UMAP & t-SNE",
                            titleWidth = 500,
                            tags$li(a(href = 'http://shinyapps.company.com',
                                      icon("power-off"),
                                      title = "Back to Apps Home"),
                                    class = "dropdown"),
                            tags$li(a(href = 'http://www.atamaianalytics.com',
                                      img(src = 'AtamaiAnalytics.jpeg',
                                          title = "Company Home", height = "30px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"))

ui <- dashboardPage(skin="green",
  dbHeader,
  dashboardSidebar(width=225,
                   sidebarMenu(
                     menuItem("Data Selection & Processing", tabName = "process", icon = icon("table")),
                     br(),
                     menuItem("UMAP", tabName = "umap", icon = icon("chart-bar")),
                     br(),
                     menuItem("t-SNE", tabName = "tsne", icon = icon("chart-bar"))
                   )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "process",
            tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
            ),
            
           fluidRow(   
              box(title="Data Selection",background = "green",solidHeader = TRUE,width = 4,
                  fluidRow(
                    column(12,fileInput('fileUpload', 'Select File'))
                  ),
                  fluidRow(
                  column(12,
                         h4("-- or --",style="font-weight:bold")
                         )
                  ),
                  fluidRow(
                  column(12,
                         checkboxInput("useRdata", "Use data selection from MASS and ggplot2 packages", FALSE)
                         )),
                  fluidRow(
                  column(6,
                         selectInput("rdata", "",rdata,selected="")
                         )),
                  hr(),
                  fluidRow(
                  column(6,
                         verbatimTextOutput("nrow")
                         )),
                  fluidRow(
                  column(12,
                         h5("For datasets with a large amount of observations consider using a subset. Datasets with > 5000 observations may be slow to process, error or timeout")
                  )),
                  fluidRow(
                    column(12,
                           checkboxInput("limitcheck", "Limit observations", FALSE)
                           )),
                  fluidRow(
                    column(3,
                           numericInput("limitnum", "",2000, min = 1, max = 5000)
                  )),
                  hr()
                ),
              box(title="Dimensions",background = "green",solidHeader = TRUE,width = 4,
                fluidRow(
                column(12,selectizeInput("samplexvars", "Dimension Selection", choices = NULL, multiple = TRUE))
                ),
                fluidRow(
                column(6,selectizeInput("sampleyvars", "Add Data Class", choices = NULL, multiple = FALSE))
                ),
                fluidRow(
                column(6,checkboxInput("classnum", "Treat numeric class as factor", FALSE))
                ),
                hr(),
                fluidRow(
                  column(6,selectInput("dview", "Data set to view",dataview,selected="Pre-processed Dataset"))
                ),
                br(),
                fluidRow(
                  column(4,downloadButton('dataview.csv', 'Download Data View',style = "color: black;background-color: #35e51d"))
                )
                
                ),
              box(title="Data Processing",background = "green",solidHeader = TRUE,width = 4,
                fluidRow(
                  column(8,verbatimTextOutput("renderprint")),
                  column(4,selectInput("impute","Missing Values",impute,selected="None"))
                  
                ),
                fluidRow(
                  column(12,h5("After nemoving NA's or imputing values select the Dimensions again. Missing values can only be removed or imputed once. To remove or impute again, set to None and re-load the data."))
                ),
                hr(),
                fluidRow(
                  column(4,selectInput("fun", "Apply a Function",fun,selected="None")),
                  column(6,selectInput("scale", "Scale Data",scale,selected="None"))
                ),
                hr(),
                fluidRow(
                  column(4,checkboxInput("appround", "Apply rounding", FALSE))
                ),
                fluidRow(
                  column(2,numericInput("round","",1,min = 0,max = 10))
                )
              )
                ),
           fluidRow(
             box(width = 12,
                DT::dataTableOutput('data')
             )
           )
      ),
      tabItem(tabName = "umap",
              tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }"
              ),
            fluidRow(
               box(background = "green",solidHeader = TRUE,width = 3,
                   h4("UMAP Parameters"),
                   br(),
                   fluidRow(
                     column(6,numericInput("n_neighbors", "Nearest Neighbours", 15, min = 2, max = 100)),
                     column(6,numericInput("bandwidth", "Bandwidth", 1, min = 1, max = 10))
                   ),
                   fluidRow(
                     column(6,selectInput("umapmetric", "Metric",umapmetric,selected="euclidian")),
                     column(6,numericInput("alpha", "Alpha", 1, min = 1, max = 10))
                   ),
                   fluidRow(
                     column(6,numericInput("n_epochs", "Epochs", 200, min = 2, max = 1000)),
                     column(6,numericInput("gamma", "Gamma", 1, min = 1, max = 10))
                   ),
                   fluidRow(
                     column(6,numericInput("min_dist", "Min. Distance", 0.1, min = 0.01, max = 10)),
                     column(6,numericInput("negative_sample_rate", "Negative Sample Rate", 5, min = 1, max = 10))
                   ),
                   fluidRow(
                     column(6,numericInput("set_op_mix_ratio", "Set op mix ratio", 1, min = 0, max = 1)),
                     column(6,numericInput("spread", "Spread", 1, min = 1, max = 10))
                   ),
                   fluidRow(
                     column(6,numericInput("local_connectivity", "Local Connectivity", 1, min = 1, max = 10)),
                     column(6,numericInput("knn_repeats", "KNN Repeats", 1, min = 1, max = 10))
                   ),
                   hr(),
                   h4("KMeans Parameters"),
                   br(),
                   fluidRow(
                     column(6,numericInput("n_clusters", "Number of Clusters",2,min = 1, max = 100)),
                     column(6,numericInput("iter_max", "Max. Iterations", 10, min = 1, max = 100))
                   ),
                   fluidRow(
                     column(6,numericInput("nstart", "Random Sets", 1, min = 1, max = 10)),
                     column(6,selectInput("kmeansalg","Algorithm",kmeansalg,selected="Hartigan-Wong"))
               )
               ),
               box(background = "green",solidHeader = TRUE,width = 2,
                  h4("Plotting"),
                   br(),
                  fluidRow(
                   column(6,actionButton("umapseed", "Generate Plots",style = "color: white;background-color: #D35400"))
                   ),
                  fluidRow(
                   column(12,h5('Select "Generate Plots" again to run with varying parameters and either a random or set seed'))
                  ),
                  hr(),
                  h4("Seeding"),
                  fluidRow(
                   column(8,verbatimTextOutput("printuseed"))
                  ),
                   checkboxInput("seed", "Set Seed", FALSE),
                  fluidRow(
                   column(6,numericInput("useedset", "Seed",1234, min = 1000, max = 9999))
                  ),
                 hr(),
                 h4("Downloads"),
                 br(),
                 fluidRow(
                 column(6,downloadButton('umapdata.csv', 'Data Grid',style = "color: black;background-color: #35e51d"))
                 ),
                 br(),
                 fluidRow(
                 column(6,downloadButton('umapplot.png', 'UMAP Plot',style = "color: black;background-color: #35e51d"))
                 ),
                 br(),
                 fluidRow(
                 column(6,downloadButton('umapclusplot.png','Cluster Plot',style = "color: black;background-color: #35e51d"))
               )
                 
               ),
               box(width = 5,
                   fluidRow(
                     box(background = "green",solidHeader = TRUE,width = 12,
                         plotOutput("umapplot") %>% withSpinner(type=4,color="#D35400")
                     )),
                   fluidRow(
                     box(background = "green",solidHeader = TRUE,width = 12,
                         plotOutput("umapcluster") %>% withSpinner(type=4,color="#D35400")
                     ))
               ),
               box(background = "green",solidHeader = TRUE,width = 2,
                   h4("Chart Parameters"),
                   br(),
                   checkboxInput("uxaxisbounds", "Set X axis boundaries", FALSE),
                   fluidRow(
                     column(6,numericInput("uxmin", "Min. X",0,min = -2000, max = 2000)),
                     column(6,numericInput("uxmax", "Max. X",0, min = -2000, max = 2000))
                   ),
                   checkboxInput("uyaxisbounds", "Set Y axis boundaries", FALSE),
                   fluidRow(
                     column(6,numericInput("uymin", "Min. Y",0,min = -2000, max = 2000)),
                     column(6,numericInput("uymax", "Max. Y",0, min = -2000, max = 2000))
                   ),
                   hr(),
                   checkboxInput("umapcluslabels", "Display Cluster Labels", TRUE),
                   hr(),
                   textInput("custmumaptitle", "UMAP Custom Title", "Add a custom title here"),
                   checkboxInput("custmumaptitlecheck", "Display UMAP Custom Title", FALSE),
                   br(),
                   textInput("custmumapclustitle", "KMeans Cluster Custom Title", "Add a custom title here"),
                   checkboxInput("custmumapclustitlecheck", "Display KMeans Custom Title", FALSE)
                
               )),
            fluidRow(
             box(width = 12,
                 DT::dataTableOutput('umap')
             ))
      ),
      tabItem(tabName = "tsne",
              tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }"
              ),
              fluidRow(
                 box(background = "green",solidHeader = TRUE,width = 3,
                    h4("t-SNE Parameters"),
                    br(),
                    fluidRow(
                      column(6,numericInput("perplexity", "Perplexity", 30, min = 10, max = 100)),
                      column(6,h5('From notes on the Rtnse function "Perplexity parameter (should not be bigger than 3 * perplexity < nrow(X) - 1"'))
                    ),
                    fluidRow(
                      column(6,numericInput("initial_dims", "Initial Dimensions", 50, min = 10, max = 100)),
                      column(6,numericInput("theta", "Theta", 0.5, min = 0, max = 1))
                    ),

                    fluidRow(
                      column(6,checkboxInput("pca", "PCA", TRUE)),
                      column(6,checkboxInput("partial_pca", "Partial PCA", FALSE))
                    ),
                    fluidRow(
                      column(6,numericInput("max_iter", "Max. Iterations", 1000, min = 500, max = 2000)),
                      column(6,checkboxInput("pca_center", "PCA Center", TRUE))#,
                    ),
                    fluidRow(
                      column(6,checkboxInput("pca_scale", "PCA Scale", FALSE)),
                      column(6,checkboxInput("normalise", "Normalise", TRUE))#,
                    ),
                    fluidRow(
                      column(6,numericInput("momentum", "Momentum",0.5, min = 0.1, max = 1)),
                      column(6,numericInput("final_momentum", "Final Momentum", 0.8, min = 0.1, max = 1))
                    ),
                    fluidRow(
                        column(6,numericInput("exaggeration_factor", "Exagg. Factor",12, min = 1, max = 20)),
                        column(6,numericInput("num_threads", "Number of Threads", 1, min = 1, max = 10))
                      ),
                    hr(),
                    h4("KMeans Parameters"),
                    br(),
                    fluidRow(
                      column(6,numericInput("tsnen_clusters", "Number of Clusters",2,min = 1, max = 100)),
                      column(6,numericInput("tsneiter_max", "Max. Iterations", 10, min = 1, max = 100))
                    ),
                    fluidRow(
                      column(6,numericInput("tsnenstart", "Random Sets", 1, min = 1, max = 10)),
                      column(6,selectInput("tsnekmeansalg","Algorithm",kmeansalg,selected="Hartigan-Wong"))
                    )
                    ),
                box(background = "green",solidHeader = TRUE,width = 2,
                    h4("Plotting"),
                    br(),
                    fluidRow(
                    column(6,actionButton("tsneseed", "Generate Plots",style = "color: white;background-color: #D35400"))
                    ),
                    fluidRow(
                    column(12,h5('Select "Generate Plots" again to run with varying parameters and either a random or set seed'))
                    ),
                    hr(),
                    h4("Seeding"),
                    fluidRow(
                      column(8,verbatimTextOutput("printtseed"))
                    ),
                      checkboxInput("seedtsne", "Set Seed", FALSE),
                    fluidRow(
                      column(6,numericInput("tseedset", "Seed",1234, min = 1000, max = 9999))
                    ),
                    hr(),
                    h4("Downloads"),
                    br(),
                    fluidRow(
                      column(6,downloadButton('tsnedata.csv', 'Data Grid',style = "color: black;background-color: #35e51d"))
                      ),
                    br(),
                    fluidRow(
                      column(3,downloadButton('tsneplot.png', 't-SNE Plot',style = "color: black;background-color: #35e51d"))
                      ),
                    br(),
                    fluidRow(
                      column(3,downloadButton('tsneclusplot.png', 'Cluster Plot',style = "color: black;background-color: #35e51d"))
                    )
                ),
                box(width = 5,
                    fluidRow(
                      box(background = "green",solidHeader = TRUE,width = 12,
                          plotOutput("tsneplot") %>% withSpinner(type=4,color="#D35400")
                      )),
                    fluidRow(
                      box(background = "green",solidHeader = TRUE,width = 12,
                          plotOutput("tsnecluster") %>% withSpinner(type=4,color="#D35400")
                      ))
                ),
                box(background = "green",solidHeader = TRUE,width = 2,
                    h4("Chart Parameters"),
                    br(),
                    checkboxInput("txaxisbounds", "Set X axis boundaries", FALSE),
                    fluidRow(
                      column(6,numericInput("txmin", "Min. X",0,min = -2000, max = 2000)),
                      column(6,numericInput("txmax", "Max. X",0, min = -2000, max = 2000))
                    ),
                    checkboxInput("tyaxisbounds", "Set Y axis boundaries", FALSE),
                    fluidRow(
                      column(6,numericInput("tymin", "Min. Y",0,min = -2000, max = 2000)),
                      column(6,numericInput("tymax", "Max. Y",0, min = -2000, max = 2000))
                    ),
                    hr(),
                    checkboxInput("tsnecluslabels", "Display Cluster Labels", TRUE),
                    hr(),
                    textInput("custmtsnetitle", "t-SNE Custom Title", "Add a custom title here"),
                    checkboxInput("custmtsnetitlecheck", "Display t-SNE Custom Title", FALSE),
                    br(),
                    textInput("custmtsneclustitle", "KMeans Cluster Custom Title", "Add a custom title here"),
                    checkboxInput("custmtsneclustitlecheck", "Display KMeans Custom Title", FALSE)
                    

                )
                ),
              fluidRow(
                box(width = 12,
                    DT::dataTableOutput('tsne')
                )
                )
      )
      )# Tab Items
      
    
  )
)