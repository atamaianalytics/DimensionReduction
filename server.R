

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(data.table)
library(MASS)

library(tidyverse)
library(umap)
library(Rtsne)
library(irlba)


server <- function(input,output,session) {
  filedata <- reactive({
    if(input$useRdata=="TRUE") {
      if(input$rdata=="iris") {
        inputData <- iris
      } else if(input$rdata=="mtcars") {
        inputData <- mtcars
      } else if(input$rdata=="diamonds") {
        inputData <- diamonds 
      } else if(input$rdata=="biopsy") {
        inputData <- biopsy
      } else if(input$rdata=="Melanoma") {
        inputData <- Melanoma
      } else if(input$rdata=="muscle") {
        inputData <- muscle
      } else if(input$rdata=="Pima.te") {
        inputData <- Pima.te
      } else if(input$rdata=="Boston") {
        inputData <- Boston
      }
    }
    else if (input$useRdata=="FALSE") {
    inputData <- input$fileUpload
    if (is.null(inputData))
      return()
    inputData <- read.csv(inputData$datapath,header = TRUE)
    }
    
    if(input$limitcheck=="TRUE") {
      inputData <- head(inputData,input$limitnum)
    } else if(input$limitcheck=="FALSE") {
      inputData <- inputData
    }
    
    if(input$impute=="Remove all NA rows") {
      inputData <- na.omit(inputData)
    } else if (input$impute!="Remove all NA rows") {
      inputData <- inputData
    }
      
  })
  

  colswithna <- reactive({
    df <- filedata()
    
    df <- df %>% 
      dplyr::select_if(function(x) any(is.na(x)))
    
    cols <- colnames(df)
    
    return(cols)
    
  })
  
  
  observe({
    varX <- colnames(filedata())
    #varX <- varX[!(varX %in% input$sampleyvars)]
    updateSelectizeInput(session, "samplexvars", "Dimension Selection", choices = varX)
  })
  
  observe({
    varY <- colnames(filedata())
    varY <- varY[!(varY %in% input$samplexvars)]
    updateSelectizeInput(session, "sampleyvars", "Add Data Class", choices = c("None",varY))
  })

  #cols <- reactive({
  #  colnames(filedata())
    
  #})
  
  # output$dims <- renderUI({
  #   data <- colnames(filedata())
  #   choice <-  data
  #   selectInput("dims","Dimension Selection", choices = choice, selected = choice[1])
  # })
  # 
  # output$foo <- reactive({
  # updateSelectizeInput(session, 'foo', choices = cols(), server = TRUE)
  # })
  # data_test <- reactive({
  #   df <- filedata()
  #   if(input$impute!="None" & input$impute!="Remove all NA rows") {
  #     for (k in colnames(df)) {
  #       if(input$impute=="Median") {
  #         i <- median(df[[k]],na.rm = T)
  #       } else if(input$impute=="Mean") {
  #         i <- mean(df[[k]],na.rm = T)
  #       } else if(input$impute=="0") {
  #         i <- 0
  #       }
  #       set(x = df, which(is.na(df[[k]])), k, i)
  #     }
  #     #return(df)
  #    }
  # })
  

  data_set <- reactive({
    df <- filedata()
    df <- df %>%
      dplyr::select(input$samplexvars)
    
    if(input$impute!="None" & input$impute!="Remove all NA rows") {
      for (k in colnames(df)) {
        if(input$impute=="Impute with Median") {
          i <- median(df[[k]],na.rm = T)
        } else if(input$impute=="Impute with Mean") {
          i <- mean(df[[k]],na.rm = T)
        } else if(input$impute=="Impute with 0") {
          i <- 0
        }
        set(x = df, which(is.na(df[[k]])), k, i)
      }
    }
    

    if(input$fun!="None") {
      if(input$fun=="abs(x)") {
        df <- abs(df)
      } else if(input$fun=="ceiling(x)") {
        df <- ceiling(df)
      } else if(input$fun=="cos(x)") {
        df <- cos(df)
      } else if(input$fun=="exp(x)") {
        df <- exp(df)
      } else if(input$fun=="floor(x)") {
        df <- floor(df)
      } else if(input$fun=="log(x)") {
        df <- log(df)
      } else if(input$fun=="log10(x)") {
        df <- log10(df)
      } else if(input$fun=="sin(x)") {
        df <- sin(df)
      } else if(input$fun=="sqrt(x)") {
        df <- sqrt(df)
      } else if(input$fun=="trunc(x)") {
        df <- trunc(df)
      } else if(input$fun=="tan(x)") {
        df <- tan(df)
      }
      
    }
    
    if(input$scale!="None") {
      if(input$scale=="center = TRUE & scale = TRUE") {
        df <- data.frame(scale(df,center = TRUE,scale = TRUE))
      } else if(input$scale=="center = FALSE & scale = TRUE") {
        df <- data.frame(scale(df,center = FALSE,scale = TRUE))
      } else if(input$scale=="center = TRUE & scale = FALSE") {
        df <- data.frame(scale(df,center = TRUE,scale = FALSE))
      } 
    }
    
    if(input$appround=="TRUE") {
      df <- round(df,input$round)
    }

    return(df)
  })
  
  summpro <- reactive({
    df <- data_set()
    df <- gather(df,
                 key = "attribute",
                  value = "value")
    df_sum <- df %>% group_by(attribute) %>% summarise_all(list(mean = mean,median = median ,min = min, max = max, sd = sd),na.rm = TRUE)

     if(input$appround=="TRUE") {
       df_sum[,2:6] <- round(df_sum[,2:6],input$round)
     }

    df_na <- df %>% group_by(attribute) %>% summarise_all(c("anyNA"))
    df <- cbind(data.frame(df_sum),data.frame(df_na)[,2])
    colnames(df)[7] <- "Has NAs"


    return(df)
  })

  
  seedrandumap <- eventReactive(input$umapseed,{
    if(input$seed=="FALSE") {
      s <- as.integer(runif(1, min = 1000, max = 9999))
    } else if(input$seed=="TRUE") {
      s <- input$useedset
    }
    
  })

  
  seedrandtsne <- eventReactive(input$tsneseed,{
    if(input$seedtsne=="FALSE") {
      s <- as.integer(runif(1, min = 1000, max = 9999))
    } else if(input$seedtsne=="TRUE") {
      s <- input$tseedset
    }
  })
  
############### UMAP ##################################
  

 
  umap_class_df <- eventReactive(input$umapseed,{
    df_input <- filedata()
    df <- data_set()
    seedrand <- as.integer(seedrandumap())
    
    
    custom.settings = umap.defaults
    custom.settings$n_neighbors=input$n_neighbors
    custom.settings$n_components=2
    custom.settings$metric=input$umapmetric
    custom.settings$n_epochs=input$n_epochs
    custom.settings$input="data"
    custom.settings$init="spectral"
    custom.settings$min_dist=input$min_dist
    custom.settings$set_op_mix_ratio=input$set_op_mix_ratio
    custom.settings$local_connectivity=input$local_connectivity
    custom.settings$bandwidth=input$bandwidth
    custom.settings$alpha=input$alpha
    custom.settings$gamma=input$gamma
    custom.settings$negative_sample_rate=input$negative_sample_rate
    custom.settings$a=NA
    custom.settings$b=NA
    custom.settings$spread=input$spread
    custom.settings$random_state=seedrand 
    custom.settings$transform_state=NA
    custom.settings$knn_repeats=input$knn_repeats
    custom.settings$verbose=FALSE
    custom.settings$umap_learn_args=NA
    
    df_umap <- umap(df,config=custom.settings)
    
    class <- filedata() 
    
    if(input$sampleyvars=="None") {
      class <- class %>% mutate(class="None") %>% dplyr::select(class)
    } else if(input$sampleyvars!="None") {
      class <- class %>% dplyr::select(input$sampleyvars)
    }
    
    
    
    df_umap_class <- data.frame(cbind(df_umap$layout,class))
    names(df_umap_class) <- c("x", "y","class")
    return(df_umap_class)
  })
  
  

  umap_clus_df <- eventReactive(input$umapseed,{
    df <- umap_class_df()
    
    seedrand <- as.integer(seedrandumap())
    
    set.seed(seedrand)
    
    clusters <- kmeans(df[,1:2],input$n_clusters,iter.max = input$iter_max, nstart = input$nstart, algorithm = input$kmeansalg)
    clusters <- clusters$cluster
    return(clusters)
  })


  umap_class_clus_df <-eventReactive(input$umapseed,{
    umap_class_df <- umap_class_df()
    umap_clus_df <- umap_clus_df()
      df_umap_class_clus <- cbind(umap_class_df,umap_clus_df)
    return(df_umap_class_clus)
  })

  umap_clus_cent_df <- eventReactive(input$umapseed,{
    umap_class_df <- umap_class_df()
    
    seedrand <- as.integer(seedrandumap())
    
    set.seed(seedrand)
    
    clusters <- kmeans(umap_class_df[,1:2],input$n_clusters,iter.max = input$iter_max, nstart = input$nstart, algorithm = input$kmeansalg)
    umap_clus_cent <- data.frame(clusters$centers)
    umap_clus_cent <- rowid_to_column(umap_clus_cent,var = "cluster")
    return(umap_clus_cent)
  })
  
  dataWithUMAPClusters <- eventReactive(input$umapseed,{
    data <- data_set()
    class <- filedata()
    if(input$sampleyvars=="None") {
      class <- data.frame("None")
      colnames(class) <- c("Class")
    } else if(input$sampleyvars!="None") {
      class <- class %>% dplyr::select(input$sampleyvars)
    }
    clus <- umap_class_clus_df()
    data <- cbind(data,class,clus$umap_clus_df)
    names(data)[names(data) == "clus$umap_clus_df"] <- "ClusterID"
    return(data)
  })

### umap plot

  output$umapplot = renderPlot({
    df_umap_class_clus <- umap_class_clus_df()

    df_umap_class_clus$x <- as.numeric(as.character(df_umap_class_clus$x))
    df_umap_class_clus$y <- as.numeric(as.character(df_umap_class_clus$y))
    
    n <- nrow(df_umap_class_clus)

    umap_clus_cent <- umap_clus_cent_df()

    umap_plot <- NULL

    umap_plot <- ggplot(df_umap_class_clus, aes(x, y))
    
      if((is.numeric(df_umap_class_clus$class) & input$classnum=="TRUE") | !is.numeric(df_umap_class_clus$class)) {
        umap_plot <- umap_plot + geom_point(aes(colour = factor(class)))
      } else if(is.numeric(df_umap_class_clus$class) & input$classnum=="FALSE") {
        umap_plot <- umap_plot + geom_point(aes(colour = class))
      }
      
    umap_plot <- umap_plot + labs(x ="X", y = "Y",colour=input$sampleyvars)

    if(input$custmumaptitlecheck=="FALSE") {
      umap_plot <- umap_plot + ggtitle("UMAP Dimension Reduction")
    } else if(input$custmumaptitlecheck=="TRUE") {
      umap_plot <- umap_plot + ggtitle(paste0(input$custmumaptitle))
    }
       
    
    if(input$umapcluslabels=="TRUE") {
      umap_plot <- umap_plot + 
        geom_point(data=umap_clus_cent, aes(x,y)) +
        geom_text(data=umap_clus_cent, aes(label=cluster),hjust=0, vjust=0, size=6)
    }
      
    if(input$uxaxisbounds=="TRUE") {
      umap_plot <- umap_plot + xlim(input$uxmin,input$uxmax) #xlim(-20,15) #xlim(input$uxmin,input$uxmax)
    }
    
    if(input$uyaxisbounds=="TRUE") {
      umap_plot <- umap_plot + ylim(input$uymin,input$uymax)
    }
      
    
    if(is.numeric(df_umap_class_clus$class) & input$classnum=="FALSE") {
      umap_plot <- umap_plot + scale_colour_gradientn(colours = terrain.colors(12))
    }

    umap_plot <- umap_plot +
      theme(axis.text.x=element_text(size = 10),
            axis.text.y=element_text(size = 10),
            axis.title.x=element_text(size = 10),
            axis.title.y=element_text(size = 10),
            legend.key = element_blank(),
            legend.text =element_text(size = 10),
            legend.position = "right",
            legend.direction = "vertical",
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(linetype = "solid", fill=NA, colour = "black"),
            panel.grid.major = element_line(colour = "lightgrey",size=0.2),
            plot.title = element_text(color="black", size=15, face="bold",hjust = 0.5),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"))

    return(umap_plot)
  }
  )
  
### UMAP plot for printing
  
  umapplotoutput = reactive({
    df_umap_class_clus <- umap_class_clus_df()
    
    df_umap_class_clus$x <- as.numeric(as.character(df_umap_class_clus$x))
    df_umap_class_clus$y <- as.numeric(as.character(df_umap_class_clus$y))
    
    n <- nrow(df_umap_class_clus)
    
    umap_clus_cent <- umap_clus_cent_df()
    umap_plot <- NULL
    
    umap_plot <- ggplot(df_umap_class_clus, aes(x, y))
    
    if((is.numeric(df_umap_class_clus$class) & input$classnum=="TRUE") | !is.numeric(df_umap_class_clus$class)) {
      umap_plot <- umap_plot + geom_point(aes(colour = factor(class)))
    } else if(is.numeric(df_umap_class_clus$class) & input$classnum=="FALSE") {
      umap_plot <- umap_plot + geom_point(aes(colour = class))
    }
    
    umap_plot <- umap_plot + labs(x ="X", y = "Y",colour=input$sampleyvars)
  
    if(input$custmumaptitlecheck=="FALSE") {
      umap_plot <- umap_plot + ggtitle("UMAP Dimension Reduction")
    } else if(input$custmumaptitlecheck=="TRUE") {
      umap_plot <- umap_plot + ggtitle(paste0(input$custmumaptitle))
    }
    
    
    if(input$umapcluslabels=="TRUE") {
      umap_plot <- umap_plot + 
        geom_point(data=umap_clus_cent, aes(x,y)) +
        geom_text(data=umap_clus_cent, aes(label=cluster),hjust=0, vjust=0, size=6)
    }
    
    if(input$uxaxisbounds=="TRUE") {
      umap_plot <- umap_plot + xlim(input$uxmin,input$uxmax) #xlim(-20,15) #xlim(input$uxmin,input$uxmax)
    }
    
    if(input$uyaxisbounds=="TRUE") {
      umap_plot <- umap_plot + ylim(input$uymin,input$uymax)
    }
    
    
    
    if(is.numeric(df_umap_class_clus$class) & input$classnum=="FALSE") {
      umap_plot <- umap_plot + scale_colour_gradientn(colours = terrain.colors(12))
    }
    
    umap_plot <- umap_plot +
      theme(axis.text.x=element_text(size = 10),
            axis.text.y=element_text(size = 10),
            axis.title.x=element_text(size = 10),
            axis.title.y=element_text(size = 10),
            legend.key = element_blank(),
            legend.text =element_text(size = 10),
            legend.position = "right",
            legend.direction = "vertical",
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(linetype = "solid", fill=NA, colour = "black"),
            panel.grid.major = element_line(colour = "lightgrey",size=0.2),
            plot.title = element_text(color="black", size=15, face="bold",hjust = 0.5),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"))
    
    return(umap_plot)
  }
  )

### umap cluster
  
  output$umapcluster = renderPlot({
    df_umap_class_clus <- umap_class_clus_df()

    df_umap_class_clus$x <- as.numeric(as.character(df_umap_class_clus$x))
    df_umap_class_clus$y <- as.numeric(as.character(df_umap_class_clus$y))

    umap_clus_cent <- umap_clus_cent_df()
    
    umap_clus <- NULL

    umap_clus <- ggplot(df_umap_class_clus, aes(x, y)) +
      geom_point(aes(colour = factor(umap_clus_df))) +
      labs(x ="X", y = "Y",colour="Cluster")
    
    if(input$uxaxisbounds=="TRUE") {
      umap_clus <- umap_clus + xlim(input$uxmin,input$uxmax) #xlim(-20,15) #xlim(input$uxmin,input$uxmax)
    }
    
    if(input$uyaxisbounds=="TRUE") {
      umap_clus <- umap_clus + ylim(input$uymin,input$uymax)
    }
      
      if(input$custmumapclustitlecheck=="FALSE") {
        umap_clus <- umap_clus + ggtitle("UMAP Dimension Reduction with KMeans Clusters")
      } else if(input$custmumapclustitlecheck=="TRUE") {
        umap_clus <- umap_clus + ggtitle(paste0(input$custmumapclustitle))
      }
      
    if(input$umapcluslabels=="TRUE") {
      umap_clus <- umap_clus + 
        geom_point(data=umap_clus_cent, aes(x,y)) +
        geom_text(data=umap_clus_cent, aes(label=cluster),hjust=0, vjust=0, size=6)
    }

    umap_clus <- umap_clus +
      theme(axis.text.x=element_text(size = 10),
            axis.text.y=element_text(size = 10),
            axis.title.x=element_text(size = 10),
            axis.title.y=element_text(size = 10),
            legend.key = element_blank(),
            legend.text =element_text(size = 10),
            legend.position = "right",
            legend.direction = "vertical",
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(linetype = "solid", fill=NA, colour = "black"),
            panel.grid.major = element_line(colour = "lightgrey",size=0.2),
            plot.title = element_text(color="black", size=15, face="bold",hjust = 0.5),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"))

    return(umap_clus)
  }
  )
  
#### UMAP cluster for printing
  
  umapclusplotoutput = reactive({
    df_umap_class_clus <- umap_class_clus_df()
    
    df_umap_class_clus$x <- as.numeric(as.character(df_umap_class_clus$x))
    df_umap_class_clus$y <- as.numeric(as.character(df_umap_class_clus$y))
    
    umap_clus_cent <- umap_clus_cent_df()
  
    umap_clus <- NULL
    
    umap_clus <- ggplot(df_umap_class_clus, aes(x, y)) +
      geom_point(aes(colour = factor(umap_clus_df))) +
      labs(x ="X", y = "Y",colour="Cluster")
    
    if(input$uxaxisbounds=="TRUE") {
      umap_clus <- umap_clus + xlim(input$uxmin,input$uxmax) #xlim(-20,15) #xlim(input$uxmin,input$uxmax)
    }
    
    if(input$uyaxisbounds=="TRUE") {
      umap_clus <- umap_clus + ylim(input$uymin,input$uymax)
    }
    
    if(input$custmumapclustitlecheck=="FALSE") {
      umap_clus <- umap_clus + ggtitle("UMAP Dimension Reduction with KMeans Clusters")
    } else if(input$custmumapclustitlecheck=="TRUE") {
      umap_clus <- umap_clus + ggtitle(paste0(input$custmumapclustitle))
    }
    
    if(input$umapcluslabels=="TRUE") {
      umap_clus <- umap_clus + 
        geom_point(data=umap_clus_cent, aes(x,y)) +
        geom_text(data=umap_clus_cent, aes(label=cluster),hjust=0, vjust=0, size=6)
    }
    
    umap_clus <- umap_clus +
      theme(axis.text.x=element_text(size = 10),
            axis.text.y=element_text(size = 10),
            axis.title.x=element_text(size = 10),
            axis.title.y=element_text(size = 10),
            legend.key = element_blank(),
            legend.text =element_text(size = 10),
            legend.position = "right",
            legend.direction = "vertical",
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(linetype = "solid", fill=NA, colour = "black"),
            panel.grid.major = element_line(colour = "lightgrey",size=0.2),
            plot.title = element_text(color="black", size=15, face="bold",hjust = 0.5),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"))
    
    return(umap_clus)
  })

#################### t-sne #######################################
  
  
  tsne_class_df <- eventReactive(input$tsneseed,{
    df_input <- filedata()
    df <- data_set()

    seedrand <- as.integer(seedrandtsne()) 
    
   
    set.seed(seedrand)
    
    df_tsne <- Rtsne(
                df, 
                dims = 2, 
                initial_dims = input$initial_dims,
                perplexity = input$perplexity, 
                theta = input$theta, 
                check_duplicates = FALSE,
                pca = input$pca, 
                partial_pca = FALSE, #input$partial_pca, 
                max_iter = input$max_iter,
                verbose = getOption("verbose", FALSE), 
                is_distance = FALSE,
                Y_init = NULL, 
                pca_center = input$pca_center, 
                pca_scale = input$pca_scale,
                normalize = input$normalise, 
                momentum = input$momentum, 
                final_momentum = input$final_momentum, 
                eta = 200,
                exaggeration_factor = input$exaggeration_factor, 
                num_threads = input$num_threads)
    
    class <- filedata() 
    
    if(input$sampleyvars=="None") {
      class <- class %>% mutate(class="None") %>% dplyr::select(class)
    } else if(input$sampleyvars!="None") {
      class <- class %>% dplyr::select(input$sampleyvars)
    }
    
    df_tsne_class <- data.frame(cbind(df_tsne$X,df_tsne$Y))
    
    df_tsne_class <- cbind(df_tsne_class,class)
    
    names(df_tsne_class) <- c("x", "y","class")
    return(df_tsne_class)
    
    
  })
  
  tsne_clus_df <- eventReactive(input$tsneseed,{
    df <- tsne_class_df()
    seedrand <- as.integer(seedrandtsne())
    
    set.seed(seedrand)
    
    clusters <- kmeans(df[,1:2],input$tsnen_clusters,iter.max = input$tsneiter_max, nstart = input$tsnenstart, algorithm = input$tsnekmeansalg)
    clusters <- clusters$cluster
    return(clusters)
  })
  

  tsne_class_clus_df <-eventReactive(input$tsneseed,{
    tsne_class_df <- tsne_class_df()
    tsne_clus_df <- tsne_clus_df()
    
    df_tsne_class_clus <- cbind(tsne_class_df,tsne_clus_df)
    return(df_tsne_class_clus)
  })
  
  
  tsne_clus_cent_df <- eventReactive(input$tsneseed,{
    tsne_class_df <- tsne_class_df()
    
    seedrand <- as.integer(seedrandtsne())
    
    set.seed(seedrand)
    
    clusters <- kmeans(tsne_class_df[,1:2],input$tsnen_clusters,iter.max = input$tsneiter_max, nstart = input$tsnenstart, algorithm = input$tsnekmeansalg)
    tsne_clus_cent <- data.frame(clusters$centers)
    tsne_clus_cent <- rowid_to_column(tsne_clus_cent,var = "cluster")
    return(tsne_clus_cent)
  })
  
  
  dataWithtSNEClusters <- eventReactive(input$tsneseed,{
    data <- data_set()
    class <- filedata()
    if(input$sampleyvars=="None") {
      class <- data.frame("None")
      colnames(class) <- c("Class")
    } else if(input$sampleyvars!="None") {
      class <- class %>% dplyr::select(input$sampleyvars)
    }
    
    clus <- tsne_class_clus_df()
    data <- cbind(data,class,clus$tsne_clus_df)
    
    names(data)[names(data) == "clus$tsne_clus_df"] <- "ClusterID"
    return(data)
  })
  
  ### tsne plot
  
  output$tsneplot = renderPlot({
    df_tsne_class_clus <- tsne_class_clus_df()
    
    df_tsne_class_clus$x <- as.numeric(as.character(df_tsne_class_clus$x))
    df_tsne_class_clus$y <- as.numeric(as.character(df_tsne_class_clus$y))
    
    n <- nrow(df_tsne_class_clus)
    
    tsne_clus_cent <- tsne_clus_cent_df()
    
    tsne_plot <- NULL
    
    tsne_plot <- ggplot(df_tsne_class_clus, aes(x, y))
    
    if((is.numeric(df_tsne_class_clus$class) & input$classnum=="TRUE") | !is.numeric(df_tsne_class_clus$class)) {
      tsne_plot <- tsne_plot + geom_point(aes(colour = factor(class)))
    } else if(is.numeric(df_tsne_class_clus$class) & input$classnum=="FALSE") {
      tsne_plot <- tsne_plot + geom_point(aes(colour = class))
    }
    
    tsne_plot <- tsne_plot + labs(x ="X", y = "Y",colour=input$sampleyvars)

    if(input$custmtsnetitlecheck=="FALSE") {
      tsne_plot <- tsne_plot + ggtitle("t-SNE Dimension Reduction")
    } else if(input$custmtsnetitlecheck=="TRUE") {
      tsne_plot <- tsne_plot + ggtitle(paste0(input$custmtsnetitle))
    }
    
    
    if(input$tsnecluslabels=="TRUE") {
      tsne_plot <- tsne_plot + 
        geom_point(data=tsne_clus_cent, aes(x,y)) +
        geom_text(data=tsne_clus_cent, aes(label=cluster),hjust=0, vjust=0, size=6)
    }
    
    if(input$txaxisbounds=="TRUE") {
      tsne_plot <- tsne_plot + xlim(input$txmin,input$txmax) 
    }
    
    if(input$tyaxisbounds=="TRUE") {
      tsne_plot <- tsne_plot + ylim(input$tymin,input$tymax)
    }
    
    
    
    if(is.numeric(df_tsne_class_clus$class) & input$classnum=="FALSE") {
      tsne_plot <- tsne_plot + scale_colour_gradientn(colours = terrain.colors(12))
    }
    
    tsne_plot <- tsne_plot +
      theme(axis.text.x=element_text(size = 10),
            axis.text.y=element_text(size = 10),
            axis.title.x=element_text(size = 10),
            axis.title.y=element_text(size = 10),
            legend.key = element_blank(),
            legend.text =element_text(size = 10),
            legend.position = "right",
            legend.direction = "vertical",
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(linetype = "solid", fill=NA, colour = "black"),
            panel.grid.major = element_line(colour = "lightgrey",size=0.2),
            plot.title = element_text(color="black", size=15, face="bold",hjust = 0.5),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"))
    
    return(tsne_plot)
    

  }
  )
  
  #### tsne plot for printing
    
  tsneplotoutput = reactive({
    df_tsne_class_clus <- tsne_class_clus_df()
    
    df_tsne_class_clus$x <- as.numeric(as.character(df_tsne_class_clus$x))
    df_tsne_class_clus$y <- as.numeric(as.character(df_tsne_class_clus$y))
    
    n <- nrow(df_tsne_class_clus)
    
    tsne_clus_cent <- tsne_clus_cent_df()
    
    tsne_plot <- NULL
    
    tsne_plot <- ggplot(df_tsne_class_clus, aes(x, y))
    
    if((is.numeric(df_tsne_class_clus$class) & input$classnum=="TRUE") | !is.numeric(df_tsne_class_clus$class)) {
      tsne_plot <- tsne_plot + geom_point(aes(colour = factor(class)))
    } else if(is.numeric(df_tsne_class_clus$class) & input$classnum=="FALSE") {
      tsne_plot <- tsne_plot + geom_point(aes(colour = class))
    }
    
    tsne_plot <- tsne_plot + labs(x ="X", y = "Y",colour=input$sampleyvars)
    
    if(input$custmtsnetitlecheck=="FALSE") {
      tsne_plot <- tsne_plot + ggtitle("t-SNE Dimension Reduction")
    } else if(input$custmtsnetitlecheck=="TRUE") {
      tsne_plot <- tsne_plot + ggtitle(paste0(input$custmtsnetitle))
    }
    
    
    if(input$tsnecluslabels=="TRUE") {
      tsne_plot <- tsne_plot + 
        geom_point(data=tsne_clus_cent, aes(x,y)) +
        geom_text(data=tsne_clus_cent, aes(label=cluster),hjust=0, vjust=0, size=6)
    }
    
    if(input$txaxisbounds=="TRUE") {
      tsne_plot <- tsne_plot + xlim(input$txmin,input$txmax)
    }
    
    if(input$tyaxisbounds=="TRUE") {
      tsne_plot <- tsne_plot + ylim(input$tymin,input$tymax)
    }
    
    if(input$txaxisbounds=="TRUE") {
      tsne_plot <- tsne_plot + xlim(input$txmin,input$txmax)
    }
    
    if(input$tyaxisbounds=="TRUE") {
      tsne_plot <- tsne_plot + ylim(input$tymin,input$tymax)
    }
    
    
    
    if(is.numeric(df_tsne_class_clus$class) & input$classnum=="FALSE") {
      tsne_plot <- tsne_plot + scale_colour_gradientn(colours = terrain.colors(12))
    }
    
    tsne_plot <- tsne_plot +
      theme(axis.text.x=element_text(size = 10),
            axis.text.y=element_text(size = 10),
            axis.title.x=element_text(size = 10),
            axis.title.y=element_text(size = 10),
            legend.key = element_blank(),
            legend.text =element_text(size = 10),
            legend.position = "right",
            legend.direction = "vertical",
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(linetype = "solid", fill=NA, colour = "black"),
            panel.grid.major = element_line(colour = "lightgrey",size=0.2),
            plot.title = element_text(color="black", size=15, face="bold",hjust = 0.5),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"))
    
    return(tsne_plot)
    
  })
  
  ### tsne cluster
  
  output$tsnecluster = renderPlot({
    df_tsne_class_clus <- tsne_class_clus_df()

    df_tsne_class_clus$x <- as.numeric(as.character(df_tsne_class_clus$x))
    df_tsne_class_clus$y <- as.numeric(as.character(df_tsne_class_clus$y))

    tsne_clus_cent <- tsne_clus_cent_df()
    
    tsne_clus <- NULL

    tsne_clus <- ggplot(df_tsne_class_clus, aes(x, y)) +
      geom_point(aes(colour = factor(tsne_clus_df))) +
      labs(x ="X", y = "Y",colour="Cluster")
    
    
    if(input$txaxisbounds=="TRUE") {
      tsne_clus <- tsne_clus + xlim(input$txmin,input$txmax) #xlim(-20,15) #xlim(input$uxmin,input$uxmax)
    }
    
    if(input$tyaxisbounds=="TRUE") {
      tsne_clus <- tsne_clus + ylim(input$tymin,input$tymax)
    }
    
    if(input$custmtsneclustitlecheck=="FALSE") {
      tsne_clus <- tsne_clus + ggtitle("t-SNE Dimension Reduction with KMeans Clusters")
    } else if(input$custmtsneclustitlecheck=="TRUE") {
      tsne_clus <- tsne_clus + ggtitle(paste0(input$custmtsneclustitle))
    }

    if(input$tsnecluslabels=="TRUE") {
      tsne_clus <- tsne_clus +
        geom_point(data=tsne_clus_cent, aes(x,y)) +
        geom_text(data=tsne_clus_cent, aes(label=cluster),hjust=0, vjust=0, size=6)
    }
    

    tsne_clus <- tsne_clus +
      theme(axis.text.x=element_text(size = 10),
            axis.text.y=element_text(size = 10),
            axis.title.x=element_text(size = 10),
            axis.title.y=element_text(size = 10),
            legend.key = element_blank(),
            legend.text =element_text(size = 10),
            legend.position = "right",
            legend.direction = "vertical",
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(linetype = "solid", fill=NA, colour = "black"),
            panel.grid.major = element_line(colour = "lightgrey",size=0.2),
            plot.title = element_text(color="black", size=15, face="bold",hjust = 0.5),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"))

    return(tsne_clus)
  }
  )
  
  #### tsne cluster for printing
  
  tsneclusplotoutput = reactive({
    
    df_tsne_class_clus <- tsne_class_clus_df()
    
    df_tsne_class_clus$x <- as.numeric(as.character(df_tsne_class_clus$x))
    df_tsne_class_clus$y <- as.numeric(as.character(df_tsne_class_clus$y))
    
    tsne_clus_cent <- tsne_clus_cent_df()
    
    tsne_clus <- NULL
    
    tsne_clus <- ggplot(df_tsne_class_clus, aes(x, y)) +
      geom_point(aes(colour = factor(tsne_clus_df))) +
      labs(x ="X", y = "Y",colour="Cluster")
    
    if(input$txaxisbounds=="TRUE") {
      tsne_clus <- tsne_clus + xlim(input$txmin,input$txmax)
    }
    
    if(input$tyaxisbounds=="TRUE") {
      tsne_clus <- tsne_clus + ylim(input$tymin,input$tymax)
    }
    
    if(input$custmtsneclustitlecheck=="FALSE") {
      tsne_clus <- tsne_clus + ggtitle("t-SNE Dimension Reduction with KMeans Clusters")
    } else if(input$custmtsneclustitlecheck=="TRUE") {
      tsne_clus <- tsne_clus + ggtitle(paste0(input$custmtsneclustitle))
    }
    
    if(input$tsnecluslabels=="TRUE") {
      tsne_clus <- tsne_clus +
        geom_point(data=tsne_clus_cent, aes(x,y)) +
        geom_text(data=tsne_clus_cent, aes(label=cluster),hjust=0, vjust=0, size=6)
    }
    
    
    tsne_clus <- tsne_clus +
      theme(axis.text.x=element_text(size = 10),
            axis.text.y=element_text(size = 10),
            axis.title.x=element_text(size = 10),
            axis.title.y=element_text(size = 10),
            legend.key = element_blank(),
            legend.text =element_text(size = 10),
            legend.position = "right",
            legend.direction = "vertical",
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(linetype = "solid", fill=NA, colour = "black"),
            panel.grid.major = element_line(colour = "lightgrey",size=0.2),
            plot.title = element_text(color="black", size=15, face="bold",hjust = 0.5),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"))
    
    return(tsne_clus)
    
  })
  
#################### data table ##################################    
  
  output$data <- DT::renderDataTable(
    DT::datatable(
      if(input$dview == "Pre-processed Dataset") {
      data = filedata()
      } else if(input$dview == "Processed Dataset" & input$sampleyvars!="None") {
        data <- data_set()
        class <- filedata()
        class <- class %>% dplyr::select(input$sampleyvars)
        data <- cbind(data,class)
      } else if(input$dview == "Processed Dataset" & input$sampleyvars=="None") {
        data = data_set()
      } else if(input$dview=="Summary") {
        data = summpro()
      },options = list(pageLength = 100,scrollX = TRUE),
                  rownames = FALSE)

  )
  
  output$nrow <- renderPrint({
      data <- nrow(filedata())
      data <- data.frame(data)
      colnames(data) <- c("No. Observations")
      print(data)
  })
  
  output$printuseed <- renderPrint({
    data <- data.frame(seedrandumap())
    colnames(data) <- c("Current Seed")
    print(data)
  })
  
  output$printtseed <- renderPrint({
    data <- data.frame(seedrandtsne())
    colnames(data) <- c("Current Seed")
    print(data)
  })
  
  output$renderprint <- renderPrint({
    na <- data.frame(colswithna())
    nrow(na)
    nona <- data.frame("None")
    
    if(nrow(na)>0) {
      colnames(na) <- c("Columns with missing values")
      print(na)
    }
    else if(nrow(na)<1) {
      colnames(nona) <- c("Columns with missing values")
      print(nona)
    }
    
  })
  
  output$dataview.csv <- downloadHandler(
    filename = function(){"dataview.csv"},
    content = function(fname){
      if(input$dview == "Pre-processed Dataset") {
      write.csv(filedata(), fname,row.names = FALSE)
      } else if(input$dview == "Processed Dataset" & input$sampleyvars!="None") {
        data <- data_set()
        class <- filedata() 
        class <- class %>% dplyr::select(input$sampleyvars)
        data <- cbind(data,class)
        write.csv(data, fname,row.names = FALSE)
      } else if(input$dview == "Processed Dataset" & input$sampleyvars=="None") {
        write.csv(data_set(), fname,row.names = FALSE)
      } else if(input$dview=="Summary") {
        write.csv(summpro(), fname,row.names = FALSE)
      }
    }
  )
  
  
  output$umap <- DT::renderDataTable(
    DT::datatable(
        data = dataWithUMAPClusters()
        ,options = list(pageLength = 100,scrollX = TRUE),
      rownames = FALSE)
    
  )
  
  output$tsne <- DT::renderDataTable(
    DT::datatable(
      data = dataWithtSNEClusters()
      ,options = list(pageLength = 100,scrollX = TRUE),
      rownames = FALSE)
    
  )
  
  output$umapdata.csv <- downloadHandler(
    filename = function(){"umapdata.csv"},
    content = function(fname){
        write.csv(dataWithUMAPClusters(), fname,row.names = FALSE)
      }
  )
  
  output$umapplot.png <- downloadHandler(
    filename = function() { paste("UMAPPlot", ".png", sep='') },
    content = function(file) {
      ggsave(file, plot = umapplotoutput(), device = "png",width = 20, height = 20, units = "cm")
    }
  )
  
  output$umapclusplot.png <- downloadHandler(
    filename = function() { paste("UMAPClusterPlot", ".png", sep='') },
    content = function(file) {
      ggsave(file, plot = umapclusplotoutput(), device = "png",width = 20, height = 20, units = "cm")
    }
  )
  
  output$tsnedata.csv <- downloadHandler(
    filename = function(){"tsnedata.csv"},
    content = function(fname){
      write.csv(dataWithtSNEClusters(), fname,row.names = FALSE)
    }
  )
  
  output$tsneplot.png <- downloadHandler(
    filename = function() { paste("tSNEPlot", ".png", sep='') },
    content = function(file) {
      ggsave(file, plot = tsneplotoutput(), device = "png",width = 20, height = 20, units = "cm")
    }
  )
  
  output$tsneclusplot.png <- downloadHandler(
    filename = function() { paste("tSNEClusterPlot", ".png", sep='') },
    content = function(file) {
      ggsave(file, plot = tsneclusplotoutput(), device = "png",width = 20, height = 20, units = "cm")
    }
  )
  
  
}
