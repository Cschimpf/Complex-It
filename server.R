library(shiny)
suppressMessages(library(SOMbrero))
library(cluster)
library(FactoMineR)
library(plot3D)
library(shinyFiles)

server <- function(input, output, session) {
  
  output$complexit_logo <- renderImage(list(src="complexit_logo.png"), 
                                       deleteFile=FALSE)
  

  #### Panel 'Import data'
  ##############################################################################
  dInput <- reactive({
    in.file <- input$file1
    if (is.null(in.file))
      return(NULL)
    
    
    the.sep <- switch(input$sep, "Comma"=",", "Semicolon"=";", "Tab"="\t",
                      "Space"="")
   
    the.quote <- switch(input$quote, "None"="","Double Quote"='"',
                        "Single Quote"="'")
    
    
      the.table <- na.omit(read.csv(in.file$datapath, header=input$header, 
                                    sep=the.sep, quote=the.quote))
    
    
 #right now this just deselects not numeric data columns, later should it auto subset these?
  output$varchoice <- renderUI(div(
    checkboxGroupInput(inputId="varchoice", label="Input variables:",
                       choices=as.list(colnames(the.table)[
                         sapply(the.table, class) %in%
                           c("integer", "numeric")]),
                       selected=as.list(colnames(the.table)[
                         sapply(the.table, class) %in%
                           c("integer", "numeric")])), 
    actionButton(inputId = "subset_data", label = "Subset Data")))
  column_type_identifier(the.table) 
  full_data <<- the.table ###this may be redundant and unnecessary??
  current_data_file <<- the.table[numeric_only_columns]
  the.table#this is set such that the last thing in this method/function is the table and is therefore set to dInput
  
  })
  observeEvent(input$subset_data,{
    d.input <- dInput()
    
    subset_list <- input$varchoice
    if(length(ncol(d.input) >= length(subset_list))){
      #subset_list <- c()
      #for(i in 1:length(input$varchoice)){
        #subset_list <-c(subset_list, input$varchoice[i])
      #}
      current_data_file <<- d.input[subset_list]
    }
    
  
  })
  # data preview table
  # once data is populated, with the dInput call here, it will automatically be populated
  output$view <- renderTable({
    #print(current_data_file)
    d.input <- dInput()
    if (is.null(d.input)) 
      return(NULL)
    if (ncol(full_data)>input$ncol.preview) 
      full_data <- full_data[,1:input$ncol.preview]
    head(full_data, n=input$nrow.preview)
  })
  
  #### Panel 'Cluster Data'
  #############################################################################
  #this observe event looks for users to press 'get clusters'
  observeEvent(input$init_kmeans, { 
    if(is.null(current_data_file)){return()}  #this function will return nothing if there is no data
    #print(current_data_file)
    k <- kmeans(current_data_file, isolate(input$clusters))
    
    
    #for now I left your code here so there is a trace of what changes were made
    #I added 'sol' to the parameters so the kmeans already calculated could be used in this function
    pseudoF = function(X,sol, k){
      #nk = length(k)
      #n = nrow(X)
      T = sum(scale(X,scale=F)^2)
      #W = rep(T, nk)
      #for (i in 1:nk){
      #cli = kmeans(X, k[i], nstart=ns,algorithm="Lloyd")
      #W[i] = sum(cli$withinss)
      #}
      W <- sum(sol$withinss) #this is using the withinss from 'k' above
      pF = ((T-W)/(k-1))/(W/(nrow(X)-k))
      return(pF)
    }
    
    #uses the number of data rows to calculate the height of the silhouette 
    #will return 400 as a default height if the scaled nrows is less than 400
    silhouette_height = function(X){
      height = (nrow(X) * 5)
      if (height < 400) 
      {height = 400}
      return(height)
    }
   
    FSTAT <- pseudoF(current_data_file, k,input$clusters) 
    
    output$kmeans_tab <- renderTable({
      
      #this block of code appends the cluster labels to the data and then writes out to the C:\ directory
      clus <- k$cluster
      k_data <- cbind(current_data_file,clus)
      current_kmeans_solution <<- create_user_saved_kmeans_res("default_name", k$centers, k$cluster, k$size)
      ### Save the original kmeans solution as well to more easily call off-the-shelf prediction routines
      current_kmeans_original_solution <<- k
      
      ### Initialize the predict.kmeans function:
      predict.kmeans=
        function(km, data)
        {k <- nrow(km$centers)
        n <- nrow(data)
        d <- as.matrix(dist(rbind(km$centers, data)))[-(1:k),1:k]
        out <- apply(d, 1, which.min)
        return(out)}
      
      
      ### see if predicted is selected
      if (input$Predicted_Kmeans_solution == TRUE) {
      ### Then predict new results assuming it is in the current data file
      predicted_cluster_data <<- predict.kmeans(current_kmeans_original_solution, current_data_file)
      }
      
  
      
   
      #this block creates the 'Cluster 1, 2...n' labels for the table display in Shiny
      clus_label =c()
      for(i in 1: nrow(k$centers)){
        clus_label = c(clus_label, paste(c("Cluster"), toString(i), sep = " "))
      }
      clus_size <- k$size
      cen_tab <<- cbind(clus_label, k$centers, clus_size)
      
    })
    #initializes the directory and save function
    roots =c(wd='.')
    shinyFileSave(input, 'save', roots=roots)
    observeEvent(input$save, {
      
      fileinfo <- parseSavePath(roots, input$save)
           
      k_data <- append_cluster_labels(current_kmeans_solution, current_data_file)
      ### if predicted is selected, use the new solutions
      if (input$Predicted_Kmeans_solution == TRUE) {
        ### assign k_data to the new cluster values
        current_kmeans_solution_predicted<-current_kmeans_solution
        current_kmeans_solution_predicted@uclusters<-as.list(as.numeric(predicted_cluster_data))
        k_data <- append_cluster_labels(current_kmeans_solution_predicted, current_data_file)
          }
    
            
      write.csv(k_data, as.character(fileinfo$datapath))
      
      #save the cluster summary details
      sumfileinfo <- extend_filename(fileinfo$datapath, "_kstats.")
      summarykstats <- cbind(as.data.frame(current_kmeans_solution@ucentroids), "clus_size" = current_kmeans_solution@usize)
      
      write.csv(summarykstats, sumfileinfo)
      
    })
   
    #this function displays the pseudoF
    if (input$pseudo_f == TRUE) {
      output$pseudoF <- renderText({
        paste("Pseudo F: ", FSTAT)
      })}
    if (input$silhouette == TRUE){
      output$kmeans_silh <- renderPlot({
        dissM <- daisy(current_data_file)
        plot(silhouette(k$cluster, dissM)) 
      }, width = 500, height = silhouette_height(current_data_file))
    }
  })
  #### Panel 'Train the SOM'
  #############################################################################
  output$initproto <- renderUI({
    selectInput("initproto", label="Prototypes initialization method:", 
                choices=c("random","obs","pca"), 
                selected="random")
  })
  
  output$scaling <- renderUI({
    selectInput(inputId="scaling", label="Data scaling:", 
                choices =c("unitvar", "none", "center"),
                selected ="unitvar")
  })

  observeEvent(input$trainbutton, {
    tmp_data <- current_data_file
    mapped_labels = NULL
    if(!is.null(current_kmeans_solution)){
      mapped_labels <- create_kmeans_SOM_mapping()
      rownames(tmp_data) <- mapped_labels
    }
    
    set.seed(input$rand.seed)
    current_som_solution<<- trainSOM(tmp_data, dimension=c(input$dimx,input$dimy), 
             maxit=input$maxit, scaling=input$scaling, init.proto=input$initproto)
    updatePlotSomVar() # update variable choice for som plots
    ########  
  })  
  output$trainnotice <- renderUI({
    if(input$trainbutton == 0){
      return()
    }
    else{
      ### post the quality control factors as well
      qc<-quality(current_som_solution)
      h4(strong(paste("Trained SOM : Topo Error : Quant Error ", format(Sys.time(),format="%Y-%m-%d-%H:%M:%S"),format(qc$topographic,digits=4),format(qc$quantization,digits=4),sep=" : ")))
    }
  })
  #### Panel 'Plot'
  #############################################################################
  observe({
    updateSelectInput(session, "somplottype", 
                      choices=all.somplot.types[["numeric"]][[
                        input$somplotwhat]])
  })
  
  # update variables available for plotting
  updatePlotSomVar <- function() observe({
    tmp.names <- colnames(current_som_solution$data)
   
    updateSelectInput(session, "somplotvar", choices=tmp.names)
    updateSelectInput(session, "somplotvar2", choices=tmp.names, 
                      selected=tmp.names[1:min(5,length(tmp.names))])
  })
  # Plot the SOM
  output$somplot <- renderPlot({
    if(is.null(current_data_file))
      return(NULL)
    if(input$trainbutton ==0)
      return(NULL)
    
    tmp.view <- NULL
    if (input$somplottype =="boxplot") {
      tmp.var <- (1:ncol(current_som_solution$data))[colnames(current_som_solution$data) %in% 
                                                       input$somplotvar2]
      print(tmp.var)
    } 
    else {tmp.var <- input$somplotvar}
    plot(x=current_som_solution, what=input$somplotwhat, type=input$somplottype,
         variable=tmp.var,view=tmp.view)
    
  })
}


########################Discarded Code
# output$download_clusters <- downloadHandler(
#   filename = function() {
#     paste(substr(input$file1$name,1, nchar(input$file1$name)-4), format(Sys.time(),format="_%Y-%m-%d_%H.%M"), ".csv", sep="")
#   },
#   content = function(file){
#     write.csv(cen_tab, file)
#   }, 
#   contentType = "text/csv"
#   
# )