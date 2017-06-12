library(shiny)
library(SOMbrero)
library(cluster)
library(FactoMineR)
library(plot3D)

server <- function(input, output) {
  
  output$complexit_logo <- renderImage(list(src="complexit_logo.png"), 
                                       deleteFile=FALSE)
  
  dirs = c("C:\\ComplexIt", "C:\\ComplexIt\\Data from User", "C:\\ComplexIt\\temp")
  for(i in 1:length(dirs)){
    if (dir.exists(dirs[i]) == TRUE) {
      print("Folder already exists")
    }
    else{
      dir.create(dirs[i])
      print("Folder created")
    }
  }
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
    
    if (input$rownames) {
      the.table <- read.csv(in.file$datapath, header=input$header, 
                            sep=the.sep, quote=the.quote, row.names=1,
                            dec=the.dec)
    } else {
      the.table <- na.omit(read.csv(in.file$datapath, header=input$header, 
                                    sep=the.sep, quote=the.quote))
    }
    
 
  output$varchoice <- renderUI(div(
    checkboxGroupInput(inputId="varchoice", label="Input variables:",
                       choices=as.list(colnames(the.table)),
                       selected=as.list(colnames(the.table)[
                         sapply(the.table, class) %in%
                           c("integer", "numeric")])), 
    actionButton(inputId = "subset_data", label = "Subset Data")))
  current_data_file <<- the.table
  print(current_data_file[1:5,])
  the.table #this is set such that the last thing in this method/function is the table and is therefore set to dInput
  
  })
  observeEvent(input$subset_data,{
    d.input <- dInput()
    if(length(ncol(d.input) >= length(input$varchoice))){
      subset_list <- c()
      for(i in 1:length(input$varchoice)){
        subset_list <-c(subset_list, input$varchoice[i])
      }
      current_data_file <<- d.input[subset_list]
    }
  
  })
  # data preview table
  output$view <- renderTable({
    #print(current_data_file)
    d.input <- dInput()
    if (is.null(d.input)) 
      return(NULL)
    if (ncol(current_data_file)>input$ncol.preview) 
      d.input <- d.input[,1:input$ncol.preview]
    head(d.input, n=input$nrow.preview)
  })
  
  #### Panel 'Cluster Data'
  #############################################################################
  #this observe event looks for users to press 'get clusters'
  observeEvent(input$init_kmeans, { 
    if(is.null(current_data_file)){return()}  #this function will return nothing if there is no data
    k <- kmeans(current_data_file, isolate(input$clusters))
    
    
    #for now I left your code here so there is a trace of what changes were made
    #I added 'sol' to the parameters so the kmeans already calculated could be used in this function
    pseudoF = function(X,sol, k, ns = 25){
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
      height = (nrow(X) * 4)
      if (height < 400) 
      {height = 400}
      return(height)
    }
   
    FSTAT <- pseudoF(current_data_file, k,input$clusters) 
    
    output$kmeans_tab <- renderTable({
      
      #this block of code appends the cluster labels to the data and then writes out to the C:\ directory
      clus <- k$cluster
      k_data <- cbind(current_data_file,clus)
      file_a = input$file1
      fpath <- paste(c("C:\\ComplexIt\\temp\\"), file_a$name, sep = "")
      write.csv(k_data, file = fpath)
      #the double arrow declares k_data global to pass the clusters from Kmean to SOM
     
      clustered_data <<- k_data
      current_kmeans_solution <<- create_user_saved_kmeans_res("default_name", k$centers, k$cluster, input$clusters)
      
   
      #this block creates the 'Cluster 1, 2...n' labels for the table display in Shiny
      clus_label =c()
      for(i in 1: nrow(k$centers)){
        clus_label = c(clus_label, paste(c("Cluster"), toString(i), sep = " "))
      }
      clus_size <- k$size
      cen_tab <- cbind(clus_label, k$centers, clus_size)
      
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
      }, width = 400, height = silhouette_height(current_data_file))
    }
  })
  #### Panel 'Train the SOM'
  #############################################################################
  observeEvent(input$trainbutton, {
    #Select the global kmeans clustered data object, except for the last column 
    cluster_data=clustered_data[,-1]
    cluster_tags<-as.character(clustered_data[,ncol(clustered_data)])
    set.seed(255)
    # Train the SOM and created the "somiris" data object 
   
    som.trained<- trainSOM(cluster_data)  #trainSOM is the SOMbrero training algorithm
    print(current_kmeans_solution)
    #somiris
    # pcairis
    #textiris
    # Now, project the multdimensional clusted_data onto a 3D surface with theh Som map
    pca.project <- PCA(rbind(som.trained$prototypes, cluster_data), ncp = 3, graph = FALSE,
                       ind.sup = 1:nrow(som.trained$prototypes)) #PCA is the FactoMineR principal component analysis
    
    #At this point, all the SOM training and PCA projection is complete
    # The remaining code in this section was created by Nathalie at SOMbrero
    # to get the information ready to plot as a 3D mesh
    # She uses Plot3D for the plotting library focusing on the use of two commands:
    # Segments3D will plot the node/segment surface map
    # text3D will superimpose the cluster numbers in the last column on top of the mesh
    
    kmeans_som_labels <- aggregate(cluster_tags, list(som.trained$clustering), table)
    clust_coord <- pca.project$ind.sup$coord
    
    coords <- som.trained$parameters$the.grid$coord
    # the distance matrix is a classic matrix measurement algorithm
    # and is used to determine the distances between array points in the coordinats
    distg <- as.matrix(dist(coords))
    #redirect the console to write everything to a PDF until the dev off command is listed below
    #dev.set()  
    #pdf('./data/somresults.pdf') 
    output$som_3Dplot <- renderPlot({
    # the segmentPCA needs to setup 6 columns (3 for start and 3 for end) for the segments3D command
    segmentpca <- matrix(ncol = 6)
    text.data <- matrix(ncol = max(clustered_data[,ncol(clustered_data)]))
    
    for (ind in 1:nrow(coords)) {
      whereclust <- match(ind, kmeans_som_labels[ ,1])
      if (!is.na(whereclust)) {
        text.data <- rbind(text.data, kmeans_som_labels[whereclust,2:ncol(kmeans_som_labels)])
      } else text.data <- rbind(text.data, rep(0, ncol(kmeans_som_labels)))
      
      sel_nei <- which(distg[ind, ] == 1)
      sel_nei <- sel_nei[sel_nei > ind]
      for (ind2 in seq_along(sel_nei)) {
        segmentpca <- rbind(segmentpca, 
                            c(clust_coord[ind, ], clust_coord[sel_nei[ind2], ]))
      }
    }
    
    #Now that all the manipulations are completed and the for loops complete
    # it is time to clean up and missing or invalid data
    segmentpca <- na.omit(segmentpca)
    text.data <- na.omit(text.data)
    #maxtext helps find the largest label to best scale the text later
    #in the text3D command below
    maxtext <- max(as.vector(text.data))
    
    # Finally ready to plot the 3D mesh using segments3D from the Plot3D package
    # Changing phi and theta might be a future enhancement for users to rotate the plot
    # the "b" and the "red" can be changed for better coloring
    segments3D(segmentpca[,1], segmentpca[,2], segmentpca[,3], segmentpca[,4],
               segmentpca[,5], segmentpca[,6], phi=-30, theta=-10, bty = "b", 
               col="red")
    
    # Now, point by point, loop through and add the cluster numbers to the right
    #nodes in the 3D mesh
    for (ind in 1:nrow(text.data)) {
      nonzeros <- which(text.data[ind, ] != 0)
      delay <- 1
      if (length(nonzeros) > 0) {
        for (ind2 in nonzeros) {
          text3D(clust_coord[ind,1], clust_coord[ind,2],
                 delay*clust_coord[ind,3],
                 labels = paste0(ind, ": ", colnames(text.data)[ind2]), #labels are the text strings
                 cex = sqrt(text.data[ind,ind2]) * 2 / sqrt(maxtext), add = TRUE) #cex sets the default size for text
          delay <- delay + 0.2
        }
      } 
    } 
    # now, show the SOM results, after redirecting the console with dev off
    #dev.off()
    #system2('open', args = './data/somresults.pdf', wait = FALSE)  
    })
    ########  
  })  
}