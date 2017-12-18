####Global Variables#####


# Any code in this global.r file will run one time prior to launching
# the Shiny App.  The code will not continually scan in the background
# while the App runs, so a good place for intialization of variables using
# code

#Data Variables
####
current_data_file = NULL
full_data = NULL
current_kmeans_solution = NULL
current_som_solution = NULL
previous_som = NULL


#Reference Variables

#Load the previous SOM and rename the workspace variable then remove the old one
load("./Agent_SOM_Object")
Agent_SOM_loaded=previous_som
rm("previous_som")
#Input for a 5 X 5 SOM 
SOMgriddata <- structure(c(1:25), .Dim = c(5L,5L))


#this gets the cluster
the.table_agent_clusters <- read.csv("Agent_Cluster_Data.csv")

#this gets the cases
the.table_agent_cases <- read.csv("Agent_Case_Data.csv")


the.table_agent_clusters <- read.csv("Agent_Cluster_Data.csv")
####
#numeric_only_columns = NULL
#other_columns = NULL
all.somplot.types <- list("numeric"=
                            list("prototypes"=
                                   list("3d", "barplot",
                                        "smooth distances"="smooth.dist",
                                        "U matrix distances"="umatrix"),
                                 "obs"=c("color", "barplot", 
                                         "names", "boxplot")))

####Global Classes and related Functions####
setClass("user_gen_kmeans_solution", representation(save_name = "character", uclusters = "integer", ucenters = "matrix",
                                                    utotss = "numeric", uwithinss = "numeric", utotwithinss = "numeric",
                                                    ubetweenss = "numeric", usize = "integer"))

create_user_gen_kmeans_solution <- function(objectname, km){
  new_class = new("user_gen_kmeans_solution", save_name = objectname, uclusters = km$cluster, ucenters = km$centers, utotss = km$totss,
                  uwithinss = km$withinss, utotwithinss = km$tot.withinss, ubetweenss = km$betweenss, usize = km$size)
  return(new_class)
}
                                            
                    

setClass("user_saved_kmeans_res", representation(save_name = "character", ucentroids = "list", uclusters = "list", usize = "integer"))

create_user_saved_kmeans_res <- function(objectname, ucenters, cluster_labels, k_size){
  new_labels <- list_cluster_labels(cluster_labels)
  ucenters <- list(ucenters)
  new_class = new("user_saved_kmeans_res", save_name = objectname, ucentroids = ucenters, uclusters = new_labels, usize = k_size)
  return(new_class)
}

list_cluster_labels <-function(unlist){
  new_list = list()
  for(i in 1:length(unlist)){
    new_list[[i]] = as.numeric(unlist[i])
  }
  return(new_list)
}

setGeneric(name="append_cluster_labels", 
           def = function(cluster_obj, data_obj){
             
             standardGeneric("append_cluster_labels")
           })

setMethod(f = "append_cluster_labels", 
          signature = "user_saved_kmeans_res",
          definition = function(cluster_obj, data_obj){
            
            clus_labels =c()
            for(i in 1: length(cluster_obj@uclusters)){
              newnum <- as.numeric(cluster_obj@uclusters[i])
              clus_labels = c(clus_labels, newnum)
            }
            appended_labels <- cbind(data_obj, clus_labels)
            return(appended_labels)
          })


#####General Purpose Functions####


#### 'General'
##################################

extend_filename <- function(filename, ext){
 
  splitname = strsplit(as.character(filename), "[.]")
  
  if(length(splitname[[1]]) == 3){
    splitname[[1]][2] = paste(splitname[[1]][2], ext, sep="")
    finalname = paste('.', splitname[[1]][2], splitname[[1]][3], sep="")
  }
  return(finalname)
}

#uses the number of data rows to calculate a dimension for the graph, width/height 
#will return 400 as a default height if the scaled nrows is less than 400
graph_dimension = function(data, scale = 5){
  dimension = (nrow(data) * scale)
  if (dimension < 400) 
  {dimension = 400}
  return(dimension)
}


####Panel 'Import Data'
##################################

column_type_identifier <-function(data){
  numeric_only_columns <-sapply(data, class) %in% c("integer", "numeric")
  return(numeric_only_columns)
  #other_columns <<- logic_vector_inverter(numeric_only_columns)
}

logic_vector_inverter <- function(logic_vec){
  new_logic_vec = logic_vec
  for(i in 1:length(logic_vec)){
    if(logic_vec[i] == TRUE){
      new_logic_vec[i] = FALSE
    }
    else{
      new_logic_vec[i] = TRUE
    }
  }
  return(new_logic_vec)
}


####Panel 'Cluster Data'
##################################

#for now I left your code here so there is a trace of what changes were made

pseudoF = function(data,sol, k){
  #nk = length(k)
  #n = nrow(X)
  T = sum(scale(data, scale=F)^2)
  #W = rep(T, nk)
  #for (i in 1:nk){
  #cli = kmeans(X, k[i], nstart=ns,algorithm="Lloyd")
  #W[i] = sum(cli$withinss)
  #}
  W <- sum(sol@uwithinss) #this is using the withinss from 'k' above
  pF = ((T-W)/(k-1))/(W/(nrow(data)-k))
  return(pF)
}

plot_silhouette <- function(data, km){
  dissM <- daisy(data)
  sil_plot <- plot(silhouette(km@uclusters, dissM)) 
  return(sil_plot)
}

####Panel 'Self Organize'
##################################

#this creates a list of rownames that have the pattern: "clus#_Xcase#"
create_kmeans_SOM_mapping <- function(){
  if(is.null(current_kmeans_solution)){return()}
  label_merge = c()
  for(i in 1:length(current_kmeans_solution@uclusters)){
    new_label <- paste(current_kmeans_solution@uclusters[i], ";", i, sep="")
    label_merge =c(label_merge, new_label)
  }
  return(label_merge)
  
}



###############Still Under Development for Future Versions###############

list_cluster_centers <-function(unlist, k_number) {
  
}
#in order to this right it will need to maybe have its own class
#that starts with the name of the var clustered and then includes
#the centers below it. Right now can't retrieve var names and
#wonder if its useful to put the centroids into a list with no var name
#references 
create_centers_indexes <-function(unlist, k_number){
  for (i in 1:(length(unlist)/k_number)){
    print(5)
  }
}

convert_list_to_vector <-function(a_list){
  new_vec =c()
  for(i in 1:length(current_kmeans_solution@uclusters)){
    new_vec = c(new_vec, as.integer(current_kmeans_solution@uclusters[i]))
  }
  return(new_vec)
}