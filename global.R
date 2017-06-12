####Global Variables#####
current_data_file = NULL
current_kmeans_solution = c()

####Global Classes and related Functions####
setClass("user_saved_kmeans_res", representation(save_name = "character", ucentroids = "list", uclusters = "list"))

create_user_saved_kmeans_res <- function(objectname, ucenters, cluster_labels, k_number){
  new_labels <- list_cluster_labels(cluster_labels)
  ucenters <- list(ucenters)
  new_class = new("user_saved_kmeans_res", save_name = objectname, ucentroids = ucenters, uclusters = new_labels)
  return(new_class)
}

list_cluster_labels <-function(unlist){
  new_list =c()
  for(i in 1:length(unlist)){
    new_list = c(new_list, as.numeric(unlist[i]))
  }
  return(list(new_list))
}

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

