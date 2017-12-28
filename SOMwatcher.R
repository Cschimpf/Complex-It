# SOMwatcher.r 12/2/2017
#
# Plot the SOM map in the left of the Plot Window and case/cluster assignment history in the right of the Plot Windos
# Keep rerunning the code  and watch the where the predict function puts the neuron
#

# Load in the need libraries
suppressMessages(library(SOMbrero))
library(plotrix) 


#Load the previous SOM and rename the workspace variable then remove the old one
load("./Agent_SOM_Object")
Agent_SOM <- previous_som

#Input for a 5 X 5 SOM 
SOMgriddata <- structure(c(1:25), .Dim = c(5L,5L))
SOMgriddatanew<-SOMgriddata #for the clusters
SOMgriddatanewcases<-SOMgriddata #for the cases

#get the clusters and save a baseline
the.table_agent_clusters_baseline <- read.csv("Agent_Cluster_Data.csv")
the.table_agent_clusters <- the.table_agent_clusters_baseline

#get the cases and save a baseline
the.table_agent_cases_baseline <- read.csv("Agent_Case_Data.csv")
the.table_agent_cases <- the.table_agent_cases_baseline

#Initialize the plot holding array, say, 25 runs
neuronnumber<-integer(25)
neuronnumbercases<-integer(25)
testrun<-1:25
i=1

#Plot the baseline plots of the clusters and neurons
#plot the neuron map
par(mfrow=c(2,2))
color2D.matplot(SOMgriddatanew, show.values = TRUE, axes = FALSE, xlab = "clusters", ylab = "", vcex = 2, vcol = "black",extremes = c("white", "blue"),na.color="black")
plot(testrun,neuronnumber)
points(testrun[i], neuronnumber[i], pch = 21, col = "red", bg = "yellow")
color2D.matplot(SOMgriddatanewcases, show.values = TRUE, axes = FALSE, xlab = "cases", ylab = "", vcex = 2, vcol = "black",extremes = c("white", "blue"),na.color="black")
plot(testrun,neuronnumbercases)
points(testrun[i], neuronnumbercases[i], pch = 21, col = "red", bg = "yellow")

##
##
## NOW PREDICT AND WATCH BY RUNNING THE CODE FROM HERE DOWN UNTIL YOU USE UP THE 25 RUNS
##    OR ADD SOME MORE THAN 25 IF YOU NEED THAT MANY
##
##
##

#Pick variables, cases, or clusters you want keep changing
#try a few clusters
the.table_agent_clusters$Sepal.L[1]<-the.table_agent_clusters$Sepal.L[1]*1.05
the.table_agent_clusters$Sepal.W[1]<-the.table_agent_clusters$Sepal.W[1]*0.96
the.table_agent_clusters$Petal.L[2]<-the.table_agent_clusters$Petal.L[2]*1.05
the.table_agent_clusters$Petal.W[2]<-the.table_agent_clusters$Petal.W[2]*0.95
the.table_agent_clusters$Petal.L[3]<-the.table_agent_clusters$Petal.L[3]*1.05
the.table_agent_clusters$Petal.W[3]<-the.table_agent_clusters$Petal.W[3]*0.95
# try three various cases for example
the.table_agent_cases$Sepal.L[1]<-the.table_agent_cases$Sepal.L[1]*1.2
the.table_agent_cases$Sepal.L[20]<-the.table_agent_cases$Sepal.L[20]*1.2
the.table_agent_cases$Sepal.L[100]<-the.table_agent_cases$Sepal.L[100]*1.2

#Predict the Cluster Neurons from the centroids
Agents_Clusters_predicted <- predict(Agent_SOM, the.table_agent_clusters[,2:(as.numeric(length(the.table_agent_clusters))-1)])

#Predict the Cases Neurons from the Cases
Agents_Cases_predicted <- predict(Agent_SOM,the.table_agent_cases[,2:(as.numeric(length(the.table_agent_cases))-1)])
Agents_Cases_predicted_subset<-Agents_Cases_predicted[c(1,20,100)]
#First, initialize the Agent_SOM to zeros creating an empty array
for (m in 1:length(SOMgriddatanew)){ 
  SOMgriddatanew[m]<-0
}
for (m in 1:length(SOMgriddatanewcases)){ 
  SOMgriddatanewcases[m]<-0
}

#Next replace the labels with the cluster neurons (this works because the number of clusters is less than the number of neurons)
for (m in 1:length(the.table_agent_clusters$clus_size)){
  SOMgriddatanew[Agents_Clusters_predicted[m]]<-m
  SOMgriddatanewcases[Agents_Cases_predicted_subset[m]]<-m
} 


#increment the neuronnumber plot, say for Neuron 3 for clusters and 11 for cases
neuronnumber[i]<-SOMgriddatanew[3]
neuronnumbercases[i]<-SOMgriddatanewcases[11]

#plot the neuron map
par(mfrow=c(2,2))
color2D.matplot(SOMgriddatanew, show.values = TRUE, axes = FALSE, xlab = "clusters", ylab = "", vcex = 2, vcol = "black",extremes = c("white", "blue"),na.color="black")
plot(testrun,neuronnumber)
points(testrun[i], neuronnumber[i], pch = 21, col = "red", bg = "yellow")
color2D.matplot(SOMgriddatanewcases, show.values = TRUE, axes = FALSE, xlab = "cases", ylab = "", vcex = 2, vcol = "black",extremes = c("white", "blue"),na.color="black")
plot(testrun,neuronnumbercases)
points(testrun[i], neuronnumbercases[i], pch = 21, col = "red", bg = "yellow")
#
i=i+1
