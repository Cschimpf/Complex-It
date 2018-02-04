# SOMwatcher2.r 2/3/2018
#
# Opens a project csv file containing various baselines and changed files
# then outputs a csv with 5 selected output cases as they travel through changes
#
# Revision History
# 02/03/2018 Original
# 02/04/2018 added better output plot formats

# Load in the project csv file
# Column 1 Data File
# Column 2 Case Data Filename with color select column added
# Column 3 Trained SOM Object Filename
# COlumn 4 SOM Object Label
# Column 5 Description
# Column 6 SOM Quadrant Matcher

# Load in the need libraries
#suppressMessages(library(SOMbrero))

# Select the folder containing the project to analyze
#setwd("C:\\Users\\carl\\Desktop\\Complexit_New\\data\\Dataset_Wales_AdminWards\\Baselines")

#open the project file
project_file <- read.csv("SOMWatcher-Project.csv")
output_point <- nrow(project_file)

#initialize the output array with a row for each output point
#and column for Case Number Selected; Color Selected; Quadrant; Matched Quadrant; Label;Desc
# 
final_output <- matrix(data=NA, nrow=output_point, ncol=12)
colnames(final_output)<-c("Blk Case","Red Case","Grn Case","Blu Case","Yel Case","Blk Quad","Red Quad","Grn Quad","Blu Quad","Yel Quad","Label","Description")
rownames(final_output, do.NULL = FALSE, prefix = "Point")
rownames(final_output) <- 1:output_point

# initialize plot array
xplot=matrix(c(1:3),nrow=output_point,ncol=5)
yplot<-xplot

#Open the first file to size the output array
for(i in 1:3){ 
case_data_file <- read.csv(as.character(project_file[i,2]))
matcher_file <- read.csv(as.character(project_file[i,6]))

# get the number of rows
for(j in 1:nrow(case_data_file)){ 
    current_color_selection<-case_data_file[j,c("Selection")]
  if(is.na(current_color_selection)){current_color_selection=0}
  current_case_selection<-case_data_file[j,c("X")]
  #current_quadrant_selection<-case_data_file[j,c("Matched.Neuron")]
  current_quadrant_selection<-matcher_file[j,c("Matched.Neuron")]
  # if it is a number, then use it
      if(current_color_selection>0 & current_color_selection<6){
        final_output[i,current_color_selection]<-current_case_selection
        final_output[i,current_color_selection+5]<-current_quadrant_selection
        yplot[i,current_color_selection]<-as.numeric(current_quadrant_selection)
        final_output[i,11]<-as.character(project_file[i,c("SOM.Object.Label")])  #label
        final_output[i,12]<-as.character(project_file[i,c("Description")])    #description
      }  
  }

}

#now use matplot to plot the final results
matplot(xplot,yplot,type=c("b","b","b","b","b"),pch=c(1,1,1,1,1),col=c("Black","Red","Green","Blue","Yellow"),
        xlab=" ",ylab="Quadrant",lty=c(1,1,1,1,1),lwd=c(3,3,3,3,3),xaxt = "n")
axis(1,at=1:output_point,labels=final_output[1:output_point,11],las=2,label=NULL)
#axis(2,at=1:output_point,labels=final_output[1:output_point,11],las=2,label=NULL)

#save final_output to a csv
write.csv(final_output,"results.csv")
