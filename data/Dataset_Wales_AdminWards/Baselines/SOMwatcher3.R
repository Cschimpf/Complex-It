# SOMwatcher3.r 2/20/2018
#

#for my own experiments with this, adjust working directory to test this code out
setwd("C:\\Users\\Corey Schimpf\\Documents\\SACS for R\\new interface Complexit\\data\\Dataset_Wales_AdminWards\\Baselines")

# Runs a monte carlo analysis on a SOM Object
# and returns results to a text file and a latex file format
#
# Revision History
# 02/20/2018 Original

# Load in the need libraries
suppressMessages(library(SOMbrero))
suppressMessages(library(MonteCarlo))

#Load a SOM and rename the workspace variable then remove the old one
load("baseline1-SavedSOMObject")
Agent_SOM <<- previous_som

# Select Variables to randomly iterate upon.

#a Neuron 1 case
Income2014<-16.78593
Income2016<-16.15881
Employ2014<-13.10852
Employ2016<-10.3014
BirthWeight2014<-5.639955
BirthWeight2016<-5.610987
Death2014<-1046.772
Death2016<-1036.879
Stage4Capped2014<-329.3407
Stage4Capped2016<-346.1539
Violence2014<-1.325048
Violence2016<-1.736268
Theft2014<-0.4852523
Theft2016<-0.4339737
#A neuron 5 case
Income2014<-24
Income2016<-22
Employ2014<-20
Employ2016<-16
BirthWeight2014<-6.8
BirthWeight2016<-6.4
Death2014<-1234.4
Death2016<-1225.7
Stage4Capped2014<-299
Stage4Capped2016<-323
Violence2014<-1.54
Violence2016<-2.09
Theft2014<-.78
Theft2016<-.82

#Function to Predict the Quadrant (Neuron) from the variables
#Neuron <- predict(Agent_SOM,x.new=c(Income2014,Income2016,Employ2014,Employ2016,BirthWeight2014,BirthWeight2016,Death2014,Death2016,Stage4Capped2014,Stage4Capped2016,Violence2014,Violence2016,Theft2014,Theft2016))

#Run the Monte Carlo

#First, create the function to be called
runfunction<-function(Income2014,Income2016,Employ2014,Employ2016,BirthWeight2014,BirthWeight2016,Death2014,Death2016,Stage4Capped2014,Stage4Capped2016,Violence2014,Violence2016,Theft2014,Theft2016){
  # predict neuron:
  xnew<-c(Income2014,Income2016,Employ2014,Employ2016,BirthWeight2014,BirthWeight2016,Death2014,Death2016,Stage4Capped2014,Stage4Capped2016,Violence2014,Violence2016,Theft2014,Theft2016)
  neuron <- predict(Agent_SOM,xnew)
  # return result:
  return(list("neuron"=unname(neuron)))
}

#Next, setup the grid
#off of a neuron 5 case in this example
#specify the exact cases to try, or use runif for a uniform random sample with decimal points
Income2014<- runif(3,min=22,max=26)
#Income2014<-c(22,24,26)
Income2016<-c(21,22,23)
Employ2014<-c(19,20,21)
Employ2016<-c(14,16,18)
BirthWeight2014<-6.8
BirthWeight2016<-6.4
Death2014<-1234.4
Death2016<-1225.7
Stage4Capped2014<-299
Stage4Capped2016<-323
Violence2014<-1.54
Violence2016<-2.09
Theft2014<-0.78
Theft2016<-0.82


# collect parameter grids in list:
param_list=list("Income2014"=Income2014,
"Income2016"=Income2016,
"Employ2014"=Employ2014,
"Employ2016"=Employ2016,
"BirthWeight2014"=BirthWeight2014,
"BirthWeight2016"=BirthWeight2016,
"Death2014"=Death2014,
"Death2016"=Death2016,
"Stage4Capped2014"=Stage4Capped2014,
"Stage4Capped2016"=Stage4Capped2016,
"Violence2014"=Violence2014,
"Violence2016"=Violence2016,
"Theft2014"=Theft2014,
"Theft2016"=Theft2016)

# Now, finally, run the monte carlo
MC_result<-MonteCarlo(func=runfunction, nrep=100, max_grid=10000, param_list=param_list)

# And save results to text
sink("MonteCarloResults.txt")
MC_result$results #shows the neurons for all the combinations
sink()

#
summary(MC_result)

# Here is the optional Latex output
sink("MonteCarloResults.tex")
cols<-c("Income2016","Employ2014","Employ2016","BirthWeight2014","BirthWeight2016","Death2014","Death2016","Stage4Capped2014","Stage4Capped2016",
        "Violence2014",
        "Violence2016",
        "Theft2014",
        "Theft2016")
rows<-c("Income2014")
MakeTable(output=MC_result, rows=rows, cols=cols, digits=2)
sink()

