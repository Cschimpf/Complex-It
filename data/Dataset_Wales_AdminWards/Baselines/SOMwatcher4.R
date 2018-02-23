# SOMwatcher4.r 2/21/2018
#
# Runs an optimization on a SOM Object
# and returns results to the console
#
# Revision History
# 02/21/2018 Original

# Load in the need libraries
suppressMessages(library(SOMbrero))
suppressMessages(library(nloptr))

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


#Run the Optimization

a <- 5  #select the neuron number to find

#First, create the function to be called, in this case two variables selected
eval_f0<-function (x,a){
  Income2014<-x[1]
  Income2016<-x[2]
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
  # predict neuron:
  xnew<-c(Income2014,Income2016,Employ2014,Employ2016,BirthWeight2014,BirthWeight2016,Death2014,Death2016,Stage4Capped2014,Stage4Capped2016,Violence2014,Violence2016,Theft2014,Theft2016)
  neuron <- predict(Agent_SOM,xnew)-a
  return(neuron)
}

# Then select the Optimization routine, one without gradients, but with contraints
opts = list("algorithm"="NLOPT_LN_COBYLA",
                         "xtol_rel"=1.0e-8)

# Run the optimizer
res0 <- nloptr( x0=c(10,10),
                eval_f=eval_f0,
                lb = c(10,10),
                ub = c(40,40),
                a = a,
                opts = opts)

# Show the optimized values
print(res0)
