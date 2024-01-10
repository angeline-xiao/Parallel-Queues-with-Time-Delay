#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#SIMULATION EXAMPLE #1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Load data and libraries -------------------------------------------------------
library(tidyverse)

#Call functions ----------------------------------------------------------------
source("simulationfunctions.R")

#The most basic use of these functions are to run a simulation:
#Simulations with synchronous updates are run using syncsimulation
#The default parameter settings are shown and run below
#This will run the same as syncsimulation(), when no parameters are named
syncsimulation(lambda = 0.4, 
               mu1 = 0.25, 
               mu2 = 0.25, 
               beta = 0.1, 
               ntime = 100, 
               startingstate = c(0, 10, 10, 10, 10, 0, 0, 0),
               method = randomqueue,
               methodname = "notparticlefilter")

#Running the function above will return a ntime x 8 data frame with a few exceptions
#The  8 columns will be (T, N1, N2, N1', N2', A1, A2, T0), explained below

#lambda is the arrival rate to the system
#mu1 and mu2 are service rates of queue 1 and 2 
#beta is the update rate
#startingstate is a vector of 8 numbers formatted:
  #c(T, N1, N2, N1', N2', A1, A2, T0)
  # T represents the time, generally this should always be set to 0 at the starting state
  # N1 and N2 are the initial people in the queue
  # N1' and N2' are the number of people that the dispatcher knows are in queue 1 and queue 2
    # at the starting state, it is good for N1 = N1' and N2 = N2'
  # A1 and A2 are the number of people who have arrived to queue 1 and queue 2 since the last update,
    # at the starting state, both should be 0 
  # T0 is the time since the last update, set to 0 at the start
#method specifies which policy to use
  #for syncsimulation, the method functions that we can use are:
  #randomqueue, randomqueueadjust, jsq, jsw, jsmq, jsmw, jseq1, jsew1, jseq2, jsew2, particlefilter, particlefiltersim



