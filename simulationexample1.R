#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#SIMULATION EXAMPLE #1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Load data and libraries -------------------------------------------------------
library(tidyverse)

#Call functions ----------------------------------------------------------------
source("simulationfunctions.R")

#Synchronous simulations -------------------------------------------------------

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

#Running the function above will return a ntime x 8 data frame with 1 exception
#The  8 columns will be (T, N1, N2, N1', N2', A1, A2, T0), explained below

#lambda is the arrival rate to the system
#mu1 and mu2 are service rates of queue 1 and 2 
#beta is the update rate
#ntime is the number of events that will occur before the simulation stops running
#startingstate is a vector of 8 numbers formatted:
  #c(T, N1, N2, N1', N2', A1, A2, T0)
  #T represents the time, generally this should always be set to 0 at the starting state
  #N1 and N2 are the initial people in the queue
  #N1' and N2' are the number of people that the dispatcher knows are in queue 1 and queue 2
    #at the starting state, it is good for N1 = N1' and N2 = N2'
  #A1 and A2 are the number of people who have arrived to queue 1 and queue 2 since the last update,
    # at the starting state, both should be 0 
  #T0 is the time since the last update, set to 0 at the start
#method specifies which policy to use
  #for syncsimulation, the method functions that we can use are:
  #randomqueue, randomqueueadjust, jsq, jsw, jsmq, jsmw, jseq1, jsew1, jseq2, jsew2, particlefilter, particlefiltersim
#methodname can generally be set to a string with the same name as method
#IMPORTANT: if using particlefilter, particlefiltersim, jseq2, or jsew2 then method MUST be the same as methodname

#Notes:
#when mu1 = mu2, randomqueue, jsq, jsmq, jseq1, jseq2, particlefilter, particlefiltersim should be used
#when mu1 =/= mu2, randomqueueadjust, jsw, jsmw, jsew1, jsew2, particlefilter, particlefiltersim should be used
#when the policy particlefilter is used, a list will be returned with a dataframe and a list, 
  #the dataframe is the usual dataframe
  #the list will be 100 ntime x 3 dataframes, with columns (T, N1, N2) for each of the simulated paths
#particlefilter and particlefiltersim operate in a similar manner, however particlefiltersim does not return all the simulated paths
  #therefore particlefiltersim is a lot more efficient and useful for longer simulations

#some more examples
data2 = syncsimulation(lambda = 0.4, 
               mu1 = 0.1, 
               mu2 = 0.4, 
               beta = 0.1, 
               ntime = 100, 
               startingstate = c(0, 10, 10, 10, 10, 0, 0, 0),
               method = jsew1,
               methodname = "jsew1")

data3 = syncsimulation(lambda = 0.4, 
                      mu1 = 0.1, 
                      mu2 = 0.4, 
                      beta = 0.1, 
                      ntime = 100, 
                      startingstate = c(0, 10, 10, 10, 10, 0, 0, 0),
                      method = particlefilter,
                      methodname = "particlefilter")

#Asynchronous simulations -------------------------------------------------------

#asyncsimulation work in a very similar way to syncsimulation
#the default settings are as follows
asyncsimulation(lambda = 0.4, 
               mu1 = 0.25, 
               mu2 = 0.25, 
               beta1 = 0.1, 
               beta2 = 0.1,
               ntime = 100, 
               startingstate = c(0, 10, 10, 10, 10, 0, 0, 0, 0),
               method = randomqueue,
               methodname = "notparticlefilter")

#Running the function above will return a ntime x 8 data frame with 1 exception
#The 9 columns will be (T, N1, N2, N1', N2', A1, A2, T1, T2)
  #instead of T0, time since update, now T1 and T2 are time since update of Queue 1 and 2 respectively
#lambda, mu1, mu2 are the same as syncsimulation
#beta1 and beta2 are the rate of update for Queue 1 and Queue 2
#ntime is the number of events

#startingstate now is a vector of length 9, taking the same states as the 9 columns of the dataframe
  #T1 and T2 should be set to 0 at the starting state, everything else is the same as a synchronous simulation

#method specifies which policy to use
#for asyncsimulation, the method functions that we can use are:
#randomqueue, randomqueueadjust, jsq, jsw, jsmq, jsmw, jseq1async, jsew1async, jseq2async, jsew2async, particlefilterasync, particlefilterasyncsim
#methodname can generally be set to a string with the same name as method
#IMPORTANT: if using particlefilterasync, particlefilterasyncsim, jseq2async, or jsew2async then method MUST be the same as methodname

#Notes:
#when mu1 = mu2, randomqueue, jsq, jsmq, jseq1async, jseq2async, particlefilterasync, particlefilterasyncsim should be used
#when mu1 =/= mu2, randomqueueadjust, jsw, jsmw, jsew1async and jsew2async, particlefilterasync, particlefilterasyncsim should be used
#when the policy particlefilter is used, a list will be returned with a dataframe and a list, 
  #the dataframe is the usual dataframe
  #the list will be 100 ntime x 3 dataframes, with columns (T, N1, N2) for each of the simulated paths
#particlefilterasync and particlefilterasyncsim operate in a similar manner, however particlefilterasyncsim does not return all the simulated paths
#therefore particlefilterasyncsim is a lot more efficient and useful for longer simulations

#some more examples:
asyncsimulation(lambda = 0.4, 
                mu1 = 0.1, 
                mu2 = 0.4, 
                beta1 = 0.1, 
                beta2 = 0.1,
                ntime = 100, 
                startingstate = c(0, 10, 10, 10, 10, 0, 0, 0, 0),
                method = particlefilterasyncsim,
                methodname = "particlefilterasyncsim")

asyncsimulation(lambda = 0.4, 
                mu1 = 0.25, 
                mu2 = 0.25, 
                beta1 = 0.0001, 
                beta2 = 0.0001,
                ntime = 100, 
                startingstate = c(0, 10, 10, 10, 10, 0, 0, 0, 0),
                method = jseq2async,
                methodname = "jseq2async")

#plotting sample paths ---------------------------------------------------------

#instead of outputting a dataframe we can generate the sample paths immediately
    #right now I haven't made the function so you can generate both the sample path and the corresponding dataframe 
    #if you want that added let me know :)
#the functions are called plotsync and plotasync, and essentially work identically to syncsimulation and asyncsimulation
#they are shown below with their default parameters

plotsync(ntime = 1000,
         lambda = 0.4, 
         mu1 = 0.25, 
         mu2 = 0.25, 
         beta = 0.01, 
         startingstate = c(0, 10, 10, 10, 10, 0, 0, 0),
         method = randomqueue,
         methodname = "notparticlefilter")

plotasync(ntime = 1000,
          lambda = 0.4, 
          mu1 = 0.25, 
          mu2 = 0.25, 
          beta1 = 0.01, 
          beta2 = 0.01,
          startingstate = c(0, 10, 10, 10, 10, 0, 0, 0, 0),
          method = randomqueue,
          methodname = "notparticlefilter")

#All the methods that can be used for syncsimulation can be used in plotsync except particlefilter
  #likewise with asyncsimulation except particlefilterasync

#DO NOT USE particlefilter OR particlefilterasync WITH plotsync AND plotasync
#use particlefiltersim or particlefilterasyncsim

#Right now whatever is in methodname will show in the title
#I may make an adjustment to put custom title text in later
#particlefiltersim, particlefilterasyncsim, jseq2, jsew2, jseq2async, and jsew2 still need to have matching method and methodname


#plotting particle filter ------------------------------------------------------
#we can generate special sample paths for particlefilter and particlefilter async that show the simulated paths
particleplot(ntime = 1000, 
             lambda = 0.4, 
             mu1 = 0.1, 
             mu2 = 0.4, 
             beta = 0.01, 
             startingstate = c(0, 10, 10, 10, 10, 0, 0, 0),
             method = particlefilter, 
             methodname = "particlefilter")

particleplotasync(ntime = 1000, 
                  lambda = 0.4, 
                  mu1 = 0.1, 
                  mu2 = 0.4, 
                  beta = 0.01, 
                  startingstate = c(0, 10, 10, 10, 10, 0, 0, 0, 0),
                  method = particlefilterasync, 
                  methodname = "particlefilterasync")


#comparison functions ----------------------------------------------------------

#there is a difference and a differenceasync function, which can compare the queues
#below the default settings for difference and difference async is shown 

difference(ntime = 500, 
           nsim = 30,
           lambda = 0.4, 
           mu1 = 0.1, 
           mu2 =0.4, 
           beta = 0.01, 
           startingstate = c(0,10,10,10,10,0,0,0), 
           method = randomqueue, 
           methodname = "randomqueue")

differenceasync(ntime = 500, 
                nsim = 30,
                lambda = 0.4, 
                mu1 = 0.1, 
                mu2 =0.4, 
                beta1 = 0.01,
                beta2 = 0.01,
                startingstate = c(0,10,10,10,10,0,0,0,0), 
                method = randomqueue, 
                methodname = "randomqueue")

#the function will simulate nsim samples of ntime length simulations
#it will take one ntime period as a burn-in

#a folder will be created in the working directory called "lambda = 0.4, mu1 = 0.1, mu2 = 0.4, beta = 0.01, method = randomqueue"

#it will contain two csv files, summaries.csv and data.csv
    #note that summaries.csv has some randomly numbers in front so I was able to open two in Excel at the same time

#data.csv will have nsim rows
#for each simulation it will give:
  #mean absolute difference
  #mean difference
  #mean absolute proportional difference (absolute difference of wait times)
  #mean proportional difference (difference in wait times)
  #mean queue 1 length
  #mean queue 2 length

#summaries.csv will average over these and give the mean and variance over the nsim samples for each of these values
#it will also give the mean and variance of the mean queue 1 and 2 wait time

#NOTES: 
#Do not use particlefilter or particlefilterasync for these simulations, use particlefiltersim or particlefilterasyncsim
#some simulations, especially Particle Filter or JSEQ2 variants may take a long time
#I generally limit the product of nsim and ntime to 30000, which means the maximum simulation will take about 20mins for me
#I have found a product of nsim and ntime of 200,000 can have a runtime of up to 2 hours
