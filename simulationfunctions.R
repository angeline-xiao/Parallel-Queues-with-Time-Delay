
#install.packages("tidyverse")


library(tidyverse)

randomqueue = function(...){ #function for random queue policy when there is no difference in service rate
  num = rbinom(1, 1, 0.5)
  if (num == 1){
    return(c(0, 1))
  }
  else{
    return(c(1, 0))
  }
}

randomqueueadjust = function(..., mu = c(mu1, mu2)){ #function for random queue policy when there is difference in service rate
  num = rbinom(1, 1, mu[1]/sum(mu))
  if (num == 0){
    return(c(0, 1))
  }
  else{
    return(c(1, 0))
  }
}

jsq = function(laststate = state[i,], ...){ #function for join the shortest queue policy
  if (laststate$N1_<laststate$N2_){
    return(c(1,0))
  }
  else if (laststate$N1_ > laststate$N2_){
    return(c(0,1))
  }
  else{
    randomqueue()
  }
}

jsw = function(laststate = state[i,], ..., mu = c(mu1, mu2)){ #function for join the shortest wait policy
  if (laststate$N1_/mu[1]<laststate$N2_/mu[2]){
    return(c(1,0))
  }
  else if (laststate$N1_/mu[1] > laststate$N2_/mu[2]){
    return(c(0,1))
  }
  else{
    randomqueueadjust(mu = mu)
  }
}

jsmq = function(laststate = state[i,], ...){ #function for join the shortest maximum queue policy
  if (laststate$N1_+laststate$A1 < laststate$N2_+laststate$A2){
    return(c(1,0))
  }
  else if (laststate$N1_+laststate$A1 > laststate$N2_+laststate$A2){
    return(c(0,1))
  }
  else{
    randomqueue()
  }
}

jsmw = function(laststate = state[i,], ..., mu = c(mu1, mu2)){ #function for join the shortest maximum wait policy
  if ((laststate$N1_+laststate$A1)/mu[1]<(laststate$N2_+laststate$A2)/mu[2]){
    return(c(1,0))
  }
  else if ((laststate$N1_+laststate$A1)/mu[1]>(laststate$N2_+laststate$A2)/mu[2]){
    return(c(0,1))
  }
  else {
    randomqueueadjust(mu = mu)
  }
}

jseq1 = function(laststate = state[i,], ..., mu = c(mu1, mu2)){ #function for join the shortest estimated queue 1 policy for the synchronous case
  t = laststate$T0
  estqueue1 = max(laststate$N1_ + laststate$A1 - laststate$T0*mu[1], 0)
  estqueue2 = max(laststate$N2_ + laststate$A2 - laststate$T0*mu[2], 0)
  if (estqueue1 < estqueue2){
    return(c(1,0))
  }
  else if (estqueue1 > estqueue2){
    return(c(0,1))
  }
  else {
    randomqueue()
  }
}

jseq1async = function(laststate = state[i,], ..., mu = c(mu1, mu2)){ #function for join the shortest estimated queue 1 policy for the asynchronous case
  estqueue1 = max(0, laststate$N1_ + laststate$A1 - laststate$T1*mu[1])
  estqueue2 = max(0, laststate$N2_ + laststate$A2 - laststate$T2*mu[2])
  
  if (estqueue1 < estqueue2){
    return(c(1,0))
  }
  else if (estqueue1 > estqueue2){
    return(c(0,1))
  }
  else{
    randomqueue()
  }
}

jsew1 = function(laststate = state[i,], ..., mu = c(mu1, mu2)){ #function for join the shortest estimated wait 1 policy for the synchronous case
  t = laststate$T0
  estqueue1 = max((laststate$N1_ + laststate$A1)/mu[1] - laststate$T0, 0)
  estqueue2 = max((laststate$N2_ + laststate$A2)/mu[2] - laststate$T0, 0)
  #does not consider what happens if the queue length is 0
  if (estqueue1 < estqueue2){
    return(c(1,0))
  }
  else if (estqueue1 > estqueue2){
    return(c(0,1))
  }
  else {
    randomqueueadjust(mu = mu)
  }
}

jsew1async = function(laststate = state[i,], ..., mu = c(mu1, mu2)){ #function for join the shortest estimated wait 1 policy for the asynchronous case
  estqueue1 = max(0, (laststate$N1_ + laststate$A1)/mu[1] - laststate$T1*mu[1])
  estqueue2 = max(0, (laststate$N2_ + laststate$A2)/mu[2] - laststate$T2*mu[2])
  
  if (estqueue1 < estqueue2){
    return(c(1,0))
  }
  else if (estqueue1 > estqueue2){
    return(c(0,1))
  }
  else{
    randomqueueadjust(mu = mu)
  }
}

jseq2 = function(laststate = state[i,], sim = simulations, mu = c(mu1, mu2)){ #function for join the shortest estimated queue 2 policy for the synchronous case
  Time = laststate$T
  
  if(laststate$T0 == 0){ #at time of update
    updaterow = c(Time, laststate$N1, laststate$N2)
    sim = rbind(sim, updaterow)
    change = c(0, 0)
  }
  else {
    
    N1 = max(sim[nrow(sim), 2] - mu[1]*(Time-sim[nrow(sim),1]), 0)
    
    N2 = max(sim[nrow(sim), 3] - mu[2]*(Time-sim[nrow(sim),1]), 0)
    
    sim = rbind(sim, c(Time, N1, N2))
    
    
    if (N1 < N2){
      sim = sim[nrow(sim),] + c(0, 1, 0)    
      change = c(1,0)
      return(list(sim = sim, change = change))
    } else if (N2 < N1){
      sim = sim[nrow(sim),] + c(0, 0, 1)
      change = c(0,1)
      return(list(sim = sim, change = change))
    } else{
      change = randomqueueadjust(mu = mu)
      
      sim= sim[nrow(sim),] + c(0, change)
      
      return(list(sim = sim, change = change))
      
    }
  }
  
  
  return(list(sim = sim[nrow(sim),], change = change))
  
}

jseq2async = function(laststate = state[i,], sim = simulations, mu = c(mu1, mu2)){  #function for join the shortest estimated queue 2 policy for the asynchronous case
  Time = laststate$T
  
  if(laststate$T1 == 0){
    len = nrow(sim)
    updaterow = c(Time, laststate$N1, sim[len, 3])
    sim = rbind(sim, updaterow)          
    
    change = c(0, 0)
  }else if(laststate$T2 == 0){
    len = nrow(sim)
    updaterow = c(Time, sim[len, 2], laststate$N2)
    sim = rbind(sim, updaterow)          
    
    change = c(0, 0)
  } else {
    len = nrow(sim)
    
    
    N1 = max(sim[len, 2] - mu[1]*(laststate$T-sim[len,1]), 0)
    
    N2 = max(sim[len, 3] - mu[2]*(laststate$T-sim[len,1]), 0)
    
    sim = rbind(sim, c(Time, N1, N2))
    
    
    if (N1 < N2){
      sim = sim[nrow(sim),] + c(0, 1, 0)    
      return(list(sim = sim, change = c(1, 0)))
    } else if (N2 < N1){
      sim = sim[nrow(sim),] + c(0, 0, 1)
      return(list(sim = sim, change = c(0, 1)))
    } else{
      change = randomqueueadjust(mu = mu)
      
      sim= sim[nrow(sim),] + c(0, change)
      
      return(list(sim = sim, change = change))
      
    }
  }
  
  
  return(list(sim = sim[nrow(sim),], change = change))
  
}


jsew2 = function(laststate = state[i,], sim = simulations, mu = c(mu1, mu2)){ #function for join the shortest estimated wait 2 policy for the synchronous case
  Time = laststate$T
  
  if(laststate$T0 == 0){ #at time of update
    updaterow = c(Time, laststate$N1, laststate$N2)
    sim = rbind(sim, updaterow)
    change = c(0, 0)
  }
  else {
    
    N1 = max(sim[nrow(sim), 2] - mu[1]*(Time-sim[nrow(sim),1]), 0)
    
    N2 = max(sim[nrow(sim), 3] - mu[2]*(Time-sim[nrow(sim),1]), 0)
    
    sim = rbind(sim, c(Time, N1, N2))
    
    
    if (N1/mu[1] < N2/mu[2]){
      sim = sim[nrow(sim),] + c(0, 1, 0)    
      change = c(1,0)
      return(list(sim = sim, change = change))
    } else if (N2/mu[2] < N1/mu[1]){
      sim = sim[nrow(sim),] + c(0, 0, 1)
      change = c(0,1)
      return(list(sim = sim, change = change))
    } else{
      change = randomqueueadjust(mu = mu)
      
      sim= sim[nrow(sim),] + c(0, change)
      
      return(list(sim = sim, change = change))
      
    }
  }
  
  
  return(list(sim = sim[nrow(sim),], change = change))
  
}

jsew2async = function(laststate = state[i,], sim = simulations, mu = c(mu1, mu2)){  #function for join the shortest estimated wait 2 policy for the asynchronous case
  Time = laststate$T
  
  if(laststate$T1 == 0){
    len = nrow(sim)
    updaterow = c(Time, laststate$N1, sim[len, 3])
    sim = rbind(sim, updaterow)          
    
    change = c(0, 0)
  }else if(laststate$T2 == 0){
    len = nrow(sim)
    updaterow = c(Time, sim[len, 2], laststate$N2)
    sim = rbind(sim, updaterow)          
    
    change = c(0, 0)
  } else {
    len = nrow(sim)
    
    
    N1 = max(sim[len, 2] - mu[1]*(laststate$T-sim[len,1]), 0)
    
    N2 = max(sim[len, 3] - mu[2]*(laststate$T-sim[len,1]), 0)
    
    sim = rbind(sim, c(Time, N1, N2))
    
    
    if (N1/mu[1] < N2/mu[2]){
      sim = sim[nrow(sim),] + c(0, 1, 0)    
      return(list(sim = sim, change = c(1, 0)))
    } else if (N2/mu[2] < N1/mu[1]){
      sim = sim[nrow(sim),] + c(0, 0, 1)
      return(list(sim = sim, change = c(0, 1)))
    } else{
      change = randomqueueadjust(mu = mu)
      
      sim= sim[nrow(sim),] + c(0, change)
      
      return(list(sim = sim, change = change))
      
    }
  }
  
  
  return(list(sim = sim[nrow(sim),], change = change))
  
}


particlefilter = function(laststate = state[i,], sim = simulations, mu = c(mu1, mu2)){ #particle filter policy for synchronous case
  Time = laststate$T  #includes all sample paths so if not plotting then use particlefiltersim for efficiency
  
  if(laststate$T0 == 0){ #at time of update
    for (i in 1:100){
      updaterow = c(Time, laststate$N1, laststate$N2)
      sim[[i]] = rbind(sim[[i]], updaterow)
    }
    change = c(0, 0)
  }
  else {
    for (i in 1:100){ #at time of arrival
      len = nrow(sim[[i]])
      
      if(laststate$A1 == 0){
        N1 = max(laststate$N1 - mu[1]*(laststate$T-sim[[i]][len,1]), 0)
      } else if (sim[[i]][len, 2] == 0){ #if this is the start
        N1 = 0 #no departures in that time
      }else{
        N1 = max(0, sim[[i]][len, 2] - rpois(1, mu[1]*(laststate$T-sim[[i]][len,1])))
      }
      
      if(laststate$A2 == 0){
        N2 = max(laststate$N2 - mu[2]*(laststate$T-sim[[i]][len,1]), 0)
      } else if (sim[[i]][len, 3] == 0){
        N2 = 0
      }else{
        N2 = max(0, sim[[i]][len, 3] - rpois(1, mu[2]*(laststate$T-sim[[i]][len,1])))
      }
      
      sim[[i]] = rbind(sim[[i]], c(Time, N1, N2))
      
    }
    meanN1 = mean(sapply(sim, function(df) {tail(df$N1, n = 1)}))
    meanN2 = mean(sapply(sim, function(df) {tail(df$N2, n = 1)}))
    
    if (meanN1/mu[1] < meanN2/mu[2]){
      for (i in 1:100){
        len2 = nrow(sim[[i]])
        sim[[i]] = rbind(sim[[i]], c(Time, sim[[i]][len2, 2]+1, sim[[i]][len2, 3]))    
      }
      return(list(sim = sim, change = c(1, 0)))
    } else if (meanN2/mu[2] < meanN1/mu[1]){
      for (i in 1:100){
        len2 = nrow(sim[[i]])
        sim[[i]] = rbind(sim[[i]], c(Time, sim[[i]][len2, 2], sim[[i]][len2, 3]+1))
      }
      return(list(sim = sim, change = c(0, 1)))
    } else{
      change = randomqueueadjust(mu = mu)
      for (i in 1:100){
        len2 = nrow(sim[[i]])
        changed = unlist(c(sim[[i]][len2, 2], sim[[i]][len2, 3])) + change
        sim[[i]] = rbind(sim[[i]], c(Time, changed))
      }
    }
  }
  return(list(sim = sim, change = change))
  
}

particlefiltersim = function(laststate = state[i,], sim = simulations, mu = c(mu1, mu2)){ #function for particle filter policy for synchronous case, does not save sample paths so better for bigger simulations
  Time = laststate$T
  
  if(laststate$T0 == 0){ #at time of update
    for (i in 1:100){
      updaterow = c(Time, laststate$N1, laststate$N2)
      sim[[i]] = rbind(sim[[i]], updaterow)
    }
    change = c(0, 0)
  }
  else {
    for (i in 1:100){ #at time of arrival
      len = nrow(sim[[i]])
      
      if(laststate$A1 == 0){
        N1 = max(laststate$N1 - mu[1]*(laststate$T-sim[[i]][len,1]), 0)
      } else if (sim[[i]][len, 2] == 0){ #if this is the start
        N1 = 0 #no departures in that time
      }else{
        N1 = max(0, sim[[i]][len, 2] - rpois(1, mu[1]*(laststate$T-sim[[i]][len,1])))
      }
      
      if(laststate$A2 == 0){
        N2 = max(laststate$N2 - mu[2]*(laststate$T-sim[[i]][len,1]), 0)
      } else if (sim[[i]][len, 3] == 0){
        N2 = 0
      }else{
        N2 = max(0, sim[[i]][len, 3] - rpois(1, mu[2]*(laststate$T-sim[[i]][len,1])))
      }
      
      sim[[i]] = rbind(sim[[i]], c(Time, N1, N2))
      
    }
    meanN1 = mean(sapply(sim, function(df) {tail(df$N1, n = 1)}))
    meanN2 = mean(sapply(sim, function(df) {tail(df$N2, n = 1)}))
    
    if (meanN1/mu[1] < meanN2/mu[2]){
      for (i in 1:100){
        len2 = nrow(sim[[i]])
        sim[[i]] = sim[[i]][len2,] + c(0, 1, 0)
      }
      return(list(sim = sim, change = c(1, 0)))
    } else if (meanN2/mu[2] < meanN1/mu[1]){
      for (i in 1:100){
        len2 = nrow(sim[[i]])
        sim[[i]] = sim[[i]][len2,] + c(0, 0, 1)
      }
      return(list(sim = sim, change = c(0, 1)))
    } else{
      change = randomqueueadjust(mu = mu)
      for (i in 1:100){
        len2 = nrow(sim[[i]])
        sim[[i]] = sim[[i]][len2,] + c(0, change)
      }
      return(list(sim = sim, change = change))
    }
  }
  
  for (i in 1:100){
    len = nrow(sim[[i]])
    sim[[i]] = sim[[i]][len,]
  }  
  return(list(sim = sim, change = change))
  
}

particlefilterasync = function(laststate = state[i,], sim = simulations, mu = c(mu1, mu2)){ #particle filter policy for asynchronous case
  Time = laststate$T   #includes all sample paths so if not plotting then use particlefilterasyncsim for efficiency
  if(laststate$T1 == 0){
    for (i in 1:100){
      len = nrow(sim[[i]])
      updaterow = c(Time, laststate$N1, sim[[i]][len, 3])
      sim[[i]] = rbind(sim[[i]], updaterow)          
    }
    change = c(0, 0)
  }else if(laststate$T2 == 0){
    for (i in 1:100){
      len = nrow(sim[[i]])
      updaterow = c(Time, sim[[i]][len, 2], laststate$N2)
      sim[[i]] = rbind(sim[[i]], updaterow)          
    }
    change = c(0, 0)
  }
  else {
    for (i in 1:100){ #at time of arrival
      len = nrow(sim[[i]])
      
      if(laststate$A1 == 0){
        N1 = max(laststate$N1 - mu[1]*(laststate$T-sim[[i]][len,1]), 0)
      } else if (sim[[i]][len, 2] == 0){ #if this is the start
        N1 = 0 #no departures in that time
      }else{
        N1 = max(0, sim[[i]][len, 2] - rpois(1, mu[1]*(laststate$T-sim[[i]][len,1])))
      }
      
      if(laststate$A2 == 0){
        N2 = max(laststate$N2 - mu[2]*(laststate$T-sim[[i]][len,1]), 0)
      } else if (sim[[i]][len, 3] == 0){
        N2 = 0
      }else{
        N2 = max(0, sim[[i]][len, 3] - rpois(1, mu[2]*(laststate$T-sim[[i]][len,1])))
      }
      
      sim[[i]] = rbind(sim[[i]], c(Time, N1, N2))
      
    }
    meanN1 = mean(sapply(sim, function(df) {tail(df$N1, n = 1)}))
    meanN2 = mean(sapply(sim, function(df) {tail(df$N2, n = 1)}))
    
    if (meanN1/mu[1] < meanN2/mu[2]){
      for (i in 1:100){
        len2 = nrow(sim[[i]])
        sim[[i]] = rbind(sim[[i]], sim[[i]][len2,] + c(0, 1, 0))
      }
      return(list(sim = sim, change = c(1, 0)))
    } else if (meanN2/mu[2] < meanN1/mu[1]){
      for (i in 1:100){
        len2 = nrow(sim[[i]])
        sim[[i]] = rbind(sim[[i]], sim[[i]][len2,] + c(0, 0, 1))
      }
      return(list(sim = sim, change = c(0, 1)))
    } else{
      change = randomqueueadjust(mu = mu)
      for (i in 1:100){
        len2 = nrow(sim[[i]])
        sim[[i]] = rbind(sim[[i]], sim[[i]][len2,] + c(0, change))
      }
      return(list(sim = sim, change = c(1, 0)))
    }
  }
  return(list(sim = sim, change = change))
  
}

particlefilterasyncsim = function(laststate = state[i,], sim = simulations, mu = c(mu1, mu2)){ #function for particle filter policy for asynchronous case, does not save sample paths so better for bigger simulations
  Time = laststate$T
  if(laststate$T1 == 0){
    for (i in 1:100){
      updaterow = c(Time, laststate$N1, sim[[i]][1, 3])
      sim[[i]] = rbind(sim[[i]], updaterow)
    }
    change = c(0, 0)
  }else if(laststate$T2 == 0){
    for (i in 1:100){
      len = nrow(sim[[i]])
      updaterow = c(Time, sim[[i]][len, 2], laststate$N2)
      sim[[i]] = rbind(sim[[i]], updaterow)          
    }
    change = c(0, 0)
  }
  else {
    for (i in 1:100){ #at time of arrival
      len = nrow(sim[[i]])
      
      if(laststate$A1 == 0){
        N1 = max(laststate$N1 - mu[1]*(laststate$T-sim[[i]][len,1]), 0)
      } else if (sim[[i]][len, 2] == 0){ #if this is the start
        N1 = 0 #no departures in that time
      }else{
        N1 = max(0, sim[[i]][len, 2] - rpois(1, mu[1]*(laststate$T-sim[[i]][len,1])))
      }
      
      if(laststate$A2 == 0){
        N2 = max(laststate$N2 - mu[2]*(laststate$T-sim[[i]][len,1]), 0)
      } else if (sim[[i]][len, 3] == 0){
        N2 = 0
      }else{
        N2 = max(0, sim[[i]][len, 3] - rpois(1, mu[2]*(laststate$T-sim[[i]][len,1])))
      }
      
      sim[[i]] = rbind(sim[[i]], c(Time, N1, N2))
      
    }
    meanN1 = mean(sapply(sim, function(df) {tail(df$N1, n = 1)}))
    meanN2 = mean(sapply(sim, function(df) {tail(df$N2, n = 1)}))
    
    if (meanN1/mu[1] < meanN2/mu[2]){
      for (i in 1:100){
        len2 = nrow(sim[[i]])
        sim[[i]] = rbind(sim[[i]], c(Time, sim[[i]][len2, 2]+1, sim[[i]][len2, 3]))    
      }
      return(list(sim = sim, change = c(1, 0)))
    } else if (meanN2/mu[2] < meanN1/mu[1]){
      for (i in 1:100){
        len2 = nrow(sim[[i]])
        sim[[i]] = rbind(sim[[i]], c(Time, sim[[i]][len2, 2], sim[[i]][len2, 3]+1))
      }
      return(list(sim = sim, change = c(0, 1)))
    } else{
      change = randomqueueadjust(mu = mu)
      for (i in 1:100){
        len2 = nrow(sim[[i]])
        changed = unlist(c(sim[[i]][len2, 2], sim[[i]][len2, 3])) + change
        sim[[i]] = rbind(sim[[i]], c(Time, changed))
      }
    }
  }
  for (i in 1:100){
    len = nrow(sim[[i]])
    sim[[i]] = sim[[i]][len,]
  }
  return(list(sim = sim, change = change))
  
}

syncsimulation = function(lambda = 0.4, mu1 = 0.25, mu2 = 0.25, beta = 0.1, ntime = 100, startingstate  = c(0,10, 10, 10, 10, 0, 0, 0), method = randomqueue, methodname = "notparticlefilter"){
  #function to simulate a process with synchronous updates
  state = as.data.frame(matrix(nrow = ntime, ncol = 8))
  colnames(state) = c("T", "N1", "N2", "N1_", "N2_", "A1", "A2", "T0")
  state[1,] = startingstate
  if(methodname == "Particle Filter" | methodname == "particlefilter"){
    simulations = lapply(1:100, function(i) {
      df <- data.frame(matrix(c(state$T[1], state$N1[1], state$N2[1]), ncol = 3))
      colnames(df) <- c("T", "N1", "N2")
      df
    })
  } else if(methodname == "Join the Shortest Estimated Queue 2"){
    jseqdf = function() {
      df <- data.frame(matrix(c(state$T[1], state$N1[1], state$N2[1]), ncol = 3))
      colnames(df) <- c("T", "N1", "N2")
      df
    }
    simulations = jseqdf()
  }
  
  timetonextevent = c(rexp(1, lambda), rexp(1, mu1), rexp(1, mu2), rexp(1, beta))
  names(timetonextevent) = c("arrive", "leave1", "leave2", "update")
  for (i in 2:ntime){
    #x = sample(c("arrive", "leave1", "leave2", "update"), size = 1, replace = TRUE, prob = c(lambda, mu1, mu2, beta1, beta2))
    
    state[i,] = state[i-1,]
    state[i, c("T", "T0")] = state[i-1, c("T", "T0")] + min(timetonextevent)
    
    if(which.min(timetonextevent) == 1){ #arrive
      if(methodname == "Particle Filter" | methodname == "particlefilter" | methodname == "Join the Shortest Estimated Queue 2"){
        pf = method(laststate = state[i,], sim = simulations, mu = c(mu1, mu2))
        simulations = pf$sim
        change = pf$change
      } else{
        change = method(laststate = state[i,], mu = c(mu1, mu2))
      }
      state[i, c("N1", "N2")] = state[i, c("N1", "N2")] + change
      state[i, c("A1", "A2")] = state[i, c("A1", "A2")] + change
      timetonextevent = timetonextevent - min(timetonextevent)
      timetonextevent["arrive"] = rexp(1, lambda)
      
    }
    
    else if(which.min(timetonextevent) == 2){ #leave1
      
      if(state[i, "N1"] > 0){
        state[i,"N1"] = state[i,"N1"]-1
      }
      timetonextevent = timetonextevent - min(timetonextevent)
      timetonextevent["leave1"] = rexp(1, mu1)
    }
    else if(which.min(timetonextevent) == 3){ #leave2
      if(state[i, "N2"] > 0){
        state[i,"N2"] = state[i,"N2"]-1
      }
      timetonextevent = timetonextevent - min(timetonextevent)
      timetonextevent["leave2"] = rexp(1, mu2)
    }
    else{ #update
      if(methodname == "Particle Filter"| methodname == "particlefilter" | methodname == "Join the Shortest Estimated Queue 2"){
        state[i, "T0"] = 0
        state[i, c("A1", "A2")] = 0
        state[i, c("N1_", "N2_")] = state[i, c("N1", "N2")]
        pf = method(laststate = state[i,], sim = simulations, mu = c(mu1, mu2))
        simulations = pf$sim
        timetonextevent = timetonextevent - min(timetonextevent)
        timetonextevent["update"] = rexp(1, beta)
      } else{
        state[i, "T0"] = 0
        state[i, c("A1", "A2")] = 0
        state[i, c("N1_", "N2_")] = state[i, c("N1", "N2")]
        timetonextevent = timetonextevent - min(timetonextevent)
        timetonextevent["update"] = rexp(1, beta)
      }
    }
  }
  if(methodname == "particlefilter"){
    return(list(state = state, sim = simulations))
  } else {
    return(state = state)
  }
}

asyncsimulation = function(lambda = 0.4, mu1 = 0.25, mu2 = 0.25, beta1 = 0.1, beta2 = 0.1, ntime = 100, startingstate  = c(0, 10, 10, 10, 10, 0, 0, 0, 0), method = randomqueue, methodname = "notparticlefilter"){
  #function to simulate a process with asynchronous updates
  
  state = as.data.frame(matrix(nrow = ntime, ncol = 9))
  colnames(state) = c("T", "N1", "N2", "N1_", "N2_", "A1", "A2", "T1", "T2")
  state[1,] = startingstate
  if(methodname == "Particle Filter"| methodname == "particlefilter"){
    simulations = lapply(1:100, function(i) {
      df <- data.frame(matrix(c(state$T[1], state$N1[1], state$N2[1]), ncol = 3))
      colnames(df) <- c("T", "N1", "N2")
      df
    })
  } else if(methodname == "Join the Shortest Estimated Queue 2"){
    jseqdf = function() {
      df <- data.frame(matrix(c(state$T[1], state$N1[1], state$N2[1]), ncol = 3))
      colnames(df) <- c("T", "N1", "N2")
      df
    }
    simulations = jseqdf()
  }
  
  timetonextevent = c(rexp(1, lambda), rexp(1, mu1), rexp(1, mu2), rexp(1, beta1), rexp(1, beta2))
  names(timetonextevent) = c("arrive", "leave1", "leave2", "update1", "update2")
  for (i in 2:ntime){
    
    state[i,] = state[i-1,]
    state[i, c("T", "T1", "T2")] = state[i-1, c("T", "T1", "T2")] + min(timetonextevent)
    
    if(which.min(timetonextevent) == 1){ #arrive
      if(methodname == "Particle Filter"| methodname == "particlefilter" | methodname == "Join the Shortest Estimated Queue 2"){
        pf = method(laststate = state[i,], sim = simulations, mu = c(mu1, mu2))
        simulations = pf$sim
        change = pf$change
      } else{
        change = method(laststate = state[i,], mu = c(mu1, mu2))
      }
      state[i, c("N1", "N2")] = state[i, c("N1", "N2")] + change
      state[i, c("A1", "A2")] = state[i, c("A1", "A2")] + change
      timetonextevent = timetonextevent - min(timetonextevent)
      timetonextevent["arrive"] = rexp(1, lambda)
    }
    else if(which.min(timetonextevent) == 2){ #leave1
      
      if(state[i, "N1"] > 0){
        state[i,"N1"] = state[i,"N1"]-1
      }
      timetonextevent = timetonextevent - min(timetonextevent)
      timetonextevent["leave1"] = rexp(1, mu1)
    }
    else if(which.min(timetonextevent) == 3){ #leave2
      if(state[i, "N2"] > 0){
        state[i,"N2"] = state[i,"N2"]-1
      }
      timetonextevent = timetonextevent - min(timetonextevent)
      timetonextevent["leave2"] = rexp(1, mu2)
    }
    else if(which.min(timetonextevent) == 4){ #update1
      if(methodname == "Particle Filter"| methodname == "particlefilter" | methodname == "Join the Shortest Estimated Queue 2"){
        state[i, "T1"] = 0
        state[i, "A1"] = 0
        state[i, c("N1_")] = state[i, c("N1")]
        pf = method(laststate = state[i,], sim = simulations, mu = c(mu1, mu2))
        simulations = pf$sim
        timetonextevent = timetonextevent - min(timetonextevent)
        timetonextevent["update1"] = rexp(1, beta1)
      } else{
        state[i, "T1"] = 0
        state[i, "A1"] = 0
        state[i, "N1_"] = state[i, "N1"]
        timetonextevent = timetonextevent - min(timetonextevent)
        timetonextevent["update1"] = rexp(1, beta1)
      }
    }    
    else{ #update2
      if(methodname == "Particle Filter"| methodname == "particlefilter" | methodname == "Join the Shortest Estimated Queue 2"){
        state[i, "T2"] = 0
        state[i, "A2"] = 0
        state[i, "N2_"] = state[i, "N2"]
        pf = method(laststate = state[i,], sim = simulations, mu = c(mu1, mu2))
        simulations = pf$sim
        timetonextevent = timetonextevent - min(timetonextevent)
        timetonextevent["update2"] = rexp(1, beta2)
      } else{
        state[i, "T2"] = 0
        state[i, "A2"] = 0
        state[i, "N2_"] = state[i, "N2"]
        timetonextevent = timetonextevent - min(timetonextevent)
        timetonextevent["update2"] = rexp(1, beta2)
      }
    }
  }
  if(methodname == "particlefilter"){
    return(list(state = state, sim = simulations))
  } else {
    return(state = state)
  }
}

#creating sample paths for synchronous simulations 
plotsync = function(ntime = 1000, lambda = 0.4, mu1 = 0.25, mu2 = 0.25, beta = 0.01,  startingstate  = c(0, 10, 10, 10, 10, 0, 0, 0), method = randomqueue, methodname = "notparticle"){
  datas = syncsimulation(ntime = ntime, lambda = lambda, mu1 = mu1, mu2 = mu2, beta = beta,  startingstate  = startingstate, method = method, methodname = methodname)
  
  ggplot(data = datas) +
    geom_line(aes(x = `T`, y=`N1`, colour = "N1/ \U003BC 1"), size = 1.1) +
    geom_line(aes(x = `T`, y=`N2`, colour = "N2/ \U003BC 2"), size = 1.1) + 
    labs(subtitle = paste("\u03BB = ", lambda, ", \U003BC 1 = ", mu1, ", \U003BC 2 = ", mu2, ", \u03B2 = ", beta, sep= "")) +
    xlab("Time") + 
    ylab("Queue Length") + 
    labs(colour='Queue') +
    ggtitle(paste(methodname, " with synchronous updates", sep = "")) +
    geom_vline(data = subset(datas, T0 == 0), aes(xintercept = subset(datas, T0 == 0)$T), linetype = "dashed", color = "black")
}

#creating sample paths for asynchronous simulations 
plotasync = function(ntime = 1000, lambda = 0.4, mu1 = 0.25, mu2 = 0.25, beta1 = 0.01,  beta2 = 0.01, startingstate  = c(0, 10, 10, 10, 10, 0, 0, 0, 0), method = randomqueue, methodname = "notparticle"){
  dataa = asyncsimulation(ntime = ntime, lambda = lambda, mu1 = mu1, mu2 = mu2, beta1 = beta1, beta2 = beta2, startingstate  = startingstate, method = method, methodname = methodname)
  
  ggplot(data = dataa) +
    geom_line(aes(x = `T`, y=`N1`, colour = "N1/ \U003BC 1"), size = 1.1) +
    geom_line(aes(x = `T`, y=`N2`, colour = "N2/ \U003BC 2"), size = 1.1) + 
    labs(subtitle = paste("\u03BB = ", lambda, ", \U003BC 1 = ", mu1, ", \U003BC 2 = ", mu2, ", \u03B2 1 = ", beta1, ", \u03B2 2 = ", beta2, sep= "")) +
    xlab("Time") + 
    ylab("Queue Length") + 
    labs(colour='Queue') +
    ggtitle(paste(methodname, " with asynchronous updates", sep = "")) +
    geom_vline(data = subset(dataa, T1 == 0), aes(xintercept = subset(dataa, T1 == 0)$T), linetype = "dashed", color = "red") +
    geom_vline(data = subset(dataa, T2 == 0), aes(xintercept = subset(dataa, T2 == 0)$T), linetype = "dashed", color = "blue") 
}


#data = syncsimulation(ntime = 1000, mu1 = 0.1, mu2 = 0.4, method = particlefilter, methodname = "particlefilter", beta = 0.01)
#to plot sample path of particle filter in synchronous case
#the titles and subtitles are not automated for this yet and must be manually changed

particleplot = function(ntime = 1000, lambda = 0.4, mu1 = 0.1, mu2 = 0.4, method = particlefilter, methodname = "particlefilter", beta = 0.01){ 
  #method used when generating data must be particle filter
  data = syncsimulation(ntime = ntime, lambda = lambda, mu1 = mu1, mu2 = mu2, method = method, methodname = methodname, beta = beta)
  plot(x = NULL, y = NULL, xlim = c(200, 500), ylim = c(0, 40), 
       type = "n", xlab = "Time", ylab = "Wait Length", main = "Particle filter (synchronous updates)")
  mtext(paste("\u03BB = ", lambda, ", \U003BC 1 = ", mu1, ", \U003BC 2 = ", mu2, ", \u03B2 = ", beta, sep= ""))
  
  # Loop through the list of data frames and add lines to the plot
  for (i in 1:100) {
    lines(data$sim[[i]][,1], data$sim[[i]][,2]/0.1, col = rgb(248, 118, 109, max = 255, alpha = 20))
    lines(data$sim[[i]][,1], data$sim[[i]][,3]/0.4, col = rgb(0, 191, 196, max = 255, alpha = 10))
  }
  lines(data$state[,1], data$state[,2], col = "red")
  lines(data$state[,1], data$state[,3], col = "blue")
  
  t0_zero_indices <- which(data$state[,8] == 0)
  vertical_x_values <- data$state[,1][t0_zero_indices]
  for (x in vertical_x_values) {
    abline(v = x, col = "black", lty = "dashed")
  }
  
  # Add a legend
  legend("topright", legend = c("N1/mu1", "N2/mu2"), col = c("red", "blue"), lty = 1)
}


#to plot sample path of particle filter in asynchronous case
particleplotasync = function(ntime = 1000, method = particlefilterasync, methodname = "particlefilter", beta1 = 0.01, beta2= 0.01, mu1 = 0.1, mu2 = 0.4, lambda = 0.4){
  data = asyncsimulation(ntime = ntime, method = method, methodname = methodname, beta1 = beta1, beta2= beta2, mu1 = mu1, mu2 = mu2, lambda = lambda)
  plot(x = NULL, y = NULL, xlim = c(0, 1000), ylim = c(0, 40), 
     type = "n", xlab = "Time", ylab = "Wait Length", main = "Particle filter with asynchronous updates")
mtext(paste("\u03BB = ", lambda, ", \U003BC 1 = ", mu1, ", \U003BC 2 = ", mu2, ", \u03B2 1 = ", beta1, ", \u03B2 2 = ", beta2, sep= ""))

# Loop through the list of data frames and add lines to the plot
for (i in 1:100) {
  lines(data$sim[[i]][,1], data$sim[[i]][,2]/0.1, col = rgb(248, 118, 109, max = 255, alpha = 20))
  lines(data$sim[[i]][,1], data$sim[[i]][,3]/0.4, col = rgb(0, 191, 196, max = 255, alpha = 10))
}
lines(data$state[,1], data$state[,2], col = "red")
lines(data$state[,1], data$state[,3], col = "blue")

t1_zero_indices <- which(data$state[,8] == 0)
vertical_t1_values <- data$state[,1][t1_zero_indices]
for (x in vertical_t1_values) {
  abline(v = x, col = "red", lty = "dashed")
}

t2_zero_indices <- which(data$state[,9] == 0)
vertical_t2_values <- data$state[,1][t2_zero_indices]
for (x in vertical_t2_values) {
  abline(v = x, col = "blue", lty = "dashed")
}


# Add a legend
legend("topright", legend = c("N1/mu1", "N2/mu2"), col = c("red", "blue"), lty = 1)
}

#long term sampling and finding summary statistics for synchronous simulations

difference = function(ntime = 500, lambda = 0.4, mu1 = 0.1, mu2 = 0.4, beta = 0.01,  startingstate  = c(0, 10, 10, 10, 10, 0, 0, 0), method = jseq1async, nsim = 30, methodname = "jseq1async"){
  x = matrix(ncol = 6)
  colnames(x) = c("absolute difference", "difference", "absolute proportional difference", "proportional difference", "average queue 1 length", "average queue 2 length")
  
  
  data = syncsimulation(ntime = ntime*nsim+ntime, lambda = lambda, mu1=mu1, mu2=mu2, beta=beta, startingstate=startingstate, method = method, methodname = methodname)
  a = matrix(nrow = nsim, ncol = 6)
  for(i in 1:nsim){
    absdiff = abs(data$N1[(ntime*i):(ntime*i+ntime)] - data$N2[(ntime*i):(ntime*i+ntime)])
    a[i,1] = mean(absdiff)
    
    diff = data$N1[(ntime*i):(ntime*i+ntime)] - data$N2[(ntime*i):(ntime*i+ntime)]
    a[i,2] = mean(diff)
    
    abspropdiff = abs(data$N1[(ntime*i):(ntime*i+ntime)]/mu1 - data$N2[(ntime*i):(ntime*i+ntime)]/mu2)
    a[i,3] = mean(abspropdiff)
    
    propdiff = data$N1[(ntime*i):(ntime*i+ntime)]/mu1 - data$N2[(ntime*i):(ntime*i+ntime)]/mu2
    a[i,4] = mean(propdiff)      
    
    a[i,5] = mean(data$N1[(ntime*i):(ntime*i+ntime)])
    a[i,6] = mean(data$N2[(ntime*i):(ntime*i+ntime)])
    
  }
  x = rbind(x, a)
  
  x = x[-1,]
  
  folder_name <- paste("lambda=", lambda, ", mu1=", mu1, ", mu2=", mu2, ", beta=", beta, ", method=", methodname, sep= "")
  dir.create(folder_name)
  
  dataname = paste(folder_name, "data.csv", sep = "/")
  write.csv(x, dataname)
  
  summaries = matrix(nrow = 2, ncol = 8)
  colnames(summaries) = c("absolute difference", "difference", "absolute proportional difference", "proportional difference", "average queue 1 length", "average queue 2 length", "average queue 1 wait", "average queue 2 wait")
  rownames(summaries) = c("mean", "variance")
  summaries[1,1:8] = c(mean(x[,1]), mean(x[,2]), mean(x[,3]), mean(x[,4]), mean(x[,5]), mean(x[,6]), mean(x[,5])/mu1, mean(x[,6])/mu2)
  summaries[2,1:8] = c(var(x[,1]), var(x[,2]), var(x[,3]), var(x[,4]), var(x[,5]), var(x[,6]), var(x[,5]/mu1), var(x[,6]/mu2))
  
  summariesname = paste(folder_name,"/", round(runif(1),4), "summaries.csv", sep = "")
  write.csv(summaries, summariesname)
  
}

#long term sampling and finding summary statistics for asynchronous simulations
differenceasync = function(ntime = 500, lambda = 0.4, mu1 = 0.1, mu2 = 0.4, beta1 = 0.01, beta2 = 0.01,  startingstate  = c(0, 10, 10, 10, 10, 0, 0, 0, 0), method = jseq1async, nsim = 100, methodname = "jseq1async"){
  x = matrix(ncol = 6)
  colnames(x) = c("absolute difference", "difference", "absolute proportional difference", "proportional difference", "average queue 1 length", "average queue 2 length")
  
  data = asyncsimulation(ntime = ntime*nsim+ntime, lambda = lambda, mu1=mu1, mu2=mu2, beta1=beta1, beta2=beta2, startingstate=startingstate, method = method, methodname = methodname)
  a = matrix(nrow = nsim, ncol = 6)
  for(i in 1:nsim){
    absdiff = abs(data$N1[(ntime*i):(ntime*i+ntime)] - data$N2[(ntime*i):(ntime*i+ntime)])
    a[i,1] = mean(absdiff)
    
    diff = data$N1[(ntime*i):(ntime*i+ntime)] - data$N2[(ntime*i):(ntime*i+ntime)]
    a[i,2] = mean(diff)
    
    abspropdiff = abs(data$N1[(ntime*i):(ntime*i+ntime)]/mu1 - data$N2[(ntime*i):(ntime*i+ntime)]/mu2)
    a[i,3] = mean(abspropdiff)
    
    propdiff = data$N1[(ntime*i):(ntime*i+ntime)]/mu1 - data$N2[(ntime*i):(ntime*i+ntime)]/mu2
    a[i,4] = mean(propdiff)      
    
    a[i,5] = mean(data$N1[(ntime*i):(ntime*i+ntime)])
    a[i,6] = mean(data$N2[(ntime*i):(ntime*i+ntime)])
  }
  x = rbind(x, a)
  
  x = x[-1,]
  folder_name <- paste("lambda=", lambda, ", mu1=", mu1, ", mu2=", mu2, ", beta1=", beta1, ", beta2=", beta2, ", method=", methodname, sep= "")
  dir.create(folder_name)
  
  dataname = paste(folder_name, "data.csv", sep = "/")
  write.csv(x, dataname)
  
  summaries = matrix(nrow = 2, ncol = 8)
  colnames(summaries) = c("absolute difference", "difference", "absolute proportional difference", "proportional difference", "average queue 1 length", "average queue 2 length", "average queue 1 wait", "average queue 2 wait")
  rownames(summaries) = c("mean", "variance")
  summaries[1,1:8] = c(mean(x[,1]), mean(x[,2]), mean(x[,3]), mean(x[,4]), mean(x[,5]), mean(x[,6]), mean(x[,5])/mu1, mean(x[,6])/mu2)
  summaries[2,1:8] = c(var(x[,1]), var(x[,2]), var(x[,3]), var(x[,4]), var(x[,5]), var(x[,6]), var(x[,5]/mu1), var(x[,6]/mu2))
  
  summariesname = paste(folder_name, "/", round(runif(1),4), "summaries.csv", sep = "")
  write.csv(summaries, summariesname)
  
}