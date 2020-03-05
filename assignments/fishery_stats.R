### Assignment 2: Fishing

# create vector of possible fish 
possible.fish = c("salmon","steelhead","shark","tuna","cod")
possible.locations = c("A","B","C","D","E")
fish.prices = runif(min=5, max=10, n=length(possible.fish))

# we can use sample to simulate a random recording of catch by fisherman, lets say we pick 20 fish from the net

prices = data.frame(fish = possible.fish, price = fish.prices)

catch_reef = sample(possible.fish, size=50, prob = c(0.2, 0.2, 0.1, 0.1, 0.4), replace=T)
catch_ocean = sample(possible.fish, size=50, prob = c(0.4, 0.2, 0.1, 0.1, 0.2), replace=T)
  
catch= data.frame(catch_reef, catch_ocean)

catch


fishery_stats = function(catch) {
  data = as.data.frame(matrix(nrow=ncol(catch), ncol=3))
  for(i in 1:ncol(catch)){
    data[i,1] = colnames(catch)[i] 
    data[i,2] = names(which.max(summary(catch[[i]])))
    data[i,3] = max(summary(catch[[i]]))
  }
  return(data)
  }

fishery_stats(catch)
