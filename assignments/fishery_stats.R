### Assignment 2: Fishing

# create vector of possible fish, possible locations, and fish prices
possible.fish = c("salmon","steelhead","shark","tuna","cod")
possible.locations = c("A","B","C","D","E")
fish.prices = runif(min=5, max=10, n=length(possible.fish))

# we can use sample to simulate a random recording of catch by fisherman, lets say we pick 20 fish from the net

prices = data.frame(fish = possible.fish, price = fish.prices) %>% 
  arrange(fish)

catch_reef = sample(possible.fish, size=50, prob = c(0.2, 0.2, 0.1, 0.1, 0.4), replace=T)
catch_ocean = sample(possible.fish, size=50, prob = c(0.4, 0.2, 0.1, 0.1, 0.2), replace=T)
  
catch= data.frame(catch_reef, catch_ocean)

catch
############################
df = data.frame(summary(catch[[1]]))
names(summary(catch[[1]]))

##########################
revenue = data.frame(matrix(nrow=nrow(prices), ncol=ncol(catch)))
for(i in 1:ncol(catch)){
  revenue[i] = summary(catch[[i]])
}

names= names(summary(catch[[1]]))
names
revenue$fish = names

############################
revenue1 <- revenue %>% 
  mutate(price = case_when(
    fish == prices$fish[1]  ~ prices$price[1]))

#############################
# Write the function

fishery_stats = function(catch) {
#create data frame for most frequent fish caught at each location.
  frequency = as.data.frame(matrix(nrow=ncol(catch), ncol=3))
  for(i in 1:ncol(catch)){
    frequency[i,1] = colnames(catch)[i] 
    frequency[i,2] = names(which.max(summary(catch[[i]])))
    frequency[i,3] = max(summary(catch[[i]]))
  }
  
  revenue = as.data.frame(matrix(nrow=nrow(prices), ncol=ncol(catch)))
  for(i in 1:ncol(catch)){
    revenue[i] = summary(catch[[i]])
  }
  names= names(summary(catch[[1]]))
  revenue$fish = names
  
  data=data.frame((matrix(nrow=nrow(revenue), ncol=ncol(revenue))))
  for (i in 1:ncol(catch)) {
    for (j in 1:length(revenue)) {
      data[i,j]= if catch[ncol(catch),j] == price(value=damages[i],       discount=discount_rates[j],time=yr )
      
    }
  }
  
  return(revenue)
  }

fishery_stats(catch)
