### Assignment 2: Fishing

library(tidyverse)

# create vectors of possible fish, possible locations, and fish prices
possible.fish = c("salmon","steelhead","shark","tuna","cod")
possible.locations = c("A","B","C","D","E")
fish.prices = runif(min=5, max=10, n=length(possible.fish))

# Make table of prices for fish
prices = data.frame(fish = possible.fish, price = fish.prices) %>% 
  arrange(fish)

# Generate catch data for two sites
catch_reef = sample(possible.fish, size=50, prob = c(0.2, 0.2, 0.1, 0.1, 0.4), replace=T)
catch_ocean = sample(possible.fish, size=50, prob = c(0.4, 0.2, 0.1, 0.1, 0.2), replace=T)
  
catch= data.frame(catch_reef, catch_ocean)

##########################
# Get counts of each fish for each location

revenue = data.frame(matrix(nrow=nrow(prices), ncol=ncol(catch)))
for(i in 1:ncol(catch)){
  revenue[i] = summary(catch[[i]])
}

# Add fish names to dataframe
names= names(summary(catch[[1]]))
revenue$fish = names

############################
# How to get fish prices into same dataframe as fish counts to do revenue calculations?
revenue1 <- revenue %>% 
  mutate(price = case_when(
    fish == prices$fish[1]  ~ prices$price[1]))

revenue$price = case_when(
    fish == prices$fish[1]  ~ prices$price[1])

### One way to get prices into the revenue data frame?
for(i in 1:length(revenue))
  {
  for (j in 1:length(prices))
  {
  revenue$price[i]= case_when(
    revenue$fish[i] == prices$fish[j]  ~ prices$price[j])
  }
}

### Another way to do it?
for(i in 1:length(revenue))
{
  for (j in 1:length(prices))
  {
    if(revenue$fish[i] == prices$fish[j]) {revenue$price[i] = prices$price[j]} 
  }
}

#### But neither one works :(

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
  
  #Create data frame of counts of each fish at each location
  revenue = as.data.frame(matrix(nrow=nrow(prices), ncol=ncol(catch)))
  for(i in 1:ncol(catch)){
    revenue[i] = summary(catch[[i]])
  }
  names= names(summary(catch[[1]]))
  revenue$fish = names
  
  return(revenue)
}

fishery_stats(catch)
