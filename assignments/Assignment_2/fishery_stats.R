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

#############################
# Here's the function

fishery_stats = function(catch, graph = FALSE, total_as_text = FALSE) {
# Create data frame for most frequent fish caught at each location.
  frequency = as.data.frame(matrix(nrow=ncol(catch), ncol=3))
  for(i in 1:ncol(catch)){
    frequency[i,1] = colnames(catch)[i] 
    frequency[i,2] = names(which.max(summary(catch[[i]])))
    frequency[i,3] = max(summary(catch[[i]]))
  }
  
# Create data frame of counts of each fish at each location
  revenue = as.data.frame(matrix(nrow=nrow(prices), ncol=ncol(catch)))
  for(i in 1:ncol(catch)){
    revenue[i] = summary(catch[[i]])
  }
  names= names(summary(catch[[1]]))
  revenue$fish = names
  
# Add prices to dataframe
  for(i in 1:nrow(revenue))
  {
    for (j in 1:nrow(prices))
    {
      if(revenue$fish[i] == prices$fish[j]) {revenue$price[i] = prices$price[j]} 
    }
  }
  
# Calculate revenue for each fish at each location
  revenue_df= as.data.frame(matrix(nrow=nrow(prices), ncol=ncol(catch)))
  for(i in 1:ncol(catch)){
    revenue_df[,i] = revenue[,i] * revenue$price
  }

# Calculate total revenue for each location 
  revenue_df1= as.data.frame(matrix(nrow=1, ncol=ncol(catch)))
  for(i in 1:ncol(catch)){
    revenue_df1[i]=sum(revenue_df[,i])
  }

# Calculate total fishery revenue  
  total_revenue=sum(revenue_df1)
  
  if(graph == TRUE){
    revenue_df_longer= pivot_longer(data = revenue_df1, cols = V1:V2, names_to = "location", values_to = "revenue")
    
    rev_graph= ggplot(data = revenue_df_longer, aes(x = location, y = revenue)) +
      geom_col()
  }

  return(list(frequency, revenue_df1, total_revenue, rev_graph))
}


fishery_stats(catch, graph = TRUE)

