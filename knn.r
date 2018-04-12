predict.knn=function(x0,x,y,k=3)
  # x0 = next y value
  # x = y value
  # y = predictor variable
  # k = number of neighbors to use
{
  # calculate manhattan distance
  dist = abs(x0-x)
  # return a vector with rank in terms of being the lowest value
  # lowest distance is rank.dist[84] which has value 1
  # if the distance between two or more points are equal,
  # these values are randomly inserted in the indeces of the values that
  # are equal
  rank.dist = rank(dist,ties.method="random")
  # rank.dist <= k fetches the indeces of the k lowest distances
  # y[rank.dist<=k] returns the value of the k lowest distances
  # take the mean of this value
  ypred = mean(y[rank.dist<=k])  # get the values 
  return(ypred)
}

# create variables
y.next = 0
y.curr = 0
return.knn = 0
# loop through rows
for(i in 1:nrow(data1)) {
  # store 
  y.next[i] = data1$y[i + 1]
  y.curr[i] = data1$y[i]
  return.knn[i] = predict.knn(x0 = y.next, x = y.curr, y = data1$y)
}

plot(data1$x, data1$y)
lines(data1$x, return.knn, col="blue")

# predict.knn = predict.knn(x0 = y.next, x = y.curr, y = data1$y)

# C)

# poor hacky solution to fill NA with 0
# data1$dist[100] = 0

# create variables
predicted = 0
loo.se = 0
i = 0
# loop through rows
for(i in i:nrow(data1)) {
  predicted[i] = predict.knn(x0 = data1$y[i + 1],
                              x = data1$y[i],
                              y = data1$y)
  print(paste0(data1$y[i + 1]))
  #print(paste0(predicted[i]))
}
loo.se
loo.mse = loo.se / nrow(data1)


rank.test = rank(data1$distAbs,ties.method="random")

data1$dist = 0
for(i in 1:nrow(data1)) {
  data1$dist[i] = data1$y[i + 1] - data1$y[i]
}




#
## Explore data
###
plot(data1$x,data1$y)
n_distinct(data1$y) # all values distinct
