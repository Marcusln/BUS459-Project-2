predict.knn=function(x0,x,y,k=3)
  # x0 = 
  # x = 
  # y =
  # k = number of neighbors
{
  dist=abs(x0-x) # calculate distance 
  rank.dist=rank(dist,ties.method="random")
  ypred=mean(y[rank.dist<=k])
  return(ypred)
}

predict.knn()

data1$dist = 0
for(i in 1:nrow(data1)) {
  data1$dist[i] = data1$y[i + 1] - data1$y[i]
}





#
## Explore data
###
plot(data1$x,data1$y)
n_distinct(data1$y) # all values distinct
