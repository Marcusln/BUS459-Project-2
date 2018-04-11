predict.knn=function(x0,x,y,k=3)
{
  dist=abs(x0-x)
  rank.dist=rank(dist,ties.method="random")
  ypred=mean(y[rank.dist<=k])
  return(ypred)
}