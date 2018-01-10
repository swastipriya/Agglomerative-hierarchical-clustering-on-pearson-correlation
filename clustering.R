# converting clusters pair stored in static array into dynamic list

singleele = c()
for( i in 1:nrow(pm))
{
  if(!(i %in% clust))
  {
    singleele = c(singleele, i)
  }
}
# forming the clusters from the array
clusters = list()
clust_array = c()
for(k in seq(from=1, to=length(clust), by= 2))
{
  present = 0
  if(is.null(clust_array))
  {
    clusters = list(c(clust[1], clust[2]))
    clust_array = c(clust_array, clust[1], clust[2])
  }
  else
  {
    if(clust[k] %in% clust_array || clust[k+1] %in% clust_array)
    {
      # if both are present in the same cluster
      present = 1
      f=0
      s=0
      for(i in 1: length(clusters))
      {
        for(j in 1:length(clusters[[i]]))
        {
          if(clusters[[i]][j] == clust[k])
          {
            f = i
          }
          if(clusters[[i]][j] == clust[k+1])
          {
            s = i
          }
        }
      }
      # if both first and second element are already present in the clusters
      if( f != 0 && s != 0)
      {
        clusters[[f]] = c(clusters[[f]], clusters[[s]])
        clusters[[s]] = 0
      }
      # if only first element is present in the clusters
      else if( f != 0 )
      {
        clusters[[f]][length(clusters[[f]]) + 1] = clust[k+1]
        clust_array=c(clust_array, clust[k+1])
      }
      # if only second element is present element in the clusters
      else if(s != 0)
      {
        clusters[[s]][length(clusters[[s]]) + 1] = clust[k]
        clust_array = c(clust_array, clust[k])
      }
    }
    
    else 
    {
      clusters[[length(clusters) + 1]] = c(clust[k], clust[k+1])
      #clusters[length(clusters) + 1] = list(c(clust[k], clust[k+1]))
      clust_array = c(clust_array, clust[k], clust[k+1])
    }
  }
}
for( i in 1:length(singleele))
{
  clusters[[length(clusters) + 1]] = singleele[i]
  clust_array = c(clust_array, singleele[i])
}
