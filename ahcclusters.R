##converting the clusters array into a dynamic clusters list

# forming the clusters from the array
clusters = list()
for(k in seq(from=1, to=length(clust), by= 2))
{
  #print(i)
  if(length(clusters) ==0)
  {
    clusters = list(c(clust[1], clust[2]))
  }
  else
  {
    present = 0
    
    for( i in 1:length(clusters))
    {
      for( j in 1: length(clusters[[i]]))
      {
        if(clusters[[i]][j] == clust[k])
        {
          present = 1
          clusters[[i]][length(clusters[[i]]) + 1] = clust[k+1]
          break 
        }
        else if(clusters[[i]][j] == clust[k+1])
        {
          present = 1
          clusters[[i]][length(clusters[[i]]) + 1] = clust[k]
          break
        }
      }
    }
    
    if(present == 0)
    {
      clusters[i+1] = list(c(clust[k], clust[k+1]))
    }
  }
  
}
