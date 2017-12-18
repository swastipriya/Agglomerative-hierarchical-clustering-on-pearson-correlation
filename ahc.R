## agglomerative hierarchical clustering

#here pm is the matrix containing pearson correlation values between the instances/nodes
pm = proximity_mat[1:10, 1:10]

delta= 0.1 #threshold

pm1 = pm #alternate copy

# dist - table containing closest nodes and its distance
dist = matrix(0, nrow = nrow(pm), ncol=2)

#list of cluster : every odd and even pair form a cluster
clust = c()
f_users = c()

while( length(which(pm[, ] > delta)) > 0)
{
  for(i in 1:nrow(pm))
  {
    if(any((pm[i, ])) != 0 && (pm[i, which.max(pm[i, ])] > delta) ) 
    {
      dist[i, 1]=which(pm[i,] %in% max(pm[i, which(pm[i,] > delta)]))[1]
      dist[i, 2] =max(pm[i, which(pm[i, ] > delta)])
    }
    else
    {
      dist[i, 1] =0
      dist[i, 2] =0
    }
  }
  
  #loop for multiple maximum pearson values
  for( i in 1: length(which(dist[, 2] == max(dist[, 2]))))
  {
    f_users = c()
    # finding the max value in second column and its corresponding neighbor
    first_user=which(dist[, 2] == max(dist[, 2]))[i]
    second_user= dist[first_user,1]
    
    #updating proximity matrix
    # complete linkage =maximum distance between the nodes = minimum of two rows (Pearson Correlation values)
    if(any((pm[first_user, ])) != 0 && (pm[first_user, which.max(pm[first_user, ])] > delta) && any((pm[second_user, ])) != 0 && (pm[second_user, which.max(pm[second_user, ])] > delta))
    {
      if(!(first_user %in% f_users || second_user %in% f_users))
      { 
        clust = c(clust,c(first_user, second_user))
        f_users = c(f_users, first_user)
        for(j in 1:ncol(pm))
        {
          pm[first_user,j ]= min(pm[first_user,j ], pm[second_user, j])
          pm[j, first_user]= min(pm[j, first_user], pm[j, second_user])
        }
        
        pm[second_user,]= 0
        pm[, second_user]= 0
        
        diag(pm)= 0
      }
    }
  }
}
