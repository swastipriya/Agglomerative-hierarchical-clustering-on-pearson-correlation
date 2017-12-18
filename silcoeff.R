#silhouette coefficient

sc= c()

#nulling out zeroes in the main diagonal in pm
for(i in 1:nrow(pm1))
{
  for(j in 1:ncol(pm1))
  {
    if( i==j)
      pm1[i, j] =NA
  }
}

for(k in 1: nrow(pm1))
{
  #calculation of ai
  present = 0
  for(i in 1:length(clusters))
  {
    for(j in 1:length(clusters[[i]]))
    {
      if(k == clusters[[i]][j])
      {
        present=1
        break
      }
    }
    if(present == 1)
      break;
  }
  d= pm1[k, clusters[[i]][1:length(clusters[[i]])]]
  d= d[!is.na(d)]
  ai= sum(d)/length(d)
  
  #calculation of bi
  mean_d=c()
  
  for(a in 1:length(clusters))
  {
    if(a != i)
    {
      d= pm1[k, clusters[[a]][1:length(clusters[[a]])]]
      d= d[!is.na(d)]
      mean_d= c(mean_d,(sum(d)/length(d)) )
    }
  }
  bi = max(mean_d)
  
  #calculation of silhouette coefficient
  sc= c(sc, ((bi-ai)/min(ai, bi)) )
}

sil_c = sum(sc)/length(sc)
