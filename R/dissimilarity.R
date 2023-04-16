dissimilarity = function(site1,site2,method="Sorenson"){
  tab= data.frame(matrix(nrow = 1,ncol=4))
  tab[1,]=0
  for(i in 1:length(site1)){
    if(site1[i]==0 & site2[i]==0){ tab[1,4]=tab[1,4]+1

    }
    if(site1[i]>0 & site2[i]>0){tab[1,2]=tab[1,2]+1

    }
    if(site1[i]>0 & site2[i]==0){tab[1,1]=tab[1,1]+1

    }
    if(site1[i]==0 & site2[i]>0){tab[1,3]=tab[1,3]+1

    }

  }
  if(method=="Sorenson"){
    dis = (tab[1,1]+tab[1,3])/(2*tab[1,2]+tab[1,1]+tab[1,3])
  } else if(method=="Simpson"){
    dis = (min(tab[1,1],tab[1,3]))/(tab[1,2]+min(tab[1,1],tab[1,3]))
  } else if(method=="Nestedness"){
    dis =(tab[1,1]+tab[1,3])/(2*tab[1,2]+tab[1,1]+tab[1,3])-((min(tab[1,1],tab[1,3]))/(tab[1,2]+min(tab[1,1],tab[1,3])))
  }else{
    cat("Use correct method ;P")
  }

  cat(method,"Dissimilarity :")
  return(dis)
}
