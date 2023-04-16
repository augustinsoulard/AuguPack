turn_over = function(site1,site2,title){
  tab= data.frame(matrix(nrow = 1,ncol=4))
  if(missing(title)){
    colnames(tab)=c("site1","site1+site2","site2","Absent")
  }else{
    colnames(tab) = title
  }

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
  return(tab)
}
