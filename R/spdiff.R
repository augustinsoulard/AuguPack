spdiff = function(site1,site2){
  site01 = NULL
  site12 = NULL
  site02 = NULL
  absent = NULL

  for(i in 1:length(site1)){
    if(site1[i]==0 & site2[i]==0){ absent = c(absent,names(site1[i]))

    }
    if(site1[i]>0 & site2[i]>0){site12 = c(site12,names(site1[i]))

    }
    if(site1[i]>0 & site2[i]==0){site01 = c(site01,names(site1[i]))

    }
    if(site1[i]==0 & site2[i]>0){site02 = c(site02,names(site1[i]))

    }

  }
  list = list(site1 = site01,site12 = site12,site2 = site02,absent = absent)
  return(list)
}
