numerise = function(data,factor=NULL){
  if(is.null(factor)==FALSE){
    for(i in 1:ncol(data)){
      fac = FALSE
      for(j in 1:length(factor)){
        if(isTRUE(colnames(data)[i]==colnames(data)[factor[j]]) | colnames(data)[i]==factor[j]){
          fac = TRUE}
      }
      if(fac==FALSE){
        data[,i]=as.numeric(as.character(data[,i]))
      }
    }
  } else{
    for(i in 1:ncol(data)){
     data[,i]=as.numeric(as.character(data[,i]))
    }
  }
  return(data)
}

