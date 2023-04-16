NAto0 = function(data){
  for(i in 1:nrow(data)){
    for(j in 1:ncol(data)){
      if(is.na(data[i,j])){data[i,j]=0}
    }}
  return(data)
}
