AbrevSp = function(sp,size = 3){
  abrev = substring(sp,1,size)
  long = length(sp)-1
  for(i in 1:long){
    if(abrev[i]==abrev[i+1]){
      j = 0
      while(substring(sp[i],1,size)==substring(sp[i+j],1,size) & isTRUE(i+j==length(sp))==FALSE){
        j=j+1
        abrev[i]= paste(substring(sp[i],1,size),"1")
        abrev[i+j]= paste(substring(sp[i],1,size),as.character(1+j))
      }
    }
  }
  sp = data.frame(sp)
  abrev = data.frame(abrev)
  tab = cbind(abrev,sp)
  return(tab)
}
?AbrevSp
