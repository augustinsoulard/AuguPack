sptogenra = function(sp,sep="_"){
  split = str_split(row.names(sp), sep)
  tab =sp
  long = nrow(tab)-1

  lgenre= NULL
  ####Cretion des noms de genre
  for(i in 1:nrow(tab)){
    lgenre = c(lgenre,split[[i]][1])
  }
  ####Somme des ligne de même genre et mettre 0 si même genre
  for (i in 1:long){
    if(lgenre[i]==lgenre[i+1]){
      tab[i+1,]= tab[(i+1),]+tab[i,]
      row.names(tab)[i] =  paste("suppression",as.character(i))
    }
  }
  ###Suppression des ligne où juste des 0
  i =0
  while(i<nrow(tab)){
    i = i+1
    if(substring(row.names(tab)[i],1,11)=="suppression"){
      while(substring(row.names(tab)[i],1,11)=="suppression"){
        tab = tab[-i,]
        lgenre = lgenre[-i]
      }
    }
  }
  row.names(tab)=lgenre
  return(tab)
}
