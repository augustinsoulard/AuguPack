# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

names.col = function(data,col=1){
  names = NULL
  for(i in 1:ncol(data)){
    names = c(names,as.character(data[col,i]))
  }
  colnames(data)=names
  if(typeof(col)=="double"){
    data = data[-col,]
  } else if(typeof(col)=="character"){
    data = data[row.names(data)!=col,]
  }

  else{
    cat("Bad data type in select row")
  }
  return(data)
}
