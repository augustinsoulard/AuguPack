equitability = function(data){
  div=data.frame(div = diversity(data, index = "shannon"))
  equi=data.frame(equi = div$div/log(specnumber(data),base=exp(1)))
  return(equi)
}

