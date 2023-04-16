# - stringr : str_replace
# - emmeans : emmeans
# - multcomp : cld

cld.grp.letters = function(emm){
  if(!require("stringr")){
    install.packages("stringr",repos="http://cran.irsn.fr")
    require("stringr")
  }#Pour str_replace
  if(!require("emmeans")){
    install.packages("emmeans",repos="http://cran.irsn.fr")
    require("emmeans")
  }#Pour emmeans
  if(!require("multcomp")){
    install.packages("multcomp",repos="http://cran.irsn.fr")
    require("multcomp")
  }#Pour cld
  cld = cld(emm,detail=TRUE)
  semm = summary(emm)
  cld = cld$emmeans
  nb.grp = max(nchar(cld$.group))-1
  ###Calcule du emmeans
  for(i in 1:nrow(cld)){
    cld$.group = str_replace(cld$.group, as.character(as.factor(i)),letters[i])
  }
  #On retir les espaces
  for(i in 1:nrow(cld)){
    cld$.group = sub(" ","",cld$.group)
  }
  #On récupère
  bsign = NULL
  for(i in 1:nrow(cld)){
    for(j in 1:nrow(cld)){
      if(cld[j,1] == semm[i,1] && cld[j,2] == semm[i,2]){
        bsign = c(c(bsign),c(cld$.group[j]))
      }
    }
  }
  ##Remet les lettres dans le bon ordre
  letters.grp = letters[1:nb.grp]
  bsign = strsplit(bsign, "")
  for(i in 1:length(bsign)){
    for(j in 1:length(bsign[[i]])){
      for(e in 1:length(letters.grp)){
        if(bsign[[i]][j]==letters.grp[e]){
          sign = length(letters.grp)-e+1
          bsign[[i]][j] = letters.grp[sign]
          cat(letters.grp[sign])
          break()
        }

      }
    }
  }
  bsign.grp = NULL
  for(i in 1:length(bsign)){
    bsign.grp = c(bsign.grp,paste(sort(bsign[[i]]), collapse = ""))
  }
  return(bsign.grp)
}




