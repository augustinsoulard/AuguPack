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

barmod=function(mod,formule,title="",errorbar = "IC",xlab=names(semm)[ncol(semm)-5],ylab = names(semm)[ncol(semm)-4],ymin = 0, ymax = (max(mean)+max(IC))*1.6,bartitle = btitle,barcolor=rainbow(nrow(summary(emm))),barsignification = bsign,hauteurText = (ymax-(max(mean)+max(IC)))/5,labelsize = 1.5,pos.legend ="topright",title.legend = colnames(semm)[1],horiz = TRUE)
{
  ##############Chargement des Packages
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
  ###Calcule du emmeans
  emm = emmeans(mod,formule,type="response")
  semm = summary(emm)
  ### Creation de barsignification par defaut
  cld1 = cld(emm, details = TRUE)$emmeans
  for(i in 1:nrow(cld1)){
    cld1$.group = str_replace(cld1$.group, as.character(as.factor(i)),letters[i])
  }
  #On retir les espaces
  for(i in 1:nrow(cld1)){
    cld1$.group = sub(" ","",cld1$.group)
  }
  #On récupère
  bsign = NULL
  for(i in 1:nrow(cld1)){
    for(j in 1:nrow(cld1)){
      if(cld1[j,1] == semm[i,1] && cld1[j,2] == semm[i,2]){
        bsign = c(c(bsign),c(cld1$.group[j]))
      }
    }
  }
  ###Initialisation des variables
  mean = NULL
  IC = NULL
  SE = NULL
  cformule = as.character(formule)
  splitformule = str_split(cformule," ")
  sansInter = cformule[2]== splitformule[[2]][1]

  if (sansInter == FALSE){
    #####Fonction perso remplaçant tapply tapply(tabdata[,1],list(tabdata[,2],tabdata[,3]),FUN="IC")
    cld1[,1] = as.factor(cld1[,1])
    cld1[,2] = as.factor(cld1[,2])
    lvl1 =levels(cld1[,1])
    lvl2 =levels(cld1[,2])
    # IC =  tapply(tabdata[,1],list(tabdata[,2],tabdata[,3]),FUN="??IC??")
    IC = matrix(data = 1:nrow(cld1), nrow = length(lvl1), ncol = length(lvl2), byrow = FALSE,
                dimnames = NULL)
    row.names(IC)= levels(cld1[,1])
    colnames(IC) = levels(cld1[,2])

    for(e in 1:nrow(cld1)){
      for(i in 1:length(lvl2)){
        for(j in 1:length(lvl1)){
          if(colnames(IC)[i]== cld1[e,2] && row.names(IC)[j]==cld1[e,1]){
            IC[j,i] = (cld1$asymp.UCL[e]-cld1$asymp.LCL[e])/2
          }
        }
      }
    }
    # SE =  tapply(tabdata[,1],list(tabdata[,2],tabdata[,3]),FUN="SE")
    SE = matrix(data = 1:nrow(cld1), nrow = length(lvl1), ncol = length(lvl2), byrow = FALSE,
                dimnames = NULL)
    row.names(SE)= levels(cld1[,1])
    colnames(SE) = levels(cld1[,2])

    for(e in 1:nrow(cld1)){
      for(i in 1:length(lvl2)){
        for(j in 1:length(lvl1)){
          if(colnames(SE)[i]== cld1[e,2] && row.names(SE)[j]==cld1[e,1]){
            SE[j,i] = cld1$SE[e]
          }
        }
      }
    }
    # mean =  tapply(tabdata[,1],list(tabdata[,2],tabdata[,3]),FUN="mean")
    mean = matrix(data = 1:nrow(cld1), nrow = length(lvl1), ncol = length(lvl2), byrow = FALSE,
                  dimnames = NULL)
    row.names(mean)= levels(cld1[,1])
    colnames(mean) = levels(cld1[,2])

    for(e in 1:nrow(cld1)){
      i = 0
      j = 0
      for(i in 1:length(lvl2)){
        for(j in 1:length(lvl1)){
          if(colnames(mean)[i]== cld1[e,2] && row.names(mean)[j]==cld1[e,1]){
            mean[j,i] = cld1[e,3]
          }
        }
      }
    }
    if(missing(bartitle)){
      bp = barplot(mean,beside=TRUE,main = title,legend.text = TRUE,ylim=c(ymin,ymax),xlab = xlab, ylab=ylab,args.legend = list(x = pos.legend, bty = "n",title=title.legend,horiz=horiz))

    }else{
      bp = barplot(mean,beside=TRUE,main = title,legend.text = TRUE,ylim=c(ymin,ymax),names.arg = bartitle,xlab = xlab, ylab=ylab,args.legend = list(x = pos.legend, bty = "n",title=title.legend,horiz=horiz))

    }

  }else{
    ###Creation de btitle pour remplir bartitle
    if(missing(bartitle)){
      btitle = NULL
      for (j in 1:nrow(semm)){
        lemm = as.character(semm[j,1])
        btitle = c(c(btitle),c(lemm))
      }
    }
    ##Calcule du graph simple
    for (i in 1:nrow(semm)){
      mean = c(c(mean),c(semm[i,ncol(semm)-4]))
      IC = c(c(IC),c((semm$asymp.UCL[i]-semm$asymp.LCL[i])/2))
      SE = c(c(SE),c((semm$SE[i])/2))
    }
    bp = barplot(mean,width = 0.5,space = 0.5,main = title,names.arg = bartitle,col=barcolor,ylim=c(ymin,ymax),xlab = xlab, ylab=ylab)

  }

  #########Création du graphique
  ###Condition pour choisir IC ou ES
  if(errorbar == "IC"){
    ####On ajoute les barres d'erreurs (IC)
    arrows(bp,mean-IC,bp, mean+IC, lwd=1.5, angle=90,length=0.1,code=3)
    ###On ajoute les a pour la significativit?
    text(bp,mean+IC+hauteurText,barsignification,cex = labelsize)
  }else if(errorbar == "ES"){
    ###On ajoute les barres d'erreurs (ES)
    arrows(bp,mean-SE,bp, mean+SE, lwd=1.5, angle=90,length=0.1,code=3)

    ###On ajoute les a pour la significativit?
    text(bp,mean+SE+hauteurText,barsignification,cex = labelsize)

  }else{
    cat("Error in argument 'errorbar' choose 'IC' or 'ES'")
  }
}
#barmod(mod,formule,errorbar = "IC")
