# packages needed for the Shiny application
library(shiny, quietly=TRUE, warn.conflicts = FALSE)
library(shinythemes, quietly=TRUE, warn.conflicts = FALSE)
library(shinyBS, quietly=TRUE, warn.conflicts = FALSE)
library(stringr, quietly=TRUE, warn.conflicts = FALSE)
library(shinydashboard, quietly=TRUE, warn.conflicts = FALSE)
library(shinyjs, quietly=TRUE, warn.conflicts = FALSE)
library(shinyWidgets, quietly=TRUE, warn.conflicts = FALSE)
library(shinyhelper, quietly=TRUE, warn.conflicts = FALSE)
library(colourpicker, quietly=TRUE, warn.conflicts = FALSE) #couleur selecteur
library(shinyFeedback, quietly=TRUE, warn.conflicts = FALSE) #met des warning aux inputs
library(shinycssloaders, quietly=TRUE, warn.conflicts = FALSE)
#require(shinyjqui) #redimentionner tableau image etc / créé une div
#require(shinyFiles) #géner des directory

library(DT, quietly=TRUE, warn.conflicts = FALSE)
library(data.table, quietly=TRUE, warn.conflicts = FALSE)
library(readr, quietly=TRUE, warn.conflicts = FALSE)
library(ggplot2, quietly=TRUE, warn.conflicts = FALSE)
library(dplyr, quietly=TRUE, warn.conflicts = FALSE)
library(lubridate, quietly=TRUE, warn.conflicts = FALSE)
library(RColorBrewer, quietly=TRUE, warn.conflicts = FALSE)
library(plotly, quietly=TRUE, warn.conflicts = FALSE)
library(ggvis, quietly=TRUE, warn.conflicts = FALSE) ##ggviz interactive plot
library(gplots, quietly=TRUE, warn.conflicts = FALSE)
library(ade4, quietly=TRUE, warn.conflicts = FALSE)
library(factoextra, quietly=TRUE, warn.conflicts = FALSE)
library(rmarkdown, quietly=TRUE, warn.conflicts = FALSE)
library(knitr, quietly=TRUE, warn.conflicts = FALSE)
library(heatmaply, quietly=TRUE, warn.conflicts = FALSE)

##
library(broom, quietly=TRUE, warn.conflicts = FALSE)
library(ggstatsplot, quietly=TRUE, warn.conflicts = FALSE)
library(purrr, quietly=TRUE, warn.conflicts = FALSE)

#FUNCTIONS
#---------------------------------------------------------------------------------------------------------------

Data_Moyenne <- function(table,var1,var2){
    datatable = table %>% group_by(.dots = as.character(var2)) %>%
      summarise(Count = n(),
                Median = median(.data[[var1]]),
                Mean = mean(.data[[var1]]),
                Sd = sd(.data[[var1]])
      )
  datatable = as.data.frame(datatable)
  return(datatable)
}

#--------------------------------------------------------------------------------------------------------------
#anova 

anov <- function(tab,var1,var2){
  output = list()
  if(length(var2) == 1){
    res.aov = aov(tab[[var1]] ~ tab[[var2[1]]], tab)
    x = summary(res.aov)
    rownames(x[[1]]) <- c(var2[1], "residuals")

    tukey = TukeyHSD(res.aov)
    names(tukey) <- c(var2[1])
    
    #ll.letters <- multcompLetters(tukey[[1]][,4], threshold = 0.05)[[1]]
    #ll.letters <- data.frame(MeanComparison = names(ll.letters), Group = as.character(ll.letters), stringsAsFactors = FALSE)
    #ll.letters <- ll.letters[order(-xtfrm(ll.letters$MeanComparison)), ]
    #dfLetters <-   ll.letters
    
    output[[1]] = x
    output[[2]] = tukey
    #output[[3]] = dfLetters
  }
  else if(length(var2) >= 2){
    res.aov = aov(tab[[var1]] ~ tab[[var2[1]]] * tab[[var2[2]]], tab)
    x = summary(res.aov)
    rownames(x[[1]]) <- c(var2[1], var2[2], paste(var2[1],"*",var2[2]), "residuals")
    
    tukey = TukeyHSD(res.aov)
    names(tukey) <- c(var2[1], var2[2], paste(var2[1],"*",var2[2]))
    
    # ll.letters <- multcompLetters(tukey[[1]][,4], threshold = 0.05)[[1]]
    # ll.letters <- data.frame(MeanComparison = names(ll.letters), Group = as.character(ll.letters), stringsAsFactors = FALSE)
    # ll.letters <- ll.letters[order(-xtfrm(ll.letters$MeanComparison)), ]
    # dfLetters <-   ll.letters
    
    output[[1]] = x
    output[[2]] = tukey
    #output[[3]] = dfLetters
    
  }
  return(output)
}

anovplot <- function(tab,var1,var2){
  if(length(var2) == 1){
    x <- ggplot(tab, aes(x=tab[[var2[1]]], y=as.numeric(tab[[var1]]), fill = tab[[var2[1]]]))
    x <- x + geom_boxplot()
    x <- x + labs(x=var2[1], y=var1, fill=var2[1]) 
    #x <- x + stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red")
    #x <- x + geom_jitter()
  }
  else if(length(var2) >= 2){
    #inter = interaction(tab[[var1]], tab[[var2[1]]])
    x <- ggplot(tab, aes(x=tab[[var2[1]]], y=as.numeric(tab[[var1]]), fill = tab[[var2[2]]]))
    x <- x + geom_boxplot(position = position_dodge2())
    x <- x + labs(x = var2[1], y=var1, fill=var2[2]) 
    #x <- x + stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red")
  }
  return(x)
}

# ---------------------------------------------------------------------------------------------------
# ACP

ACPPlot <- function(){

d = dcast(datatable,Strain_name~Plant_genotype, value.var = "Mean")
rownames(d) = d[,1]
d = d[,-1]
pca.res = FactoMineR::PCA(d)
return(pca.res)
}

# ---------------------------------------------------------------------------------------------------
# HEATMAPS

heatplot <- function(tab,var1,var2,var3,row,col){

  varF = c(var2, var3)
  datatable = Data_Moyenne(tab,var1,varF)
  
  MAX = max(datatable$Mean)
  MIN = min(datatable$Mean)
  MID = (MAX + MIN) / 2

  x = matrix(1,nrow=length(unique(datatable[,var2])),ncol=length(unique(datatable[,var3])))

  colnames(x) = (unique(datatable[,var3]))
  rownames(x) = (unique(datatable[,var2]))

  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      for(ligne in 1:nrow(datatable)){
        if((colnames(x)[j] == datatable[ligne,var3]) && (rownames(x)[i] == datatable[ligne,var2])){
          x[i,j] = as.numeric(as.character(datatable$Mean[ligne]))
        }
      }
    }
  }
  x=data.matrix(x)
  kolor = c("#FFFFFF","#CCCCFF","#9999FF","#330099","#000033")
  p = heatmaply(x, Colv = col, Rowv = row, colors=kolor,  draw_cellnote = TRUE, digits = 1)
  
  HEAT = list()
  HEAT$plot = p
  HEAT$tab = x
  return(HEAT)
}

heatplot2 <- function(x,row,col,S){ 
    xh2 = x
    for(i in 1:length(S)){
      for (k in 1:nrow(xh2)){
        for(y in 1:ncol(xh2)){
          if(x[k,y] <= as.numeric(S[i]) & (xh2[k,y] == x[k,y])){
            xh2[k,y] = paste("C",i,sep="")
          }
        }
      }
    }      
    for (k in 1:nrow(xh2)){
      for(y in 1:ncol(xh2)){
        if(xh2[k,y] == "C1"){
           xh2[k,y] = 1
        }
        else if(xh2[k,y] == "C2"){
          xh2[k,y] = 2
        }
        else if(xh2[k,y] == "C3"){
          xh2[k,y] = 3
        }
        else if(xh2[k,y] == "C4"){
          xh2[k,y] = 4
        }
        else if(xh2[k,y] == "C5"){
          xh2[k,y] = 5
        }
        else if(xh2[k,y] == "C6"){
          xh2[k,y] = 6
        }
        else{
          xh2[k,y] = length(S)+1
        }
      }
    }

    xh3=data.frame()
    xh4=data.frame()
    for (i in 1:nrow(xh2)){
      for (j in 1:ncol(xh2)){
        if(length(S) == 1){
          if (xh2[i,j] == "1"){
            xh4[i,j] = "R"
          }
          else{
            xh4[i,j] = "S"
          }
        }
          xh3[i,j] = as.numeric(as.integer(xh2[i,j]))
      }
    }
    rownames(xh3)=rownames(xh2)
    colnames(xh3)=colnames(xh2)
    
    if(length(S) ==1){
       kolor = c("white","red")
    }
    if(length(S) ==2){
      kolor = c("white", "yellow", "red")
    }
    if(length(S) ==3){
      kolor = c("white", "yellow", "orange", "red")
    }
    if(length(S) ==4){
      kolor = c("white", "yellow", "orange", "red", "green")
    }
    if(length(S) ==5){
      kolor = c("white", "yellow", "orange", "red", "green", "blue")
    }
    if(length(S) == 1){
      p2 = heatmaply(xh3, Colv = col, Rowv = row, colors = kolor, cellnote = xh4, draw_cellnote = TRUE, hide_colorbar=TRUE)
    }
    else{
      p2 = heatmaply(xh3, Colv = col, Rowv = row, colors = kolor, draw_cellnote = TRUE)
    }
  
    #dataframe of cluster
    
    groups = unique(xh3)
    rownames(groups) = c(1:nrow(groups))
    xh5 = data.frame()
    for(i in 1:nrow(xh3)){
      
      for(n in 1:nrow(groups)){
          if(all(xh3[i,] == groups[n,])){
            xh5[i,1] = paste("group",rownames(groups)[n],sep="")
          }
      }
      for (j in 1:ncol(xh3)){
        xh5[i,j+1] = xh3[i,j]
      }
    }
    rownames(xh5)=rownames(xh3)
    colnames(xh5)=c("groups",colnames(xh3))
    
    if (length(S) == 1){
      for (i in 1:nrow(xh5)){
        for (j in 1:ncol(xh5)){
          if (xh5[i,j] == "1"){
            xh5[i,j] = "R"
          }
          else if (xh5[i,j] == "2"){
            xh5[i,j] = "S"
          }
        }
      }
    }
    HEAT = list()
    HEAT$plot = p2
    HEAT$tab = xh5
  return(HEAT)
}

ResistanceFrequency <- function(y, S){
  
  if (length(S)==1){
    for (i in 1:nrow(y)){
      for (j in 1:ncol(y)){
        if (y[i,j] == "R"){
          y[i,j] = "1"
        }
        else if (y[i,j] == "S"){
          y[i,j] = "2"
        }
      }
    }
  }
  
  colnames(y) = c("races", colnames(y)[2:length(colnames(y))])
  groupes = as.data.frame(y[1])
  
  Resistance = as.matrix(y[,c(2:length(colnames(y)))])
  
  Resistance2 = Resistance
  R1Vect = list()
  R2Vect = list()
  R3Vect = list()
  R4Vect = list()
  R5Vect = list()
  R6Vect = list()
  for(i in 1:nrow(Resistance)){
    R1 = sum(Resistance[i,]=="1") / ncol(Resistance) * 100
    R2 = sum(Resistance[i,]=="2") / ncol(Resistance) * 100
    R3 = sum(Resistance[i,]=="3") / ncol(Resistance) * 100
    R4 = sum(Resistance[i,]=="4") / ncol(Resistance) * 100
    R5 = sum(Resistance[i,]=="5") / ncol(Resistance) * 100
    R6 = sum(Resistance[i,]=="6") / ncol(Resistance) * 100
    R1Vect = c(R1Vect,R1)
    R2Vect = c(R2Vect,R2)
    R3Vect = c(R3Vect,R3)
    R4Vect = c(R4Vect,R4)
    R5Vect = c(R5Vect,R5)
    R6Vect = c(R6Vect,R6)
  }
  Resistance2 = cbind(Resistance2, R1Vect, R2Vect, R3Vect, R4Vect , R5Vect , R6Vect)
  Resistance2 = as.data.frame(Resistance2)
  setDT(Resistance2, keep.rownames = "Ricelines")[]
  
  names = Resistance2[,1]
  Percentage = c(R1Vect, R2Vect, R3Vect, R4Vect, R5Vect, R6Vect)
  Type = c(rep("R1",length(R1Vect)),rep("R2",length(R2Vect)),rep("R3",length(R3Vect)),rep("R4",length(R4Vect)),rep("R5",length(R5Vect)),rep("R6",length(R6Vect)))
  PercentageSens = c(R1Vect,R1Vect,R1Vect,R1Vect,R1Vect,R1Vect)

  ResDF = cbind(rbind(names,names,names,names,names,names),Percentage,Type,PercentageSens)
  ResDF2 <- as.data.frame(lapply(ResDF, unlist))

  p <- ggplot()
  p <- p + geom_bar(aes(x=reorder(ResDF2[,1], ResDF2$PercentageSens), y = as.numeric(as.character(ResDF2$Percentage)), fill = factor(ResDF2$Type)), data = ResDF2, stat="identity")
  p <- p + labs(x="Ricelines", y = "Percentage of resistance", fill="Resistance")
  p <- p + theme_minimal()
  p <- p + theme(axis.title.y = element_text(size = 14, margin = margin(t = 5, r = 5, b = 5, l = 5)),
                 axis.title.x = element_text(size = 14, margin = margin(t = 5, r = 5, b = 5, l = 5)),
                 axis.text = element_text(size = 14, margin = margin(t = 5, r = 5, b = 5, l = 5)),
                 axis.text.x = element_text(angle = 90, hjust = 1, margin = margin(t = 5, r = 5, b = 5, l = 5)),
                 axis.text.y = element_text(size = 14, margin = margin(t = 5, r = 5, b = 5, l = 5)))
  return(p)
}



maxMean <- function(tab,var1,var2,var3){
  varF = c(var2, var3)
  data_moyenne = Data_Moyenne(tab,var1,varF)
  x = ceiling(max(data_moyenne$Mean))
  return(x)
}

#---------------------------------------------------------------------------------------------------------------------
#evolution

GraphTime <- function(tab,tim,var1,var2,var3,var4,timeselecter, smoothing){
  
  if(var4 == "None"){
    var4 = NULL
  }
  if(var2 == "None" || var3 == "None"){
    var2 = NULL
    var3 = NULL
  }
  
  varF = c(var2, var3, var4)
  
  allmoy = Data_Moyenne(tab,var1,c(tim,varF))
  
  if(timeselecter == "dmy"){
    allmoy[,tim] = dmy(allmoy[,tim])
  }
  else if(timeselecter == "ymd"){
    allmoy[,tim] = ymd(allmoy[,tim])
  }
  
  if(!is.null(var4)){
    p <- ggplot(allmoy, aes(x = allmoy[,tim], y = allmoy$Mean, group=allmoy[,var4], color = allmoy[,var4]))
  }
  else{
    p <- ggplot(allmoy, aes(x = allmoy[,tim], y = allmoy$Mean))
  }
  
  if(!is.null(var2) && !is.null("None")){
    p <- p + facet_grid(allmoy[,var2] ~ allmoy[,var3])
  }
  
  p <- p + geom_point(size=(allmoy$Count/sum(allmoy$Count)*100), show.legend = TRUE) + geom_errorbar(aes(ymin=allmoy$Mean-allmoy$Sd, ymax=allmoy$Mean+allmoy$Sd), width =.2)
  if(!is.null(var4)){
    p <- p + labs(color=var4 ,x=tim, y= var1)
  }
  else{
    p <- p + labs(x=tim, y= var1)
  }
  if (smoothing == "smooth"){
    p <- p + geom_smooth(se = FALSE)
  }
  else{
    p <- p+ geom_line()
  }
  p <- p + theme(axis.title.y = element_text(size = 14, margin = margin(t = 30, r = 20, b = 0, l = 0)), 
                axis.title.x = element_text(size = 14),
                axis.text = element_text(size = 12), 
                axis.text.x = element_text(angle = 90, margin = margin(t = 30, r = 20, b = 0, l = 0)))
  
  return(p)
  
}

#---------------------------------------
#visu

NiceGraph <-  function(tab,var1,var2,var3,var4){
  tab = as.data.frame(tab)
  p <- ggplot(data=tab, aes(x=reorder(tab[,var2], as.numeric(as.character(tab[,var1])),FUN = median), y=as.numeric(as.character(tab[,var1])))) + geom_boxplot()
  if(var3 != "None" && !is.null(var3) && var3 !=""){
    p <- p + geom_jitter(aes(colour=tab[,var3]),width = 0.2)
  }
  if(var4 != "None" && !is.null(var4) && var4 !=""){
    p <- p + facet_grid(tab[,var4] ~ .)
    p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(face = "bold",size = 12))
  }
  p <- p + labs(y=var1, x =var2, colour = var3)
  p <- p +theme_minimal()

  p <-p + theme(axis.title.y = element_text(size = 14, margin = margin(t = 30, r = 20, b = 0, l = 0)),
                axis.title.x = element_text(size = 14),
                axis.text = element_text(size = 12),
                axis.text.x = element_text(angle = 90, margin = margin(t = 30, r = 20, b = 0, l = 0)))

  return(p)
}

#Normality test (Shapiro wilk)

normality <- function(data, var1){
  if(is.numeric(data[,var1])){
    x = shapiro.test(data[,var1])
    x[4] = var1
    return(x)
  }
  else{
    return("response variable is not numeric.")
  }
}

vizBarplot <- function(tab, var1, var2, var3, var4){
  if (var3 == 'None'){
    var3 = NULL
  }
  varF = c(var2, var3, var4)
  data_moyenne = Data_Moyenne(tab, var1, varF)
  if (!is.null(var3)){
    p <- ggplot(data=data_moyenne, aes(x=data_moyenne[,var2], y=data_moyenne$Mean, fill = data_moyenne[,var4])) 
    p <- p + geom_bar(stat="identity", position=position_dodge2(preserve="single"))
    p <- p + geom_errorbar(aes(ymin=data_moyenne$Mean-data_moyenne$Sd, ymax=data_moyenne$Mean+data_moyenne$Sd), width=.2, position = position_dodge(0.9))
    p <- p + labs(y=var1, x =var2, fill = var4)
    #p <- p + scale_fill_brewer(palette="Paired") 
    p <- p + theme_minimal()
    p <- p + theme(axis.title.y = element_text(size = 14, margin = margin(t = 30, r = 20, b = 0, l = 0)), 
                  axis.title.x = element_text(size = 14),
                  axis.text = element_text(size = 12), 
                  axis.text.x = element_text(angle = 90, margin = margin(t = 30, r = 20, b = 0, l = 0)))
    p <- p + facet_grid(data_moyenne[,var3] ~ .)
  }
  else {
    p<- ggplot(data=data_moyenne, aes(x=data_moyenne[[var2]], y=as.numeric(as.character(data_moyenne$Mean)), fill = data_moyenne[[var4]])) 
    p <- p + geom_bar(stat="identity", position=position_dodge2(preserve="single"))
    p <- p + geom_errorbar(aes(ymin=data_moyenne$Mean-data_moyenne$Sd, ymax=data_moyenne$Mean+data_moyenne$Sd), width=.2, position = position_dodge(0.9))
    p <- p + labs(y=var1, x=var2, fill=var4)
    #p <- p + scale_fill_brewer(palette="Paired") 
    p <- p + theme_minimal()
    p = p + theme(axis.title.y = element_text(size = 14, margin = margin(t = 30, r = 20, b = 0, l = 0)), 
                  axis.title.x = element_text(size = 14),
                  axis.text = element_text(size = 12), 
                  axis.text.x = element_text(angle = 90, margin = margin(t = 30, r = 20, b = 0, l = 0)))
  }
  return(p)
}

#--------------------------------------------------------------------------------------------------------------------------------------------------
#ACP

adeACP <- function(data, var1, var2, var3, center, scale, nf, axisViz){
  
  varF = c(var2, var3)
  datatable = Data_Moyenne(data,var1,varF)
  x = matrix(1,nrow=length(unique(datatable[,var2])),ncol=length(unique(datatable[,var3])))
  
  colnames(x) = (unique(datatable[,var3]))
  rownames(x) = (unique(datatable[,var2]))
  
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      for(ligne in 1:nrow(datatable)){
        if((colnames(x)[j] == datatable[ligne,var3]) && (rownames(x)[i] == datatable[ligne,var2])){
          x[i,j] = as.numeric(as.character(datatable$Mean[ligne]))
        }
      }
    }
  }
  x=data.matrix(x)
  adePCA = dudi.pca(x, center = center, scale = scale, nf = nf, scannf = FALSE)
  
  if(axisViz == "axis1 vs axis2"){
    VP = fviz_eig(adePCA)
    ind = fviz_pca_ind(adePCA, axes = c(1, 2))
    var = fviz_pca_var(adePCA, axes = c(1, 2))
    both = fviz_pca_biplot(adePCA, repel = TRUE,
                    axes = c(1, 2),
                    col.var = "#2E9FDF", 
                    col.ind = "#696969"  
    )
  }
  else if(axisViz == "axis2 vs axis3"){
    VP = fviz_eig(adePCA)
    ind = fviz_pca_ind(adePCA, axes = c(2, 3))
    var = fviz_pca_var(adePCA, axes = c(2, 3))
    both = fviz_pca_biplot(adePCA, repel = TRUE,
                           axes = c(2, 3),
                           col.var = "#2E9FDF", 
                           col.ind = "#696969"  
    )
  }
  else if(axisViz == "axis1 vs axis3"){
    VP = fviz_eig(adePCA)
    ind = fviz_pca_ind(adePCA, axes = c(1, 3))
    var = fviz_pca_var(adePCA, axes = c(1, 3))
    both = fviz_pca_biplot(adePCA, repel = TRUE,
                           axes = c(1, 3),
                           col.var = "#2E9FDF", 
                           col.ind = "#696969"  
    )
  }
  
  ade = list()
  ade$VP = VP
  ade$ind = ind
  ade$var = var
  ade$both = both
  
  return(ade)
}


meanplot <- function(tab, response, explicative, groupi, test_type, MeanPlotting, PlotType, do_comps){

  if(groupi == 'None'){
    p = ggbetweenstats(
      data = tab,
      x = !!explicative,
      y = !!response,
      
      nboot = 10,
      mean.label.size = 5,
      type = test_type,
      conf.level = 0.95,
      pairwise.comparisons = {{as.logical(do_comps)}},
      p.adjust.method = "fdr",
      package = "pals",
      palette = "alphabet",
      sample.size.label = FALSE,
      mean.plotting = {{as.logical(MeanPlotting)}},
      plot.type = PlotType,
      mean.ci = TRUE,
      ggplot.component = theme(axis.text.x = element_text(angle = 90, hjust = 1))

    )
  }
  else{
    p = grouped_ggbetweenstats(
      data = tab,
      x = !!explicative,
      y = !!response,
      grouping.var = {{groupi}},
      
      nboot = 10,
      type = test_type,
      mean.label.size = 5,
      conf.level = 0.95,
      pairwise.comparisons = {{as.logical(do_comps)}},
      p.adjust.method = "fdr",
      package = "pals",
      palette = "alphabet",
      sample.size.label = FALSE,
      mean.plotting = {{as.logical(MeanPlotting)}},
      plot.type = PlotType,
      mean.ci = TRUE,
      ggplot.component = theme(axis.text.x = element_text(angle = 90, hjust = 1))
    )
  }
  return(p)
}
