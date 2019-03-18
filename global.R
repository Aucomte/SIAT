# packages needed for the Shiny application
require(shiny)
require(shinythemes)
require(shinyBS)
require(stringr)
require(shinydashboard)
require(shinyjs)
require(shinyWidgets)
require(DT)
require(shinyhelper)

require(colourpicker) #couleur selecteur
require(shinyFeedback) #met des warning aux inputs
require(shinyjqui) #redimentionner tableau image etc / créé une div
require(shinyFiles) #géner des directory

library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(shinycssloaders)
library(plotly)

library(ggvis) ##ggviz interactive plot
library(gplots)

library(ade4)
library(factoextra)

#mettre les fonctions ici

Data_Moyenne <- function(table,var1,var2){
  datatable = table %>% group_by(.dots = as.character(var2)) %>%
    summarise(nb = n(),
              Mean = mean(.data[[var1]]),
              Sd = sd(.data[[var1]])
    )
  datatable = as.data.frame(datatable)

  return(datatable)
}

anov <- function(tab,var1,var2){
  if(length(var2) == 1){
    res.aov = aov(tab[[var1]] ~ tab[[var2[1]]], tab)
    x = summary(res.aov)
    rownames(x[[1]]) <- c(var2[1], "residuals")
  }
  else if(length(var2) >= 2){
    res.aov = aov(tab[[var1]] ~ tab[[var2[1]]] * tab[[var2[2]]], tab)
    x = summary(res.aov)
    rownames(x[[1]]) <- c(var2[1], var2[2], paste(var2[1],"*",var2[2]), "residuals")
  }
  return(x)
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
  
  color.palette  <- colorRampPalette(c("white", "orange", "red"))
  if (row == TRUE && col == TRUE){
    dend = "both"
  }
  else if (row == FALSE && col == FALSE){
    dend = "none"
  }
  else if (row == TRUE && col == FALSE){
    dend = "row"
  }
  else if (row == FALSE && col == TRUE){
    dend = "col"
  }
  p = gplots::heatmap.2(x, dendrogram = dend, trace = "none", col=color.palette, cellnote = round(x,1), notecol="black", cexCol=.9, cexRow = .9, margins = c(6, 6), srtCol=45)
  return(p)
}

heatplotSR <- function(tab,SR,var1,var2,var3,row,col){

  varF = c(var2, var3)
  data_moyenne = Data_Moyenne(tab,var1,varF)
  
  data_moyenne$Mean[data_moyenne$Mean <= SR] = 0
  data_moyenne$Mean[data_moyenne$Mean > SR] = 1
  
  x = matrix(1,nrow=length(unique(data_moyenne[,var2])),ncol=length(unique(data_moyenne[,var3])))
  
  colnames(x) = (unique(data_moyenne[,var3]))
  rownames(x) = (unique(data_moyenne[,var2]))
  
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      for(ligne in 1:nrow(data_moyenne)){
        if((colnames(x)[j] == data_moyenne[ligne,var3]) && (rownames(x)[i] == data_moyenne[ligne,var2])){
          x[i,j] = as.numeric(as.character(data_moyenne$Mean[ligne]))
        }
      }
    }
  }
  x=data.matrix(x)
  
  if (row == TRUE && col == TRUE){
    dend = "both"
  }
  else if (row == FALSE && col == FALSE){
    dend = "none"
  }
  else if (row == TRUE && col == FALSE){
    dend = "row"
  }
  else if (row == FALSE && col == TRUE){
    dend = "col"
  }
  
  p = gplots::heatmap.2(x, key = FALSE, dendrogram = dend, col=c("yellow","red"),  breaks=c(-1,0.9,1.2), trace = "none", cexCol=.9, cexRow = .9, margins = c(6, 6), srtCol=45)
  return(p)
}

heatplot2 <- function(tab,var1,var2,var3,var4){
  if(!is.null(var4)){
    if (var4 == "None"){
      var4 = NULL
    }
  }
  varF = c(var2, var3, var4)
  data_moyenne = Data_Moyenne(tab,var1,varF)
  
  MAX = max(data_moyenne$Mean)
  MIN = min(data_moyenne$Mean)
  MID = (MAX + MIN) / 2
  
  jBuPuFun <- colorRampPalette(brewer.pal(n = 9, "BuPu"))
  paletteSize <- 256
  jBuPuPalette <- jBuPuFun(paletteSize)
  
  p <- ggplot(data = data_moyenne, aes(x=data_moyenne[[varF[1]]], y=data_moyenne[[varF[2]]], fill=data_moyenne$Mean)) + geom_tile()
  if(var4 != "None" && var4 !="" && !is.null(var4)){
    p <- p + facet_grid( . ~ data_moyenne[[varF[3]]])
  }
  p <- p +  geom_text(aes(data_moyenne[[varF[1]]], data_moyenne[[varF[2]]], label = round(data_moyenne$Mean,digits=2)), color = "black", size = 4)
  p <- p + labs(x = varF[1], y=varF[2])
  p <- p +  scale_fill_gradient2(low = jBuPuPalette[1],
                                 mid = jBuPuPalette[paletteSize/2],
                                 high = jBuPuPalette[paletteSize],
                                 midpoint = MID,
                                 limit = c(MIN,MAX),
                                 space = "Lab",
                                 name = var1)
  p = p + theme(axis.title.y = element_text(size = 14, margin = margin(t = 30, r = 20, b = 0, l = 0)), 
                axis.title.x = element_text(size = 14),
                axis.text = element_text(size = 12), 
                axis.text.x = element_text(angle = 45, margin = margin(t = 30, r = 20, b = 0, l = 0)))
  return(p)
}

maxMean <- function(tab,var1,var2,var3){
  varF = c(var2, var3)
  data_moyenne = Data_Moyenne(tab,var1,varF)
  x = ceiling(max(data_moyenne$Mean))
  return(x)
}

#--------------------------------
#evolution

GraphTime <- function(tab,tim,var1,var2,var3,var4,timeselecter){

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
  
  p <- p + geom_point(size=(allmoy$nb/sum(allmoy$nb)*100), show.legend = TRUE) + geom_errorbar(aes(ymin=allmoy$Mean-allmoy$Sd, ymax=allmoy$Mean+allmoy$Sd), width =.2)
  if(!is.null(var4)){
    p <- p + labs(color=var4 ,x=tim, y= var1)
  }
  else{
    p <- p + labs(x=tim, y= var1)
  }
  p <- p + geom_smooth(se = FALSE)
  
  p = p + theme(axis.title.y = element_text(size = 14, margin = margin(t = 30, r = 20, b = 0, l = 0)), 
                axis.title.x = element_text(size = 14),
                axis.text = element_text(size = 12), 
                axis.text.x = element_text(angle = 45, margin = margin(t = 30, r = 20, b = 0, l = 0)))
  
  return(p)

}


#---------------------------------------
#visu

NiceGraph <-  function(tab,var1,var2,var3,var4){
  
  p <- ggplot(data=tab, aes(x=tab[,var2], y=as.numeric(as.character(tab[,var1])))) + geom_boxplot()
  p <- p + geom_jitter(aes(colour=tab[,var3]),width = 0.2)
  if(var4 != "None" && !is.null(var4) && var4 !=""){
    p <- p + facet_grid(tab[,var4] ~ .)
    p <- p + theme(legend.text = element_text(size = 12), legend.title = element_text(face = "bold",size = 12))
  }
  p <- p + labs(y=var1, x =var2, colour = var3)
  p + theme_minimal()
  
  p = p + theme(axis.title.y = element_text(size = 14, margin = margin(t = 30, r = 20, b = 0, l = 0)), 
                axis.title.x = element_text(size = 14),
                axis.text = element_text(size = 12), 
                axis.text.x = element_text(angle = 45, margin = margin(t = 30, r = 20, b = 0, l = 0)))
  
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

#--------------------------------------------------------------------------------------------------------------------------------------------------
#ACP

adeACP <- function(data, var1, var2, var3, center, scale, nf){
  
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
  VP = fviz_eig(adePCA)
  ind = fviz_pca_ind(adePCA)
  var = fviz_pca_var(adePCA)
  both = fviz_pca_biplot(adePCA, repel = TRUE,
                  col.var = "#2E9FDF", 
                  col.ind = "#696969"  
  )
  ade = list()
  ade$VP = VP
  ade$ind = ind
  ade$var = var
  ade$both = both
  
  return(ade)
}

