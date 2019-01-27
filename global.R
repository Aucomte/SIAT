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

library(ggvis)

##ggviz interactive plot


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
    x = boxplot(as.numeric(tab[[var1]])~tab[[var2[1]]], tab, xlab = var2[1], ylab=var1)
  }
  else if(length(var2) >= 2){
    x = boxplot(as.numeric(tab[[var1]])~tab[[var2[1]]] * tab[[var2[2]]], tab, xlab = paste(var2[1],"*",var2[2]), ylab=var1)
  }
  return(x)
}

heatplot <- function(tab,var1,var2,var3,var4){
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
                                   name=var1)
  return(p)
}

heatplotSR <- function(tab,SR,var1,var2,var3,var4){
  if(!is.null(var4)){
    if(var4 == "None"){
      var4 = NULL
    }
  }
  varF = c(var2, var3, var4)
  data_moyenne = Data_Moyenne(tab,var1,varF)
  
  data_moyenne$Mean[data_moyenne$Mean <= SR] = 0
  data_moyenne$Mean[data_moyenne$Mean > SR] = 1
  
  p <- ggplot(data = data_moyenne, aes(x=data_moyenne[[varF[1]]], y=data_moyenne[[varF[2]]], fill=data_moyenne$Mean)) + geom_tile()
  if(var4 != "None" && var4 !="" && !is.null(var4)){
      p <- p + facet_grid(. ~ data_moyenne[[varF[3]]])
  }
  p <- p + labs(x = varF[1], y=varF[2])

  p <- p +  scale_fill_gradient2(low = "white",
                                 high = "red",
                                 name=var1)
  p
  return(p)
}

maxMean <- function(tab,var1,var2,var3,var4){
  if(!is.null(var4)){
   if(var4 == "None"){
      var4 = NULL
   }
  }
  varF = c(var2, var3, var4)
    data_moyenne = Data_Moyenne(tab,var1,varF)
    x = ceiling(max(data_moyenne$Mean))
    return(x)
}

GraphTime <- function(tab,date,var1,var2,var3,var4){
  
  if(!is.null(var4)){
    if(var4 == "None"){
      var4 = NULL
    }
  }
  varF = c(var2, var3, var4)
  allmoy = Data_Moyenne(tab,var1,c(date,varF))

  if(!is.null(var4)){
    p <- ggplot(allmoy, aes(x = allmoy[date], y = allmoy$Mean, group=allmoy[var4], color = allmoy[var4]))
  }
  else{
    p <- ggplot(allmoy, aes(x = allmoy[date], y = allmoy$Mean))
  }
  p <- p + facet_grid(allmoy[var1] ~ allmoy[var2])
  p <- p + geom_point(size=allmoy$nb, show.legend = TRUE) + geom_errorbar(aes(ymin=allmoy$Mean-allmoy$Sd, ymax=allmoy$moyenne+allmoy$Sd), width =.2)
  if(!is.null(var4)){
    p <- p + labs(color=var4 ,x=date, y= var1)
  }
  else{
    p <- p + labs(x=date, y= var1)
  }
  p + geom_smooth()
  return(p)
}

NiceGraph <-  function(tab,var1,var2,var3,var4){
  
  p <- ggplot(data=tab, aes(x=tab[,var2], y=as.numeric(as.character(tab[,var1])))) + geom_boxplot()
  p <- p + geom_jitter(aes(colour=tab[,var3]),width = 0.2)
  if(var4 != "None" && !is.null(var4) && var4 !=""){
    p <- p + facet_grid(data[,var4] ~ .)
  }
  p <- p + labs(y=var1, x =var2, colour = var3)
  p + theme_minimal()
  return(p)
}


