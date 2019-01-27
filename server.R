#server options : 3 Mo maximum.
#options(shiny.maxRequestSize = 3*1024^2)

server <-function(input,output,session){

#liste des réactifs

  sr <- reactiveValues(
    
    # panel 1 : lecture de la table
    
    booTable = 0,
    table = NULL,
    timefactor = NULL,
    sep = ";",
    head = TRUE,
    dec = ",",
    outVar = NULL,
    
    # panel 2 : Moyenne / SD
    
    resp1 = NULL,
    fact1 = NULL,
    
    # panel 3 : Anova
    
    respanov = NULL,
    factanov = NULL,
    
    # panel 4 : ACP
    
    # panel 5 : Heatmap
    
    respheat = NULL,
    factH1 = NULL,
    factH2 = NULL,
    factH3 = NULL,
    slidethresSH = NULL

    
  )
  
  # panel 1 : lecture de la table
  observeEvent(input$TimeFactor, {
    sr$timefactor = input$TimeFactor
    if(!is.null(sr$timefactor) &&  sr$timefactor != "None" && sr$timefactor != ""){
      sr$table[,sr$timefactor] = dmy(sr$table[,sr$timefactor])
    }
  })
  observeEvent(input$sep, {
    sr$sep = input$sep
    if(sr$booTable == 1) {
      sr$table = read.table(input$file1$datapath, header = sr$head, sep=sr$sep, dec=sr$dec, fill =TRUE, row.names=NULL)
      sr$table = as.data.frame(sr$table)
    }
  })
  observeEvent(input$dec, {
    sr$dec = input$dec
    if(sr$booTable == 1) {
      sr$table = read.table(input$file1$datapath, header = sr$head, sep=sr$sep, dec=sr$dec, fill =TRUE, row.names=NULL)
      sr$table = as.data.frame(sr$table)
    }
  })
  observeEvent(input$head, {
    sr$head = input$head
    if(sr$booTable == 1) {
      sr$table = read.table(input$file1$datapath, header = sr$head, sep=sr$sep, dec=sr$dec, fill =TRUE, row.names=NULL)
      sr$table = as.data.frame(sr$table)
    }
  })
  observeEvent(input$file1, {
    sr$booTable = 1
    sr$table = read.table(input$file1$datapath, header = sr$head, sep=sr$sep, dec=sr$dec, fill =TRUE, row.names=NULL)
    sr$table = as.data.frame(sr$table)
    sr$outVar = colnames(sr$table)
    updateSelectInput(session, inputId = "TimeFactor", choices = c("None", sr$outVar), selected = "None")
    
    updateSelectInput(session, inputId = "responseVar1", choices = sr$outVar, selected = sr$outVar[length(sr$outVar)])
    updateSelectInput(session, inputId = "factors1", choices = sr$outVar, selected = sr$outVar[1])
    
    updateSelectInput(session, inputId = "responseVar", choices = sr$outVar, selected = sr$outVar[length(sr$outVar)])
    updateSelectInput(session, inputId = "factors", choices = sr$outVar, selected = sr$outVar[1])
    
    updateSelectInput(session, inputId = "responseVarHeat", choices = sr$outVar, selected = sr$outVar[length(sr$outVar)])
    updateSelectInput(session, inputId = "factorH1", choices = sr$outVar, selected = sr$outVar[1])
    updateSelectInput(session, inputId = "factorH2", choices = sr$outVar, selected = sr$outVar[2])
    updateSelectInput(session, inputId = "factorH3", choices = c("None",sr$outVar, selected = ""))
    
  })
  observe({
    if(sr$booTable == 1) {
      output$DataSet <- renderDT({
        datatable(sr$table, filter = c("none", "bottom", "top"))
      })
    }
  })
  
  # panel 2 : Moyenne / SD
  
  observeEvent(input$responseVar1,{
    sr$resp1 = input$responseVar1
  })
  observeEvent(input$factors1,{
    sr$fact1 = input$factors1
  })
  observe({
    if(sr$booTable==1){
      output$moyenne <- renderDT({
        datatable(Data_Moyenne(sr$table,sr$resp1,sr$fact1), filter = c("none", "bottom", "top"))
      })
    }
  })
  
  # panel 3 : Anova
  observeEvent(input$responseVar,{
    sr$respanov = input$responseVar
  })
  observeEvent(input$factors,{
    vector=c()
    sr$factanov = c(vector,input$factors)
  })
  observe({
    if(sr$booTable==1){
      output$anov <- renderPrint({
        anov(sr$table,sr$respanov,sr$factanov)
      })
      output$anovplot <- renderPlot({
        anovplot(sr$table,sr$respanov,sr$factanov)
      })
    }
  })
  
  # panel 4 : ACP
  
  # panel 5 : Heatmap
  
  observeEvent(input$responseVarHeat, {
    sr$respheat = input$responseVarHeat
  })
  observeEvent(input$factorH1, {
    sr$factH1 = input$factorH1
  })
  observeEvent(input$factorH2, {
    sr$factH2 = input$factorH2
  })
  observeEvent(input$factorH3, {
    sr$factH3 = input$factorH3
  })
  observeEvent(input$thresSR, {
    sr$slidethresSR = input$thresSR
  })
  observe({
    if(sr$booTable==1){
      if(!is.null(sr$factH1) && !is.null(sr$factH2) && !is.null(sr$factH3) && !is.null(sr$respheat)){
       # updateSliderInput(session, inputId = "thresSR", value = maxMean(sr$table,sr$respheat,sr$factH1,sr$factH2,sr$factH3)/2, min=0, max=maxMean(sr$table,sr$respheat,sr$factH1,sr$factH2,sr$factH3), step=1) 
      }
      output$heatplot <- renderPlot({
        heatplot(sr$table,sr$respheat,sr$factH1,sr$factH2,sr$factH3)
      })
      output$heatplotSR <- renderPlot({
        heatplotSR(sr$table,sr$slidethresSR,sr$respheat,sr$factH1,sr$factH2,sr$factH3)
     })
    }
  })
}



#   #heatmap reactive
#   respheat <- reactive({
#     input$responseVarHeat
#   })
#   factH1 <- reactive({
#     input$factorH1
#   })
#   factH2 <- reactive({
#     input$factorH2
#   })
#   factH3 <- reactive({
#     input$factorH3
#   })
#   slidethresSR <- reactive({
#     input$thresSR
#   })
#   
#   #pretty plot reactive
#   respPG <- reactive({
#     input$responseVarPG
#   })
#   factPG1 <- reactive({
#     input$factorPG1
#   })
#   factPG2 <- reactive({
#     input$factorPG2
#   })
#   factPG3 <- reactive({
#     input$factorPG3
#   })
#   
#   #time plot reactive
#   respT <- reactive({
#     input$responseVarT
#   })
#   factT1 <- reactive({
#     input$factorT1
#   })
#   factT2 <- reactive({
#     input$factorT2
#   })
#   factT3 <- reactive({
#     input$factorT3
#   })
#   
#   #What happens when we add datas
#   observeEvent(input$file1, {
#     
#      
#     updateSelectInput(session, inputId = "TimeFactor", choices = c("None", outVar()), selected = "")
#     
#     observe({
#       updateSelectInput(session, inputId = "responseVar1", choices = outVar(), selected = outVar()[length(outVar())])
#       updateSelectInput(session, inputId = "factors1", choices = outVar(), selected = outVar()[1])
#       updateSelectInput(session, inputId = "responseVar", choices = outVar(), selected = outVar()[length(outVar())])
#       updateSelectInput(session, inputId = "factors", choices = outVar(), selected = outVar()[1])
#       
#       updateSelectInput(session, inputId = "responseVarHeat", choices = outVar(), selected = outVar()[length(outVar())])
#       updateSelectInput(session, inputId = "factorH1", choices = outVar(), selected = outVar()[1])
#       updateSelectInput(session, inputId = "factorH2", choices = outVar(), selected = outVar()[2])
#       updateSelectInput(session, inputId = "factorH3", choices = c("None",outVar()), selected = "")
#       updateSelectInput(session, inputId = "factors", choices = outVar(), selected = outVar()[1])
#       
#       updateSelectInput(session, inputId = "responseVarPG", choices = outVar(), selected = outVar()[length(outVar())])
#       updateSelectInput(session, inputId = "factorPG1", choices = outVar(), selected = outVar()[1])
#       updateSelectInput(session, inputId = "factorPG2", choices = outVar(), selected = outVar()[2])
#       updateSelectInput(session, inputId = "factorPG3", choices = c("None",outVar()), selected = "")
#       
#       updateSelectInput(session, inputId = "responseVarT", choices = outVar(), selected = outVar()[length(outVar())])
#       updateSelectInput(session, inputId = "factorT1", choices = outVar(), selected = outVar()[1])
#       updateSelectInput(session, inputId = "factorT2", choices = outVar(), selected = outVar()[2])
#       updateSelectInput(session, inputId = "factorT3", choices = c("None",outVar()), selected = "")
#       
#       output$DataSet <- renderDT({
#         datatable(table())
#       })
#     })
#     observeEvent(input$TimeFactor, {
#         output$DataSet <- renderDT({
#           datatable(table())
#         })
#     })
#     observeEvent(input$CalculationMean, {
# 
#       output$moyenne <- renderDT({
#         datatable(Data_Moyenne(table(),resp1(),fact1()), filter = c("none", "bottom", "top"))
#       })
#     })
#     observeEvent(input$CalculationAnov, {
#       output$anov <- renderPrint({
#         anov(table(),respanov(),factanov())
#       })
#       output$anovplot <- renderPlot({
#         anovplot(table(),respanov(),factanov())
#       })
#     })
#     observeEvent(input$CalculationHeat, {
#       observe({
#         updateSliderInput(session, inputId = "thresSR", value = maxMean(table(),respheat(),factH1(),factH2(),factH3())/2, min=0, max=maxMean(table(),respheat(),factH1(),factH2(),factH3()), step=1) 
#       })
#       output$heatplot <- renderPlot({
#         heatplot(table(),respheat(),factH1(),factH2(),factH3())
#       })
#       output$heatplotSR <- renderPlot({
#         heatplotSR(table(),slidethresSR(),respheat(),factH1(),factH2(),factH3())
#       })
#     })
#     observeEvent(input$CalculationPG, {
#       output$PrettyG <- renderPlot({
#         NiceGraph(table(),respPG(),factPG1(),factPG2(),factPG3())
#       })
#     })
#     observeEvent(input$CalculationT, {
#       output$TimePlot <- renderPlot({
#         GraphTime(table(),timefactor(),respT(),factT1(),factT2(),factT3())
#       })
#     })
#   })
# }
