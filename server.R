#server options : 3 Mo maximum.
#options(shiny.maxRequestSize = 3*1024^2)

server <-function(input,output,session){

#liste des réactifs

  sr <- reactiveValues(
    # pour l'analyse
    booTable = 0,
    table = NULL,
    timefactor = NULL,
    sep = ";",
    head = TRUE,
    dec = ",",
    outVar = NULL
  )
  
  observeEvent(input$timefactor, {
    sr$timefactor = input$timefactor
  })
  observeEvent(input$sep, {
    sr$sep = input$sep
  })
  observeEvent(input$dec, {
    sr$dec = input$dec
  })
  observeEvent(input$head, {
    sr$head = input$head
  })
  observeEvent(input$file1, {
    sr$booTable = 1
  })
  observe({
    if(sr$booTable == 1) {
      sr$table = read.table(input$file1$datapath, header = sr$head, sep=sr$sep, dec=sr$dec, fill =TRUE)
      if(!is.null(sr$timefactor) &&  sr$timefactor != "None"){
        sr$table[,sr$timefactor] = parse_date_time(sr$table[,sr$timefactor])
      }
      sr$table = as.data.frame(sr$table)
    }
  })
  observe({
    if(!is.null(sr$table)){
      sr$outVar = colnames(sr$table)
      updateSelectInput(session, inputId = "TimeFactor", choices = c("None", sr$outVar), selected = "None")
      
      output$DataSet <- renderDT({
          datatable(sr$table, filter = c("none", "bottom", "top"))
       })
    }
  })
 
}

#   outVar <-reactive({
#     var = colnames(table())
#     return(var)
#   })
#   #mean reactives
#   resp1 <-reactive({
#     input$responseVar1
#   })
#   fact1 <-reactive({
#     input$factors1
#   })
#   #anova reactive
#   respanov <-reactive({
#     input$responseVar
#   })
#   factanov <-reactive({
#     vector = c()
#     y = c(vector, input$factors)
#     return(y)
#   })
# 
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
#     reset("DataSet")
#     
#     reset("TimeFactor")
#     reset("responseVar1")
#     reset("factors1")
#     reset("responseVar")
#     reset("factors")
#     reset("responseVarHeat")
#     reset("factorH1")
#     reset("factorH2")
#     reset("factorH3")
#     reset("thresSR")
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
