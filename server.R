#server options : 3 Mo maximum.
#options(shiny.maxRequestSize = 3*1024^2)

server <-function(input,output,session){

  observe_helpers() # active help icon
  
#liste des réactifs
  
  sr <- reactiveValues(
    
    # panel 1 : lecture de la table
    
    booTable = 0,
    table = NULL,
    table2 = NULL,
    resp0 = NULL,
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
    
    respacp = NULL,
    individual = NULL,
    variable = NULL,
    center = FALSE,
    reduct = FALSE,
    axis = NULL,
    
    # panel 5 : Heatmap
    
    respheat = NULL,
    factH1 = NULL,
    factH2 = NULL,
    dendocol = TRUE,
    dendorow = TRUE,
    slidethresSH = NULL,
    
    # panel 6 : Visu
    
    responseVarPG = NULL,
    factorPG1 = NULL,
    factorPG2 = NULL,
    factorPG3 = NULL,
    
    # panel 7 : Time
    
    responseVarT= NULL,
    TimeFactor = NULL,
    TimeSelect = NULL,
    factorT2 = NULL,
    factorT3 = NULL,
    factorT4 = NULL
    
  )
  
  # panel 1 : lecture de la table

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
  
  # observeEvent(input$DataSet_state, {
  #   str(input$DataSet_state)
  #   k = sapply(input$DataSet_state$columns, function(x) x$search$search)
  #   k=dataTableProxy('DataSet', session)
  # })
  

  observeEvent(input$responseVar0, {
    sr$resp0 = input$responseVar0
  })
  
  observeEvent(c(
    input$file1,
    input$sep,
    input$dec,
    input$head),
    {
    if(sr$booTable == 1) {
      myCSV <- reactiveFileReader(100, session, input$file1$datapath, read.csv, header = sr$head, sep=sr$sep, dec=sr$dec, fill =TRUE)
      sr$table = as.data.frame(myCSV())
      
      # selected_row = input$DataSet_rows_all
      # print(selected_row)
      # if(length(selected_row) > 0 && length(selected_row) < nrow(sr$table)){
      #   sr$table2 = sr$table[selected_row,]
      # }
      # else{
      #   sr$table2 = sr$table
      # }
        sr$outVar = colnames(myCSV())
        
        updateSelectInput(session, inputId = "responseVar0", choices = sr$outVar)
        
        updateSelectInput(session, inputId = "responseVar1", choices = sr$outVar, selected = sr$outVar[length(sr$outVar)])
        updateSelectInput(session, inputId = "factors1", choices = sr$outVar, selected = sr$outVar[1])
        
        updateSelectInput(session, inputId = "responseVar", choices = sr$outVar, selected = sr$outVar[length(sr$outVar)])
        updateSelectInput(session, inputId = "factors", choices = sr$outVar, selected = sr$outVar[1])
        
        updateSelectInput(session, inputId = "responseVarHeat", choices = sr$outVar, selected = sr$outVar[length(sr$outVar)])
        updateSelectInput(session, inputId = "factorH1", choices = sr$outVar, selected = sr$outVar[1])
        updateSelectInput(session, inputId = "factorH2", choices = sr$outVar, selected = sr$outVar[1])
        
        updateSelectInput(session, inputId = "respacp", choices = sr$outVar, selected = sr$outVar[length(sr$outVar)])
        updateSelectInput(session, inputId = "individual", choices = sr$outVar, selected = sr$outVar[1])
        updateSelectInput(session, inputId = "variable", choices = sr$outVar, selected = sr$outVar[2])
        
        updateSelectInput(session, inputId = "responseVarPG", choices = sr$outVar, selected = sr$outVar[length(sr$outVar)])
        updateSelectInput(session, inputId = "factorPG1", choices = sr$outVar, selected = sr$outVar[1])
        updateSelectInput(session, inputId = "factorPG2", choices = sr$outVar, selected = sr$outVar[1])
        updateSelectInput(session, inputId = "factorPG3", choices = c("None", sr$outVar), selected = "None")
        
        updateSelectInput(session, inputId = "responseVarT", choices = sr$outVar, selected = sr$outVar[length(sr$outVar)])
        updateSelectInput(session, inputId = "TimeFactor", choices = sr$outVar, selected = sr$outVar[1])
        updateSelectInput(session, inputId = "factorT2", choices = c("None", sr$outVar), selected = sr$outVar[1])
        updateSelectInput(session, inputId = "factorT3", choices = c("None", sr$outVar), selected = sr$outVar[1])
        updateSelectInput(session, inputId = "factorT4", choices = c("None", sr$outVar), selected = "None")
      }
    })
        
  observe({
    if(sr$booTable == 1) {
      output$DataSet <- DT::renderDataTable(
        sr$table, 
        #class = "display nowrap compact", # style
        filter = "top", 
        options = list(
          stateSave = TRUE,
          scrollX = TRUE
          )
      )
    
      if(!is.null(sr$resp0) && (sr$resp0 != "") && is.numeric(sr$table[[sr$resp0]])){
        output$ShapiroWilk <- renderPrint({
          normality(sr$table, sr$resp0)
        })
      }
      else{
        output$ShapiroWilk <- renderPrint({
          "Check your inputs variables please"
        })
      }
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
    if(sr$booTable==1 && is.numeric(sr$table[[sr$resp1]])){
      output$moyenne <- renderDT({
        datatable(Data_Moyenne(sr$table,sr$resp1,sr$fact1))
      })
    }
    else{
      output$moyenne <- renderDT({ 
        NULL
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
    if(sr$booTable==1 && is.numeric(sr$table[[sr$respanov]])){
      output$anov <- renderPrint({
        anov(sr$table,sr$respanov,sr$factanov)
      })
      output$anovplot <- renderPlot({
        anovplot(sr$table,sr$respanov,sr$factanov)
      })
    }
    else{
      output$anov <- renderPrint({ 
        "Can't print anything. Check your inputs."
      })
      output$anovplot <- renderPlot({ 
        NULL
      })
    }
  })
  
  # panel 4 : ACP
  observeEvent(input$respacp,{
    sr$respacp = input$respacp
  })
  observeEvent(input$individual,{
    sr$individual = input$individual
  })
  observeEvent(input$variable,{
    sr$variable = input$variable
  })
  observeEvent(input$reduct,{
    sr$reduct = input$reduct
  })
  observeEvent(input$center,{
    sr$center = input$center
  })
  observeEvent(input$axis,{
    sr$axis = input$axis
  })
  observe({
    if(sr$booTable==1 && is.numeric(sr$table[[sr$respacp]]) && length(unique(sr$table[[sr$individual]])) > 1 && length(unique(sr$table[[sr$variable]])) > 1){
      out = adeACP(sr$table, sr$respacp, sr$individual, sr$variable, sr$center, sr$reduct, sr$axis)
      output$indPlot <- renderPlot({
        out$ind
      })
      output$varPlot <- renderPlot(
        out$var
      )
      output$vpPlot <- renderPlot({
        out$VP
      })
      output$bothPlot <- renderPlot(
        out$both
      )
    }
    else{
     output$indPlot <- renderPlot({
        NULL
      })
     output$varPlot <- renderPlot({
       NULL
     })
     output$vpPlot <- renderPlot({
       NULL
     })
     output$bothPlot <- renderPlot({
       NULL
     })
    }
  })
  
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
  observeEvent(input$row, {
    sr$dendorow = input$row
  })
  observeEvent(input$column, {
    sr$dendocol = input$column
  })
  observeEvent(input$thresSR, {
    sr$slidethresSR = input$thresSR
  })
  observe({
    if(sr$booTable==1 && is.numeric(sr$table[[sr$respheat]])){
      if(!is.null(sr$factH1) && !is.null(sr$factH2) && sr$factH1 != "" && sr$factH2 != ""){
       updateSliderInput(session, inputId = "thresSR", value = maxMean(sr$table,sr$respheat,sr$factH1,sr$factH2)/2, min=0, max=maxMean(sr$table,sr$respheat,sr$factH1,sr$factH2), step=1) 
        output$heatplot <- renderPlot({
          heatplot(sr$table,sr$respheat,sr$factH1,sr$factH2, sr$dendorow, sr$dendocol)
        })
        output$heatplotSR <- renderPlot({
          heatplotSR(sr$table,sr$slidethresSR,sr$respheat,sr$factH1,sr$factH2)
       })
      }
    }
    else{
      output$heatplot <- renderPlot({
        NULL
      })
      output$heatplotSR <- renderPlot({
        NULL
      })
    }
  })
  
  # panel 6 : Visu
  observeEvent(input$responseVarPG, {
    sr$responseVarPG = input$responseVarPG
  })
  observeEvent(input$factorPG1, {
    sr$factorPG1 = input$factorPG1
  })
  observeEvent(input$factorPG2, {
    sr$factorPG2 = input$factorPG2
  })
  observeEvent(input$factorPG3, {
    sr$factorPG3 = input$factorPG3
  })
  observe({
    if(sr$booTable==1 && is.numeric(sr$table[[sr$responseVarPG]])){
      output$PrettyG <- renderPlot({
        NiceGraph(sr$table,sr$responseVarPG,sr$factorPG1,sr$factorPG2,sr$factorPG3)
      })
    }
    else{
      output$PrettyG <- renderPlot({
        NULL
      })
    }
  })
  
  # panel 7 : Time
  observeEvent(input$responseVarT, {
    sr$responseVarT = input$responseVarT
  })
  observeEvent(input$TimeFactor, {
    sr$TimeFactor = input$TimeFactor
  })
  observeEvent(input$Time, {
    sr$TimeSelect = input$Time
  })
  observeEvent(input$factorT2, {
    sr$factorT2 = input$factorT2
  })
  observeEvent(input$factorT3, {
    sr$factorT3 = input$factorT3
  })
  observeEvent(input$factorT4, {
    sr$factorT4 = input$factorT4
  })
  observe({
    if(sr$booTable==1 && is.numeric(sr$table[[sr$responseVarT]])){
      output$TimePlot <- renderPlot({
        GraphTime(sr$table,sr$TimeFactor,sr$responseVarT,sr$factorT2,sr$factorT3,sr$factorT4,sr$TimeSelect)
      })
    }
    else{
      output$TimePlot <- renderPlot({
        NULL
      })
    }
  })
}

