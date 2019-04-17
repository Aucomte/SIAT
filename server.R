#server options : 3 Mo maximum.
#options(shiny.maxRequestSize = 3*1024^2)

server <-function(input,output,session){

  observe_helpers() # active help icon
  
#liste des réactifs
  
  sr <- reactiveValues(
    
    # panel 1 : lecture de la table
    
    booTable = 0,
    table = NULL,
    tableF = NULL,
    resp0 = NULL,
    sep = ";",
    dec = ",",
    outVar = NULL,
    filtered_data = NULL,
    selected_row = NULL,
    log = 0,
    
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
    dendocol2 = TRUE,
    dendorow2 = TRUE,
    categories = 2,
    S = NULL,
    outheatH1 = NULL,
    outheatH2 = NULL,
    outheattab = NULL,
    outheatx = NULL,
    
    #2
    respheat2 = NULL,
    factH21 = NULL,
    factH22 = NULL,
    
    # seuils
    thresSR21 = NULL,
    thresSR31= NULL,
    thresSR32= NULL,
    thresSR41= NULL,
    thresSR42= NULL,
    thresSR43= NULL,
    thresSR51= NULL,
    thresSR52= NULL,
    thresSR53= NULL,
    thresSR54= NULL,
    thresSR61= NULL,
    thresSR62= NULL,
    thresSR63= NULL,
    thresSR64= NULL,
    thresSR65= NULL,
    
    # panel 6 : Boxplot
    
    responseVarPG = NULL,
    factorPG1 = NULL,
    factorPG2 = NULL,
    factorPG3 = NULL,
    
    # panel 7 : Barplot
    
    responseVarBar = NULL,
    factorBar1 = NULL,
    factorBar2 = NULL,
    factorBar3 = NULL,
    
    # panel 8 : Time
    
    responseVarT= NULL,
    TimeFactor = NULL,
    TimeSelect = NULL,
    factorT2 = NULL,
    factorT3 = NULL,
    factorT4 = NULL
  )
  
  # panel 1 : lecture de la table
  
  #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #download file test
  output$downloadData <- downloadHandler(
    filename = "DataTest.csv",
    content = function(filename) {
      file.copy("www/DataTest2.csv", filename)
    },
    contentType = "csv"
  )
  
  observeEvent(input$logTrans,{
    if(input$logTrans == TRUE){
      sr$log = 1
    }
    else{
      sr$log = 0
    }
  })
  ## FILTRES
  observeEvent(input$DataSet_rows_all, {
    sr$filtered_data <- input$DataSet_rows_all
  })
  ## LIGNES SELECTIONNEES
  observeEvent(input$DataSet_rows_selected, {
    sr$selected_row = input$DataSet_rows_selected
  })
  
  observeEvent(input$sep, {
    sr$sep = input$sep
  })
  observeEvent(input$dec, {
    sr$dec = input$dec
    if(sr$booTable == 1) {
      myCSV <- reactiveFileReader(100, session, input$file1$datapath, read.csv, header = TRUE, sep=sr$sep, dec=sr$dec, fill =TRUE)
      sr$table = as.data.frame(myCSV())
    }
  })
  observeEvent(input$file1, {
    sr$booTable = 1
  })
  observeEvent(c(input$responseVar0, input$sep), ignoreInit = TRUE, {
    sr$resp0 = input$responseVar0
      if(sr$booTable == 1) {
          updateSelectInput(session, inputId = "responseVar1", choices = sr$outVar, selected = sr$resp0)
          updateSelectInput(session, inputId = "factors1", choices = sr$outVar, selected = "")
          
          updateSelectInput(session, inputId = "responseVar", choices = sr$outVar, selected = sr$resp0)
          updateSelectInput(session, inputId = "factors", choices = sr$outVar, selected = sr$outVar[1])
          
          updateSelectInput(session, inputId = "responseVarHeat", choices = sr$outVar, selected = sr$resp0)
          updateSelectInput(session, inputId = "factorH1", choices = sr$outVar, selected = "")
          updateSelectInput(session, inputId = "factorH2", choices = sr$outVar, selected = "")
          #updateSelectInput(session, inputId = "factorH3", choices = c("None", sr$outVar), selected = "None")
          
          updateSelectInput(session, inputId = "responseVarHeat2", choices = sr$outVar, selected = sr$resp0)
          updateSelectInput(session, inputId = "factorH21", choices = sr$outVar, selected = "")
          updateSelectInput(session, inputId = "factorH22", choices = sr$outVar, selected = "")
          #updateSelectInput(session, inputId = "factorH3", choices = c("None", sr$outVar), selected = "None")
          
          updateSelectInput(session, inputId = "respacp", choices = sr$outVar, selected = sr$resp0)
          updateSelectInput(session, inputId = "individual", choices = sr$outVar, selected = "")
          updateSelectInput(session, inputId = "variable", choices = sr$outVar, selected = "")
          
          updateSelectInput(session, inputId = "responseVarPG", choices = sr$outVar, selected = sr$resp0)
          updateSelectInput(session, inputId = "factorPG1", choices = sr$outVar, selected = sr$outVar[1])
          updateSelectInput(session, inputId = "factorPG2", choices = sr$outVar, selected = sr$outVar[2])
          updateSelectInput(session, inputId = "factorPG3", choices = c("None", sr$outVar), selected = "None")
          
          updateSelectInput(session, inputId = "responseVarBar", choices = sr$outVar, selected = sr$resp0)
          updateSelectInput(session, inputId = "factorBar1", choices = sr$outVar, selected = sr$outVar[1])
          updateSelectInput(session, inputId = "factorBar2", choices = c("None", sr$outVar), selected = "None")
          updateSelectInput(session, inputId = "factorBar3", choices = sr$outVar, selected = sr$outVar[2])
          
          updateSelectInput(session, inputId = "responseVarT", choices = sr$outVar, selected = sr$resp0)
          updateSelectInput(session, inputId = "TimeFactor", choices = sr$outVar, selected = sr$outVar[1])
          updateSelectInput(session, inputId = "factorT2", choices = c("None", sr$outVar), selected = "None")
          updateSelectInput(session, inputId = "factorT3", choices = c("None", sr$outVar), selected = "None")
          updateSelectInput(session, inputId = "factorT4", choices = c("None", sr$outVar), selected = "None")
      }
  })
  
  observeEvent(c(
    input$file1,
    input$sep), ignoreInit = TRUE,{
    if(sr$booTable == 1) {
      myCSV <- reactiveFileReader(100, session, input$file1$datapath, read.csv, sep = sr$sep, dec=sr$dec, fill =TRUE)
      sr$table = as.data.frame(myCSV())
        sr$outVar = colnames(myCSV())
        updateSelectInput(session, inputId = "responseVar0", choices = c("",sr$outVar))
    }
  })
 
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    output$DataSet <- DT::renderDataTable(
      DT::datatable(
        sr$table, 
        filter = list(position = 'top', clear = TRUE, plain = FALSE), 
        options = list(
          scrollX = TRUE,
          dom = 'Blfrtip',
          lengthMenu = list( c(10, 20, -1), c(10, 20, "All")),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#3C3C3C', 'color': '#fff'});",
            "}"
          )
        )
      )
    )
    output$filtered_DataSet <- DT::renderDataTable( server = FALSE, {
        if (!is.null(sr$filtered_data)){
          DT::datatable(
            sr$tableF,
            extensions = 'Buttons', 
            options = list(
              dom = 'Blfrtip', 
              buttons = list(
                'copy', 
                'print',
                list(
                 extend = "collection", 
                 text = "Download entire dataset",
                 #buttons = c("csv","excel","pdf")
                 action = DT::JS("function ( e, dt, node, config ) { Shiny.setInputValue('test', true, {priority: 'event'});}")
              )
            ),
            lengthMenu = list( c(10, 20, -1), c(10, 20, "All")),
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#3C3C3C', 'color': '#fff'});",
                "}"
              )
            )
          )
        }
      }
    )
    
    output$download1 <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.table(sr$tableF, file, sep="\t", dec= ",", col.names = T, row.names = F)
      }
    )
    myModal <- function() {
      div(id = "test",
         modalDialog(downloadButton("download1","Download as csv"),easyClose = TRUE, title = "Download Table")
      )
    }
    observeEvent(input$test, {
      showModal(myModal())
    })
    
  #-------------------------------------------------------------------------------------------------------
  observeEvent(c(sr$resp0,sr$table,sr$filtered_data), ignoreInit = TRUE,{
    if(sr$booTable == 1) {
      if(!is.null(sr$resp0) && (sr$resp0 != "") && is.numeric(sr$table[[sr$resp0]])){
        
        #création de la table filtrée
        
        sr$tableF = sr$table[sr$filtered_data,]
        
        if(sr$log == 1){
          isolate({
            sr$tableF[[sr$resp0]] = log(sr$table[sr$filtered_data,][[sr$resp0]])
          })
        }
        else{
          isolate({
            sr$tableF[[sr$resp0]] = sr$table[sr$filtered_data,][[sr$resp0]]
          })
        }
        output$ShapiroWilk <- renderPrint({
          normality(sr$tableF, sr$resp0)
        })
      }
      else{
        output$ShapiroWilk <- renderText({
          "Check your inputs variables please"
        })
      }
      
      if(!is.null(sr$resp0) && (sr$resp0 != "")){
        if(is.numeric(sr$table[[sr$resp0]])){
          output$CheckPoint <- renderPrint({
            "Looks like everything is fine now ! :)"
          })
        }
        else{
          output$CheckPoint <- renderText({
            "The Response Variable is not numeric !
Change the decimal parameter or the response Variable!"
          })
        }
      }
      else{
        output$CheckPoint <- renderText({
          "First of all, choose your separator until your columns look good
Then, you need to choose a quantitative response variable (ex: Lenght)"
        })
      }
    }
    else{
      output$CheckPoint <- renderText({
        "You need to upload a CSV file"
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
        datatable(
          Data_Moyenne(sr$tableF,sr$resp1,sr$fact1),
            extensions = 'Buttons', 
            options = list(
              dom = 'Blfrtip', 
              buttons = list(
                'copy', 
                'print',
                list(
                  extend = "collection", 
                  text = "Download entire dataset",
                  #buttons = c("csv","excel","pdf")
                  action = DT::JS("function ( e, dt, node, config ) { Shiny.setInputValue('test', true, {priority: 'event'});}")
                )
              ),
              lengthMenu = list( c(10, 20, -1), c(10, 20, "All")),
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#3C3C3C', 'color': '#fff'});",
                "}"
              )
            )
          )
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
  
  PlotAnov <- function(){
    anovplot(sr$tableF,sr$respanov,sr$factanov)
  }
  
  output$downloadAnov <- downloadHandler(
    filename = "outputAnova.png",
    content = function(file) {
      png(file, width = 2000, height = 2000, res = 300)
      print(PlotAnov())
      dev.off()
    },
    contentType = 'image/png'
  )
  observe({
    if(sr$booTable==1){
      output$anov <- renderPrint({
        anov(sr$tableF,sr$respanov,sr$factanov)[[1]]
      })
      output$Tukey <- renderPrint({ 
        anov(sr$tableF,sr$respanov,sr$factanov)[[2]]
      })
      output$TukLetter <- renderDT({
        datatable(
          anov(sr$tableF,sr$respanov,sr$factanov)[[3]],
            extensions = 'Buttons', 
            options = list(
              dom = 'Blfrtip', 
              buttons = list(
                'copy', 
                'print',
                list(
                  extend = "collection", 
                  text = "Download entire dataset",
                  #buttons = c("csv","excel","pdf")
                  action = DT::JS("function ( e, dt, node, config ) { Shiny.setInputValue('test', true, {priority: 'event'});}")
                )
              ),
              lengthMenu = list( c(10, 20, -1), c(10, 20, "All")),
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#3C3C3C', 'color': '#fff'});",
                "}"
              )
            )
          )
      })

      renderPrint({ 
        anov(sr$tableF,sr$respanov,sr$factanov)[[3]]
      })
      output$anovplot <- renderPlot({
        PlotAnov()
      })
    }
    else{
      output$anov <- renderPrint({ 
        "Can't print anything. Check your inputs."
      })
      output$Tukey <- renderPrint({ 
        "Can't print anything. Check your inputs."
      })
      output$anovplot <-  renderPlot({ 
        NULL
      })
    }
  })
  
  # panel 4 : ACP

  outind <- function(){
    ACP = adeACP(sr$tableF, sr$respacp, sr$individual, sr$variable, sr$center, sr$reduct, sr$axis)
    return(ACP$ind)
  }
  outvar <- function(){
    ACP = adeACP(sr$tableF, sr$respacp, sr$individual, sr$variable, sr$center, sr$reduct, sr$axis)
    return(ACP$var)
  }
  outvp <- function(){
    ACP = adeACP(sr$tableF, sr$respacp, sr$individual, sr$variable, sr$center, sr$reduct, sr$axis)
    return(ACP$VP)
  }
  outboth <- function(){
    ACP = adeACP(sr$tableF, sr$respacp, sr$individual, sr$variable, sr$center, sr$reduct, sr$axis)
    return(ACP$both)
  }
  
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
    if(sr$booTable==1 && length(unique(sr$table[[sr$individual]])) > 1 && length(unique(sr$table[[sr$variable]])) > 1){
      if(length(unique(sr$tableF[[sr$individual]])) > 1 && length(unique(sr$tableF[[sr$variable]])) > 1){
        out = adeACP(sr$tableF, sr$respacp, sr$individual, sr$variable, sr$center, sr$reduct, sr$axis)
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
  output$downloadACPind <- downloadHandler(
    filename = "outputACPind.png",
    content = function(filename) {
      png(filename)
      print(outind())
      dev.off()
    },
    contentType = 'image/png'
  )
  output$downloadACPVar <- downloadHandler(
    filename = "outputACPvar.png",
    content = function(filename) {
      png(filename)
      print(outvar())
      dev.off()
    },
    contentType = 'image/png'
  )
  output$downloadACPVP <- downloadHandler(
    filename = "outputACPVP.png",
    content = function(filename) {
      png(filename)
      print(outvp())
      dev.off()
    },
    contentType = 'image/png'
  )
  output$downloadACPBoth <- downloadHandler(
    filename = "outputACPBoth.png",
    content = function(filename) {
      png(filename)
      print(outboth())
      dev.off()
    },
    contentType = 'image/png'
  )
  
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
  
  observeEvent(input$responseVarHeat2, {
    sr$respheat2 = input$responseVarHeat2
  })
  observeEvent(input$factorH21, {
    sr$factH21 = input$factorH21
  })
  observeEvent(input$factorH22, {
    sr$factH22 = input$factorH22
  })
  
  observeEvent(input$row, {
    sr$dendorow = input$row
  })
  observeEvent(input$column, {
    sr$dendocol = input$column
  })
  observeEvent(input$row2, {
    sr$dendorow2 = input$row2
  })
  observeEvent(input$column2, {
    sr$dendocol2 = input$column2
  })
  observeEvent(input$categories, {
    sr$categories = input$categories
  })
  observeEvent(input$thresSR21, {
    sr$thresSR21 = input$thresSR21
  })
  observeEvent(input$thresSR31, {
    sr$thresSR31 = input$thresSR31
    updateSliderInput(session, inputId = "thresSR32", min=sr$thresSR31)
  })
  observeEvent(input$thresSR32, {
    sr$thresSR32 = input$thresSR32
  })
  observeEvent(input$thresSR41, {
    sr$thresSR41 = input$thresSR41
    updateSliderInput(session, inputId = "thresSR42", min=sr$thresSR41)
  })
  observeEvent(input$thresSR42, {
    sr$thresSR42 = input$thresSR42
    updateSliderInput(session, inputId = "thresSR43", min=sr$thresSR42)
  })
  observeEvent(input$thresSR43, {
    sr$thresSR43 = input$thresSR43
  })
  observeEvent(input$thresSR51, {
    sr$thresSR51 = input$thresSR51
    updateSliderInput(session, inputId = "thresSR52", min=sr$thresSR51)
  })
  observeEvent(input$thresSR52, {
    sr$thresSR52 = input$thresSR52
    updateSliderInput(session, inputId = "thresSR53", min=sr$thresSR52)
  })
  observeEvent(input$thresSR53, {
    sr$thresSR53 = input$thresSR53
    updateSliderInput(session, inputId = "thresSR54", min=sr$thresSR53)
  })
  observeEvent(input$thresSR54, {
    sr$thresSR54 = input$thresSR54
  })
  observeEvent(input$thresSR61, {
    sr$thresSR61 = input$thresSR61
    updateSliderInput(session, inputId = "thresSR62", min=sr$thresSR61)
  })
  observeEvent(input$thresSR62, {
    sr$thresSR62 = input$thresSR62
    updateSliderInput(session, inputId = "thresSR63", min=sr$thresSR62)
  })
  observeEvent(input$thresSR63, {
    sr$thresSR63 = input$thresSR63
    updateSliderInput(session, inputId = "thresSR64", min=sr$thresSR63)
  })
  observeEvent(input$thresSR64, {
    sr$thresSR64 = input$thresSR64
    updateSliderInput(session, inputId = "thresSR65", min=sr$thresSR64)
  })
  observeEvent(input$thresSR65, {
    sr$thresSR65 = input$thresSR65
  })
  
  observeEvent(c(sr$tableF,sr$respheat,sr$factH1,sr$factH2, sr$categories), {
    if(sr$booTable==1){
      if(!is.null(sr$factH1) && !is.null(sr$factH2) && sr$factH1 != "" && sr$factH2 != ""){
        #update des sliders
        if(sr$categories == 2){
          updateSliderInput(session, inputId = "thresSR21", min=0, max=maxMean(sr$tableF,sr$respheat,sr$factH1,sr$factH2), step=1) 
        }
        else if(sr$categories == 3){
          updateSliderInput(session, inputId = "thresSR31", min=0, max=maxMean(sr$tableF,sr$respheat,sr$factH1,sr$factH2), step=1) 
          updateSliderInput(session, inputId = "thresSR32", min=sr$thresSR31, max=maxMean(sr$tableF,sr$respheat,sr$factH1,sr$factH2), step=1)
        }
        else if(sr$categories == 4){
          updateSliderInput(session, inputId = "thresSR41", min=0, max=maxMean(sr$tableF,sr$respheat,sr$factH1,sr$factH2), step=1)
          updateSliderInput(session, inputId = "thresSR42", min=sr$thresSR41, max=maxMean(sr$tableF,sr$respheat,sr$factH1,sr$factH2), step=1) 
          updateSliderInput(session, inputId = "thresSR43", min=sr$thresSR42, max=maxMean(sr$tableF,sr$respheat,sr$factH1,sr$factH2), step=1)
        }
        else if(sr$categories == 5){
          updateSliderInput(session, inputId = "thresSR51", min=0, max=maxMean(sr$tableF,sr$respheat,sr$factH1,sr$factH2), step=1) 
          updateSliderInput(session, inputId = "thresSR52", min=sr$thresSR51, max=maxMean(sr$tableF,sr$respheat,sr$factH1,sr$factH2), step=1) 
          updateSliderInput(session, inputId = "thresSR53", min=sr$thresSR52, max=maxMean(sr$tableF,sr$respheat,sr$factH1,sr$factH2), step=1)           
          updateSliderInput(session, inputId = "thresSR54", min=sr$thresSR53, max=maxMean(sr$tableF,sr$respheat,sr$factH1,sr$factH2), step=1)
        }
        else if(sr$categories == 6){
          updateSliderInput(session, inputId = "thresSR61", min=0, max=maxMean(sr$tableF,sr$respheat,sr$factH1,sr$factH2), step=1) 
          updateSliderInput(session, inputId = "thresSR62", min=sr$thresSR61, max=maxMean(sr$tableF,sr$respheat,sr$factH1,sr$factH2), step=1) 
          updateSliderInput(session, inputId = "thresSR63", min=sr$thresSR62, max=maxMean(sr$tableF,sr$respheat,sr$factH1,sr$factH2), step=1)           
          updateSliderInput(session, inputId = "thresSR64", min=sr$thresSR63, max=maxMean(sr$tableF,sr$respheat,sr$factH1,sr$factH2), step=1)
          updateSliderInput(session, inputId = "thresSR65", min=sr$thresSR64, max=maxMean(sr$tableF,sr$respheat,sr$factH1,sr$factH2), step=1) 
        }
      }
    }
  })

  observeEvent(input$submitCAT2,{
    sr$S = c(sr$thresSR21)
  })
  observeEvent(input$submitCAT3,{
    sr$S = c(sr$thresSR31,sr$thresSR32)
  })
  observeEvent(input$submitCAT4,{
    sr$S = c(sr$thresSR41,sr$thresSR42,sr$thresSR43)
  })
  observeEvent(input$submitCAT5,{
    sr$S = c(sr$thresSR51,sr$thresSR52,sr$thresSR53,sr$thresSR54)
  })
  observeEvent(input$submitCAT6,{
    sr$S = c(sr$thresSR61,sr$thresSR62,sr$thresSR63,sr$thresSR64,sr$thresSR65)
  })
  
  observeEvent(c(sr$tableF,sr$respheat,sr$factH1,sr$factH2, sr$dendorow, sr$dendocol),{
    if(sr$booTable==1){
      if(!is.null(sr$factH1) && !is.null(sr$factH2) && sr$factH1 != "" && sr$factH2 != ""){
        outheat = heatplot(sr$tableF,sr$respheat,sr$factH1,sr$factH2, sr$dendorow, sr$dendocol)
          sr$outheatH1= outheat$plot
          sr$outheatx = outheat$tab
        }
      }
    })
  observeEvent(c(input$submitCAT2,
                 input$submitCAT3,
                 input$submitCAT4,
                 input$submitCAT5,
                 input$submitCAT6), {
    if(sr$booTable==1){
      if(!is.null(sr$factH21) && !is.null(sr$factH22) && sr$factH21 != "" && sr$factH22 != ""){
        outheat = heatplot(sr$tableF,sr$respheat2,sr$factH21,sr$factH22, sr$dendorow, sr$dendocol)
        outheat2 = heatplot2(outheat$tab, sr$dendorow2, sr$dendocol2, sr$S)
          sr$outheatH2 = outheat2$plot
          sr$outheattab = outheat2$tab
      }
    }
  })
  
    output$heatplot <- renderPlotly({
      sr$outheatH1
    })
    output$heatplotSR <- renderPlotly({
      sr$outheatH2
    })
    output$tabsouches <- DT::renderDataTable(
      DT::datatable(
        sr$outheattab,
        filter = list(position = 'top', clear = TRUE, plain = FALSE),
        options = list(
          scrollX = TRUE,
          dom = 'Blfrtip',
          lengthMenu = list( c(10, 20, -1), c(10, 20, "All")),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#3C3C3C', 'color': '#fff'});",
            "}"
          )
        )
      )
    )  
  
  # panel 6 : Visu
  
  outVisu <- function(){
    x = NiceGraph(sr$tableF,sr$responseVarPG,sr$factorPG1,sr$factorPG2,sr$factorPG3)
    return(x)
  }
  
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
    if(sr$booTable==1){
      output$PrettyG <- renderPlotly({
        NiceGraph(sr$tableF,sr$responseVarPG,sr$factorPG1,sr$factorPG2,sr$factorPG3)
      })
    }
    else{
      output$PrettyG <- renderPlotly({
        NULL
      })
    }
  })
  output$downloadVisu <- downloadHandler(
    filename = "outputVisu.png",
    content = function(filename) {
      png(filename)
      print(outVisu())
      dev.off()
    },
    contentType = 'image/png'
  )
  # panel 7 : Barplot
  
  outBarPlot <- function(){
    x = vizBarplot(sr$tableF,sr$responseVarBar,sr$factorBar1,sr$factorBar2,sr$factorBar3)
    return(x)
  }
  
  observeEvent(input$responseVarBar,{
    sr$responseVarBar = input$responseVarBar
  })
  observeEvent(input$factorBar1,{
    sr$factorBar1 = input$factorBar1
  })
  observeEvent(input$factorBar2,{
    sr$factorBar2 = input$factorBar2
  })
  observeEvent(input$factorBar3,{
    sr$factorBar3 = input$factorBar3
  })
  observeEvent(input$factorBar4,{
    sr$factorBar4 = input$factorBar4
  })
  
  observe({
    if(sr$booTable==1){
      output$BarPlot <- renderPlot({
        vizBarplot(sr$tableF,sr$responseVarBar,sr$factorBar1,sr$factorBar2,sr$factorBar3)
      })
    }
    else{
      output$BarPlot <- renderPlot({
        NULL
      })
    }
  })
  output$downloadBarplot <- downloadHandler(
    filename = "outBarPlot.png",
    content = function(filename) {
      png(filename)
      print(outBarPlot())
      dev.off()
    },
    contentType = 'image/png'
  )
  
  # panel 8 : Time
  
  outTime <- function(){
    x = GraphTime(sr$tableF,sr$TimeFactor,sr$responseVarT,sr$factorT2,sr$factorT3,sr$factorT4,sr$TimeSelect)
    return(x)
  }
  
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
    if(sr$booTable==1){
      output$TimePlot <- renderPlotly({
        GraphTime(sr$tableF,sr$TimeFactor,sr$responseVarT,sr$factorT2,sr$factorT3,sr$factorT4,sr$TimeSelect)
      })
    }
    else{
      output$TimePlot <- renderPlotly({
        NULL
      })
    }
  })
  output$downloadEvol <- downloadHandler(
    filename = "outputTime.png",
    content = function(filename) {
      png(filename)
      print(outTime())
      dev.off()
    },
    contentType = 'image/png'
  )
  
  ## Panel REPORT
  
  output$downloadRMD <- downloadHandler(
    filename = "reportRMD.html",
    content = function(filename) {
      rmarkdown::render("www/report.Rmd")
      file.copy("www/report.html", filename, overwrite = TRUE)
    }
  )
  
  output$save <- downloadHandler(
    filename = "save",
    content = function(filename) {
      save.image(filename)
    }
  )
  
}


