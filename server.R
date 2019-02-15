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
    dec = ",",
    outVar = NULL,
    filtered_data = NULL,
    
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
    
    # panel 5-2 : Heatmap2
    
    respheat2 = NULL,
    factH12 = NULL,
    factH22 = NULL,
    factH32 = NULL,
    
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
  
  #download file test
  output$downloadData <- downloadHandler(
    filename = "dataExemple.csv",
    content = function(file) {
      file.copy("www/dataExemple.csv", file)
    },
    contentType = "csv"
  )
  
  observeEvent(input$DataSet_rows_all, {
    sr$filtered_data <- input$DataSet_rows_all
  })
  observeEvent(input$sep, {
    sr$sep = input$sep
  })
  observeEvent(input$dec, {
    sr$dec = input$dec
    if(sr$booTable == 1) {
      #myCSV <- reactiveFileReader(100, session, input$file1$datapath, read.csv, header = TRUE, sep=sr$sep, dec=sr$dec, fill =TRUE)
      myCSV <- reactiveFileReader(100, session, input$file1$datapath, read.table, header = TRUE, sep=sr$sep, dec=sr$dec, fill =TRUE)
      
      sr$table = as.data.frame(myCSV())
    }
  })
  observeEvent(input$file1, {
    sr$booTable = 1
  })
  observeEvent(input$responseVar0, {
    sr$resp0 = input$responseVar0
  })
  
  observeEvent(c(
    input$file1,
    input$sep),
    {
    if(sr$booTable == 1) {
      myCSV <- reactiveFileReader(100, session, input$file1$datapath, read.csv, header = TRUE, sep=sr$sep, dec=sr$dec, fill =TRUE)
      sr$table = as.data.frame(myCSV())
      
        sr$outVar = colnames(myCSV())
        
        updateSelectInput(session, inputId = "responseVar0", choices = c("",sr$outVar))
        
        updateSelectInput(session, inputId = "responseVar1", choices = sr$outVar, selected = sr$outVar[length(sr$outVar)])
        updateSelectInput(session, inputId = "factors1", choices = sr$outVar, selected = sr$outVar[1])
        
        updateSelectInput(session, inputId = "responseVar", choices = sr$outVar, selected = sr$outVar[length(sr$outVar)])
        updateSelectInput(session, inputId = "factors", choices = sr$outVar, selected = sr$outVar[1])
        
        updateSelectInput(session, inputId = "responseVarHeat", choices = sr$outVar, selected = sr$outVar[length(sr$outVar)])
        updateSelectInput(session, inputId = "factorH1", choices = sr$outVar, selected = sr$outVar[1])
        updateSelectInput(session, inputId = "factorH2", choices = sr$outVar, selected = sr$outVar[1])
        
        updateSelectInput(session, inputId = "responseVarHeat2", choices = sr$outVar, selected = sr$outVar[length(sr$outVar)])
        updateSelectInput(session, inputId = "factorH12", choices = sr$outVar, selected = sr$outVar[1])
        updateSelectInput(session, inputId = "factorH22", choices = sr$outVar, selected = sr$outVar[1])
        updateSelectInput(session, inputId = "factorH32", choices = c("None", sr$outVar), selected = "None")
        
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
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"
          )
        )
      )
    )
    output$filtered_DataSet <- DT::renderDataTable(
      if (!is.null(sr$filtered_data)){
        DT::datatable(
          sr$table[sr$filtered_data,],
          extensions = 'Buttons', 
          options = list(
            dom = 'Blfrtip', 
            buttons = list(
              'copy', list(
               extend = "collection"
              , text = "Download entire dataset",
              action = DT::JS("function ( e, dt, node, config ) { Shiny.setInputValue('test', true, {priority: 'event'});}")
            )
          ),
          lengthMenu = list( c(10, 20, -1), c(10, 20, "All")),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
              "}"
            )
          )
        )
      }
    )
    
    output$download1 <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv( sr$table[sr$filtered_data,], file)
      }
    )
    myModal <- function() {
      div(id = "test",
          modalDialog(downloadButton("download1","Download as csv"),
                      easyClose = TRUE, title = "Download Table")
      )
    }
    
    observeEvent(input$test, {
      showModal(myModal())
    })
    
  observe({
    if(sr$booTable == 1) {
      if(!is.null(sr$resp0) && (sr$resp0 != "") && is.numeric(sr$table[[sr$resp0]])){
        output$ShapiroWilk <- renderPrint({
          normality(sr$table[sr$filtered_data,], sr$resp0)
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
    if(sr$booTable==1 && is.numeric(sr$table[[sr$resp1]])){
      output$moyenne <- renderDT({
        datatable(Data_Moyenne(sr$table[sr$filtered_data,],sr$resp1,sr$fact1))
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
        anov(sr$table[sr$filtered_data,],sr$respanov,sr$factanov)
      })
      PlotAnov <- reactive({
        anovplot(sr$table[sr$filtered_data,],sr$respanov,sr$factanov)
      })
      output$anovplot <- renderPlot({
        PlotAnov()
      })
      # output$downloadAnov <- downloadHandler(
      #   filename = function() {
      #     "outputAnova.png"
      #     },
      #   content = function(file) {
      #     png(file)
      #     print(PlotAnov())
      #   },
      #   contentType = 'image/png'
      # )
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
      if(length(unique(sr$table[sr$filtered_data,][[sr$individual]])) > 1 && length(unique(sr$table[sr$filtered_data,][[sr$variable]])) > 1){
        out = adeACP(sr$table[sr$filtered_data,], sr$respacp, sr$individual, sr$variable, sr$center, sr$reduct, sr$axis)
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
       updateSliderInput(session, inputId = "thresSR", value = maxMean(sr$table[sr$filtered_data,],sr$respheat,sr$factH1,sr$factH2)/2, min=0, max=maxMean(sr$table[sr$filtered_data,],sr$respheat,sr$factH1,sr$factH2), step=1) 
        output$heatplot <- renderPlot({
          heatplot(sr$table[sr$filtered_data,],sr$respheat,sr$factH1,sr$factH2, sr$dendorow, sr$dendocol)
        })
        output$heatplotSR <- renderPlot({
          heatplotSR(sr$table[sr$filtered_data,],sr$slidethresSR,sr$respheat,sr$factH1,sr$factH2)
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
  
  # panel 5-2 : Heatmap2
  
  observeEvent(input$responseVarHeat2, {
    sr$respheat2 = input$responseVarHeat2
  })
  observeEvent(input$factorH12, {
    sr$factH12 = input$factorH12
  })
  observeEvent(input$factorH22, {
    sr$factH22 = input$factorH22
  })
  observeEvent(input$factorH32, {
    sr$factH32 = input$factorH32
  })
  observe({
    if(sr$booTable==1 && is.numeric(sr$table[[sr$respheat]])){
      if(!is.null(sr$factH12) && sr$factH12 != "" && !is.null(sr$factH22) && sr$factH22 != "" && !is.null(sr$factH32) && sr$factH32 != ""){
        output$heatplot2 <- renderPlot({
          heatplot2(sr$table[sr$filtered_data,],sr$respheat2,sr$factH12,sr$factH22, sr$factH32)
        })
      }
    }
    else{
      output$heatplot2 <- renderPlot({
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
        NiceGraph(sr$table[sr$filtered_data,],sr$responseVarPG,sr$factorPG1,sr$factorPG2,sr$factorPG3)
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
        GraphTime(sr$table[sr$filtered_data,],sr$TimeFactor,sr$responseVarT,sr$factorT2,sr$factorT3,sr$factorT4,sr$TimeSelect)
      })
    }
    else{
      output$TimePlot <- renderPlot({
        NULL
      })
    }
  })
}

