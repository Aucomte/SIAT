
header <- dashboardHeader(title = "Symptoms length Analysis", titleWidth = 300)

sidebar <- dashboardSidebar(
  width = 150,
  sidebarMenu(
    menuItem("Table", tabName = "tabHome", icon = icon("book-open")),
    menuItem("Mean/Sd", tabName = "Mean", icon = icon("calculator")),
    menuItem("Anova", tabName = "Anova", icon = icon("calculator")),
    menuItem("ACP", tabName = "ACP", icon = icon("calculator")),
    menuItem("Heatmap", tabName = "Heatmap", icon = icon("eye")),
    menuItem("Visu", tabName = "Visu", icon = icon("eye")),
    menuItem("Evolution", tabName = "Evolution", icon = icon("eye")),
    menuItem("Debug", tabName = "tabDebug", icon = icon("dashboard"))
  )
)

body <- dashboardBody(
  #includeCSS('www/styles.css'),
  useShinyjs(),
  useShinyFeedback(),
  tabItems(
    tabItem(
      tabName ="tabHome",
      fluidRow(
        box(
          fileInput("file1", "CSV File", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")) %>% 
                 helper(icon = "question",
                        type = "markdown",
                        content = "file1"),
          pickerInput(inputId='responseVar0', label ='Choose the response variable', ""),
          checkboxInput("header", "Header", TRUE)
        ),
        box(
          radioButtons('sep', 'Separator',
                            c(Comma=',',
                              Semicolon=';',
                              Tab='\t'),
                            ';'),
          radioButtons('dec', 'decimal',
                           c(Comma=',',
                             Dot='.'),
                            ',')
        )
      ),
      fluidRow(
        box(width = 12,
          DTOutput(outputId = "DataSet")
        )
      ),
      fluidRow(
        box(width = 12,
            verbatimTextOutput(outputId = "ShapiroWilk")
        )
      )
    ),
    
    tabItem(
      tabName ="Mean",
      fluidRow(
        box(width = 12,
          pickerInput(inputId='responseVar1', label ='Choose the response variable', ""),
          pickerInput(inputId='factors1', label ='Choose the factors', "", multiple = TRUE)
        )
      ),
      fluidRow(
        box(width = 12,
          DTOutput(outputId = "moyenne")
        )
      )
    ),
    
    tabItem(
      tabName ="Anova",
      fluidRow(
        box(width = 12,
            pickerInput(inputId='responseVar', label ='Choose the response variable', ""),
            pickerInput(inputId='factors', label ='Choose the factors', "", multiple = TRUE)
        )
      ),
      fluidRow(
        box(width = 12,
            verbatimTextOutput(outputId = "anov")
        ),
        fluidRow(
          box(width = 12, 
            plotOutput(outputId = "anovplot", height = "1000px")
          )
        )
      )
    ),
    tabItem(
      tabName = "ACP",
      fluidRow(
        box(width = 12,
            pickerInput(inputId='respacp', label ='Choose the response variable', ""),
            column(width = 6,
              pickerInput(inputId='individual', label ='individuals', "")
            ),
            column(width = 6,
              pickerInput(inputId='variable', label ='variables', "")
            ),
            column(width = 3,
              checkboxInput("reduct", "reduct variable", FALSE)
            ),
            column(width = 3,
              checkboxInput("center", "center variable", FALSE)
            ),
            column(width = 6,
              pickerInput(inputId='axis', label ='Number of axis', selected = 2, choices = c(1,2,3,4,5))
            )
        )
      ),
      fluidRow(
        box(width = 6,
            plotOutput(outputId = "indPlot")
        ),
        box(width = 6,
            plotOutput(outputId = "varPlot")
        )
      ),
      fluidRow(
        box(width = 6,
            plotOutput(outputId = "vpPlot")
        ),
        box(width = 6,
            plotOutput(outputId = "bothPlot")
        )
      )
      # ,
      # fluidRow(
      #   box(width = 12,
      #     downloadButton("downloadRMD", "Download Analysis")
      #   )
      # )
    ),
    tabItem(
      tabName = "Heatmap",
        fluidRow(
          box(width = 12,
             pickerInput(inputId='responseVarHeat', label ='Choose the response variable', ""),
             pickerInput(inputId='factorH1', label ='Choose the first factor', ""),
             pickerInput(inputId='factorH2', label ='Choose the second factor', ""),
             
             column(width=3,
                HTML("Clusterisation : ")
             ),
             column(width=3,
                checkboxInput("column", "col", TRUE)
             ),
             column(width=3,
                checkboxInput("row", "row", TRUE)
             )  
          )
        ),
      fluidRow(
        box(width = 12,
          plotOutput(outputId = "heatplot")
        )
      ),
      fluidRow(
        box(width = 12,
           sliderInput(inputId="thresSR", label = "Threshold of sensibility/resistance", value = 12, min=0, max=20, step=1)
          )
        ),
      fluidRow(
        box(width = 12,
            column(width = 6,
                HTML("
                 <br>
                 <p align='left'>
                 <img src='legendSR.png' width='20%' height='20%'>
                 </p>
                ")
            ),
            column(width = 12,
              plotOutput(outputId = "heatplotSR")
            )
        )
      )
    ),
    tabItem(
      tabName = "Visu",
      fluidRow(
        box(width=12,
          pickerInput(inputId='responseVarPG', label ='Choose the response variable', ""),
          pickerInput(inputId='factorPG1', label ='Choose the first factor', ""),
          pickerInput(inputId='factorPG2', label ='Choose the second factor', ""),
          pickerInput(inputId='factorPG3', label ='Choose the third factor', "")
        )
      ),
      fluidRow(
        box(width = 12,
          plotOutput(outputId = "PrettyG", height = "1000px")
        )
      )
    ),
    tabItem(
      tabName = "Evolution",
        fluidRow(
          box(width = 12,
            column(width = 6,
              pickerInput(inputId='responseVarT', label ='Choose the response variable', "")
            ),
            column(width = 6,
              pickerInput(inputId='factorT1', label ='Choose the Time factor (x)', "")
            ),
            column(width = 6,
              pickerInput(inputId='factorT2', label ='Choose the y', "")
            ),
            column(width = 6,
              pickerInput(inputId='factorT3', label ='Choose the z', "")
            )
        )
      ),
      fluidRow(
        box(width = 12,
          plotOutput(outputId = "TimePlot", height = "1000px")  
        )
      )
    )
    # ,
    # tabItem(
    #   tabName = "tabDebug",
    #   h1("DEBUG"),
    #   verbatimTextOutput("debug")
    # )
  )
)

shinyUI(
  dashboardPage(title = "symptom", skin = "yellow", header, sidebar, body)
)

